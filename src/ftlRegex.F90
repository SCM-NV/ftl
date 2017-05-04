! Copyright (c) 2017  Robert Rüger
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License along
! with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.


module ftlRegexModule

   use iso_c_binding
   use ftlStringModule

   implicit none
   private


   type, public :: ftlRegex
      private

      character(len=:,kind=C_char), allocatable :: pattern
      integer(C_int)                            :: cflags = 0_C_int

      type(C_ptr)                     :: preg = C_NULL_ptr
      character(kind=C_char), pointer :: regdata(:) => null()

   contains
      private

      procedure            :: NewCopyOther
      procedure            :: NewRaw
      procedure            :: NewString
      generic  , public    :: New => NewRaw, NewString
      procedure            :: Compile
      procedure            :: PrintError

      procedure, public    :: Delete
      final                :: Finalizer

      procedure            :: NumMatchesRaw
      procedure            :: NumMatchesString
      generic  , public    :: NumMatches => NumMatchesRaw, NumMatchesString
      procedure            :: MatchFirstRaw
      procedure            :: MatchFirstString
      generic  , public    :: MatchFirst => MatchFirstRaw, MatchFirstString
      procedure            :: MatchRaw
      procedure            :: MatchString
      generic  , public    :: Match => MatchRaw, MatchString
      procedure            :: ReplaceRawWithRaw
      procedure            :: ReplaceStringWithRaw
      procedure            :: ReplaceRawWithString
      procedure            :: ReplaceStringWithString
      generic  , public    :: Replace => ReplaceRawWithRaw, ReplaceStringWithRaw, ReplaceRawWithString, ReplaceStringWithString

      procedure, pass(rhs) :: OpMatchesRaw
      procedure, pass(rhs) :: OpMatchesString
      generic  , public    :: operator(.matches.) => OpMatchesRaw, OpMatchesString

      generic  , public    :: assignment(=) => NewCopyOther

      procedure            :: OpEqualOther
      generic  , public    :: operator(==) => OpEqualOther
      procedure            :: OpUnequalOther
      generic  , public    :: operator(/=) => OpUnequalOther

   end type


   ! Constructor functions:

   interface ftlRegex
      module procedure NewRawConstr
      module procedure NewStringConstr
   end interface


   type, public :: ftlRegexGroup
      type(ftlString) :: text
      integer         :: begin = 0
      integer         :: end   = 0
   end type

   type, public :: ftlRegexMatch
      logical                          :: matches = .false.
      type(ftlString)                  :: text
      integer                          :: begin = 0
      integer                          :: end   = 0
      type(ftlRegexGroup), allocatable :: group(:)
   end type


   ! Interfaces for the functions, types, etc. in POSIX regex.h

#include "configure_ftlRegex.inc"

   type, bind(C) :: C_regmatch_t
      integer(C_int) :: rm_so
      integer(C_int) :: rm_eo
   end type

   interface

      function C_regcomp(preg, pattern, flags) result(status) bind(C,name='regcomp')
         import
         type(C_ptr)           , intent(in), value :: preg
         character(kind=C_char), intent(in)        :: pattern(*)
         integer(C_int)        , intent(in), value :: flags
         integer(C_int)                            :: status
      end function

      subroutine C_regfree(preg) bind(C,name='regfree')
         import
         type(C_ptr), intent(in), value :: preg
      end subroutine

      function C_regerror(errcode, preg, errbuf, errbuf_size) result(errlen) bind(C,name='regerror')
         import
         integer(C_int)        , intent(in) , value :: errcode
         type(C_ptr)           , intent(in) , value :: preg
         character(kind=C_char), intent(out)        :: errbuf(*)
         integer(C_size_t)     , intent(in) , value :: errbuf_size
         integer(C_size_t)                          :: errlen
      end function

      function C_regexec(preg, string, nmatch, pmatch, eflags) result(status) bind(C,name='regexec')
         import
         type(C_ptr)           , intent(in) , value :: preg
         character(kind=C_char), intent(in)         :: string(*)
         integer(C_size_t)     , intent(in) , value :: nmatch
         type(C_regmatch_t)    , intent(out)        :: pmatch(*)
         integer(C_int)        , intent(in) , value :: eflags
         integer(C_int)                             :: status
      end function

   end interface


contains



   impure elemental subroutine NewCopyOther(self, other)
      class(ftlRegex), intent(inout) :: self
       type(ftlRegex), intent(in)  :: other

      call self%Delete()

      if (.not.allocated(other%pattern)) return

      self%pattern = other%pattern
      self%cflags = other%cflags

      call self%Compile()

   end subroutine
   !
   subroutine NewRaw(self, pattern, basic, icase, nosub, newline)
      class(ftlRegex) , intent(out)           :: self
      character(len=*), intent(in)            :: pattern
      logical         , intent(in) , optional :: basic, icase, nosub, newline

      self%pattern = pattern // C_NULL_char

      if (present(basic)) then
         if (basic) then
            self%cflags = 0_C_int
         else
            self%cflags = ior(0_C_int, REG_EXTENDED)
         endif
      else
         self%cflags = ior(0_C_int, REG_EXTENDED) ! use extended POSIX regexes by default
      endif

      if (present(icase)) then
         if (icase) self%cflags = ior(self%cflags, REG_ICASE)
      endif

      if (present(nosub)) then
         if (nosub) self%cflags = ior(self%cflags, REG_NOSUB)
      endif

      if (present(newline)) then
         if (newline) self%cflags = ior(self%cflags, REG_NEWLINE)
      endif

      call self%Compile()

   end subroutine
   !
   subroutine NewString(self, pattern, basic, icase, nosub, newline)
      class(ftlRegex), intent(out)           :: self
      type(ftlString), intent(in)            :: pattern
      logical        , intent(in) , optional :: basic, icase, nosub, newline

      call self%NewRaw(pattern%raw, basic, icase, nosub, newline)

   end subroutine



   subroutine Compile(self)
      class(ftlRegex), intent(inout) :: self

      integer :: status

      allocate(self%regdata(sizeof_C_regex_t))
      self%preg = c_loc(self%regdata(1))
      status = C_regcomp(self%preg, self%pattern, self%cflags)
      if (status /= 0) call self%PrintError(status)

   end subroutine
   !
   subroutine PrintError(self, status)
      use, intrinsic :: iso_fortran_env, only: error_unit
      class(ftlRegex), intent(in) :: self
      integer(C_int) , intent(in) :: status

      character(len=1024,kind=C_char) :: errbuf
      integer(C_size_t) :: errlen

      errlen = C_regerror(status, self%preg, errbuf, int(len(errbuf), C_size_t))
      write (error_unit,'(2A)') 'ftlRegex ERROR: ', errbuf(:errlen-1)

   end subroutine



   ! Constructor functions:
   !
   type(ftlRegex) function NewRawConstr(pattern, basic, icase, nosub, newline) result(regex)
      character(len=*), intent(in)           :: pattern
      logical         , intent(in), optional :: basic, icase, nosub, newline
      call regex%NewRaw(pattern, basic, icase, nosub, newline)
   end function
   !
   type(ftlRegex) function NewStringConstr(pattern, basic, icase, nosub, newline) result(regex)
      type(ftlString), intent(in)           :: pattern
      logical        , intent(in), optional :: basic, icase, nosub, newline
      call regex%NewRaw(pattern%raw, basic, icase, nosub, newline)
   end function



   impure elemental subroutine Delete(self)
      class(ftlRegex), intent(inout) :: self

      if (c_associated(self%preg)) then
         call C_regfree(self%preg)
         self%preg = C_NULL_ptr
         deallocate(self%regdata)
         deallocate(self%pattern)
      endif

   end subroutine
   !
   impure elemental subroutine Finalizer(self)
      type(ftlRegex), intent(inout) :: self

      call self%Delete()

   end subroutine



   integer function NumMatchesRaw(self, string) result (numMatches)
      class(ftlRegex) , intent(in) :: self
      character(len=*), intent(in) :: string

      type(ftlRegexMatch) :: m
      integer :: begin

      begin = 1
      numMatches = 0
      do while (begin <= len(string))
         m = self%MatchFirst(string(begin:))
         if (.not.m%matches) exit
         numMatches = numMatches + 1
         begin = begin + m%end - 1
      enddo

   end function
   !
   integer function NumMatchesString(self, string) result (numMatches)
      class(ftlRegex), intent(in) :: self
      type(ftlString), intent(in) :: string

      numMatches = self%NumMatches(string%raw)

   end function



   type(ftlRegexMatch) function MatchFirstRaw(self, string) result(match)
      class(ftlRegex) , intent(in) :: self
      character(len=*), intent(in) :: string

      character(len=:,kind=C_char), allocatable :: cstring
      integer(C_size_t), parameter :: nmatch = 1024
      type(C_regmatch_t) :: pmatch(nmatch)
      integer(C_int) :: status

      integer :: nGroups, iGroup

      cstring = string // C_NULL_char
      status = C_regexec(self%preg, cstring, nmatch, pmatch, 1_C_int)
      if (status /= 0 .and. status /= REG_NOMATCH) call self%PrintError(status)
      if (status == REG_NOMATCH) return

      match%matches = .true.

      if (and(self%cflags,REG_NOSUB) == REG_NOSUB) then
         nGroups = 0
      else
         match%text    = string(pmatch(1)%rm_so+1:pmatch(1)%rm_eo)
         match%begin   = pmatch(1)%rm_so+1
         match%end     = pmatch(1)%rm_eo+1
         nGroups = count(pmatch(2:)%rm_so /= -1)
      endif
      allocate(match%group(nGroups))
      if (nGroups == 0) return
      do iGroup = 1, nGroups
         match%group(iGroup)%text  = string(pmatch(iGroup+1)%rm_so+1:pmatch(iGroup+1)%rm_eo)
         match%group(iGroup)%begin = pmatch(iGroup+1)%rm_so+1
         match%group(iGroup)%end   = pmatch(iGroup+1)%rm_eo+1
      enddo

   end function
   !
   type(ftlRegexMatch) function MatchFirstString(self, string) result(match)
      class(ftlRegex), intent(in) :: self
      type(ftlString), intent(in) :: string

      match = self%MatchFirstRaw(string%raw)

   end function



   function MatchRaw(self, string) result(matches)
      class(ftlRegex)    , intent(in)  :: self
      character(len=*)   , intent(in)  :: string
      type(ftlRegexMatch), allocatable :: matches(:)

      integer :: begin, iMatch, iGroup

      allocate(matches(self%NumMatches(string)))
      begin = 1
      do iMatch = 1, size(matches)
         matches(iMatch) = self%MatchFirst(string(begin:))
         matches(iMatch)%begin = matches(iMatch)%begin + begin - 1
         matches(iMatch)%end   = matches(iMatch)%end + begin - 1
         if (allocated(matches(iMatch)%group)) then
            do iGroup = 1, size(matches(iMatch)%group)
               matches(iMatch)%group(iGroup)%begin = matches(iMatch)%group(iGroup)%begin + begin - 1
               matches(iMatch)%group(iGroup)%end   = matches(iMatch)%group(iGroup)%end + begin - 1
            enddo
         endif
         begin = matches(iMatch)%end
      enddo

   end function
   !
   function MatchString(self, string) result(matches)
      class(ftlRegex)    , intent(in)  :: self
      type(ftlString)    , intent(in)  :: string
      type(ftlRegexMatch), allocatable :: matches(:)

      matches = self%MatchRaw(string%raw)

   end function



   type(ftlString) function ReplaceRawWithRaw(self, string, sub, doGroupSub) result(replaced)
      class(ftlRegex) , intent(in)           :: self
      character(len=*), intent(in)           :: string
      character(len=*), intent(in)           :: sub
      logical         , intent(in), optional :: doGroupSub

      type(ftlRegexMatch), allocatable :: matches(:)
      integer :: iMatch, iGroup, begin, end
      type(ftlString) :: thisMatchSub
      logical :: doGroupSub_

      doGroupSub_ = .false.
      if (present(doGroupSub)) then
         doGroupSub_ = doGroupSub
      endif

      matches = self%MatchRaw(string)
      if (size(matches) == 0) then
         replaced%raw = string
         return
      endif

      ! TODO: avoid slowly growing the string (for performance reasons ...)

      replaced%raw = ''
      do iMatch = 1, size(matches)
         if (iMatch == 1) then
            begin = 1
         else
            begin = matches(iMatch-1)%end
         endif
         end = matches(iMatch)%begin
         if (doGroupSub_) then
            thisMatchSub = sub
            do iGroup = 1, size(matches(iMatch)%group)
               thisMatchSub = thisMatchSub%Replace('\'//ftlString(iGroup), matches(iMatch)%group(iGroup)%text%raw)
            enddo
            replaced%raw = replaced%raw // string(begin:end-1) // thisMatchSub%raw
         else
            replaced%raw = replaced%raw // string(begin:end-1) // sub
         endif
      enddo
      replaced%raw = replaced%raw // string(matches(size(matches))%end:)

   end function
   !
   type(ftlString) function ReplaceStringWithRaw(self, string, sub, doGroupSub) result(replaced)
      class(ftlRegex) , intent(in)           :: self
      type(ftlString) , intent(in)           :: string
      character(len=*), intent(in)           :: sub
      logical         , intent(in), optional :: doGroupSub

      replaced = self%ReplaceRawWithRaw(string%raw, sub, doGroupSub)

   end function
   !
   type(ftlString) function ReplaceRawWithString(self, string, sub, doGroupSub) result(replaced)
      class(ftlRegex) , intent(in)           :: self
      character(len=*), intent(in)           :: string
      type(ftlString) , intent(in)           :: sub
      logical         , intent(in), optional :: doGroupSub

      replaced = self%ReplaceRawWithRaw(string, sub%raw, doGroupSub)

   end function
   !
   type(ftlString) function ReplaceStringWithString(self, string, sub, doGroupSub) result(replaced)
      class(ftlRegex), intent(in)           :: self
      type(ftlString), intent(in)           :: string
      type(ftlString), intent(in)           :: sub
      logical        , intent(in), optional :: doGroupSub

      replaced = self%ReplaceRawWithRaw(string%raw, sub%raw, doGroupSub)

   end function



   logical function OpMatchesRaw(lhs, rhs) result(matches)
      character(len=*), intent(in) :: lhs
      class(ftlRegex) , intent(in) :: rhs

      type(ftlRegexMatch) :: m

      m = rhs%MatchFirst(lhs)
      matches = m%matches

   end function
   !
   logical function OpMatchesString(lhs, rhs) result(matches)
      type(ftlString), intent(in) :: lhs
      class(ftlRegex), intent(in) :: rhs

      matches = lhs%raw .matches. rhs

   end function



   logical function OpEqualOther(self, other) result(equal)
      class(ftlRegex), intent(in) :: self
       type(ftlRegex), intent(in) :: other

      equal = (self%pattern == other%pattern) .and. (self%cflags == other%cflags)

   end function
   !
   logical function OpUnequalOther(self, other) result(unequal)
      class(ftlRegex), intent(in) :: self
       type(ftlRegex), intent(in) :: other

      unequal = .not.(self == other)

   end function



end module
