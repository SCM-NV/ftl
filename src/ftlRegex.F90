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


!   ftlRegex
!   ========
!
! TODO: description

module ftlRegexModule

   use iso_c_binding
   use ftlStringModule

   implicit none
   private


   type, public :: ftlRegex
      private

      character(len=:,kind=C_char), allocatable :: pattern

      type(C_ptr)                     :: preg = C_NULL_ptr
      character(kind=C_char), pointer :: regdata(:) => null()

   contains
      private

      procedure         :: NewRaw
      generic  , public :: New => NewRaw

      procedure, public :: Delete
#ifndef FTL_NO_FINALIZERS
      final             :: Finalizer
#endif

      procedure         :: MatchRaw
      generic  , public :: Match => MatchRaw

      procedure         :: PrintError

   end type

   type, public :: ftlRegexGroup
      type(ftlString) :: text
      integer         :: begin = 0
      integer         :: end   = 0
   end type

   type, public :: ftlRegexMatch
      type(ftlString)                  :: text
      integer                          :: begin = 0
      integer                          :: end   = 0
      type(ftlRegexGroup), allocatable :: group(:)
   !contains
      !procedure :: Matches
   end type


   ! Interfaces for the functions, types, etc. in POSIX regex.h

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



   subroutine NewRaw(self, pattern, flags)
      class(ftlRegex) , intent(inout)           :: self
      character(len=*), intent(in)              :: pattern
      integer         , intent(in)   , optional :: flags(:)

      integer(C_int) :: status

      call self%Delete()

      self%pattern = pattern // C_NULL_char

      allocate(self%regdata(64))
      self%preg = c_loc(self%regdata(1))
      status = C_regcomp(self%preg, self%pattern, 1_C_int)
      if (status /= 0) then
         call self%PrintError(status)
         stop 'ERROR compiling regex'
      endif

   end subroutine



   subroutine Delete(self)
      class(ftlRegex), intent(inout) :: self

      if (c_associated(self%preg)) then
         call C_regfree(self%preg)
         self%preg = C_NULL_ptr
         deallocate(self%regdata)
         deallocate(self%pattern)
      endif

   end subroutine

#ifndef FTL_NO_FINALIZERS
   subroutine Finalizer(self)
      type(ftlRegex), intent(inout) :: self
      call self%Delete()
   end subroutine
#endif



   type(ftlRegexMatch) function MatchRaw(self, string, flags) result(match)
      class(ftlRegex) , intent(in)           :: self
      character(len=*), intent(in)           :: string
      integer         , intent(in), optional :: flags(:)

      character(len=:,kind=C_char), allocatable :: cstring
      integer(C_size_t), parameter :: nmatch = 1024
      type(C_regmatch_t) :: pmatch(nmatch)
      integer(C_int) :: status

      integer :: nGroups, iGroup

      cstring = string // C_NULL_char
      status = C_regexec(self%preg, cstring, nmatch, pmatch, 1_C_int)
      if (status /= 0 .and. status /= 1) then
         call self%PrintError(status)
         stop 'ERROR matching regex'
      endif
      if (status == 1) return

      match%text  = string(pmatch(1)%rm_so+1:pmatch(1)%rm_eo)
      match%begin = pmatch(1)%rm_so+1
      match%end   = pmatch(1)%rm_eo+1

      nGroups = count(pmatch(2:)%rm_so /= -1)
      if (nGroups == 0) return
      allocate(match%group(nGroups))
      do iGroup = 1, nGroups
         match%group(iGroup)%text  = string(pmatch(iGroup+1)%rm_so+1:pmatch(iGroup+1)%rm_eo)
         match%group(iGroup)%begin = pmatch(iGroup+1)%rm_so+1
         match%group(iGroup)%end   = pmatch(iGroup+1)%rm_eo+1
      enddo

   end function



   subroutine PrintError(self, status)
      use, intrinsic :: iso_fortran_env, only: error_unit
      class(ftlRegex), intent(in) :: self
      integer(C_int) , intent(in) :: status

      character(len=1024,kind=C_char) :: errbuf
      integer(C_size_t) :: errlen

      errlen = C_regerror(status, self%preg, errbuf, int(len(errbuf), C_size_t))
      write (error_unit,'(2A)') 'ftlRegex ERROR: ', errbuf(:errlen-1)

   end subroutine


end module
