! Copyright (c) 2016, 2017  Robert Rüger
! Copyright (c) 2018  Software for Chemistry & Materials BV
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


!   ftlString
!   =========
!
! Strings are objects that represent sequences of characters.
!
! ftlString provides an interface similar to other FTL containers. Furthermore it also provides lots of convenience methods
! that operate on strings. These are mostly taken from the Python string, which provides a more convenient interface than
! C++'s std::string.

#define FTL_CONTAINER ftlString
#define FTL_CONTAINER_PROVIDES_RANDOM_ACCESS_ITERATOR

#ifndef FTL_SKIP_IMPLEMENTATION

module ftlStringModule

   use ftlKindsModule

   implicit none
   private


   ! Python string constants:
   character       , parameter, public :: FTL_STRING_NEWLINE     = NEW_LINE('a')
   character(len=*), parameter, public :: FTL_STRING_LOWERCASE   = 'abcdefghijklmnopqrstuvwxyz'
   character(len=*), parameter, public :: FTL_STRING_UPPERCASE   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(len=*), parameter, public :: FTL_STRING_LETTERS     = FTL_STRING_LOWERCASE//FTL_STRING_UPPERCASE
   character(len=*), parameter, public :: FTL_STRING_DIGITS      = '0123456789'
   character(len=*), parameter, public :: FTL_STRING_HEXDIGITS   = '0123456789ABCDEF'
   character(len=*), parameter, public :: FTL_STRING_OCTDIGITS   = '01234567'
   character(len=*), parameter, public :: FTL_STRING_PUNCTUATION = '!"#$%&'//achar(39)//'()*+,-./:;<=>?@['//achar(92)//']^_`{|}~'
   character(len=*), parameter, public :: FTL_STRING_WHITESPACE  = ' '//char(9)//char(10)//char(13)//char(11)//char(12)
   character(len=*), parameter, public :: FTL_STRING_PRINTABLE   = FTL_STRING_LETTERS//FTL_STRING_DIGITS// &
                                                                   FTL_STRING_PUNCTUATION//FTL_STRING_WHITESPACE


! ====== Type of the ftlString container itself ==================================================================================

   type, public :: ftlString
      private

      character(len=:), allocatable, public :: raw

   contains
      private

      procedure         :: NewDefault
      procedure         :: NewCopyOther
      procedure         :: NewFromRaw
      procedure         :: NewFromInt
      procedure         :: NewFromReal
      procedure         :: NewFromComplex
      procedure         :: NewFromLogical
      generic  , public :: New => NewDefault, NewCopyOther, NewFromRaw, NewFromInt, NewFromReal, NewFromComplex, NewFromLogical
      procedure, public :: NewFromCString ! not in New interface, because signature clashes with elemental NewFromRaw

      procedure         :: AllocatedString
      generic  , public :: Allocated => AllocatedString
      procedure, public :: Delete

      procedure, public :: Size => ftlLen

      procedure         :: AtString
      generic  , public :: At => AtString

      procedure         :: BeginString
      generic  , public :: Begin => BeginString
      procedure         :: EndString
      generic  , public :: End => EndString

      ! Conversion to numeric types:
      procedure, public :: IsNumber
      procedure, public :: IsInt
      procedure, public :: ToInt
      procedure, public :: IsReal
      procedure, public :: ToReal
      procedure, public :: IsComplex
      procedure, public :: ToComplex
      procedure, public :: IsLogical
      procedure, public :: ToLogical

      ! File reading:
      procedure, public :: ReadLine
      procedure, public :: ReadUntilEOF

      ! Python string methods:
      procedure, public :: Center
      procedure         :: CountRaw
      procedure         :: CountOther
      generic  , public :: Count => CountRaw, CountOther
      procedure         :: PartitionRaw
      procedure         :: PartitionOther
      generic  , public :: Partition => PartitionRaw, PartitionOther
      procedure         :: SplitWords
      procedure         :: SplitSepRaw
      procedure         :: SplitSepOther
      generic  , public :: Split => SplitWords, SplitSepRaw, SplitSepOther
      procedure, public :: SplitLines
      procedure         :: JoinBound
      generic  , public :: Join => JoinBound
      procedure         :: StartsWithRaw
      procedure         :: StartsWithOther
      procedure         :: StartsWithArray
      generic  , public :: StartsWith => StartsWithRaw, StartsWithOther, StartsWithArray
      procedure         :: EndsWithRaw
      procedure         :: EndsWithOther
      procedure         :: EndsWithArray
      generic  , public :: EndsWith => EndsWithRaw, EndsWithOther, EndsWithArray
      procedure         :: StripWhitespace
      procedure         :: StripRaw
      procedure         :: StripString
      generic  , public :: Strip => StripWhitespace, StripRaw, StripString
      procedure         :: RStripWhitespace
      procedure         :: RStripRaw
      procedure         :: RStripString
      generic  , public :: RStrip => RStripWhitespace, RStripRaw, RStripString
      procedure         :: LStripWhitespace
      procedure         :: LStripRaw
      procedure         :: LStripString
      generic  , public :: LStrip => LStripWhitespace, LStripRaw, LStripString
      procedure         :: FindRaw
      procedure         :: FindOther
      generic  , public :: Find => FindRaw, FindOther
      procedure, public :: Upper
      procedure, public :: Lower
      procedure, public :: IsSpace
      procedure         :: ReplaceRawWithRaw
      procedure         :: ReplaceStringWithString
      procedure         :: ReplaceRawWithString
      procedure         :: ReplaceStringWithRaw
      generic  , public :: Replace => ReplaceRawWithRaw, ReplaceStringWithString, ReplaceRawWithString, ReplaceStringWithRaw
      procedure         :: ReplaceImplementationEqualLength
      procedure         :: ReplaceImplementationSingleChar
      procedure         :: ReplaceImplementationGeneral

      ! Other string methods:
      procedure, public :: CountWords

      ! Assignment:
#if defined(__INTEL_COMPILER) && __INTEL_COMPILER < 1900
      ! ifort 18 (and possibly <18) seems to have problems with cleaning up the left hand side of a character(:), allocatable
      ! assignment.  This is normally what would happen in the intrinsic assignments of ftlStrings. Therefore we make a defined
      ! assignment for ftlString that does the cleanup of the lhs explicitly, to at least fix these memory leaks when using
      ! ftlStrings ...
      generic  , public :: assignment(=) => NewFromRaw, NewCopyOther
#else
      generic  , public :: assignment(=) => NewFromRaw
#endif

      ! Overloaded operators:

      ! == comparison like for raw strings
      procedure, pass(lhs) :: StringEqualString
      procedure, pass(lhs) :: StringEqualChar
      procedure, pass(rhs) :: CharEqualString
      generic  , public    :: operator(==) => StringEqualString, StringEqualChar, CharEqualString

      ! /= comparison like for raw strings
      procedure, pass(lhs) :: StringUnequalString
      procedure, pass(lhs) :: StringUnequalChar
      procedure, pass(rhs) :: CharUnequalString
      generic  , public    :: operator(/=) => StringUnequalString, StringUnequalChar, CharUnequalString

      ! < comparison like for raw strings
      procedure, pass(lhs) :: StringLessString
      procedure, pass(lhs) :: StringLessChar
      procedure, pass(rhs) :: CharLessString
      generic  , public    :: operator(<) => StringLessString, StringLessChar, CharLessString

      ! > comparison like for raw strings
      procedure, pass(lhs) :: StringGreaterString
      procedure, pass(lhs) :: StringGreaterChar
      procedure, pass(rhs) :: CharGreaterString
      generic  , public    :: operator(>) => StringGreaterString, StringGreaterChar, CharGreaterString

      ! // operator with raw Fortran string output
      procedure, pass(lhs) :: StringCatString
      procedure, pass(lhs) :: StringCatChar
      procedure, pass(rhs) :: CharCatString
      generic  , public    :: operator(//) => StringCatString, StringCatChar, CharCatString

      ! + operator with ftlString output
      procedure, pass(lhs) :: StringCatOpString
      procedure, pass(lhs) :: StringCatOpChar
      procedure, pass(rhs) :: CharCatOpString
      generic  , public    :: operator(+) => StringCatOpString, StringCatOpChar, CharCatOpString

      ! Python style .in. operator
      procedure, pass(lhs) :: StringInString
      procedure, pass(lhs) :: StringInChar
      procedure, pass(rhs) :: CharInString
      generic  , public    :: operator(.in.) => StringInString, StringInChar, CharInString

   end type


   ! Constructor functions:

   interface ftlString
      module procedure NewDefaultConstr
      module procedure NewCopyOtherConstr
      module procedure NewFromRawConstr
      module procedure NewFromIntConstr
      module procedure NewFromRealConstr
      module procedure NewFromComplexConstr
      module procedure NewFromLogicalConstr
   end interface


   ! Assignment of ftlString to raw Fortran strings

   public :: assignment(=)
   interface assignment(=)
      module procedure AssignToAllocatableRaw
   end interface

   public :: Raw
   interface Raw
      module procedure StringToRaw
   end interface


   !! Derived-type IO

   !public :: write(formatted)
   !interface write(formatted)
   !   module procedure writeFormatted
   !end interface

   !public :: write(unformatted)
   !interface write(unformatted)
   !   module procedure writeUnformatted
   !end interface

   !public :: read(formatted)
   !interface read(formatted)
   !   module procedure readFormatted
   !end interface

   !public :: read(unformatted)
   !interface read(unformatted)
   !   module procedure readUnformatted
   !end interface


   ! Free versions of some type-bound procedures:

   public :: Begin
   interface Begin
      module procedure BeginString
   end interface

   public :: End
   interface End
      module procedure EndString
   end interface

   public :: size
   interface size
      module procedure ftlLen
   end interface

   interface operator(+)
      module procedure CharCatOpChar
   end interface

   public :: Join
   interface Join
      module procedure JoinFree
   end interface


   ! Conversion to numeric types:

   public :: int
   interface int
      module procedure ToInt
   end interface

   public :: real
   interface real
      module procedure ToReal
   end interface

   public :: complex
   interface complex
      module procedure ToComplex
   end interface

   public :: logical
   interface logical
      module procedure ToLogical
   end interface


   ! Fortran standard methods:

   public :: len
   interface len
      module procedure ftlLen
   end interface

   public :: len_trim
   interface len_trim
      module procedure ftlLenTrim
   end interface

   public :: trim
   interface trim
      module procedure ftlTrim
   end interface

   public :: adjustl
   interface adjustl
      module procedure ftlAdjustl
   end interface

   public :: adjustr
   interface adjustr
      module procedure ftlAdjustr
   end interface

   public :: repeat
   interface repeat
      module procedure ftlRepeat
   end interface

   public :: index
   interface index
      module procedure ftlIndexOther, ftlIndexRaw
   end interface

   public :: scan
   interface scan
      module procedure ftlScanOther, ftlScanRaw
   end interface

   public :: verify
   interface verify
      module procedure ftlVerifyOther, ftlVerifyRaw
   end interface


   ! FTL methods:

   public :: ftlHash
   interface ftlHash
      module procedure ftlHashString
   end interface

   public :: ftlSwap
   interface ftlSwap
      module procedure ftlSwapString
   end interface

   public :: ftlMove
   interface ftlMove
      module procedure ftlMoveString
      module procedure ftlMoveRawToString
      module procedure ftlMoveStringToRaw
   end interface



! ====== Type of an iterator over a ftlString container ==========================================================================

   type, public :: ftlStringIterator
      private

      type(ftlString), pointer         :: str => null()
      integer                          :: index = 0
      character      , pointer, public :: value => null()

   contains
      private

      procedure         :: NewItDefault
      procedure         :: NewItCopyOther
      generic  , public :: New => NewItDefault, NewItCopyOther

      procedure, public :: Inc
      procedure, public :: Dec

   end type

   public :: operator(+)
   interface operator(+)
      module procedure AdvanceN
   end interface

   public :: operator(-)
   interface operator(-)
      module procedure ReverseN, DiffOther
   end interface

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther
   end interface

   public :: operator(/=)
   interface operator(/=)
      module procedure UnequalOther
   end interface

   public :: operator(<)
   interface operator(<)
      module procedure SmallerOther
   end interface

   public :: operator(<=)
   interface operator(<=)
      module procedure SmallerEqualOther
   end interface

   public :: operator(>)
   interface operator(>)
      module procedure GreaterOther
   end interface

   public :: operator(>=)
   interface operator(>=)
      module procedure GreaterEqualOther
   end interface


contains



! ====== Implementation of ftlString methods =====================================================================================


   ! Constructs a string object, initializing its value depending on the constructor version used:
   !
   subroutine NewDefault(self)
      class(ftlString), intent(inout) :: self

      ! Constructs an empty string, with a length of zero characters.

      self%raw = ''

   end subroutine
   !
   pure subroutine NewCopyOther(self, other)
      class(ftlString), intent(inout) :: self
       type(ftlString), intent(in)    :: other

      ! Constructs a copy of other.

      if (.not.allocated(self%raw) .and. .not.allocated(other%raw)) then
         return
      else if (.not.allocated(self%raw)) then
         self%raw = other%raw
      else if (.not.allocated(other%raw)) then
         deallocate(self%raw)
      else
         if (len(self%raw) == len(other%raw)) then
            self%raw(:) = other%raw(:)
         else
            deallocate(self%raw)
            self%raw = other%raw
         endif
      endif

   end subroutine
   !
   elemental subroutine NewFromRaw(self, raw)
      class(ftlString), intent(inout) :: self
      character(len=*), intent(in)    :: raw

      character(len=:), allocatable :: tmp

      ! Constructs an ftlString from a raw Fortran string

      tmp = raw ! raw and self%raw might alias! Make sure self%raw is not deallocated before we read from raw ...
      call move_alloc(tmp, self%raw)

   end subroutine
   !
   subroutine NewFromCString(self, cstr)
      use, intrinsic :: iso_c_binding
      class(ftlString)      , intent(inout) :: self
      character(kind=C_CHAR), intent(in)    :: cstr(*)

      integer :: i, strlen

      ! Constructs an ftlString from a C_NULL_CHAR terminated C string.

      i = 1
      do while (cstr(i) /= C_NULL_CHAR)
         i = i + 1
      enddo
      strlen = i - 1 ! exclude terminating C_NULL_CHAR
      self%raw = repeat(C_NULL_CHAR, strlen)
      do i = 1, strlen
         self%raw(i:i) = cstr(i)
      enddo

   end subroutine
   !
   subroutine NewFromInt(self, i, format)
      class(ftlString), intent(inout)        :: self
      integer         , intent(in)           :: i
      character(len=*), intent(in), optional :: format

      character(len=64) :: tmp

      ! Constructs an ftlString from an integer

      if (present(format)) then
         write (tmp,format) i
         self%raw = trim(tmp)
      else
         write (tmp,*) i
         self%raw = trim(adjustl(tmp))
      endif

   end subroutine
   !
   subroutine NewFromReal(self, r, format)
      class(ftlString), intent(inout)        :: self
      real(FTL_KREAL) , intent(in)           :: r
      character(len=*), intent(in), optional :: format

      character(len=64) :: tmp

      ! Constructs an ftlString from a real

      if (present(format)) then
         write (tmp,format) r
         self%raw = trim(tmp)
      else
         write (tmp,*) r
         self%raw = trim(adjustl(tmp))
      endif

   end subroutine
   !
   subroutine NewFromComplex(self, c, format)
      class(ftlString)  , intent(inout)        :: self
      complex(FTL_KREAL), intent(in)           :: c
      character(len=*)  , intent(in), optional :: format

      character(len=128) :: tmp

      ! Constructs an ftlString from a complex

      if (present(format)) then
         write (tmp,format) c
         self%raw = trim(tmp)
      else
         write (tmp,*) c
         self%raw = trim(adjustl(tmp))
      endif

   end subroutine
   !
   subroutine NewFromLogical(self, l, format)
      class(ftlString), intent(out)          :: self
      logical         , intent(in)           :: l
      character(len=*), intent(in), optional :: format

      character(len=16) :: tmp

      ! Constructs an ftlString from a logical

      if (present(format)) then
         write (tmp,format) l
         self%raw = trim(tmp)
      else
         if (l) then
            self%raw = 'True'
         else
            self%raw = 'False'
         endif
      endif

   end subroutine




   ! Constructor functions:
   !
   type(ftlString) function NewDefaultConstr() result(str)
      call str%NewDefault()
   end function
   !
   type(ftlString) function NewCopyOtherConstr(other) result(str)
      class(ftlString), intent(in) :: other
      call str%NewCopyOther(other)
   end function
   !
   type(ftlString) function NewFromRawConstr(raw) result(str)
      character(len=*), intent(in) :: raw
      call str%NewFromRaw(raw)
   end function
   !
   type(ftlString) function NewFromIntConstr(i, format) result(str)
      integer         , intent(in)           :: i
      character(len=*), intent(in), optional :: format
      call str%NewFromInt(i, format)
   end function
   !
   type(ftlString) function NewFromRealConstr(r, format) result(str)
      real(FTL_KREAL) , intent(in)           :: r
      character(len=*), intent(in), optional :: format
      call str%NewFromReal(r, format)
   end function
   !
   type(ftlString) function NewFromComplexConstr(c, format) result(str)
      complex(FTL_KREAL), intent(in)           :: c
      character(len=*)  , intent(in), optional :: format
      call str%NewFromComplex(c, format)
   end function
   !
   type(ftlString) function NewFromLogicalConstr(l, format) result(str)
      logical         , intent(in)           :: l
      character(len=*), intent(in), optional :: format
      call str%NewFromLogical(l, format)
   end function



   ! Checks whether an ftlString is initialized (that is the raw string is allocated)
   !
   elemental logical function AllocatedString(self)
      class(ftlString), intent(in) :: self

      AllocatedString = allocated(self%raw)

   end function



   ! Destroys the ftlString object. This deallocates all the storage capacity allocated by the ftlString.
   !
   elemental subroutine Delete(self)
      class(ftlString), intent(inout) :: self

      if (allocated(self%raw)) deallocate(self%raw)

   end subroutine



   ! =============>  Assignment of ftlString to raw Fortran strings:



   subroutine AssignToAllocatableRaw(lhs, rhs)
      character(len=:), allocatable, intent(inout) :: lhs
      type(ftlString)              , intent(in)    :: rhs

      if (allocated(rhs%raw)) then
         lhs = rhs%raw
      else
         if (allocated(lhs)) deallocate(lhs)
      endif

   end subroutine



   ! This free function can be used to convert an ftlString (e.g. returned from a function) to a raw Fortran string.
   ! If length is specified that the raw string will have precisely this length, either padding with spaces of truncating.
   ! If length is not specified the raw string will have exactly the size of the ftlString.
   !
   function StringToRaw(str, length) result(raw)
      type(ftlString)  , intent(in) :: str
      integer, optional, intent(in) :: length
      character(len=:), allocatable :: raw

      if (present(length)) then
         raw = repeat(' ', length)
         raw(1:min(len(str%raw), length)) = str%raw(1:min(len(str%raw), length))
      else
         raw = str%raw
      endif

   end function



   !! =============> Derived-type IO:



   !subroutine writeUnformatted(self, unit, iostat, iomsg)
   !   class(ftlString), intent(in)    :: self
   !   integer         , intent(in)    :: unit
   !   integer         , intent(out)   :: iostat
   !   character(len=*), intent(inout) :: iomsg

   !   write (unit, iostat=iostat, iomsg=iomsg) self%raw

   !end subroutine
   !!
   !subroutine writeFormatted(self, unit, iotype, v_list, iostat, iomsg)
   !   class(ftlString), intent(in)    :: self
   !   integer         , intent(in)    :: unit
   !   character(len=*), intent(in)    :: iotype
   !   integer         , intent(in)    :: v_list(:)
   !   integer         , intent(out)   :: iostat
   !   character(len=*), intent(inout) :: iomsg

   !   write (unit, '(A)', iostat=iostat, iomsg=iomsg) self%raw

   !end subroutine



   !subroutine readUnformatted(self, unit, iostat, iomsg)
   !   class(ftlString), intent(inout) :: self
   !   integer         , intent(in)    :: unit
   !   integer         , intent(out)   :: iostat
   !   character(len=*), intent(inout) :: iomsg

   !   call self%ReadLine(unit, iostat)

   !end subroutine
   !!
   !subroutine readFormatted(self, unit, iotype, vlist, iostat, iomsg)
   !   class(ftlString), intent(inout) :: self
   !   integer         , intent(in)    :: unit
   !   character(len=*), intent(in)    :: iotype
   !   integer         , intent(in)    :: vlist(:)
   !   integer         , intent(out)   :: iostat
   !   character(len=*), intent(inout) :: iomsg

   !   call self%ReadLine(unit, iostat)

   !end subroutine


   ! =============> Character wise access:



   function AtString(self, idx) result(At)
      class(ftlString), intent(in), target :: self
      integer         , intent(in)         :: idx
      character, pointer                   :: At

      At => self%raw(idx:idx)

   end function



   ! =============> Overloaded operators:



   ! == comparison like for raw strings
   !
   elemental logical function StringEqualString(lhs, rhs) result(equal)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then ! because raw Fortran strings of unequal length can compare equal
         equal = .false.
      else
         equal = (lhs%raw == rhs%raw)
      endif

   end function
   !
   elemental logical function StringEqualChar(lhs, rhs) result(equal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then ! because raw Fortran strings of unequal length can compare equal
         equal = .false.
      else
         equal = (lhs%raw == rhs)
      endif

   end function
   !
   elemental logical function CharEqualString(lhs, rhs) result(equal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then ! because raw Fortran strings of unequal length can compare equal
         equal = .false.
      else
         equal = (lhs == rhs%raw)
      endif

   end function



   ! /= comparison like for raw strings
   !
   elemental logical function StringUnequalString(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

   end function
   !
   elemental logical function StringUnequalChar(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

   end function
   !
   elemental logical function CharUnequalString(lhs, rhs) result(unequal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

   end function



   ! < comparison like for raw strings
   !
   elemental logical function StringLessString(lhs, rhs) result(less)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      less = lhs%raw < rhs%raw

   end function
   !
   elemental logical function StringLessChar(lhs, rhs) result(less)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      less = lhs%raw < rhs

   end function
   !
   elemental logical function CharLessString(lhs, rhs) result(less)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      less = lhs < rhs%raw

   end function



   ! > comparison like for raw strings
   !
   elemental logical function StringGreaterString(lhs, rhs) result(greater)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      greater = lhs%raw > rhs%raw

   end function
   !
   elemental logical function StringGreaterChar(lhs, rhs) result(greater)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      greater = lhs%raw > rhs

   end function
   !
   elemental logical function CharGreaterString(lhs, rhs) result(greater)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      greater = lhs > rhs%raw

   end function



   ! // operator with raw Fortran string output
   !
   pure function StringCatString(lhs, rhs) result(concat)
      class(ftlString), intent(in)  :: lhs
       type(ftlString), intent(in)  :: rhs
      character(len=:), allocatable :: concat

      concat = lhs%raw//rhs%raw

   endfunction
   !
   pure function StringCatChar(lhs, rhs) result(concat)
      class(ftlString), intent(in)  :: lhs
      character(len=*), intent(in)  :: rhs
      character(len=:), allocatable :: concat

      concat = lhs%raw//rhs

   endfunction
   !
   pure function CharCatString(lhs, rhs) result(concat)
      character(len=*), intent(in)  :: lhs
      class(ftlString), intent(in)  :: rhs
      character(len=:), allocatable :: concat

      concat = lhs//rhs%raw

   endfunction



   ! .cat. operator with ftlString output
   !
   elemental function StringCatOpString(lhs, rhs) result(concat)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs
       type(ftlString)             :: concat

      concat%raw = lhs%raw//rhs%raw

   endfunction
   !
   elemental function StringCatOpChar(lhs, rhs) result(concat)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs
       type(ftlString)             :: concat

      concat%raw = lhs%raw//rhs

   endfunction
   !
   elemental function CharCatOpString(lhs, rhs) result(concat)
      character(len=*), intent(in)  :: lhs
      class(ftlString), intent(in)  :: rhs
       type(ftlString)              :: concat

      concat%raw = lhs//rhs%raw

   endfunction
   !
   elemental function CharCatOpChar(lhs, rhs) result(concat)
      character(len=*), intent(in)  :: lhs, rhs
       type(ftlString)              :: concat

      concat%raw = lhs//rhs

   endfunction



   ! Python style .in. operator
   !
   elemental logical function StringInString(lhs, rhs) result(in)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

       in = (index(rhs%raw, lhs%raw) /= 0)

   endfunction
   !
   elemental logical function StringInChar(lhs, rhs) result(in)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      in = (index(rhs, lhs%raw) /= 0)

   endfunction
   !
   elemental logical function CharInString(lhs, rhs) result(in)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      in = (index(rhs%raw, lhs) /= 0)

   endfunction



   ! =============> Iterators:



   ! Returns an iterator pointing to the first character of the string.
   !
   type(ftlStringIterator) function BeginString(self) result(Begin)
      class(ftlString), intent(in), target :: self

      Begin%str => self
      Begin%index = 1
      if (len(self) /= 0) Begin%value => self%raw(1:1)

   end function


   ! Returns an iterator pointing to the past-the-end character of the string.
   !
   ! The past-the-end character is a theoretical character that would follow the last character in the string. It shall not
   ! be dereferenced.
   !
   ! Because the ranges used by functions of the standard library do not include the element pointed by their closing
   ! iterator, this function is often used in combination with ftlString%Begin to specify a range including all the
   ! characters in the string.
   !
   ! If the object is an empty string, this function returns the same as ftlString%Begin.
   !
   type(ftlStringIterator) function EndString(self) result(End)
      class(ftlString), intent(in), target :: self

      End%str => self
      End%index = len(self) + 1

   end function



   ! =============> Fortran standard methods:



   pure integer function ftlLen(self)
      class(ftlString), intent(in) :: self

      ftlLen = len(self%raw)

   end function



   pure integer function ftlLenTrim(self)
      class(ftlString), intent(in) :: self

      ftlLenTrim = len_trim(self%raw)

   end function



   pure type(ftlString) function ftlTrim(str)
      class(ftlString), intent(in) :: str

      ftlTrim%raw = trim(str%raw)

   end function



   pure type(ftlString) function ftlAdjustl(str)
      class(ftlString), intent(in) :: str

      ftlAdjustl%raw = adjustl(str%raw)

   end function



   pure type(ftlString) function ftlAdjustr(str)
      class(ftlString), intent(in) :: str

      ftlAdjustr%raw = adjustr(str%raw)

   end function



   pure type(ftlString) function ftlRepeat(str, i)
      class(ftlString), intent(in) :: str
      integer         , intent(in) :: i

      ftlRepeat%raw = repeat(str%raw,i)

   end function



   pure integer function ftlIndexOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1
       type(ftlString), intent(in)           :: str2
      logical         , intent(in), optional :: back

      ftlIndexOther = index(str1%raw, str2%raw, back)

   end function
   !
   pure integer function ftlIndexRaw(str, raw, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: raw
      logical         , intent(in), optional :: back

      ftlIndexRaw = index(str%raw, raw, back)

   end function



   pure integer function ftlScanOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1
       type(ftlString), intent(in)           :: str2
      logical         , intent(in), optional :: back

      ftlScanOther = scan(str1%raw, str2%raw, back)

   end function
   !
   pure integer function ftlScanRaw(str, raw, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: raw
      logical         , intent(in), optional :: back

      ftlScanRaw = scan(str%raw, raw, back)

   end function



   pure integer function ftlVerifyOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1
       type(ftlString), intent(in)           :: str2
      logical         , intent(in), optional :: back

      ftlVerifyOther = verify(str1%raw, str2%raw, back)

   end function
   !
   pure integer function ftlVerifyRaw(str, raw, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: raw
      logical         , intent(in), optional :: back

      ftlVerifyRaw = verify(str%raw, raw, back)

   end function



   ! =============> Conversion to numeric types:



   elemental logical function IsNumber(self)
      class(ftlString), intent(in) :: self

      IsNumber = self%IsInt() .or. self%IsReal() .or. self%IsComplex()

   end function



   elemental logical function IsInt(self)
      class(ftlString), intent(in) :: self

      integer :: tester, stat

      if (self%CountWords() /= 1) then
         IsInt = .false.
      else
         read(self%raw,'(I128)',iostat=stat) tester
         IsInt = (stat == 0)
      endif

   end function
   !
   elemental integer function ToInt(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToInt

      ! TODO: handle strings like '1e3' in gfortran

   end function



   elemental logical function IsReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      real(FTL_KREAL) :: tester

      if (self%CountWords() /= 1) then
         IsReal = .false.
      else
         read(self%raw,'(G128.64)',iostat=stat) tester
         IsReal = (stat == 0)
      endif

   end function
   !
   elemental real(FTL_KREAL) function ToReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToReal

   end function



   elemental logical function IsComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      complex(FTL_KREAL) :: tester
      type(ftlString), allocatable :: splitOnComma(:)
      type(ftlString) :: re, im

      IsComplex = .false.

      ! We need to check a couple of things manually, since the read below is to
      ! liberal in accepting things as a complex number ...
      splitOnComma = self%Split(',')
      if (size(splitOnComma) /= 2) return
      re = splitOnComma(1)%Strip()
      if (.not.re%StartsWith('(')) return
      re%raw = re%raw(2:)
      re = re%LStrip()
      if (.not.re%IsReal()) return
      im = splitOnComma(2)%Strip()
      if (.not.im%EndsWith(')')) return
      im%raw = im%raw(:len(im%raw)-1)
      im = im%RStrip()
      if (.not.im%IsReal()) return

      read(self%raw,*,iostat=stat) tester
      IsComplex = (stat == 0)

   end function
   !
   elemental complex function ToComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToComplex

   end function



   elemental logical function IsLogical(self)
      class(ftlString), intent(in) :: self

      logical :: tester
      integer :: stat

#if __INTEL_COMPILER
      ! For some reason ifort considers '' and "" valid logical values.
      ! We want consistent behaviour across compilers, so we "fix" this ...
      if (self%raw == '""' .or. self%raw == "''") then
         IsLogical = .false.
         return
      endif
#endif

      read(self%raw,*,iostat=stat) tester
      IsLogical = (stat == 0)

   end function
   !
   elemental logical function ToLogical(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToLogical
      if (stat /= 0) ToLogical = .false.

   end function



   ! =============> File reading:



   subroutine ReadLine(self, unit, iostat)
      class(ftlString), intent(inout)         :: self
      integer         , intent(in)            :: unit
      integer         , intent(out), optional :: iostat

      integer :: ios, nRead
      character(len=256) :: buff

      self%raw = ''
      nRead = 0
      do
         read (unit, '(A)', advance='no', err=10, end=10, eor=10, size=nRead, iostat=ios) buff
         self%raw = self%raw//buff(1:nRead)
      enddo
   10 self%raw = self%raw//buff(1:nRead)
      if (present(iostat)) then
         if (is_iostat_eor(ios)) then
            ! EOR is not an error for us but just an implementation detail
            iostat = 0
         else
            iostat = ios
         endif
      endif

   end subroutine



   subroutine ReadUntilEOF(self, unit)
      class(ftlString), intent(inout) :: self
      integer         , intent(in)    :: unit

      integer :: ios, nRead, newlen
      type(ftlString) :: line
      character(len=:), allocatable :: buffer

      self = ''

      call line%ReadLine(unit, ios)
      if (is_iostat_end(ios)) return

      buffer = line%raw
      nRead = len(line)

      do while (.true.)
         call line%ReadLine(unit, ios)
         if (is_iostat_end(ios)) exit
         if (len(buffer) < nRead + 1 + len(line%raw)) then
            ! not enough space anymore, we need to enlarge the buffer
            newlen = max(2*len(buffer), nRead + 1 + len(line%raw))
            buffer = buffer // repeat('_',newlen-len(buffer))
         endif
         buffer(nRead+1:nRead+1) = FTL_STRING_NEWLINE
         nRead = nRead + 1
         buffer(nRead+1:nRead+len(line%raw)) = line%raw
         nRead = nRead + len(line%raw)
      enddo

      self = buffer(1:nRead)

   end subroutine



   ! =============> Python string methods:



   ! Return centered in a string of length width. Padding is done using the specified fillchar (default is a space). The
   ! original string is returned if width is less than or equal to len(self).
   !
   type(ftlString) function Center(self, width, fillchar)
      class(ftlString), intent(in)           :: self
      integer         , intent(in)           :: width
      character       , intent(in), optional :: fillchar

      character :: fc
      integer :: numfill

      if (present(fillchar)) then
         fc = fillchar
      else
         fc = ' '
      endif

      if (len(self%raw) >= width) then
         Center = self
      else
         numfill = width - len(self%raw)
         Center = repeat(fc, numfill/2) // self%raw // repeat(fc, numfill/2 + mod(numfill,2))
      endif

   end function



   ! Return the number of non-overlapping occurrences of substring sub in the range [start, end). Optional arguments
   ! start and end are interpreted as in Python slice notation.
   !
   integer pure function CountRaw(self, sub, start, end) result(count)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: sub
      integer         , intent(in), optional :: start, end

      if (present(start) .and. present(end)) then
         count = CountImplementation(self%raw(start:end-1), sub)
      elseif (present(start)) then
         count = CountImplementation(self%raw(start:len(self%raw)), sub)
      elseif (present(end)) then
         count = CountImplementation(self%raw(1:end-1), sub)
      else
         count = CountImplementation(self%raw, sub)
      endif

   end function
   !
   integer pure function CountOther(self, sub, start, end) result(count)
      class(ftlString), intent(in)           :: self
       type(ftlString), intent(in)           :: sub
      integer         , intent(in), optional :: start, end

      count = CountRaw(self, sub%raw, start, end)

   end function
   !
   integer pure function CountImplementation(raw, sub) result(count)
      character(len=*), intent(in) :: raw, sub

      integer doneEnd, next

      if (len(sub) == 0) then
         ! replicate Python string behavior
         count = len(raw) + 1
         return
      endif

      count = 0
      doneEnd = 1
      do while (.true.)
         next = index(raw(doneEnd:len(raw)), sub) + doneEnd - 1
         if (next >= doneEnd) then ! found one more
            count = count + 1
            doneEnd = next + len(sub)
         else
            exit
         endif
      enddo

   end function



   ! Split the string at the first occurrence of sep, and return a 3-tuple containing the part before the separator, the
   ! separator itself, and the part after the separator. If the separator is not found, return a 3-tuple containing the
   ! string itself, followed by two empty strings.
   !
   function PartitionRaw(self, sep) result(partition)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: sep
       type(ftlString)             :: partition(3)

      integer :: idx

      idx = index(self%raw, sep)
      if (idx == 0) then ! not found
         partition(1) = self
         partition(2) = ''
         partition(3) = ''
      else
         partition(1) = self%raw(1:idx-1)
         partition(2) = self%raw(idx:idx+len(sep)-1)
         partition(3) = self%raw(idx+len(sep):len(self%raw))
      endif

   end function
   !
   function PartitionOther(self, sep) result(partition)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: sep
       type(ftlString)             :: partition(3)

      partition = self%PartitionRaw(sep%raw)

   end function



   ! Return a string which is the concatenation of the strings in array. The separator between elements is the string
   ! providing this method.
   !
   function JoinBound(self, words) result(joined)
      class(ftlString), intent(in)  :: self
       type(ftlString), intent(in)  :: words(:)
       type(ftlString)              :: joined

       joined = JoinFree(self%raw, words)

   end function
   !
   function JoinFree(sep, words) result(joined)
      character(len=*), intent(in)  :: sep
       type(ftlString), intent(in)  :: words(:)
       type(ftlString)              :: joined

      integer :: joinedLen, wordIdx, writer

      if (size(words) == 0) then
         joined = ''
      else if (size(words) == 1) then
         joined = words(1)
      else

         ! calculate the length of the resulting string
         joinedLen = (size(words) - 1) * len(sep)
         do wordIdx = 1, size(words)
            joinedLen = joinedLen + len(words(wordIdx))
         enddo

         ! allocate output string with the correct length
         joined = repeat('_', joinedLen)

         ! write word by word
         joined%raw(1:len(words(1))) = words(1)%raw
         writer = len(words(1)) + 1
         do wordIdx = 2, size(words)
            joined%raw(writer:writer+len(sep)-1) = sep
            writer = writer + len(sep)
            joined%raw(writer:writer+len(words(wordIdx))-1) = words(wordIdx)%raw
            writer = writer + len(words(wordIdx))
         enddo

      endif

   end function



   ! Return a list of the words in the string, using sep as the delimiter string. If maxsplit is present, at most
   ! maxsplit splits are done (thus, the list will have at most maxsplit+1 elements). If maxsplit is not specified or
   ! -1, then there is no limit on the number of splits (all possible splits are made).
   !
   ! If sep is not present, a different splitting algorithm is applied: runs of consecutive whitespace are
   ! regarded as a single separator, and the result will contain no empty strings at the start or end if the
   ! string has leading or trailing whitespace.  Consequently, splitting an empty string or a string consisting of
   ! just whitespace without a separator returns [].
   !
   function SplitWords(self, maxsplit) result(words)
      class(ftlString), intent(in)           :: self
      integer         , intent(in), optional :: maxsplit
      type(ftlString) , allocatable          :: words(:)

      integer :: idx, wordbegin, wordidx

      if (present(maxsplit)) then
         allocate(words( min(self%CountWords(), maxsplit+1) ))
      else
         allocate(words(self%CountWords()))
      endif

      idx = 1
      do wordidx = 1, size(words)
         do while (CharIsWhitespace(self%At(idx)))
            idx = idx + 1
         enddo
         wordbegin = idx
         if (present(maxsplit) .and. wordidx == size(words)) then
            words(wordidx) = self%raw(wordbegin:len(self%raw))
            if (wordidx /= maxsplit+1) words(wordidx)%raw = trim(words(wordidx)%raw) ! TODO: remove this ugly fix
         else
            do while (idx <= len(self))
               if (CharIsWhitespace(self%At(idx))) exit
               idx = idx + 1
            enddo
            words(wordidx) = self%raw(wordbegin:idx-1)
         endif
      enddo

   end function
   !
   ! If sep is present, consecutive delimiters are not grouped together and are deemed to delimit empty strings
   ! (for example, '1,,2'%split(',') returns ['1', '', '2']). The sep argument may consist of multiple characters
   ! (for example, '1<>2<>3'%split('<>') returns ['1', '2', '3']). Splitting an empty string with a specified
   ! separator returns [''].
   !
   pure function SplitSepRaw(self, sep, maxsplit) result(words)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: sep
      integer         , intent(in), optional :: maxsplit
      type(ftlString) , allocatable          :: words(:)

      integer :: wordbegin, wordidx, nextsepidx


      if (present(maxsplit)) then
         allocate(words( min(self%Count(sep)+1, maxsplit+1) ))
      else
         allocate(words(self%Count(sep)+1))
      endif

      wordbegin = 1
      do wordidx = 1, size(words)
         if (present(maxsplit) .and. wordidx == size(words)) then
            words(wordidx) = self%raw(wordbegin:len(self%raw))
         else
            nextsepidx = self%Find(sep,begin=wordbegin)
            if (nextsepidx < wordbegin) nextsepidx = len(self%raw) + 1
            words(wordidx) = self%raw(wordbegin:nextsepidx-1)
            wordbegin = nextsepidx + len(sep)
         endif
      enddo

   end function
   !
   pure function SplitSepOther(self, sep, maxsplit) result(words)
      class(ftlString), intent(in)           :: self
      type(ftlString) , intent(in)           :: sep
      integer         , intent(in), optional :: maxsplit
      type(ftlString) , allocatable          :: words(:)

      words = SplitSepRaw(self, sep%raw, maxsplit)

   end function



   ! Return a list of the lines in the string, breaking at line boundaries. Line breaks are not included in the resulting list.
   !
   function SplitLines(self) result(lines)
      class(ftlString), intent(in)  :: self
      type(ftlString) , allocatable :: lines(:)

      type(ftlString) :: tmp

      ! TODO: more efficient implementation that doesn't make a copy

      ! special case: empty string
      if (len(self) == 0) then
         allocate(lines(0))
         return
      endif

      ! convert DOS to UNIX line endings
      tmp = self%Replace(achar(13)//achar(10), achar(10))

      ! convert old Mac to UNIX endings
      if (achar(13) .in. tmp) tmp = tmp%Replace(achar(13), achar(10))

      ! remove potential linebreak at the end of the string
      if (tmp%raw(len(tmp%raw):len(tmp%raw)) == achar(10)) then
         tmp%raw = tmp%raw(1:len(tmp%raw)-1)
      endif

      ! use normal split method to do the actual work
      lines = tmp%Split(achar(10))

   end function



   pure logical function StartsWithRaw(self, prefix)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: prefix

      if (len(self) >= len(prefix)) then
         StartsWithRaw = (self%raw(1:len(prefix)) == prefix)
      else
         StartsWithRaw = .false.
      endif

   end function
   !
   pure logical function StartsWithOther(self, prefix)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: prefix

      StartsWithOther = StartsWithRaw(self, prefix%raw)

   end function
   !
   logical function StartsWithArray(self, prefixes)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: prefixes(:)

      integer :: i

      StartsWithArray = .false.
      do i = 1, size(prefixes)
         if (self%StartsWithRaw(prefixes(i)%raw)) then
            StartsWithArray = .true.
            return
         endif
      enddo

   end function



   pure logical function EndsWithRaw(self, postfix)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: postfix

      if (len(self) >= len(postfix)) then
         EndsWithRaw = (self%raw(len(self%raw)-len(postfix)+1:len(self%raw)) == postfix)
      else
         EndsWithRaw = .false.
      endif

   end function
   !
   pure logical function EndsWithOther(self, postfix)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: postfix

      EndsWithOther = EndsWithRaw(self, postfix%raw)

   end function
   !
   logical function EndsWithArray(self, postfixes)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: postfixes(:)

      integer :: i

      EndsWithArray = .false.
      do i = 1, size(postfixes)
         if (self%EndsWithRaw(postfixes(i)%raw)) then
            EndsWithArray = .true.
            return
         endif
      enddo

   end function


   ! Return a copy of the string with the leading and trailing characters removed. The chars argument is a string
   ! specifying the set of characters to be removed. If chars is omitted it defaults to removing whitespace. The chars
   ! argument is not a prefix or suffix; rather, all combinations of its values are stripped.
   !
   elemental type(ftlString) function StripWhitespace(self) result(stripped)
      class(ftlString), intent(in) :: self

      integer :: first, last

      first = verify(self%raw, FTL_STRING_WHITESPACE)
      last = verify(self%raw, FTL_STRING_WHITESPACE, .true.)

      if (first == 0) then
         stripped%raw = ''
      else
         stripped%raw = self%raw(first:last)
      endif

   end function
   !
   elemental type(ftlString) function StripRaw(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: chars

      integer :: first, last

      first = verify(self%raw, chars)
      last = verify(self%raw, chars, .true.)

      if (first == 0) then
         stripped%raw = ''
      else
         stripped%raw = self%raw(first:last)
      endif

   end function
   !
   elemental type(ftlString) function StripString(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: chars

      stripped = self%StripRaw(chars%raw)

   end function


   ! Return a copy of the string with the trailing characters removed. The chars argument is a string specifying the set
   ! of characters to be removed. If chars is omitted it defaults to removing whitespace. The chars argument is not a
   ! prefix or suffix; rather, all combinations of its values are stripped.
   !
   elemental type(ftlString) function RStripWhitespace(self) result(stripped)
      class(ftlString), intent(in) :: self

      integer :: last

      last = verify(self%raw, FTL_STRING_WHITESPACE, .true.)
      stripped%raw = self%raw(1:last)

   end function
   !
   elemental type(ftlString) function RStripRaw(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: chars

      integer :: last

      last = verify(self%raw, chars, .true.)
      stripped%raw = self%raw(1:last)

   end function
   !
   elemental type(ftlString) function RStripString(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: chars

      stripped = self%RStripRaw(chars%raw)

   end function


   ! Return a copy of the string with leading characters removed. The chars argument is a string specifying the set of
   ! characters to be removed. If omitted or None, the chars argument defaults to removing whitespace. The chars
   ! argument is not a prefix; rather, all combinations of its values are stripped:
   !
   elemental type(ftlString) function LStripWhitespace(self) result(stripped)
      class(ftlString), intent(in) :: self

      integer :: first

      first = verify(self%raw, FTL_STRING_WHITESPACE)

      if (first == 0) then
         stripped%raw = ''
      else
         stripped%raw = self%raw(first:)
      endif

   end function
   !
   elemental type(ftlString) function LStripRaw(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: chars

      integer :: first

      first = verify(self%raw, chars)

      if (first == 0) then
         stripped%raw = ''
      else
         stripped%raw = self%raw(first:)
      endif

   end function
   !
   elemental type(ftlString) function LStripString(self, chars) result(stripped)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: chars

      stripped = self%LStripRaw(chars%raw)

   end function



   ! Return the lowest index in the string where substring sub is found within the slice s[begin:end).
   ! Returns 0 if sub is not found.
   !
   pure integer function FindOther(self, sub, begin, end) result(idx)
      class(ftlString), intent(in)           :: self
       type(ftlString), intent(in)           :: sub
      integer         , intent(in), optional :: begin, end

      idx = self%FindRaw(sub%raw, begin, end)

   end function
   !
   pure integer function FindRaw(self, sub, begin, end) result(idx)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: sub
      integer         , intent(in), optional :: begin, end

      integer :: begin_, end_

      if (present(begin)) then
         begin_ = begin
      else
         begin_ = 1
      endif

      if (present(end)) then
         end_ = end
      else
         end_ = len(self%raw) + 1
      endif

      idx = index(self%raw(begin_:end_-1), sub) + begin_ - 1

   end function



   ! Return a copy of the string with all the cased characters converted to uppercase/lowercase.
   !
   elemental type(ftlString) function Upper(self)
      class(ftlString), intent(in) :: self

      integer :: idx, ascii

      Upper = self
      do idx = 1, len(Upper%raw)
         ascii = iachar(Upper%raw(idx:idx))
         if (ascii >= 97 .and. ascii <= 122) Upper%raw(idx:idx) = achar(ascii-32)
      enddo

   end function
   !
   elemental type(ftlString) function Lower(self)
      class(ftlString), intent(in) :: self

      integer :: idx, ascii

      Lower = self
      do idx = 1, len(Lower%raw)
         ascii = iachar(Lower%raw(idx:idx))
         if (ascii >= 65 .and. ascii <= 90) Lower%raw(idx:idx) = achar(ascii+32)
      enddo

   end function



   ! Return true if there are only whitespace characters in the string and there is at least one character, false otherwise.
   !
   elemental logical function IsSpace(self)
      class(ftlString), intent(in) :: self

      IsSpace = (len(self) > 0 .and. verify(self, FTL_STRING_WHITESPACE) == 0)

   end function



   ! Return a copy of the string with all occurrences of substring old replaced by new. If the optional argument count is given,
   ! only the first count occurrences are replaced.
   !
   type(ftlString) function ReplaceRawWithRaw(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: old
      character(len=*), intent(in)           :: new
      integer         , intent(in), optional :: count

      if (len(old) == len(new)) then
         if (len(old) == 1) then
            str = self%ReplaceImplementationSingleChar(old, new, count)
         else
            str = self%ReplaceImplementationEqualLength(old, new, count)
         endif
      else
         str = self%ReplaceImplementationGeneral(old, new, count)
      endif

   end function
   !
   type(ftlString) function ReplaceStringWithRaw(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
       type(ftlString), intent(in)           :: old
      character(len=*), intent(in)           :: new
      integer         , intent(in), optional :: count

      str = self%ReplaceRawWithRaw(old%raw, new, count)

   end function
   !
   type(ftlString) function ReplaceRawWithString(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: old
       type(ftlString), intent(in)           :: new
      integer         , intent(in), optional :: count

      str = self%ReplaceRawWithRaw(old, new%raw, count)

   end function
   !
   type(ftlString) function ReplaceStringWithString(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
       type(ftlString), intent(in)           :: old
       type(ftlString), intent(in)           :: new
      integer         , intent(in), optional :: count

      str = self%ReplaceRawWithRaw(old%raw, new%raw, count)

   end function
   !
   ! Actual implementations of Replace:
   !
   type(ftlString) function ReplaceImplementationSingleChar(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character       , intent(in)           :: old
      character       , intent(in)           :: new
      integer         , intent(in), optional :: count

      integer :: i, replacements

      str%raw = self%raw
      replacements = 0
      do i = 1, len(str%raw)
         if (str%raw(i:i) == old) then
            str%raw(i:i) = new
            if (present(count)) then
               replacements = replacements + 1
               if (replacements == count) return
            endif
         endif
      enddo

   end function
   !
   type(ftlString) function ReplaceImplementationEqualLength(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: old
      character(len=*), intent(in)           :: new
      integer         , intent(in), optional :: count

      integer :: doneEnd, replacements, nextIdx

      str%raw = self%raw
      doneEnd = 1
      replacements = 0
      do while (.true.)
         nextIdx = str%Find(old, begin=doneEnd)
         if (nextIdx >= doneEnd) then ! found one more to replace
            str%raw(nextIdx:nextIdx+len(old)-1) = new
            doneEnd = nextIdx + len(old)
            if (present(count)) then
               replacements = replacements + 1
               if (replacements == count) return
            endif
         else
            return
         endif
      enddo

   end function
   !
   type(ftlString) function ReplaceImplementationGeneral(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: old
      character(len=*), intent(in)           :: new
      integer         , intent(in), optional :: count

      integer :: replacements, readEnd, writeEnd, readNext, numOcc

      ! count how many occurrences we need to replace
      numOcc = self%Count(old)
      if (present(count)) numOcc = min(numOcc, count)

      ! allocate output string with the correct size
      str%raw = repeat('_', len(self%raw)+numOcc*(len(new)-len(old)))

      ! do the replacements
      readEnd = 1
      writeEnd = 1
      replacements = 0
      do while (replacements < numOcc)
         readNext = self%Find(old, begin=readEnd)
         str%raw(writeEnd:writeEnd+(readNext-readEnd)-1) = self%raw(readEnd:readNext-1)
         writeEnd = writeEnd + (readNext-readEnd)
         readEnd = readNext
         str%raw(writeEnd:writeEnd+len(new)-1) = new
         writeEnd = writeEnd + len(new)
         readEnd = readEnd + len(old)
         replacements = replacements + 1
      enddo

      ! all replacements done, copy the rest of the string
      str%raw(writeEnd:len(str%raw)) = self%raw(readEnd:len(self%raw))

   end function



   ! =============> Other string methods:



   ! Count the number of words separated by whitespace (spaces or tabs). Ignores leading and trailing whitespace.
   !
   elemental integer function CountWords(self)
      class(ftlString), intent(in) :: self

      integer :: idx

      if (len(self%raw) == 0) then
         CountWords = 0
         return
      endif

      if (CharIsWhitespace(self%raw(1:1))) then
         CountWords = 0
      else
         CountWords = 1
      endif
      idx = 1
      do idx = 2, len(self%raw)
         if (CharIsWhitespace(self%raw(idx-1:idx-1)) .and. .not.CharIsWhitespace(self%raw(idx:idx))) then
            CountWords = CountWords + 1
         endif
      enddo

   end function



   ! =============> FTL methods:



   pure integer function ftlHashString(str)
      use ftlHashModule
      class(ftlString), intent(in) :: str

      ftlHashString = ftlHash(str%raw)

   end function



   subroutine ftlSwapString(str1, str2)
      type(ftlString), intent(inout) :: str1, str2

      character(len=:), allocatable  :: tmp

      call move_alloc(str1%raw, tmp)
      call move_alloc(str2%raw, str1%raw)
      call move_alloc(tmp, str2%raw)

   end subroutine



   subroutine ftlMoveString(src, dest)
      type(ftlString), intent(inout) :: src
      type(ftlString), intent(out)   :: dest

      call move_alloc(src%raw, dest%raw)

   end subroutine
   !
   subroutine ftlMoveRawToString(src, dest)
      character(len=:), allocatable, intent(inout) :: src
      type(ftlString)              , intent(out)   :: dest

      call move_alloc(src, dest%raw)

   end subroutine
   !
   subroutine ftlMoveStringToRaw(src, dest)
      type(ftlString)              , intent(inout) :: src
      character(len=:), allocatable, intent(out)   :: dest

      call move_alloc(src%raw, dest)

   end subroutine




! ====== Implementation of ftlDynArrayIterator methods ===========================================================================



   subroutine NewItDefault(self)
      class(ftlStringIterator), intent(out) :: self

      ! Nothing to do here: intent(out) already resets everything

   end subroutine
   !
   subroutine NewItCopyOther(self, other)
      class(ftlStringIterator), intent(out), target :: self
      class(ftlStringIterator), intent(in)          :: other

      self%str => other%str
      self%index = other%index
      if (len(self%str) > 0 .and. self%index <= len(self%str)) self%value => self%str%raw(self%index:self%index)

   end subroutine



   ! =============> Arithmetic operations:



   subroutine Inc(self)
      class(ftlStringIterator), intent(inout), target :: self

      self%index = self%index + 1
      if (self%index <= len(self%str)) then
         self%value => self%str%raw(self%index:self%index)
      else
         nullify(self%value)
      endif

   end subroutine
   !
   subroutine Dec(self)
      class(ftlStringIterator), intent(inout), target :: self

      self%index = self%index - 1
      if (self%index > 0) then
         self%value => self%str%raw(self%index:self%index)
      else
         nullify(self%value)
      endif

   end subroutine



   function AdvanceN(self, n)
      type(ftlStringIterator), intent(in) :: self
      integer                , intent(in) :: n
      type(ftlStringIterator), target     :: AdvanceN

      call AdvanceN%New(self)
      AdvanceN%index = AdvanceN%index + n
      if (AdvanceN%index <= len(AdvanceN%str%raw)) then
         AdvanceN%value => AdvanceN%str%raw(AdvanceN%index:AdvanceN%index)
      else
         nullify(AdvanceN%value)
      endif

   end function
   !
   function ReverseN(self, n)
      type(ftlStringIterator), intent(in) :: self
      integer                , intent(in) :: n
      type(ftlStringIterator), target     :: ReverseN

      call ReverseN%New(self)
      ReverseN%index = ReverseN%index - n
      if (ReverseN%index > 0) then
         ReverseN%value => ReverseN%str%raw(ReverseN%index:ReverseN%index)
      else
         nullify(ReverseN%value)
      endif

   end function



   pure integer function DiffOther(self, other)
      class(ftlStringIterator), intent(in) :: self
      class(ftlStringIterator), intent(in) :: other

      if (associated(self%str,other%str)) then
         DiffOther = self%index - other%index
      else
         DiffOther = huge(0)
      endif

   end function



   ! =============> Logical operations:



   pure logical function EqualOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      EqualOther = associated(self%str,other%str) .and. (self%index == other%index)

   end function
   !
   pure logical function UnequalOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      UnequalOther = .not.associated(self%str,other%str) .or. (self%index /= other%index)

   end function
   !
   pure logical function SmallerOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      SmallerOther = associated(self%str,other%str) .and. (self%index < other%index)

   end function
   !
   pure logical function SmallerEqualOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      SmallerEqualOther = associated(self%str,other%str) .and. (self%index <= other%index)

   end function
   !
   pure logical function GreaterOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      GreaterOther = associated(self%str,other%str) .and. (self%index > other%index)

   end function
   !
   pure logical function GreaterEqualOther(self, other)
      type(ftlStringIterator), intent(in) :: self
      type(ftlStringIterator), intent(in) :: other

      GreaterEqualOther = associated(self%str,other%str) .and. (self%index >= other%index)

   end function



! ====== Auxilliary methods working on single characters =========================================================================


   pure logical function CharIsWhitespace(c)
      character, intent(in) :: c
      CharIsWhitespace = (scan(c,FTL_STRING_WHITESPACE) /= 0)
   end function


end module
#endif
