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
   use iso_fortran_env, only: IOSTAT_INQUIRE_INTERNAL_UNIT, IOSTAT_END, INT32, INT64, REAL32, REAL64

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
      procedure         :: NewFromInt32, NewFromInt64
      procedure         :: NewFromReal32, NewFromReal64
      procedure         :: NewFromComplex32, NewFromComplex64
      procedure         :: NewFromLogical
      generic  , public :: New => NewDefault, NewCopyOther, NewFromRaw, &
                                  NewFromInt32, NewFromInt64, NewFromReal32, NewFromReal64, &
                                  NewFromComplex32, NewFromComplex64, NewFromLogical
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
      procedure, public :: SplitLinesInplace
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
      generic  , public :: RemovePrefix => RemovePrefixRaw, RemovePrefixString
      procedure         :: RemovePrefixRaw
      procedure         :: RemovePrefixString
      generic  , public :: RemoveSuffix => RemoveSuffixRaw, RemoveSuffixString
      procedure         :: RemoveSuffixRaw
      procedure         :: RemoveSuffixString

      ! Other string methods:
      procedure, public :: CountWords
      generic  , public :: LevenshteinDistance => LevenshteinDistanceRaw, LevenshteinDistanceString
      procedure         :: LevenshteinDistanceRaw
      procedure         :: LevenshteinDistanceString


      ! Assignment:
#if defined(__INTEL_COMPILER) && __INTEL_COMPILER < 1900 && __INTEL_COMPILER >= 1800
      ! ifort 18 seems to have problems with cleaning up the left hand side of a character(:), allocatable
      ! assignment.  This is normally what would happen in the intrinsic assignments of ftlStrings. Therefore we make a defined
      ! assignment for ftlString that does the cleanup of the lhs explicitly, to at least fix these memory leaks when using
      ! ftlStrings ...
      generic, public :: assignment(=) => NewCopyOther
      ! Note: ifort 18 does NOT like to have a defined assignment for ftlString in a couple of scenarios, see the
      ! testContainingTypeAssignment regression test for ftlString. Relying on the intrinsic assignment makes that test work, but
      ! will bring back the leaking, which is probably worse ...
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

      ! Fortran style .ieq. operator for comparisons ignoring leading/trailing whitspace as well as case
      procedure, pass(lhs) :: StringIeqString
      procedure, pass(lhs) :: StringIeqChar
      procedure, pass(rhs) :: CharIeqString
      generic  , public    :: operator(.ieq.) => StringIeqString, StringIeqChar, CharIeqString

      ! Fortran style .nieq. operator for comparisons ignoring leading/trailing whitspace as well as case
      procedure, pass(lhs) :: StringNieqString
      procedure, pass(lhs) :: StringNieqChar
      procedure, pass(rhs) :: CharNieqString
      generic  , public    :: operator(.nieq.) => StringNieqString, StringNieqChar, CharNieqString

   end type


   ! Constructor functions:

   interface ftlString
      module procedure NewDefaultConstr
      module procedure NewCopyOtherConstr
      module procedure NewFromRawConstr
      module procedure NewFromInt32Constr
      module procedure NewFromInt64Constr
      module procedure NewFromReal32Constr
      module procedure NewFromReal64Constr
      module procedure NewFromComplex32Constr
      module procedure NewFromComplex64Constr
      module procedure NewFromLogicalConstr
   end interface


   ! Assignment of ftlString to raw Fortran strings

   public :: assignment(=)
   interface assignment(=)
      module procedure AssignToAllocatableRaw
      module procedure AssignFromRaw
   end interface

   public :: Raw
   interface Raw
      module procedure StringToRaw
   end interface


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

      strlen = 0
      do while (cstr(strlen+1) /= C_NULL_CHAR)
         strlen = strlen + 1
      enddo
      allocate(character(len=strlen)::self%raw)
      do i = 1, strlen
         self%raw(i:i) = cstr(i)
      enddo

   end subroutine
   !
   pure subroutine NewFromInt32(self, i, format)
      class(ftlString), intent(inout)        :: self
      integer(INT32)  , intent(in)           :: i
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
   pure subroutine NewFromInt64(self, i, format)
      class(ftlString), intent(inout)        :: self
      integer(INT64)  , intent(in)           :: i
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
   pure subroutine NewFromReal32(self, r, format)
      class(ftlString), intent(inout)        :: self
      real(REAL32)    , intent(in)           :: r
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
   pure subroutine NewFromReal64(self, r, format)
      class(ftlString), intent(inout)        :: self
      real(REAL64)    , intent(in)           :: r
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
   pure subroutine NewFromComplex32(self, c, format)
      class(ftlString), intent(inout)        :: self
      complex(REAL32) , intent(in)           :: c
      character(len=*), intent(in), optional :: format

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
   pure subroutine NewFromComplex64(self, c, format)
      class(ftlString), intent(inout)        :: self
      complex(REAL64) , intent(in)           :: c
      character(len=*), intent(in), optional :: format

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
   pure subroutine NewFromLogical(self, l, format)
      class(ftlString), intent(inout)        :: self
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
   type(ftlString) elemental function NewFromInt32Constr(i, format) result(str)
      integer(INT32)  , intent(in)           :: i
      character(len=*), intent(in), optional :: format
      call str%NewFromInt32(i, format)
   end function
   !
   type(ftlString) elemental function NewFromInt64Constr(i, format) result(str)
      integer(INT64)  , intent(in)           :: i
      character(len=*), intent(in), optional :: format
      call str%NewFromInt64(i, format)
   end function
   !
   type(ftlString) elemental function NewFromReal32Constr(r, format) result(str)
      real(REAL32)    , intent(in)           :: r
      character(len=*), intent(in), optional :: format
      call str%NewFromReal32(r, format)
   end function
   !
   type(ftlString) elemental function NewFromReal64Constr(r, format) result(str)
      real(REAL64)    , intent(in)           :: r
      character(len=*), intent(in), optional :: format
      call str%NewFromReal64(r, format)
   end function
   !
   type(ftlString) elemental function NewFromComplex32Constr(c, format) result(str)
      complex(REAL32) , intent(in)           :: c
      character(len=*), intent(in), optional :: format
      call str%NewFromComplex32(c, format)
   end function
   !
   type(ftlString) elemental function NewFromComplex64Constr(c, format) result(str)
      complex(REAL64) , intent(in)           :: c
      character(len=*), intent(in), optional :: format
      call str%NewFromComplex64(c, format)
   end function
   !
   type(ftlString) elemental function NewFromLogicalConstr(l, format) result(str)
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



   ! =============>  Assignment of ftlString to/from raw Fortran strings:



   subroutine AssignToAllocatableRaw(lhs, rhs)
      character(len=:), allocatable, intent(inout) :: lhs
      type(ftlString)              , intent(in)    :: rhs

      if (allocated(rhs%raw)) then
         lhs = rhs%raw
      else
         if (allocated(lhs)) deallocate(lhs)
      endif

   end subroutine
   !
   elemental subroutine AssignFromRaw(lhs, rhs)
      type(ftlString) , intent(inout) :: lhs
      character(len=*), intent(in)    :: rhs

      character(len=:), allocatable :: tmp

      ! Assigns a raw Fortran string to an ftlString

      tmp = rhs ! rhs and lhs%raw might alias! Make sure lhs%raw is not deallocated before we read from raw ...
      call move_alloc(tmp, lhs%raw)

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
         allocate(character(len=length) :: raw)
         raw(1:min(len(str%raw), length)) = str%raw(1:min(len(str%raw), length))
         raw(min(len(str%raw), length)+1:) = ' '
      else
         raw = str%raw
      endif

   end function



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



   ! Fortran style .ieq. operator for comparisons ignoring leading/trailing whitspace as well as case.
   !
   elemental logical function StringIeqString(lhs, rhs) result(eqv)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

       eqv = lhs%StringIeqChar(rhs%raw)

   endfunction
   !
   elemental logical function StringIeqChar(lhs, rhs) result(ieq)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      integer :: lhsFirst, lhsLast, rhsFirst, rhsLast
      integer :: lhsLen, rhsLen
      integer :: ic, lc, rc

      ! Find start and end of lhs
      lhsFirst = verify(lhs%raw, FTL_STRING_WHITESPACE)
      lhsLast  = verify(lhs%raw, FTL_STRING_WHITESPACE, .true.)
      lhsLen = lhsLast-lhsFirst+1

      ! Find start and end of rhs
      rhsFirst = verify(rhs, FTL_STRING_WHITESPACE)
      rhsLast  = verify(rhs, FTL_STRING_WHITESPACE, .true.)
      rhsLen = rhsLast-rhsFirst+1

      if (lhsFirst == 0 .and. rhsFirst == 0) then
         ! both are pure whitespace
         ieq = .true.
      else if (lhsFirst == 0) then
         ! lhs is pure whitespace, but rhs is not
         ieq = .false.
      else if (rhsFirst == 0) then
         ! lhs is not pure whitespace, but rhs is
         ieq = .false.
      else if (lhsLen /= rhsLen) then
         ! both lhs and rhs are not pure whitespace but their stripped length is different
         ieq = .false.
      else
         ! both lhs and rhs are not pure whitespace and their stripped length is the same
         do ic = 0, lhsLen-1
            lc = iachar(lhs%raw(lhsFirst+ic:lhsFirst+ic))
            if (lc >= 97 .and. lc <= 122) lc = lc-32
            rc = iachar(rhs(rhsFirst+ic:rhsFirst+ic))
            if (rc >= 97 .and. rc <= 122) rc = rc-32
            if (lc /= rc) then
               ieq = .false.
               return
            endif
         enddo
         ieq = .true.
      endif

   endfunction
   !
   elemental logical function CharIeqString(lhs, rhs) result(ieq)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      ieq = rhs%StringIeqChar(lhs)

   endfunction



   ! Fortran style .nieq. operator for comparisons ignoring leading/trailing whitspace as well as case.
   !
   elemental logical function StringNieqString(lhs, rhs) result(nieq)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

       nieq = .not.lhs%StringIeqChar(rhs%raw)

   endfunction
   !
   elemental logical function StringNieqChar(lhs, rhs) result(nieq)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      nieq = .not.lhs%StringIeqChar(rhs)

   endfunction
   !
   elemental logical function CharNieqString(lhs, rhs) result(nieq)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      nieq = .not.rhs%StringIeqChar(lhs)

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

      if (allocated(self%raw)) then
         ftlLen = len(self%raw)
      else
         ftlLen = 0
      endif

   end function



   pure integer function ftlLenTrim(self)
      class(ftlString), intent(in) :: self

      if (allocated(self%raw)) then
         ftlLenTrim = len_trim(self%raw)
      else
         ftlLenTrim = 0
      endif

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
   elemental complex(FTL_KREAL) function ToComplex(self)
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



   subroutine ReadUntilEOF(self, unit, iostat)
      class(ftlString), intent(inout)         :: self
      integer         , intent(in)            :: unit
      integer         , intent(out), optional :: iostat

      integer :: ios, nRead, newlen
      type(ftlString) :: line
      character(len=:), allocatable :: buffer, newbuffer

      self = ''
      if (present(iostat)) iostat = IOSTAT_END

      call line%ReadLine(unit, ios)
      if (ios > 0 .and. ios /= IOSTAT_INQUIRE_INTERNAL_UNIT) then
         if (present(iostat)) iostat = ios
         return
      endif
      if (is_iostat_end(ios)) return

      buffer = line%raw
      nRead = len(line)

      do while (.true.)
         call line%ReadLine(unit, ios)
         if (ios > 0 .and. ios /= IOSTAT_INQUIRE_INTERNAL_UNIT) then
            if (present(iostat)) iostat = ios
            return
         endif
         if (is_iostat_end(ios)) exit
         if (len(buffer) < nRead + 1 + len(line%raw)) then
            ! not enough space anymore, we need to enlarge the buffer
            newlen = max(2*len(buffer), nRead + 1 + len(line%raw))
            allocate(character(len=newlen) :: newbuffer)
            newbuffer(1:len(buffer)) = buffer
            call move_alloc(newbuffer, buffer)
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
   ! If sep is empty, it is interpreted as the first character boundary in the string. In that case the result is
   ! ['', '', self] (for example, 'abc'%Partition('') returns ['', '', 'abc']).
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
         allocate(character(len=joinedLen) :: joined%raw)

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

      integer :: ms, i, j, k, slen, numwords
      type(ftlString), allocatable :: tmp_words(:)

      if (present(maxsplit)) then
         if (maxsplit >= 0) then
            allocate(words(maxsplit+1))
            ms = maxsplit
         else
            allocate(words(128))
            ms = HUGE(ms)
         endif
      else
         allocate(words(128))
         ms = HUGE(ms)
      endif

      i = 1
      j = 1
      slen = len(self%raw)
      numwords = 0
      do while (i <= slen)
         do while (i <= slen)
            if (CharIsWhitespace(self%raw(i:i))) then
               i = i + 1
            else
               exit
            endif
         enddo
         j = i
         do while (i <= slen)
            if (.not.CharIsWhitespace(self%raw(i:i))) then
               i = i + 1
            else
               exit
            endif
         enddo
         if (j < i) then
            if (ms <= 0) exit
            ms = ms - 1
            call PushBack(self%raw(j:i-1))
            do while (i <= slen)
               if (CharIsWhitespace(self%raw(i:i))) then
                  i = i + 1
               else
                  exit
               endif
            enddo
            j = i
         endif
      end do
      if (j <= slen) then
         call PushBack(self%raw(j:slen))
      endif

      allocate(tmp_words(numwords))
      do k = 1, numwords
         call move_alloc(words(k)%raw, tmp_words(k)%raw)
      enddo
      call move_alloc(tmp_words, words)

      contains

         subroutine PushBack(str)
            character(*), intent(in) :: str

            numwords = numwords + 1
            if (numwords > size(words)) then
               allocate(tmp_words(2*size(words)))
               do k = 1, numwords-1
                  call move_alloc(words(k)%raw, tmp_words(k)%raw)
               enddo
               call move_alloc(tmp_words, words)
            endif
            words(numwords) = str

         end subroutine

   end function
   !
   ! If sep is present, consecutive delimiters are not grouped together and are deemed to delimit empty strings
   ! (for example, '1,,2'%split(',') returns ['1', '', '2']). The sep argument may consist of multiple characters
   ! (for example, '1<>2<>3'%split('<>') returns ['1', '2', '3']). Splitting an empty string with a specified
   ! non-empty separator returns [''].
   !
   ! If sep is empty, we split at character boundaries and include leading/trailing empty strings (for example,
   ! 'ab'%split('') returns ['', 'a', 'b', '']).
   !
   pure function SplitSepRaw(self, sep, maxsplit) result(words)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: sep
      integer         , intent(in), optional :: maxsplit
      type(ftlString) , allocatable          :: words(:)

      integer :: wordbegin, wordidx, nextsepidx, slen, numSplits

      slen = len(self%raw)

      if (len(sep) == 0) then
         if (present(maxsplit)) then
            if (maxsplit == 0) then
               allocate(words(1))
               words(1) = self
               return
            endif
         endif

         if (present(maxsplit)) then
            if (maxsplit < 0) then
               numSplits = slen + 1
            else
               numSplits = min(maxsplit, slen+1)
            endif
         else
            numSplits = slen + 1
         endif

         allocate(words(numSplits+1))
         words(1) = ''

         do wordidx = 2, numSplits
            words(wordidx) = self%raw(wordidx-1:wordidx-1)
         enddo

         if (numSplits <= slen) then
            words(numSplits+1) = self%raw(numSplits:)
         else
            words(numSplits+1) = ''
         endif
         return

      endif

      if (present(maxsplit)) then
         if (maxsplit < 0) then
            allocate(words(self%Count(sep)+1))
         else
            allocate(words( min(self%Count(sep)+1, maxsplit+1) ))
         endif
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

      call self%SplitLinesInplace(lines)

   end function
   !
   subroutine SplitLinesInplace(self, lines)
      class(ftlString),              intent(in)    :: self
      type(ftlString) , allocatable, intent(inout) :: lines(:)

      integer :: i, j, k, slen, numlines
      type(ftlString), allocatable :: tmp_lines(:)

      character, parameter :: LF = achar(10)
      character, parameter :: CR = achar(13)

      ! special case: empty string
      slen = len(self%raw)
      if (slen == 0) then
         if (allocated(lines)) deallocate(lines)
         allocate(lines(0))
         return
      else if (.not.allocated(lines)) then
         allocate(lines(max(32, ceiling(real(slen)/128.0))))
      endif

      i = 1
      j = 1
      numlines = 0
      do while (i <= slen)
         do while (i <= slen)
            if (self%raw(i:i) == LF) then
               ! LF is a Unix style linebreak
               call PushBack(self%raw(j:i-1))
               i = i + 1
               j = i
            else if (self%raw(i:i) == CR) then
               ! CR     is an old Mac style linebreak
               ! CR//LF is a Windows style linebreak
               call PushBack(self%raw(j:i-1))
               i = i + 1
               j = i
               ! ... both end a line, but in case of CR//LF we need to continue reading in char further
               if (i <= slen) then
                  if (self%raw(i:i) == LF) then
                     i = i + 1
                     j = i
                  endif
               endif
            else
               i = i + 1
            endif
         enddo
      enddo
      if (j <= slen) then
         call PushBack(self%raw(j:slen))
      endif

      allocate(tmp_lines(numlines))
      do k = 1, numlines
         call move_alloc(lines(k)%raw, tmp_lines(k)%raw)
      enddo
      call move_alloc(tmp_lines, lines)

      contains

         subroutine PushBack(str)
            character(*), intent(in) :: str

            numlines = numlines + 1
            if (numlines > size(lines)) then
               allocate(tmp_lines(max(32, 2*size(lines))))
               do k = 1, numlines-1
                  call move_alloc(lines(k)%raw, tmp_lines(k)%raw)
               enddo
               call move_alloc(tmp_lines, lines)
            endif
            lines(numlines) = str

         end subroutine

   end subroutine



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
   ! If old is empty, replacement happens at character boundaries, including at the start and end of the string
   ! (for example, 'ab'%replace('', 'c') returns 'cacbc').
   !
   type(ftlString) function ReplaceRawWithRaw(self, old, new, count) result(str)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: old
      character(len=*), intent(in)           :: new
      integer         , intent(in), optional :: count

      integer :: numInsertions, readPos, writePos

      if (len(old) == 0) then
         numInsertions = len(self%raw) + 1
         if (present(count)) then
            if (count >= 0) numInsertions = min(count, len(self%raw)+1)
         endif

         allocate(character(len=len(self%raw)+numInsertions*len(new)) :: str%raw)

         readPos = 1
         writePos = 1
         do while (numInsertions > 0)
            if (len(new) > 0) then
               str%raw(writePos:writePos+len(new)-1) = new
               writePos = writePos + len(new)
            endif
            if (readPos <= len(self%raw)) then
               str%raw(writePos:writePos) = self%raw(readPos:readPos)
               writePos = writePos + 1
               readPos = readPos + 1
            endif
            numInsertions = numInsertions - 1
         enddo
         if (readPos <= len(self%raw)) str%raw(writePos:) = self%raw(readPos:)
      else if (len(old) == len(new)) then
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
         if (present(count)) then
            if (count >= 0 .and. replacements >= count) return
         endif
         if (str%raw(i:i) == old) then
            str%raw(i:i) = new
            replacements = replacements + 1
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
         if (present(count)) then
            if (count >= 0 .and. replacements >= count) return
         endif
         nextIdx = str%Find(old, begin=doneEnd)
         if (nextIdx >= doneEnd) then ! found one more to replace
            str%raw(nextIdx:nextIdx+len(old)-1) = new
            doneEnd = nextIdx + len(old)
            replacements = replacements + 1
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
      if (present(count)) then
         if (count >= 0) numOcc = min(numOcc, count)
      endif

      ! allocate output string with the correct size
      allocate(character(len=len(self%raw)+numOcc*(len(new)-len(old))) :: str%raw)

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


   ! If the string starts with the prefix string, return string[len(prefix):]. Otherwise, return a copy of the original string.
   !
   type(ftlString) function RemovePrefixRaw(self, prefix) result(str)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: prefix

      if (self%StartsWithRaw(prefix)) then
         str%raw = self%raw(len(prefix)+1:len(self%raw))
      else
         str%raw = self%raw
      endif

   end function
   !
   type(ftlString) function RemovePrefixString(self, prefix) result(str)
      class(ftlString), intent(in) :: self
      type(ftlString),  intent(in) :: prefix
      str = self%RemovePrefixRaw(prefix%raw)
   end function


   ! If the string ends with the suffix string and that suffix is not empty, return string[:-len(suffix)]. Otherwise,
   ! return a copy of the original string.
   !
   type(ftlString) function RemoveSuffixRaw(self, prefix) result(str)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: prefix

      if (self%EndsWithRaw(prefix)) then
         str%raw = self%raw(1:len(self%raw)-len(prefix))
      else
         str%raw = self%raw
      endif

   end function
   !
   type(ftlString) function RemoveSuffixString(self, prefix) result(str)
      class(ftlString), intent(in) :: self
      type(ftlString),  intent(in) :: prefix
      str = self%RemoveSuffixRaw(prefix%raw)
   end function




   ! =============> Other string methods:



   ! Count the number of words separated by whitespace (spaces or tabs). Ignores leading and trailing whitespace.
   !
   elemental integer function CountWords(self)
      class(ftlString), intent(in) :: self

      integer :: i
      logical :: inWord, ciw

      CountWords = 0
      if (len(self%raw) == 0) return

      inWord = .not.CharIsWhitespace(self%raw(1:1))
      if (inWord) CountWords = 1

      do i = 2, len(self%raw)
         ciw = CharIsWhitespace(self%raw(i:i))
         if (inWord .and. ciw) then
            inWord = .false.
         else if (.not.inWord .and. .not.ciw) then
            inWord = .true.
            CountWords = CountWords + 1
         endif
      enddo

   end function




   ! Computes the LevenshteinDistance (edit distance) between two strings.
   ! See: https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
   !
   integer function LevenshteinDistanceString(s1, s2)
      class(ftlString), intent(in) :: s1
       type(ftlString), intent(in) :: s2

      LevenshteinDistanceString = s1%LevenshteinDistanceRaw(s2%raw)

   end function
   !
   integer function LevenshteinDistanceRaw(s1, s2)
      class(ftlString), intent(in) :: s1
      character(*),     intent(in) :: s2

      integer :: i,j
      integer, allocatable :: d(:,:)

      allocate(d(0:len(s1%raw),0:len(s2))); d = 0

      do i = 0, len(s1%raw)
         d(i,0) = i
      enddo

      do j = 0, len(s2)
         d(0,j) = j
      enddo

      do j = 1, len(s2)
         do i = 1, len(s1%raw)
            if (s1%raw(i:i) == s2(j:j)) then
               d(i,j) = d(i-1,j-1)
            else
               d(i,j) = min( d(i-1,j)+1 , d(i,j-1)+1 , d(i-1,j-1)+1 )
            endif
         enddo
      enddo

      LevenshteinDistanceRaw = d(len(s1%raw),len(s2))

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
      class(ftlStringIterator), intent(inout) :: self

      nullify(self%str)
      self%index = 0
      nullify(self%value)

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

      integer :: i

      do i = 1, len(FTL_STRING_WHITESPACE)
         if (c == FTL_STRING_WHITESPACE(i:i)) then
            CharIsWhitespace = .true.
            return
         endif
      enddo
      CharIsWhitespace = .false.

   end function


end module
#endif
