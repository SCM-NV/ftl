! Copyright (c) 2016, 2017  Robert Rüger
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

! TODO: consistent behaviour for uninitialized ftlStrings (should behave like '')

#define FTL_CONTAINER ftlString
#define FTL_CONTAINER_PROVIDES_RANDOM_ACCESS_ITERATOR

#ifndef FTL_SKIP_IMPLEMENTATION

module ftlStringModule

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
      procedure, public :: Split
      procedure         :: StartsWithRaw
      procedure         :: StartsWithOther
      procedure         :: StartsWithArray
      generic  , public :: StartsWith => StartsWithRaw, StartsWithOther, StartsWithArray
      procedure         :: FindRaw
      procedure         :: FindOther
      generic  , public :: Find => FindRaw, FindOther
      procedure, public :: Upper
      procedure, public :: Lower
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

      ! Derived-type IO
      procedure         :: writeUnformatted
      generic  , public :: write(unformatted) => writeUnformatted
      procedure         :: writeFormatted
      generic  , public :: write(formatted) => writeFormatted
      procedure         :: readUnformatted
      generic  , public :: read(unformatted) => readUnformatted
      procedure         :: readFormatted
      generic  , public :: read(formatted) => readFormatted

      ! Overloaded operators:

      generic  , public :: assignment(=) => NewCopyOther, NewFromRaw

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

      ! // operator with raw Fortran string output
      procedure, pass(lhs) :: StringCatString
      procedure, pass(lhs) :: StringCatChar
      procedure, pass(rhs) :: CharCatString
      generic  , public    :: operator(//) => StringCatString, StringCatChar, CharCatString

      ! .cat. operator with ftlString output
      procedure, pass(lhs) :: StringCatOpString
      procedure, pass(lhs) :: StringCatOpChar
      procedure, pass(rhs) :: CharCatOpString
      generic  , public    :: operator(.cat.) => StringCatOpString, StringCatOpChar, CharCatOpString

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
      class(ftlString), intent(out) :: self

      ! Constructs an empty string, with a length of zero characters.

      self%raw = ''

   end subroutine
   !
   subroutine NewCopyOther(self, other)
      class(ftlString), intent(out) :: self
       type(ftlString), intent(in)  :: other

      ! Constructs a copy of other.

      if (allocated(other%raw)) then
         self%raw = other%raw
      else
         if (allocated(self%raw)) deallocate(self%raw)
      endif

   end subroutine
   !
   subroutine NewFromRaw(self, raw)
      class(ftlString), intent(out) :: self
      character(len=*), intent(in)  :: raw

      ! Constructs an ftlString from a raw Fortran string

      self%raw = raw

   end subroutine
   !
   subroutine NewFromInt(self, i, format)
      class(ftlString), intent(out)           :: self
      integer         , intent(in)            :: i
      character(len=*), intent(in) , optional :: format

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
      class(ftlString), intent(out)           :: self
      real            , intent(in)            :: r
      character(len=*), intent(in) , optional :: format

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
      class(ftlString), intent(out)           :: self
      complex         , intent(in)            :: c
      character(len=*), intent(in) , optional :: format

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
      class(ftlString), intent(out)           :: self
      logical         , intent(in)            :: l
      character(len=*), intent(in) , optional :: format

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
      real            , intent(in)           :: r
      character(len=*), intent(in), optional :: format
      call str%NewFromReal(r, format)
   end function
   !
   type(ftlString) function NewFromComplexConstr(c, format) result(str)
      complex         , intent(in)           :: c
      character(len=*), intent(in), optional :: format
      call str%NewFromComplex(c, format)
   end function
   !
   type(ftlString) function NewFromLogicalConstr(l, format) result(str)
      logical         , intent(in)           :: l
      character(len=*), intent(in), optional :: format
      call str%NewFromLogical(l, format)
   end function




   ! Destroys the ftlString object. This deallocates all the storage capacity allocated by the ftlString.
   !
   subroutine Delete(self)
      class(ftlString), intent(out) :: self

      ! Nothing to do here: intent(out) will deallocate self%raw

   end subroutine



   ! =============> Derived-type IO:



   subroutine writeUnformatted(self, unit, iostat, iomsg)
      class(ftlString), intent(in)    :: self
      integer         , intent(in)    :: unit
      integer         , intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg

      if (allocated(self%raw)) then
         write (unit, iostat=iostat, iomsg=iomsg) self%raw
      else
         write (unit, iostat=iostat, iomsg=iomsg) ''
      endif

   end subroutine
   !
   subroutine writeFormatted(self, unit, iotype, v_list, iostat, iomsg)
      class(ftlString), intent(in)    :: self
      integer         , intent(in)    :: unit
      character(len=*), intent(in)    :: iotype
      integer         , intent(in)    :: v_list(:)
      integer         , intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg

      if (allocated(self%raw)) then
         write (unit, '(A)', iostat=iostat, iomsg=iomsg) self%raw
      else
         write (unit, '(A)', iostat=iostat, iomsg=iomsg) ''
      endif

   end subroutine



   subroutine readUnformatted(self, unit, iostat, iomsg)
      class(ftlString), intent(inout) :: self
      integer         , intent(in)    :: unit
      integer         , intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg

      call self%ReadLine(unit, iostat)

   end
   !
   subroutine readFormatted(self, unit, iotype, vlist, iostat, iomsg)
      class(ftlString), intent(inout) :: self
      integer         , intent(in)    :: unit
      character(len=*), intent(in)    :: iotype
      integer         , intent(in)    :: vlist(:)
      integer         , intent(out)   :: iostat
      character(len=*), intent(inout) :: iomsg

      call self%ReadLine(unit, iostat)

   end


   ! =============> Character wise access:



   function AtString(self, idx) result(At)
      class(ftlString), intent(in), target :: self
      integer         , intent(in)         :: idx
      character, pointer                   :: At

      At => self%raw(idx:idx)

   end function



   ! =============> Overloaded operators:



   ! /= comparison like for raw strings
   !
   pure logical function StringEqualString(lhs, rhs) result(equal)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then
         equal = .false.
      else if (allocated(lhs%raw) .and. allocated(rhs%raw)) then
         equal = (lhs%raw == rhs%raw)
      else if (allocated(lhs%raw)) then
         equal = (lhs%raw == '')
      else if (allocated(rhs%raw)) then
         equal = (rhs%raw == '')
      else
         equal = .true.
      endif

   end function
   !
   pure logical function StringEqualChar(lhs, rhs) result(equal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then
         equal = .false.
      else if (allocated(lhs%raw)) then
         equal = (lhs%raw == rhs)
      else
         equal = (rhs == '')
      endif

   end function
   !
   pure logical function CharEqualString(lhs, rhs) result(equal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      if (len(lhs) /= len(rhs)) then
         equal = .false.
      else if (allocated(rhs%raw)) then
         equal = (lhs == rhs%raw)
      else
         equal = (lhs == '')
      endif

   end function



   ! /= comparison like for raw strings
   !
   pure logical function StringUnequalString(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

   end function
   !
   pure logical function StringUnequalChar(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

   end function
   !
   pure logical function CharUnequalString(lhs, rhs) result(unequal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      unequal = .not.(lhs == rhs)

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



   pure logical function IsNumber(self)
      class(ftlString), intent(in) :: self

      IsNumber = self%IsInt() .or. self%IsReal() .or. self%IsComplex()

   end function



   pure logical function IsInt(self)
      class(ftlString), intent(in) :: self

      integer :: tester, stat

      read(self%raw,*,iostat=stat) tester
      IsInt = (stat == 0)

   end function
   !
   pure integer function ToInt(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToInt
      if (stat /= 0) ToInt = -huge(ToInt)

      ! TODO: handle strings like '1e3' in gfortran

   end function



   pure logical function IsReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      real :: tester

      read(self%raw,*,iostat=stat) tester
      IsReal = (stat == 0)

   end function
   !
   pure real function ToReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToReal
      if (stat /= 0) then
         ToReal = 0.0
         ToReal = ToReal/ToReal ! results in NaN
      endif

   end function



   pure logical function IsComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      complex :: tester

      read(self%raw,*,iostat=stat) tester
      IsComplex = (stat == 0)

   end function
   !
   pure complex function ToComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%raw,*,iostat=stat) ToComplex
      if (stat /= 0) then
         ToComplex = (0.0,0.0)
         ToComplex = ToComplex/ToComplex ! results in NaN
      endif

   end function



   pure logical function IsLogical(self)
      class(ftlString), intent(in) :: self

      logical :: tester
      integer :: stat

      read(self%raw,*,iostat=stat) tester
      IsLogical = (stat == 0)

   end function
   !
   pure logical function ToLogical(self)
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
         self%raw = self%raw//buff(:nRead)
      enddo
   10 self%raw = self%raw//buff(:nRead)
      if (present(iostat)) iostat = ios

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

      self = buffer(:nRead)

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
   integer function CountRaw(self, sub, start, end) result(count)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in)           :: sub
      integer         , intent(in), optional :: start, end

      integer doneEnd, next

      if (present(start) .or. present(end)) stop 'TODO'

      if (len(sub) == 0) then
         ! replicate Python string behavior
         count = len(self%raw) + 1
         return
      endif

      count = 0
      doneEnd = 1
      do while (.true.)
         next = self%Find(sub, begin=doneEnd)
         if (next >= doneEnd) then ! found one more
            count = count + 1
            doneEnd = next + len(sub)
         else
            exit
         endif
      enddo

   end function
   !
   integer function CountOther(self, sub, start, end) result(count)
      class(ftlString), intent(in)           :: self
       type(ftlString), intent(in)           :: sub
      integer         , intent(in), optional :: start, end

      count = CountRaw(self, sub%raw, start, end)

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
         partition(3) = self%raw(idx+len(sep):)
      endif

   end function
   !
   function PartitionOther(self, sep) result(partition)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: sep
       type(ftlString)             :: partition(3)

      partition = self%PartitionRaw(sep%raw)

   end function



   ! Return a list of the words in the string, using sep as the delimiter string. If maxsplit is present, at most
   ! maxsplit splits are done (thus, the list will have at most maxsplit+1 elements). If maxsplit is not specified or
   ! -1, then there is no limit on the number of splits (all possible splits are made).
   !
   function Split(self, sep, maxsplit) result(words)
      class(ftlString), intent(in)           :: self
      character(len=*), intent(in), optional :: sep
      integer         , intent(in), optional :: maxsplit
      type(ftlString) , allocatable          :: words(:)

      integer :: idx, wordbegin, wordidx

      if (present(maxsplit)) stop 'TODO'

      if (present(sep)) then

         ! If sep is present, consecutive delimiters are not grouped together and are deemed to delimit empty strings
         ! (for example, '1,,2'%split(',') returns ['1', '', '2']). The sep argument may consist of multiple characters
         ! (for example, '1<>2<>3'%split('<>') returns ['1', '2', '3']). Splitting an empty string with a specified
         ! separator returns [''].

         stop 'TODO'

      else

         ! If sep is not present, a different splitting algorithm is applied: runs of consecutive whitespace are
         ! regarded as a single separator, and the result will contain no empty strings at the start or end if the
         ! string has leading or trailing whitespace.  Consequently, splitting an empty string or a string consisting of
         ! just whitespace without a separator returns [].

         allocate(words(self%CountWords()))

         idx = 1
         do wordidx = 1, size(words)
            do while (CharIsWhitespace(self%At(idx)))
               idx = idx + 1
            enddo
            wordbegin = idx
            do while (idx <= len(self))
               if (CharIsWhitespace(self%At(idx))) exit
               idx = idx + 1
            enddo
            words(wordidx) = self%raw(wordbegin:idx-1)
         enddo

      endif

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

      idx = index(self%raw(begin:end_-1), sub) + begin_ - 1

   end function



   ! Return a copy of the string with all the cased characters converted to uppercase/lowercase.
   !
   type(ftlString) function Upper(self)
      class(ftlString), intent(in) :: self

      integer :: idx, ascii

      Upper = self
      do idx = 1, len(Upper%raw)
         ascii = iachar(Upper%raw(idx:idx))
         if (ascii >= 97 .and. ascii <= 122) Upper%raw(idx:idx) = achar(ascii-32)
      enddo

   end function
   !
   type(ftlString) function Lower(self)
      class(ftlString), intent(in) :: self

      integer :: idx, ascii

      Lower = self
      do idx = 1, len(Lower%raw)
         ascii = iachar(Lower%raw(idx:idx))
         if (ascii >= 65 .and. ascii <= 90) Lower%raw(idx:idx) = achar(ascii+32)
      enddo

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
      str%raw(writeEnd:) = self%raw(readEnd:)

   end function



   ! =============> Other string methods:



   ! Count the number of words separater by whitespace (spaces or tabs). Ignores leading and trailing whitespace.
   !
   integer function CountWords(self)
      class(ftlString), intent(in) :: self

      integer :: idx

      if (CharIsWhitespace(self%raw(1:1))) then
         CountWords = 0
      else
         CountWords = 1
      endif
      idx = 1
      do idx = 2, len(self%raw)
         if (CharIsWhitespace(self%At(idx-1)) .and. .not.CharIsWhitespace(self%At(idx))) then
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
