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

#define FTL_CONTAINER ftlString
#define FTL_CONTAINER_PROVIDES_RANDOM_ACCESS_ITERATOR


module ftlStringModule

   implicit none
   private


! ====== Type of the ftlString container itself ==================================================================================

   type, public :: ftlString
      private

      character(len=:), allocatable, public :: raw

   contains
      private

      procedure         :: NewDefault
      procedure         :: NewCopyOther
      procedure         :: NewFromRaw
      generic  , public :: New => NewDefault, NewCopyOther, NewFromRaw

      procedure, public :: Delete

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

      ! Python string methods:
      procedure, public :: Split
      procedure         :: StartsWithRaw
      procedure         :: StartsWithOther
      procedure         :: StartsWithArray
      generic  , public :: StartsWith => StartsWithRaw, StartsWithOther, StartsWithArray
      procedure         :: FindRaw
      procedure         :: FindOther
      generic  , public :: Find => FindRaw, FindOther

      ! Other string methods:
      procedure, public :: CountWords

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

      self%raw = other%raw

   end subroutine
   !
   subroutine NewFromRaw(self, raw)
      class(ftlString), intent(out) :: self
      character(len=*), intent(in)  :: raw

      ! Constructs an ftlString from a raw Fortran string

      self%raw = raw

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




   ! Destroys the ftlString object. This deallocates all the storage capacity allocated by the ftlString.
   !
   subroutine Delete(self)
      class(ftlString), intent(out) :: self

      ! Nothing to do here: intent(out) will deallocate self%raw

   end subroutine



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

      equal = (lhs%raw == rhs%raw)

   end function
   !
   pure logical function StringEqualChar(lhs, rhs) result(equal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      equal = (lhs%raw == rhs)

   end function
   !
   pure logical function CharEqualString(lhs, rhs) result(equal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      equal = (lhs == rhs%raw)

   end function



   ! /= comparison like for raw strings
   !
   pure logical function StringUnequalString(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
       type(ftlString), intent(in) :: rhs

      unequal = (lhs%raw /= rhs%raw)

   end function
   !
   pure logical function StringUnequalChar(lhs, rhs) result(unequal)
      class(ftlString), intent(in) :: lhs
      character(len=*), intent(in) :: rhs

      unequal = (lhs%raw /= rhs)

   end function
   !
   pure logical function CharUnequalString(lhs, rhs) result(unequal)
      character(len=*), intent(in) :: lhs
      class(ftlString), intent(in) :: rhs

      unequal = (lhs /= rhs%raw)

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



   ! =============> Python string methods:



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



   ! Return the lowest index in the string where substring sub is found within the slice s[start:end]. Optional
   ! arguments start and end are interpreted as in slice notation. Return -1 if sub is not found.
   !
   pure integer function FindOther(self, sub) result(idx)
      class(ftlString), intent(in) :: self
       type(ftlString), intent(in) :: sub

      idx = index(self, sub)
      if (idx == 0) idx = -1

   end function
   !
   pure integer function FindRaw(self, sub) result(idx)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: sub

      idx = index(self, sub)
      if (idx == 0) idx = -1

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



! ====== Auxilliary methods working on single characters =========================================================================


   pure logical function CharIsWhitespace(c)
      character, intent(in) :: c
      CharIsWhitespace = (c == ' ' .or. iachar(c) == 9)
   end function


end module
