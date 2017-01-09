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

      character(len=:), allocatable, public :: fstr

   contains
      private

      procedure         :: NewDefault
      procedure         :: NewCopyOther
      procedure         :: NewFromFString
      generic  , public :: New => NewDefault, NewCopyOther, NewFromFString

      generic  , public :: assignment(=) => NewCopyOther, NewFromFString

      procedure, public :: Delete

      procedure         :: CharString
      generic  , public :: Char => CharString

      procedure         :: BeginString
      generic  , public :: Begin => BeginString
      procedure         :: EndString
      generic  , public :: End => EndString

#ifdef FTL_ENABLE_DERIVED_TYPE_IO
      generic  , public :: write(unformatted) => WriteUnformatted
#endif

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
      procedure         :: StartsWithFString
      procedure         :: StartsWithOther
      procedure         :: StartsWithArray
      generic  , public :: StartsWith => StartsWithFString, StartsWithOther, StartsWithArray

      ! Other string methods:
      procedure, public :: CountWords

   end type


   ! Cunstructor functions:

   interface ftlString
      module procedure NewDefaultConstr
      module procedure NewCopyOtherConstr
      module procedure NewFromFStringConstr
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
      module procedure ftlIndexOther, ftlIndexFString
   end interface

   public :: scan
   interface scan
      module procedure ftlScanOther, ftlScanFString
   end interface

   public :: verify
   interface verify
      module procedure ftlVerifyOther, ftlVerifyFString
   end interface

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther, EqualFString
   end interface

   public :: operator(/=)
   interface operator(/=)
      module procedure UnequalOther, UnequalFString
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

      self%fstr = ''

   end subroutine
   !
   subroutine NewCopyOther(self, other)
      class(ftlString), intent(out) :: self
      class(ftlString), intent(in)  :: other

      ! Constructs a copy of other.

      self%fstr = other%fstr

   end subroutine
   !
   subroutine NewFromFString(self, fstr)
      class(ftlString), intent(out) :: self
      character(len=*), intent(in)  :: fstr

      ! Constructs an ftlString from a normal Fortran string

      self%fstr = fstr

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
   type(ftlString) function NewFromFStringConstr(fstr) result(str)
      character(len=*), intent(in) :: fstr
      call str%NewFromFString(fstr)
   end function




   ! Destroys the ftlString object. This deallocates all the storage capacity allocated by the ftlString.
   !
   subroutine Delete(self)
      class(ftlString), intent(out) :: self

      ! Nothing to do here: intent(out) will deallocate self%fstr

   end subroutine



   ! =============> Character wise access:



   function CharString(self, idx) result(Char)
      class(ftlString), intent(in), target :: self
      integer         , intent(in)         :: idx
      character, pointer                   :: Char

      Char => self%fstr(idx:idx)

   end function



   ! =============> Iterators:



   ! Returns an iterator pointing to the first character of the string.
   !
   type(ftlStringIterator) function BeginString(self) result(Begin)
      class(ftlString), intent(in), target :: self

      Begin%str => self
      Begin%index = 1
      if (len(self) /= 0) Begin%value => self%fstr(1:1)

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

      ftlLen = len(self%fstr)

   end function



   pure integer function ftlLenTrim(self)
      class(ftlString), intent(in) :: self

      ftlLenTrim = len_trim(self%fstr)

   end function



   pure type(ftlString) function ftlTrim(str)
      class(ftlString), intent(in) :: str

      ftlTrim%fstr = trim(str%fstr)

   end function



   pure type(ftlString) function ftlAdjustl(str)
      class(ftlString), intent(in) :: str

      ftlAdjustl%fstr = adjustl(str%fstr)

   end function



   pure type(ftlString) function ftlAdjustr(str)
      class(ftlString), intent(in) :: str

      ftlAdjustr%fstr = adjustr(str%fstr)

   end function



   pure type(ftlString) function ftlRepeat(str, i)
      class(ftlString), intent(in) :: str
      integer         , intent(in) :: i

      ftlRepeat%fstr = repeat(str%fstr,i)

   end function



   pure integer function ftlIndexOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1, str2
      logical         , intent(in), optional :: back

      ftlIndexOther = index(str1%fstr, str2%fstr, back)

   end function
   !
   pure integer function ftlIndexFString(str, fstr, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: fstr
      logical         , intent(in), optional :: back

      ftlIndexFString = index(str%fstr, fstr, back)

   end function



   pure integer function ftlScanOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1, str2
      logical         , intent(in), optional :: back

      ftlScanOther = scan(str1%fstr, str2%fstr, back)

   end function
   !
   pure integer function ftlScanFString(str, fstr, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: fstr
      logical         , intent(in), optional :: back

      ftlScanFString = scan(str%fstr, fstr, back)

   end function



   pure integer function ftlVerifyOther(str1, str2, back)
      class(ftlString), intent(in)           :: str1, str2
      logical         , intent(in), optional :: back

      ftlVerifyOther = verify(str1%fstr, str2%fstr, back)

   end function
   !
   pure integer function ftlVerifyFString(str, fstr, back)
      class(ftlString), intent(in)           :: str
      character(len=*), intent(in)           :: fstr
      logical         , intent(in), optional :: back

      ftlVerifyFString = verify(str%fstr, fstr, back)

   end function



   pure logical function EqualOther(self, other)
      class(ftlString), intent(in) :: self, other

      EqualOther = (self%fstr == other%fstr)

   end function
   !
   pure logical function EqualFString(self, fstr)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: fstr

      EqualFstring = (self%fstr == fstr)

   end function



   pure logical function UnequalOther(self, other)
      class(ftlString), intent(in) :: self, other

      UnequalOther = (self%fstr /= other%fstr)

   end function
   !
   pure logical function UnequalFString(self, fstr)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: fstr

      UnequalFString = (self%fstr /= fstr)

   end function



   ! =============> Conversion to numeric types:



   pure logical function IsNumber(self)
      class(ftlString), intent(in) :: self

      IsNumber = self%IsInt() .or. self%IsReal() .or. self%IsComplex()

   end function



   pure logical function IsInt(self)
      class(ftlString), intent(in) :: self

      integer :: tester, stat

      read(self%fstr,*,iostat=stat) tester
      IsInt = (stat == 0)

   end function
   !
   pure integer function ToInt(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%fstr,*,iostat=stat) ToInt
      if (stat /= 0) ToInt = -huge(ToInt)

      ! TODO: handle strings like '1e3'

   end function



   pure logical function IsReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      real :: tester

      read(self%fstr,*,iostat=stat) tester
      IsReal = (stat == 0)

   end function
   !
   pure real function ToReal(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%fstr,*,iostat=stat) ToReal
      if (stat /= 0) then
         ToReal = 0.0
         ToReal = ToReal/ToReal ! results in NaN
      endif

   end function



   pure logical function IsComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat
      complex :: tester

      read(self%fstr,*,iostat=stat) tester
      IsComplex = (stat == 0)

   end function
   !
   pure complex function ToComplex(self)
      class(ftlString), intent(in) :: self

      integer :: stat

      read(self%fstr,*,iostat=stat) ToComplex
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
            do while (CharIsWhitespace(self%Char(idx)))
               idx = idx + 1
            enddo
            wordbegin = idx
            do while (idx <= len(self))
               if (CharIsWhitespace(self%Char(idx))) exit
               idx = idx + 1
            enddo
            words(wordidx) = self%fstr(wordbegin:idx-1)
         enddo

      endif

   end function



   pure logical function StartsWithFString(self, prefix)
      class(ftlString), intent(in) :: self
      character(len=*), intent(in) :: prefix

      if (len(self) >= len(prefix)) then
         StartsWithFString = (self%fstr(1:len(prefix)) == prefix)
      else
         StartsWithFString = .false.
      endif

   end function
   !
   pure logical function StartsWithOther(self, prefix)
      class(ftlString), intent(in) :: self
      class(ftlString), intent(in) :: prefix

      StartsWithOther = StartsWithFString(self, prefix%fstr)

   end function
   !
   logical function StartsWithArray(self, prefixes)
      class(ftlString), intent(in) :: self
      type(ftlString) , intent(in) :: prefixes(:) ! TODO: figure out why this has to be type() not class() ...

      integer :: i

      StartsWithArray = .false.
      do i = 1, size(prefixes)
         if (self%StartsWithFString(prefixes(i)%fstr)) then
            StartsWithArray = .true.
            return
         endif
      enddo

   end function



   ! =============> Other string methods:



   ! Count the number of words separater by whitespace (spaces or tabs). Ignores leading and trailing whitespace.
   !
   integer function CountWords(self)
      class(ftlString), intent(in) :: self

      integer :: idx

      if (CharIsWhitespace(self%fstr(1:1))) then
         CountWords = 0
      else
         CountWords = 1
      endif
      idx = 1
      do idx = 2, len(self%fstr)
         if (CharIsWhitespace(self%Char(idx-1)) .and. .not.CharIsWhitespace(self%Char(idx))) then
            CountWords = CountWords + 1
         endif
      enddo

   end function



   ! =============> FTL methods:



   pure integer function ftlHashString(str)
      use ftlHashModule
      class(ftlString), intent(in) :: str

      ftlHashString = ftlHash(str%fstr)

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
      if (len(self%str) > 0 .and. self%index <= len(self%str)) self%value => self%str%fstr(self%index:self%index)

   end subroutine



   ! =============> Arithmetic operations:



   subroutine Inc(self)
      class(ftlStringIterator), intent(inout), target :: self

      self%index = self%index + 1
      if (self%index <= len(self%str)) then
         self%value => self%str%fstr(self%index:self%index)
      else
         nullify(self%value)
      endif

   end subroutine
   !
   subroutine Dec(self)
      class(ftlStringIterator), intent(inout), target :: self

      self%index = self%index - 1
      if (self%index > 0) then
         self%value => self%str%fstr(self%index:self%index)
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
