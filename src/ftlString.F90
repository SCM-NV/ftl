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

      procedure         :: BeginString
      generic  , public :: Begin => BeginString
      procedure         :: EndString
      generic  , public :: End => EndString

   end type

   public :: Begin
   interface Begin
      module procedure BeginString
   end interface

   public :: End
   interface End
      module procedure EndString
   end interface

   public :: Size
   interface Size
      module procedure ftlLen
   end interface


   ! Cunstructor functions:

   interface ftlString
      module procedure NewDefaultConstr
      module procedure NewCopyOtherConstr
      module procedure NewFromFStringConstr
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


! ====== Type of an iterator over a ftlList container ============================================================================

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
      class(ftlStringIterator), intent(out) :: self
      class(ftlStringIterator), intent(in)  :: other

      self%str => other%str
      self%index = other%index
      if (len(self%str) > 0 .and. self%index <= len(self%str)) self%value => self%str%fstr(self%index:self%index)

   end subroutine



   ! =============> Arithmetic operations:



   subroutine Inc(self)
      class(ftlStringIterator), intent(inout) :: self

      self%index = self%index + 1
      if (self%index <= len(self%str)) then
         self%value => self%str%fstr(self%index:self%index)
      else
         nullify(self%value)
      endif

   end subroutine
   !
   subroutine Dec(self)
      class(ftlStringIterator), intent(inout) :: self

      self%index = self%index - 1
      if (self%index > 0) then
         self%value => self%str%fstr(self%index:self%index)
      else
         nullify(self%value)
      endif

   end subroutine


end module
