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

   end type

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

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther, EqualFString
   end interface

   public :: operator(/=)
   interface operator(/=)
      module procedure UnequalOther, UnequalFString
   end interface

   ! FTL helpers:

   public :: ftlHash
   interface ftlHash
      module procedure ftlHashString
   end interface

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



   pure logical function EqualOther(self, other)
      class(ftlString), intent(in) :: self, other

      EqualOther = (self%fstr == other%fstr)

   end function
   !
   pure logical function EqualFString(self, fstr)
      class(ftlString), intent(in) :: self, fstr

      EqualOther = (self%fstr == fstr)

   end function



   pure logical function UnequalOther(self, other)
      class(ftlString), intent(in) :: self, other

      UnequalOther = (self%fstr /= other%fstr)

   end function
   !
   pure logical function UnequalFString(self, fstr)
      class(ftlString), intent(in) :: self, fstr

      UnequalOther = (self%fstr /= fstr)

   end function



   ! =============> FTL helpers:



   pure integer function ftlHashString(str)
      use ftlHashModule
      class(ftlString), intent(in) :: str

      ftlHashString = ftlHash(str%fstr)

   end function


end module
