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


module Point2DModule

   implicit none
   private

   type, public :: Point2D
      real :: x, y
   end type

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther
   end interface

contains

   pure logical function EqualOther(self, other)
      type(Point2D), intent(in) :: self, other
      EqualOther = (self%x == other%x) .and. (self%y == other%y)
   end function

end module
