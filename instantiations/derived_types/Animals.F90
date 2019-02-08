! Copyright (c) 2019  Software for Chemistry & Materials
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

module AnimalsModule

   implicit none
   private

   type, abstract, public :: Animal
   contains
      procedure(NumberOfLegsBird), public, deferred, nopass :: NumberOfLegs
   end type

   type, extends(Animal), public :: Bird
   contains
      procedure, public, nopass :: NumberOfLegs => NumberOfLegsBird
   end type

   type, extends(Animal), public :: Insect
   contains
      procedure, public, nopass :: NumberOfLegs => NumberOfLegsInsect
   end type

contains

   integer function NumberOfLegsBird() result(nLegs)
      nLegs = 2
   end function

   integer function NumberOfLegsInsect() result(nLegs)
      nLegs = 6
   end function

end module
