! Copyright (c) 2020  Software for Chemistry & Materials BV
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

   use ftlStringModule

   implicit none
   private

   type, abstract, public :: AnimalType
      type(ftlString) :: name
   contains
      procedure(MakeSoundInterface), public, deferred :: MakeSound
   end type

   interface
      module subroutine MakeSoundInterface(self)
         class(AnimalType), intent(inout) :: self
      end subroutine
   end interface

   type, extends(AnimalType), public :: BirdType
   contains
      procedure, public :: NewBird
      procedure, public :: MakeSound => MakeSoundBird
   end type

   type, extends(AnimalType), public :: CowType
   contains
      procedure, public :: NewCow
      procedure, public :: MakeSound => MakeSoundCow
   end type

   type, public :: AnimalCageType
      class(AnimalType), allocatable :: animal
   end type

contains

   subroutine NewBird(self)
      class(BirdType), intent(out) :: self
      self%name = 'bird'
   end subroutine NewBird

   subroutine MakeSoundBird(self)
      class(BirdType), intent(inout) :: self
      print *, self%name%raw, ': Tweet'
   end subroutine

   subroutine NewCow(self)
      class(CowType), intent(out) :: self
      self%name = 'cow'
   end subroutine NewCow

   subroutine MakeSoundCow(self)
      class(CowType), intent(inout) :: self
      print *, self%name%raw, ': Moo'
   end subroutine


end module
