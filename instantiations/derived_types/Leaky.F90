! Copyright (c) 2017  Robert Rüger
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


module LeakyModule

   implicit none
   private

   type, public :: LeakyType
      character(len=:), allocatable :: name
      real            , pointer     :: dontLeakMe(:) => null()
   contains
      procedure :: New
      procedure :: Delete
      final     :: Finalizer
      procedure :: AssignOther
      generic   :: assignment(=) => AssignOther
   end type

   public :: ftlMove
   interface ftlMove
      module procedure ftlMoveLeaky
   end interface

contains


   subroutine New(self, name, n)
      class(LeakyType), intent(out) :: self
      character(len=*), intent(in)  :: name
      integer         , intent(in)  :: n

      self%name = name
      allocate(self%dontLeakMe(n))
      self%dontLeakMe = 42.0

   end subroutine


   elemental subroutine Delete(self)
      class(LeakyType), intent(inout) :: self

      if (allocated(self%name)) deallocate(self%name)
      if (associated(self%dontLeakMe)) deallocate(self%dontLeakMe)

   end subroutine


   elemental subroutine Finalizer(self)
      type(LeakyType), intent(inout) :: self

      call self%Delete()

   end subroutine


   subroutine AssignOther(self, other)
      class(LeakyType), intent(out) :: self
      type(LeakyType) , intent(in)  :: other

      if (allocated(other%name)) self%name = other%name
      if (associated(other%dontLeakMe)) then
         allocate(self%dontLeakMe(size(other%dontLeakMe)))
         self%dontLeakMe = other%dontLeakMe
      endif

   end subroutine


   subroutine ftlMoveLeaky(src, dest)
      type(LeakyType), intent(inout) :: src
      type(LeakyType), intent(out)   :: dest

      if (allocated(src%name)) then
         dest%name = src%name
      else
         if (allocated(dest%name)) deallocate(dest%name)
      endif

      dest%dontLeakMe => src%dontLeakMe
      nullify(src%dontLeakMe)

   end subroutine


end module
