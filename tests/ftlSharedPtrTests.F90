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


#include "ftlTestTools.inc"

module ftlSharedPtrTestsModule

   use ftlTestToolsModule
   use ftlSharedPtrIntModule

   implicit none
   private
   public :: ftlSharedPtrTests

contains


   subroutine ftlSharedPtrTests

      write (*,'(A)') 'Running ftlSharedPtr tests ...'

      call testAllocate
      call testAssumeOwnershipOf
      call testAssignment
      call testAssignOtherAndNullify
      call testSwap
      call testMove
      call testArrayFinalization
      call testArrayAssignment

   end subroutine


   subroutine testAllocate
      type(ftlSharedPtrInt) :: sp

      call sp%Allocate()
      sp%value = 42

      ASSERT(sp%useCount() == 1)
      ASSERT(sp%Unique())
      ASSERT(sp%Associated())
      ASSERT(associated(sp%value))
      ASSERT(sp%value == 42)

   end subroutine


   subroutine testAssumeOwnershipOf
      type(ftlSharedPtrInt) :: sp
      integer, pointer :: i

      allocate(i)
      call sp%AssumeOwnershipOf(i)
      sp%value = 5

      ASSERT(sp%useCount() == 1)
      ASSERT(sp%Unique())
      ASSERT(sp%Associated())
      ASSERT(sp%Associated(i))
      ASSERT(associated(sp%value))
      ASSERT(sp%value == 5)

   end subroutine


   subroutine testAssignment
      type(ftlSharedPtrInt) :: sp1, sp2
      integer, pointer :: i

      allocate(i)
      call sp1%AssumeOwnershipOf(i)
      sp1%value = 154

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(sp1%Associated(i))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 154)

      sp2 = sp1

      ASSERT(sp1%useCount() == 2)
      ASSERT(.not.sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(sp1%Associated(i))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 154)

      ASSERT(sp2%useCount() == 2)
      ASSERT(.not.sp2%Unique())
      ASSERT(sp2%Associated())
      ASSERT(sp2%Associated(i))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 154)

      ASSERT(sp1%Associated(sp2))

   end subroutine


   subroutine testAssignOtherAndNullify
      type(ftlSharedPtrInt) :: sp1, sp2
      integer, pointer :: i

      allocate(i)
      call sp1%AssumeOwnershipOf(i)
      sp1%value = 291287

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 291287)

      sp2 = sp1

      ASSERT(sp1%useCount() == 2)
      ASSERT(.not.sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 291287)

      ASSERT(sp2%useCount() == 2)
      ASSERT(.not.sp2%Unique())
      ASSERT(sp2%Associated())
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 291287)

      ASSERT(sp1%Associated(sp2))

      call sp1%Nullify()

      ASSERT(sp1%useCount() == 0)
      ASSERT(.not.sp1%Unique())
      ASSERT(.not.sp1%Associated())
      ASSERT(.not.associated(sp1%value))

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(sp2%Associated())
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 291287)

   end subroutine


   subroutine testSwap
      type(ftlSharedPtrInt) :: sp1, sp2

      call sp1%Allocate()
      sp1%value = 42

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 42)

      call sp2%Allocate()
      sp2%value = 1750

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(sp1%Associated())
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 1750)

      call ftlSwap(sp1, sp2)

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(sp1%Associated())
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 1750)

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(sp2%Associated())
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 42)

   end subroutine


   subroutine testMove
      type(ftlSharedPtrInt) :: src, dest

      call src%Allocate()
      src%value = 42

      ASSERT(src%useCount() == 1)
      ASSERT(src%Unique())
      ASSERT(src%Associated())
      ASSERT(associated(src%value))
      ASSERT(src%value == 42)

      call dest%Allocate()
      dest%value = 1750

      ASSERT(dest%useCount() == 1)
      ASSERT(dest%Unique())
      ASSERT(dest%Associated())
      ASSERT(associated(dest%value))
      ASSERT(dest%value == 1750)

      call ftlMove(src, dest)

      ASSERT(.not.src%Associated())
      ASSERT(.not.associated(src%value))

      ASSERT(dest%useCount() == 1)
      ASSERT(dest%Unique())
      ASSERT(dest%Associated())
      ASSERT(associated(dest%value))
      ASSERT(dest%value == 42)

   end subroutine


   subroutine testArrayFinalization
      type(ftlSharedPtrInt) :: sp(2)

      call sp(1)%Allocate()
      sp(1)%value = 42
      sp(2) = sp(1)

      ASSERT(sp(2)%Associated())
      ASSERT(sp(2)%value == 42)
      ASSERT(sp(1)%UseCount() == 2)
      ASSERT(sp(2)%UseCount() == 2)

   end subroutine


   subroutine testArrayAssignment
      type(ftlSharedPtrInt) :: sp(2)
      type(ftlSharedPtrInt) :: copy(2)

      call sp(1)%Allocate()
      sp(1)%value = 42
      sp(2) = sp(1)
      copy = sp

      ASSERT(sp(1)%Associated())
      ASSERT(sp(1)%value == 42)
      ASSERT(sp(1)%UseCount() == 4)

   end subroutine


end module
