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

module ftlMemoryTestsModule

   use ftlTestToolsModule
   use ftlMemoryIntModule

   implicit none
   private
   public :: ftlMemoryTests

contains


   subroutine ftlMemoryTests

      write (*,'(A)') 'Running ftlMemory tests ...'

      call testNewDefault
      call testNewFromRawPtr
      call testNewFromOther
      call testAssignOtherAndNullify
      call testAllocate
      call testSwap

   end subroutine


   subroutine testNewDefault
      type(ftlSharedPtrInt) :: sp

      call sp%New()

      ASSERT(sp%useCount() == 0)
      ASSERT(.not.sp%Unique())
      ASSERT(.not.associated(sp))
      ASSERT(.not.associated(sp%value))

   end subroutine


   subroutine testNewFromRawPtr
      type(ftlSharedPtrInt) :: sp
      integer, pointer :: i

      allocate(i)
      call sp%New(i)
      sp%value = 5

      ASSERT(sp%useCount() == 1)
      ASSERT(sp%Unique())
      ASSERT(associated(sp))
      ASSERT(associated(sp%value))
      ASSERT(sp%value == 5)

#ifdef FTL_NO_FINALIZERS
      call sp%Delete()
#endif

   end subroutine


   subroutine testNewFromOther
      type(ftlSharedPtrInt) :: sp1, sp2
      integer, pointer :: i

      allocate(i)
      call sp1%New(i)
      sp1%value = 154

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 154)

      call sp2%New(sp1)

      ASSERT(sp1%useCount() == 2)
      ASSERT(.not.sp1%Unique())
      ASSERT(associated(sp1))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 154)

      ASSERT(sp2%useCount() == 2)
      ASSERT(.not.sp2%Unique())
      ASSERT(associated(sp2))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 154)

      ASSERT(associated(sp1, sp2))

#ifdef FTL_NO_FINALIZERS
      call sp1%Delete()
      call sp2%Delete()
#endif

   end subroutine


   subroutine testAssignOtherAndNullify
      type(ftlSharedPtrInt) :: sp1, sp2
      integer, pointer :: i

      allocate(i)
      call sp1%New(i)
      sp1%value = 291287

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(associated(sp1))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 291287)

      sp2 = sp1

      ASSERT(sp1%useCount() == 2)
      ASSERT(.not.sp1%Unique())
      ASSERT(associated(sp1))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 291287)

      ASSERT(sp2%useCount() == 2)
      ASSERT(.not.sp2%Unique())
      ASSERT(associated(sp2))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 291287)

      ASSERT(associated(sp1, sp2))

      call nullify(sp1)

      ASSERT(sp1%useCount() == 0)
      ASSERT(.not.sp1%Unique())
      ASSERT(.not.associated(sp1))
      ASSERT(.not.associated(sp1%value))

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(associated(sp2))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 291287)

#ifdef FTL_NO_FINALIZERS
      call sp2%Delete()
#endif

   end subroutine


   subroutine testAllocate
      type(ftlSharedPtrInt) :: sp

      call allocate(sp)
      sp%value = 42

      ASSERT(sp%useCount() == 1)
      ASSERT(sp%Unique())
      ASSERT(associated(sp))
      ASSERT(associated(sp%value))
      ASSERT(sp%value == 42)

#ifdef FTL_NO_FINALIZERS
      call sp%Delete()
#endif

   end subroutine


   subroutine testSwap
      type(ftlSharedPtrInt) :: sp1, sp2

      call allocate(sp1)
      sp1%value = 42

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(associated(sp1))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 42)

      call allocate(sp2)
      sp2%value = 1750

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(associated(sp2))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 1750)

      call ftlSwap(sp1, sp2)

      ASSERT(sp1%useCount() == 1)
      ASSERT(sp1%Unique())
      ASSERT(associated(sp1))
      ASSERT(associated(sp1%value))
      ASSERT(sp1%value == 1750)

      ASSERT(sp2%useCount() == 1)
      ASSERT(sp2%Unique())
      ASSERT(associated(sp2))
      ASSERT(associated(sp2%value))
      ASSERT(sp2%value == 42)

#ifdef FTL_NO_FINALIZERS
      call sp1%Delete()
      call sp2%Delete()
#endif

   end subroutine


end module
