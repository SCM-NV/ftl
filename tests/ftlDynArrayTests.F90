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


#include "ftlTestTools.inc"

module ftlArrayTestsModule

   use ftlTestToolsModule
   use ftlArrayIntModule

   implicit none
   private
   public :: ftlArrayTests

contains


   subroutine ftlArrayTests

      write (*,'(A)') 'Running ftlArray tests ...'

      call testBegin
      call testEnd

      call testNewItDefault
      call testNewItCopyOther

      call testInc
      call testDec

      call testAdvanceReverseDiff
      call testLogicalOperations

   end subroutine


   subroutine testBegin
      integer, allocatable, target :: v(:)
      type(ftlArrayIntIterator) :: it

      v = [4,6,38,216,48468,3,2,67,9]
      it = Begin(v)

      ASSERT(associated(it%value,v(1)))
      ASSERT(it%value == 4)
      ASSERT(it%value == v(1))

   end subroutine


   subroutine testEnd
      integer, allocatable, target :: v(:)
      type(ftlArrayIntIterator) :: it

      v = [4,6,38,216,48468,3,2,67,27]
      it = End(v)
      call it%Dec()

      ASSERT(associated(it%value,v(9)))
      ASSERT(it%value == 27)
      ASSERT(it%value == v(9))

   end subroutine


   subroutine testNewItDefault
      type(ftlArrayIntIterator) :: it

      call it%New()
      ASSERT(.not.associated(it%value))

   end subroutine


   subroutine testNewItCopyOther
      integer, allocatable :: v(:)
      type(ftlArrayIntIterator) :: it1, it2

      v = [353,6,5,2,2274,33]
      it1 = Begin(v)
      call it2%New(it1)

      ASSERT(associated(it1%value,it2%value))
      ASSERT(it2%value == 353)

   end subroutine


   subroutine testInc
      integer, allocatable, target :: v(:)
      type(ftlArrayIntIterator) :: it

      v = [353,6,5,2,2274,33]
      it = Begin(v)

      ASSERT(associated(it%value,v(1)))
      ASSERT(it%value == 353)

      call it%Inc()

      ASSERT(associated(it%value,v(2)))
      ASSERT(it%value == 6)

   end subroutine


   subroutine testDec
      integer, allocatable, target :: v(:)
      type(ftlArrayIntIterator) :: it

      v = [353,6,5,2,2274,33]
      it = End(v)
      call it%Dec()

      ASSERT(associated(it%value,v(6)))
      ASSERT(it%value == 33)

      call it%Dec()

      ASSERT(associated(it%value,v(5)))
      ASSERT(it%value == 2274)

   end subroutine


   subroutine testAdvanceReverseDiff
      integer, allocatable, target :: v(:)
      type(ftlArrayIntIterator) :: it1, it2

      v = [353,6,5,2,2274,33]
      it1 = Begin(v)
      it2 = it1 + 4

      ASSERT(it2 - it1 == 4)
      ASSERT(associated(it2%value,v(5)))
      ASSERT(it2%value == 2274)

      it2 = it2 - 2
      ASSERT(it2 - it1 == 2)
      ASSERT(associated(it2%value,v(3)))
      ASSERT(it2%value == 5)

   end subroutine


   subroutine testLogicalOperations
      integer, allocatable :: v(:)
      type(ftlArrayIntIterator) :: it1, it2

      v = [4,7,3,6,8]
      it1 = Begin(v) + 2
      ASSERT(it1%value == 3)
      it2 = Begin(v)

      ASSERT(it2%value == 4)
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(     (it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(.not.(it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 7)
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(     (it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(.not.(it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 3)
      ASSERT(     (it2 == it1))
      ASSERT(.not.(it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 6)
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(.not.(it2 <= it1))
      ASSERT(     (it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 8)
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(.not.(it2 <= it1))
      ASSERT(     (it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2 == End(v))

   end subroutine


end module
