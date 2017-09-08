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

module ftlDequeTestsModule

   use ftlTestToolsModule
   use ftlDequeIntModule

   implicit none
   private
   public :: ftlDequeTests

contains


   subroutine ftlDequeTests

      write (*,'(A)') 'Running ftlDeque tests ...'

      ! Tests of the ftlDeque container itself:

      call testNewDefault

      call testPushBack
      call testPushFront

      call testPopBack
      call testPopFront
      call testPushPopAsQueue

   end subroutine


   subroutine testNewDefault
      type(ftlDequeInt) :: d

      call d%New()

   end subroutine


   subroutine testPushBack
      type(ftlDequeInt) :: d

      call d%New()
      call d%PushBack(1)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 1)
      ASSERT(d%Size() == 1)
      call d%PushBack(2)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 2)
      ASSERT(d%Size() == 2)
      call d%PushBack(3)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 3)
      ASSERT(d%Size() == 3)
      call d%PushBack(4)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 4)
      ASSERT(d%Size() == 4)
      call d%PushBack(5)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 5)
      ASSERT(d%Size() == 5)
      call d%PushBack(6)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 6)
      ASSERT(d%Size() == 6)
      call d%PushBack(7)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 7)
      ASSERT(d%Size() == 7)
      call d%PushBack(8)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 8)
      ASSERT(d%Size() == 8)
      call d%PushBack(9)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 9)
      ASSERT(d%Size() == 9)
      call d%PushBack(1)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 1)
      ASSERT(d%Size() == 10)
      call d%PushBack(2)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 2)
      ASSERT(d%Size() == 11)
      call d%PushBack(3)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 3)
      ASSERT(d%Size() == 12)
      call d%PushBack(4)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 4)
      ASSERT(d%Size() == 13)
      call d%PushBack(5)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 5)
      ASSERT(d%Size() == 14)
      call d%PushBack(6)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 6)
      ASSERT(d%Size() == 15)
      call d%PushBack(6)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 6)
      ASSERT(d%Size() == 16)
      call d%PushBack(6)
      ASSERT(d%front == 1)
      ASSERT(d%back  == 6)
      ASSERT(d%Size() == 17)

   end subroutine


   subroutine testPushFront
      type(ftlDequeInt) :: d

      call d%New()
      call d%PushFront(1)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 1)
      ASSERT(d%Size() == 1)
      call d%PushFront(2)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 2)
      ASSERT(d%Size() == 2)
      call d%PushFront(3)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 3)
      ASSERT(d%Size() == 3)
      call d%PushFront(4)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 4)
      ASSERT(d%Size() == 4)
      call d%PushFront(5)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 5)
      ASSERT(d%Size() == 5)
      call d%PushFront(6)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 6)
      ASSERT(d%Size() == 6)
      call d%PushFront(7)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 7)
      ASSERT(d%Size() == 7)
      call d%PushFront(8)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 8)
      ASSERT(d%Size() == 8)
      call d%PushFront(9)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 9)
      ASSERT(d%Size() == 9)
      call d%PushFront(1)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 1)
      ASSERT(d%Size() == 10)
      call d%PushFront(2)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 2)
      ASSERT(d%Size() == 11)
      call d%PushFront(3)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 3)
      ASSERT(d%Size() == 12)
      call d%PushFront(4)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 4)
      ASSERT(d%Size() == 13)
      call d%PushFront(5)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 5)
      ASSERT(d%Size() == 14)
      call d%PushFront(6)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 6)
      ASSERT(d%Size() == 15)
      call d%PushFront(6)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 6)
      ASSERT(d%Size() == 16)
      call d%PushFront(6)
      ASSERT(d%back == 1)
      ASSERT(d%front  == 6)
      ASSERT(d%Size() == 17)

   end subroutine


   subroutine testPopBack
      type(ftlDequeInt) :: d

      call d%New()

      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(3)

      ASSERT(d%Size() == 9)
      ASSERT(d%front == 1)
      ASSERT(d%back == 3)

      ASSERT(d%PopBack() == 3)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)

      ASSERT(d%Size() == 0)
      ASSERT(.not.associated(d%front))
      ASSERT(.not.associated(d%back))

      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(1)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(2)
      call d%PushBack(3)

      ASSERT(d%Size() == 9)
      ASSERT(d%front == 1)
      ASSERT(d%back == 3)

      ASSERT(d%PopBack() == 3)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 2)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)
      ASSERT(d%PopBack() == 1)

      ASSERT(d%Size() == 0)
      ASSERT(.not.associated(d%front))
      ASSERT(.not.associated(d%back))

   end subroutine


   subroutine testPopFront
      type(ftlDequeInt) :: d

      call d%New()

      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(3)

      ASSERT(d%Size() == 9)
      ASSERT(d%back == 1)
      ASSERT(d%front == 3)

      ASSERT(d%PopFront() == 3)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)

      ASSERT(d%Size() == 0)
      ASSERT(.not.associated(d%front))
      ASSERT(.not.associated(d%back))

      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(1)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(2)
      call d%PushFront(3)

      ASSERT(d%Size() == 9)
      ASSERT(d%back == 1)
      ASSERT(d%front == 3)

      ASSERT(d%PopFront() == 3)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 2)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)
      ASSERT(d%PopFront() == 1)

      ASSERT(d%Size() == 0)
      ASSERT(.not.associated(d%front))
      ASSERT(.not.associated(d%back))

   end subroutine


   subroutine testPushPopAsQueue
      type(ftlDequeInt) :: d1, d2
      integer :: i, k, n, discard

      call d1%New()
      call d2%New()

      do i = 1, 100
         n = mod(RandomInt(), 6)
         do k = 1, n
            call d1%PushBack(i+k)
            call d2%PushFront(i+k)
            ASSERT(d1%Size() == d2%Size())
            if (d1%Size() > 0) then
               ASSERT(d1%front == d2%back)
               ASSERT(d2%front == d1%back)
            endif
         enddo
         n = min(mod(RandomInt(), 7), d1%Size())
         do k = 1, n
            ASSERT(d2%PopBack() == d1%PopFront())
            ASSERT(d1%Size() == d2%Size())
            if (d1%Size() > 0) then
               ASSERT(d1%front == d2%back)
               ASSERT(d2%front == d1%back)
            endif
         enddo
         call d1%debugPrint
      enddo

   end subroutine


end module
