! Copyright (c) 2016, 2017  Robert Rüger
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


#include "ftlTestTools.inc"

module ftlListTestsModule

   use ftlTestToolsModule
   use ftlListIntModule

   implicit none
   private
   public :: ftlListTests

contains


   subroutine ftlListTests

      write (*,'(A)') 'Running ftlList tests ...'

      call testNewDefault
      call testNewCopyOther
      call testNewFill
      call testNewFromArray
      call testNewFromIteratorPair

      call testAssignments

      call testDelete
      call testArrayFinalizer

      call testInsertSingle
      call testInsertIntoEmpty
      call testInsertFill
      call testInsertArray
      call testInsertIteratorPair

      call testPushPopBack
      call testPushPopFront

      call testEraseSingle
      call testEraseIteratorPair

      call testSwap
      call testMove

      call testResize

      call testClear

   end subroutine


   subroutine testNewDefault
      type(ftlListInt) :: l

      call l%New()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)
      ASSERT(Size(l) == 0)
      ASSERT(l%Begin() == l%End())
      ASSERT(.not.(l%Begin() /= l%End()))

   end subroutine


   subroutine testNewCopyOther
      type(ftlListInt) :: l,o

      call o%New([5,13,41,97,17,10,88])
      call l%New(o)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(o%front,l%front))
      ASSERT(.not.associated(o%back,l%back))

   end subroutine


   subroutine testNewFill
      type(ftlListInt) :: l

      call l%New(5, 72)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 5)
      ASSERT(Size(l) == 5)
      ASSERT(l%front == 72)
      ASSERT(l%back == 72)

   end subroutine


   subroutine testNewFromArray
      type(ftlListInt) :: l

      call l%New([5,13,41,97,17,10,88])

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)

   end subroutine


   subroutine testNewFromIteratorPair
      type(ftlListInt) :: l, o

      call o%New([5,13,41,97,17,10,88])
      call l%New(o%Begin(), o%End())

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(o%front,l%front))
      ASSERT(.not.associated(o%back,l%back))

   end subroutine


   subroutine testAssignments
      type(ftlListInt) :: l, o
      type(ftlListIntIterator) :: it

      call o%New([5,13,41,97,17,10,88])
      l = o

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(o%front,l%front))
      ASSERT(.not.associated(o%back,l%back))
      it = l%Begin()
      ASSERT(it%value == 5)
      call it%Inc()
      ASSERT(it%value == 13)
      call it%Inc()
      ASSERT(it%value == 41)
      call it%Inc()
      ASSERT(it%value == 97)
      call it%Inc()
      ASSERT(it%value == 17)
      call it%Inc()
      ASSERT(it%value == 10)
      call it%Inc()
      ASSERT(it%value == 88)
      call it%Inc()
      ASSERT(it == l%End())

      l = [23,446,864,3]
      ASSERT(l%Size() == 4)
      ASSERT(l%front == 23)
      ASSERT(l%back == 3)

   end subroutine


   subroutine testDelete
      type(ftlListInt) :: l, uninit

      call uninit%Delete() ! should not crash

      call l%New([5,13,41,97,17,10,88])
      call l%Delete()

      ASSERT(size(l) == 0)
      ASSERT(l%Empty())
      ASSERT(.not.associated(l%front))
      ASSERT(.not.associated(l%back))

   end subroutine


   subroutine testArrayFinalizer
      type(ftlListInt), allocatable :: l(:)

      allocate(l(3))
      l(1) = [1,2,3,4,5]
      l(2) = [6,7,8,9]
      l(3) = [10,42]

      ! array finalizer should be called here. check that this doesn't leak with 'make memcheck'

   end subroutine


   subroutine testInsertSingle
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([23,1,6])
      it = l%Begin()
      ASSERT(it%value == 23)
      call it%Inc()
      ASSERT(it%value == 1)
      call l%Insert(it, 42)
      ASSERT(it%value == 1)
      call it%Dec()
      ASSERT(it%value == 42)
      call it%Dec()
      ASSERT(it%value == 23)

   end subroutine


   subroutine testInsertIntoEmpty
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New()
      it = l%Begin()
      call l%Insert(it, 5)

      ASSERT(size(l) == 1)
      ASSERT(l%front == 5)
      ASSERT(l%back == 5)

      call l%Insert(it, 42)

      ASSERT(size(l) == 2)
      ASSERT(l%front == 5)
      ASSERT(l%back == 42)

      ! Note: This behaviour is a bit counter intuitive, but keep in mind
      !       that Begin() == End() when the list is empty ...

   end subroutine


   subroutine testInsertFill
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([23,1,6])
      it = l%Begin()
      ASSERT(it%value == 23)
      call it%Inc()
      ASSERT(it%value == 1)
      call l%Insert(it, 2, 42)
      ASSERT(l%Size() == 5)
      ASSERT(it%value == 1)
      call it%Dec()
      ASSERT(it%value == 42)
      call it%Dec()
      ASSERT(it%value == 42)
      call it%Dec()
      ASSERT(it%value == 23)

      call l%New()
      ASSERT(l%Empty())
      call l%Insert(l%End(), 2, 42)
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 42)
      ASSERT(l%back == 42)

   end subroutine


   subroutine testInsertArray
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([23,1,6])
      it = l%Begin()
      ASSERT(it%value == 23)
      call it%Inc()
      ASSERT(it%value == 1)
      call l%Insert(it, [26,11,89])
      ASSERT(it%value == 1)
      call it%Dec()
      ASSERT(it%value == 89)
      call it%Dec()
      ASSERT(it%value == 11)
      call it%Dec()
      ASSERT(it%value == 26)
      call it%Dec()
      ASSERT(it%value == 23)

      call l%New()
      ASSERT(l%Empty())
      call l%Insert(l%End(), [4,5,6,7])
      ASSERT(l%Size() == 4)
      ASSERT(l%front == 4)
      ASSERT(l%back == 7)

   end subroutine



   subroutine testInsertIteratorPair
      type(ftlListInt) :: l, o
      type(ftlListIntIterator) :: it

      call o%New([2,3,4])
      call l%New([1,5])
      it = l%Begin()
      call it%Inc()

      ASSERT(it%value == 5)

      call l%Insert(it,o%Begin(),o%End())

      ASSERT(l%Size() == 5)
      ASSERT(l%front == 1)
      ASSERT(l%back == 5)

      it = l%Begin()
      ASSERT(it%value == 1)
      call it%Inc()
      ASSERT(it%value == 2)
      call it%Inc()
      ASSERT(it%value == 3)
      call it%Inc()
      ASSERT(it%value == 4)
      call it%Inc()
      ASSERT(it%value == 5)
      ASSERT(it /= l%End())
      call it%Inc()
      ASSERT(it == l%End())

   end subroutine



   subroutine testPushPopBack
      type(ftlListInt) :: l
      integer          :: i

      call l%New()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)

      call l%PushBack(5)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 1)
      ASSERT(l%front == 5)
      ASSERT(l%back == 5)

      call l%PushBack(6)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 5)
      ASSERT(l%back == 6)

      i = l%PopBack()

      ASSERT(i == 6)
      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 1)
      ASSERT(l%front == 5)
      ASSERT(l%back == 5)

      i = l%PopBack()

      ASSERT(i == 5)
      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)

   end subroutine


   subroutine testPushPopFront
      type(ftlListInt) :: l
      integer          :: i

      call l%New()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)

      call l%PushFront(5)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 1)
      ASSERT(l%front == 5)
      ASSERT(l%back == 5)

      call l%PushFront(6)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 6)
      ASSERT(l%back == 5)

      i = l%PopFront()

      ASSERT(i == 6)
      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 1)
      ASSERT(l%front == 5)
      ASSERT(l%back == 5)

      i = l%PopFront()

      ASSERT(i == 5)
      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)

   end subroutine


   subroutine testEraseSingle
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([4,5,6,7])
      it = l%Begin()
      call it%Inc()
      call it%Inc()
      ASSERT(it%value == 6)

      call l%Erase(it)
      ASSERT(l%Size() == 3)

      it = l%End()
      call it%Dec()
      ASSERT(it%value == 7)
      call it%Dec()
      ASSERT(it%value == 5)
      call it%Dec()
      ASSERT(it%value == 4)
      ASSERT(it == l%Begin())

      call l%Erase(l%Begin())
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 5)

   end subroutine


   subroutine testEraseIteratorPair
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it1, it2

      call l%New([12,23,34,45,56,76])

      it1 = Begin(l)
      call it1%Inc()
      call it1%Inc()
      ASSERT(it1%value == 34)

      it2 = it1
      call it2%Inc()
      call it2%Inc()
      ASSERT(it2%value == 56)

      call l%Erase(it1, it2)
      ASSERT(l%Size() == 4)

      ASSERT(it2%value == 56)
      call it2%Inc()
      call it2%Inc()
      ASSERT(it2 == l%End())

      call l%New([12,23,34,45,56,76])
      call l%Erase(l%Begin(),l%End())
      ASSERT(l%Empty())

   end subroutine


   subroutine testSwap
      type(ftlListInt) :: l, o
      type(ftlListIntIterator) :: it

      l = [4,7,813,5]
      o = [5,9,6]

      call ftlSwap(l,o)

      ASSERT(l%Size() == 3)
      ASSERT(l%front == 5)
      ASSERT(l%back == 6)

      it = l%Begin()
      ASSERT(it%value == 5)
      call it%Inc()
      ASSERT(it%value == 9)
      call it%Inc()
      ASSERT(it%value == 6)
      call it%Inc()
      ASSERT(it == l%End())

      ASSERT(o%Size() == 4)
      ASSERT(o%front == 4)
      ASSERT(o%back == 5)

      it = o%Begin()
      ASSERT(it%value == 4)
      call it%Inc()
      ASSERT(it%value == 7)
      call it%Inc()
      ASSERT(it%value == 813)
      call it%Inc()
      ASSERT(it%value == 5)
      call it%Inc()
      ASSERT(it == o%End())

   end subroutine


   subroutine testMove
      type(ftlListInt) :: src, dest, uninit
      type(ftlListIntIterator) :: it

      src = [4,7,813,5]
      dest = [5,9,6]

      call ftlMove(src,dest)

      ASSERT(dest%Size() == 4)
      ASSERT(dest%front == 4)
      ASSERT(dest%back == 5)

      it = dest%Begin()
      ASSERT(it%value == 4)
      call it%Inc()
      ASSERT(it%value == 7)
      call it%Inc()
      ASSERT(it%value == 813)
      call it%Inc()
      ASSERT(it%value == 5)
      call it%Inc()
      ASSERT(it == dest%End())

      call ftlMove(uninit,dest)

      ASSERT(dest%Size() == 0)
      ASSERT(dest%Empty())
      ASSERT(.not.associated(dest%front))
      ASSERT(.not.associated(dest%back))

   end subroutine


   subroutine testResize
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([5,-2,3,66,0])

      ASSERT(l%Size() == 5)
      ASSERT(l%back == 0)

      call l%Resize(4)

      ASSERT(l%Size() == 4)
      ASSERT(l%back == 66)

      call l%Resize(6)
      it = l%End()
      call it%Dec()
      call it%Dec()
      call it%Dec()

      ASSERT(it%value == 66)

   end subroutine


   subroutine testClear
      type(ftlListInt) :: l

      call l%New([4,5,7,8])
      call l%Clear()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)
      ASSERT(l%Begin() == l%End())

   end subroutine


end module
