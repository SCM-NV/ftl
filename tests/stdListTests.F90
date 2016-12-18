#include "stdTestTools.inc"

module stdListTestsModule

   use stdTestToolsModule
   use stdListIntModule

   implicit none
   private
   public :: stdListTests

contains


   subroutine stdListTests

      write (*,'(A)') 'Running stdList tests ...'

      ! Tests of the stdList container itself:

      call testNewDefault
      call testNewCopyOther
      call testNewFill
      call testNewFromArray
      call testNewFromIteratorPair

      call testAssignments

      call testInsertSingle
      call testInsertFill
      call testInsertArray
      call testInsertIteratorPair

      call testPushPopBack
      call testPushPopFront

      call testEraseSingle
      call testEraseIteratorPair

      call testSwap

      call testResize

      call testClear

   end subroutine


   subroutine testNewDefault
      type(stdListInt) :: l

      call l%New()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)
      ASSERT(Size(l) == 0)
      ASSERT(l%Begin() == l%End())
      ASSERT(.not.(l%Begin() /= l%End()))

   end subroutine


   subroutine testNewCopyOther
      type(stdListInt) :: l,o

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
      type(stdListInt) :: l

      call l%New(5, 72)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 5)
      ASSERT(Size(l) == 5)
      ASSERT(l%front == 72)
      ASSERT(l%back == 72)

   end subroutine


   subroutine testNewFromArray
      type(stdListInt) :: l

      call l%New([5,13,41,97,17,10,88])

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)

   end subroutine


   subroutine testNewFromIteratorPair
      type(stdListInt) :: l, o

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
      type(stdListInt) :: l, o
      type(stdListIntIterator) :: it

      call o%New([5,13,41,97,17,10,88])
      l = o

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(o%front,l%front))
      ASSERT(.not.associated(o%back,l%back))

      it = o%Begin()
      call it%Inc()
      call l%Assign(it, o%End())

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 6)
      ASSERT(l%front == 13)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(it%value,l%front))
      ASSERT(.not.associated(o%back,l%back))

      call l%Assign(5, 12)
      ASSERT(l%Size() == 5)
      ASSERT(l%front == 12)
      ASSERT(l%back == 12)

      l = [23,446,864,3]
      ASSERT(l%Size() == 4)
      ASSERT(l%front == 23)
      ASSERT(l%back == 3)

   end subroutine


   subroutine testInsertSingle
      type(stdListInt) :: l
      type(stdListIntIterator) :: it

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


   subroutine testInsertFill
      type(stdListInt) :: l
      type(stdListIntIterator) :: it

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
      it = l%End()
      call l%Insert(it, 2, 42) ! Fortran, why can't I pass l%End() directly???? Please ....
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 42)
      ASSERT(l%back == 42)

   end subroutine


   subroutine testInsertArray
      type(stdListInt) :: l
      type(stdListIntIterator) :: it

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
      it = l%End()
      call l%Insert(it, [4,5,6,7]) ! Fortran, why can't I pass l%End() directly???? Please ....
      ASSERT(l%Size() == 4)
      ASSERT(l%front == 4)
      ASSERT(l%back == 7)

   end subroutine



   subroutine testInsertIteratorPair
      type(stdListInt) :: l, o
      type(stdListIntIterator) :: it

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
      type(stdListInt) :: l
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
      type(stdListInt) :: l
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
      type(stdListInt) :: l
      type(stdListIntIterator :: it

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

      it = l%Begin()
      call l%Erase(it) ! Fortran, why can't I pass l%Begin() directly???? Please ....
      ASSERT(l%Size() == 2)
      ASSERT(l%front == 5)

   end subroutine


   subroutine testEraseIteratorPair
      type(stdListInt) :: l
      type(stdListIntIterator :: it1, it2

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
      it1 = l%Begin()
      call l%Erase(it1,l%End()) ! Fortran, why can't I pass l%Begin() directly? It works for the second parameter FFS ...
      ASSERT(l%Empty())

   end subroutine


   subroutine testSwap
      type(stdListInt) :: l, o
      type(stdListIntIterator) :: it

      l = [4,7,813,5]
      o = [5,9,6]

      call Swap(l,o)

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


   subroutine testResize
      type(stdListInt) :: l
      type(stdListIntIterator) :: it

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
      type(stdListInt) :: l

      call l%New([4,5,7,8])
      call l%Clear()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)
      ASSERT(l%Begin() == l%End())

   end subroutine


end module
