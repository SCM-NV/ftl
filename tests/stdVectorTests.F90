#include "stdTestTools.inc"

module stdVectorTestsModule

   use stdTestToolsModule
   use stdVectorIntModule

   implicit none
   private
   public :: stdVectorTests

contains


   subroutine stdVectorTests

      write (*,'(A)') 'Running stdVector tests ...'

      ! Tests of the stdVector container itself:

      call testNewDefault
      call testNewCopyOther
      call testNewFill
      call testNewFromArray

      call testDelete

      call testBegin
      call testEnd

      call testSizeAndCapacity
      call testResize
      call testEmpty
      call testReserve
      call testShrinkToFit

      call testPushBack
      call testPopBack

      call testInsertSingle
      call testInsertFill
      call testInsertArray

      call testEraseSingle
      call testEraseRange

      call testClear

      ! Tests of its iterators:

      call testNewItDefault
      call testNewItCopyOther

      call testInc
      call testDec

      call testAdvanceReverseDiff
      call testLogicalOperations

   end subroutine


   subroutine testNewDefault
      type(stdVectorInt) :: v

      call v%New()

      ASSERT(v%Empty())
      ASSERT(v%Size() == 0)
      ASSERT(Size(v) == 0)
      ASSERT(size(v%data) == 0)
      ASSERT(v%Begin() == v%End())
      ASSERT(.not.(v%Begin() /= v%End()))

   end subroutine


   subroutine testNewCopyOther
      type(stdVectorInt) :: v,o

      call o%New([5,13,41,97,17,10,88])
      call v%New(o)

      ASSERT(.not.v%Empty())
      ASSERT(v%Size() == 7)
      ASSERT(Size(v) == 7)
      ASSERT(size(v%data) == 7)
      ASSERT(all(v%data == [5,13,41,97,17,10,88]))
      ASSERT(v%front == 5)
      ASSERT(v%back == 88)
      ASSERT(v%End() - v%Begin() == 7)
      ASSERT(.not.associated(o%data,v%data))
      ASSERT(.not.associated(o%front,v%front))
      ASSERT(.not.associated(o%back,v%back))

   end subroutine


   subroutine testNewFill
      type(stdVectorInt) :: u,v

      call u%New(33)

      ASSERT(u%Size() == 33)
      ASSERT(Size(u) == 33)
      ASSERT(size(u%data) == 33)

      call v%New(5,41)

      ASSERT(.not.v%Empty())
      ASSERT(v%Size() == 5)
      ASSERT(Size(v) == 5)
      ASSERT(size(v%data) == 5)
      ASSERT(all(v%data == [41,41,41,41,41]))
      ASSERT(v%front == 41)
      ASSERT(v%back == 41)
      ASSERT(v%End() - v%Begin() == 5)

   end subroutine


   subroutine testNewFromArray
      type(stdVectorInt) :: v

      call v%New([5,13,41,97,17,10,88])

      ASSERT(.not.v%Empty())
      ASSERT(v%Size() == 7)
      ASSERT(Size(v) == 7)
      ASSERT(size(v%data) == 7)
      ASSERT(all(v%data == [5,13,41,97,17,10,88]))
      ASSERT(v%front == 5)
      ASSERT(v%back == 88)
      ASSERT(v%End() - v%Begin() == 7)

   end subroutine


   subroutine testDelete
      type(stdVectorInt) :: v

      call v%New([5,13,41,97,17,10,88])
      call v%Delete()

      ASSERT(.not.associated(v%data))
      ASSERT(.not.associated(v%front))
      ASSERT(.not.associated(v%back))

   end subroutine


   subroutine testBegin
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it

      call v%New([4,6,38,216,48468,3,2,67,9])
      it = v%Begin()

      ASSERT(associated(it%value,v%data(1)))
      ASSERT(associated(it%value,v%front))
      ASSERT(it%value == 4)
      ASSERT(it%value == v%front)

   end subroutine


   subroutine testEnd
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it

      call v%New([4,6,38,216,48468,3,2,67,27])
      it = v%End()
      call it%Dec()

      ASSERT(associated(it%value,v%data(9)))
      ASSERT(associated(it%value,v%back))
      ASSERT(it%value == 27)
      ASSERT(it%value == v%back)

   end subroutine


   subroutine testSizeAndCapacity
      type(stdVectorInt) :: v
      integer            :: i

      call v%New()

      ASSERT(v%Size() == 0)
      ASSERT(v%Capacity() >= 0)

      do i = 1, 32
         call v%PushBack(i)
         ASSERT(v%Size() == i)
         ASSERT(v%Capacity() >= i)
      enddo

   end subroutine


   subroutine testResize
      type(stdVectorInt) :: v

      call v%New([246,57,2,6,7,38,245,2,6274,446])
      call v%Resize(20,1)

      ASSERT(v%Size() == 20)
      ASSERT(Size(v) == 20)
      ASSERT(size(v%data) == 20)
      ASSERT(all(v%data(1:10) == [246,57,2,6,7,38,245,2,6274,446]))
      ASSERT(all(v%data(11:20) == [1,1,1,1,1,1,1,1,1,1]))
      ASSERT(v%front == 246)
      ASSERT(associated(v%front,v%data(1)))
      ASSERT(v%back == 1)
      ASSERT(associated(v%back,v%data(20)))

   end subroutine


   subroutine testEmpty
      type(stdVectorInt) :: v

      call v%New()
      ASSERT(v%Empty())
      call v%Insert(1,[7,8,9])
      ASSERT(.not.v%Empty())
      call v%Clear()
      ASSERT(v%Empty())

   end subroutine


   subroutine testReserve
      type(stdVectorInt) :: v

      call v%New([4,5,6,7])
      call v%Reserve(50)
      ASSERT(v%Capacity() >= 50)
      call v%Reserve(30)
      ASSERT(v%Capacity() >= 50)

   end subroutine


   subroutine testShrinkToFit
      type(stdVectorInt) :: v

      call v%New([4,5,6,7])
      call v%Reserve(50)
      ASSERT(v%Capacity() >= 50)
      call v%PushBack(9)
      call v%ShrinkToFit()
      ASSERT(v%Capacity() == 5)

   end subroutine


   subroutine testPushBack
      type(stdVectorInt) :: v
      integer            :: i

      call v%New()
      do i = 1, 32
         call v%PushBack(i)
         ASSERT(v%back == i)
      enddo

   end subroutine


   subroutine testPopBack
      type(stdVectorInt) :: v

      call v%New([4,62,78,836,3])
      ASSERT(v%PopBack() == 3)
      ASSERT(v%PopBack() == 836)
      ASSERT(v%PopBack() == 78)
      ASSERT(v%PopBack() == 62)
      ASSERT(v%PopBack() == 4)
      ASSERT(v%Empty())

   end subroutine


   subroutine testInsertSingle
      type(stdVectorInt) :: v

      call v%New([4,6,8,3,737])

      call v%Insert(2,1)
      ASSERT(v%Size() == 6)
      ASSERT(all(v%data == [4,1,6,8,3,737]))

      call v%Insert(1,320)
      ASSERT(v%Size() == 7)
      ASSERT(v%front == 320)
      ASSERT(all(v%data == [320,4,1,6,8,3,737]))


      call v%Insert(8,29)
      ASSERT(v%Size() == 8)
      ASSERT(v%back == 29)
      ASSERT(all(v%data == [320,4,1,6,8,3,737,29]))

   end subroutine


   subroutine testInsertFill
      type(stdVectorInt) :: v

      call v%New([4,6,8,3,737])

      call v%Insert(2,3,1)
      ASSERT(v%Size() == 8)
      ASSERT(all(v%data == [4,1,1,1,6,8,3,737]))

      call v%Insert(1,2,320)
      ASSERT(v%Size() == 10)
      ASSERT(v%front == 320)
      ASSERT(all(v%data == [320,320,4,1,1,1,6,8,3,737]))


      call v%Insert(11,5,29)
      ASSERT(v%Size() == 15)
      ASSERT(v%back == 29)
      ASSERT(all(v%data == [320,320,4,1,1,1,6,8,3,737,29,29,29,29,29]))

   end subroutine


   subroutine testInsertArray
      type(stdVectorInt) :: v

      call v%New([4,6,8,3,737])

      call v%Insert(2,[8,9,1])
      ASSERT(v%Size() == 8)
      ASSERT(all(v%data == [4,8,9,1,6,8,3,737]))

      call v%Insert(1,[320,321])
      ASSERT(v%Size() == 10)
      ASSERT(v%front == 320)
      ASSERT(all(v%data == [320,321,4,8,9,1,6,8,3,737]))


      call v%Insert(11,[29,30,31,32,33])
      ASSERT(v%Size() == 15)
      ASSERT(v%back == 33)
      ASSERT(all(v%data == [320,321,4,8,9,1,6,8,3,737,29,30,31,32,33]))

   end subroutine


   subroutine testEraseSingle
      type(stdVectorInt) :: v

      call v%New([3,6,12,-4,733])

      call v%Erase(2)
      ASSERT(v%Size() == 4)
      ASSERT(all(v%data == [3,12,-4,733]))

      call v%Erase(1)
      ASSERT(v%Size() == 3)
      ASSERT(v%front == 12)
      ASSERT(all(v%data == [12,-4,733]))

      call v%Erase(3)
      ASSERT(v%Size() == 2)
      ASSERT(v%back == -4)
      ASSERT(all(v%data == [12,-4]))

   end subroutine


   subroutine testEraseRange
      type(stdVectorInt) :: v

      call v%New([1,-5,2,5126,-356,33,823,3,1,2])

      call v%Erase(2,5)
      ASSERT(v%Size() == 7)
      ASSERT(all(v%data == [1,-356,33,823,3,1,2]))

      call v%Erase(1,3)
      ASSERT(v%Size() == 5)
      ASSERT(v%front == 33)
      ASSERT(all(v%data == [33,823,3,1,2]))

      call v%Erase(4,6)
      ASSERT(v%Size() == 3)
      ASSERT(v%back == 3)
      ASSERT(all(v%data == [33,823,3]))

   end subroutine


   subroutine testClear
      type(stdVectorInt) :: v

      call v%New([1,-5,2,5126,-356,33,823,3,1,2])
      call v%Clear()

      ASSERT(v%Empty())
      ASSERT(v%Size() == 0)

   end subroutine


   subroutine testNewItDefault
      type(stdVectorIntIterator) :: it

      call it%New()
      ASSERT(.not.associated(it%value))

   end subroutine


   subroutine testNewItCopyOther
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it1, it2

      call v%New([353,6,5,2,2274,33])
      it1 = v%Begin()
      call it2%New(it1)

      ASSERT(associated(it1%value,it2%value))
      ASSERT(it2%value == 353)

   end subroutine


   subroutine testInc
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it

      call v%New([353,6,5,2,2274,33])
      it = v%Begin()

      ASSERT(associated(it%value,v%data(1)))
      ASSERT(it%value == 353)

      call it%Inc()

      ASSERT(associated(it%value,v%data(2)))
      ASSERT(it%value == 6)

   end subroutine


   subroutine testDec
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it

      call v%New([353,6,5,2,2274,33])
      it = v%End()
      call it%Dec()

      ASSERT(associated(it%value,v%data(6)))
      ASSERT(it%value == 33)

      call it%Dec()

      ASSERT(associated(it%value,v%data(5)))
      ASSERT(it%value == 2274)

   end subroutine


   subroutine testAdvanceReverseDiff
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it1, it2

      call v%New([353,6,5,2,2274,33])
      it1 = v%Begin()
      it2 = it1 + 4

      ASSERT(it2 - it1 == 4)
      ASSERT(associated(it2%value,v%data(5)))
      ASSERT(it2%value == 2274)

      it2 = it2 - 2
      ASSERT(it2 - it1 == 2)
      ASSERT(associated(it2%value,v%data(3)))
      ASSERT(it2%value == 5)

   end subroutine


   subroutine testLogicalOperations
      type(stdVectorInt) :: v
      type(stdVectorIntIterator) :: it1, it2

      call v%New([4,7,3,6,8])
      it1 = v%Begin() + 2
      ASSERT(it1%value == 3)
      it2 = v%Begin()

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

      ASSERT(it2 == v%End())

   end subroutine


end module
