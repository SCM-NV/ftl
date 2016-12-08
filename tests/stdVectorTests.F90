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

      call testNewDefault
      call testNewCopyOther
      call testNewFill
      call testNewFromArray

      call testDelete

      call testBegin
      call testEnd

      call testResize

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


end module
