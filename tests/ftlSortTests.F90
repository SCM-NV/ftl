#include "ftlTestTools.inc"

module ftlSortTestsModule

   use ftlTestToolsModule
   use ftlVectorIntModule
   use ftlSortftlVectorIntModule
   use ftlListIntModule
   use ftlSortftlListIntModule

   implicit none
   private
   public :: ftlSortTests

contains


   subroutine ftlSortTests

      write (*,'(A)') 'Running ftlSort tests ...'

      call testSortVector
      call testSortList

   end subroutine


   subroutine testSortVector
      type(ftlVectorInt) :: v

      call v%New([9,6,3,4,7])
      call ftlSort(v)

      ASSERT(v%Size() == 5)
      ASSERT(v%front == 3)
      ASSERT(v%back == 9)
      ASSERT(all(v%data == [3,4,6,7,9]))

   end subroutine


   subroutine testSortList
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([9,6,3,4,7])
      call ftlSort(l)

      ASSERT(l%Size() == 5)
      ASSERT(l%front == 3)
      ASSERT(l%back == 9)

      it = l%Begin()
      ASSERT(it%value == 3)
      call it%Inc()
      ASSERT(it%value == 4)
      call it%Inc()
      ASSERT(it%value == 6)
      call it%Inc()
      ASSERT(it%value == 7)
      call it%Inc()
      ASSERT(it%value == 9)
      call it%Inc()
      ASSERT(it == l%End())

   end subroutine


end module
