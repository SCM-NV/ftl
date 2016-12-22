#include "ftlTestTools.inc"

module ftlAlgorithmsTestsModule

   use ftlTestToolsModule
   use ftlVectorIntModule
   use ftlVectorIntAlgorithmsModule
   use ftlListIntModule
   use ftlListIntAlgorithmsModule

   implicit none
   private
   public :: ftlAlgorithmsTests

contains


   subroutine ftlAlgorithmsTests

      write (*,'(A)') 'Running ftlAlgorithms tests ...'

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

      call ftlSort(v, Greater)

      ASSERT(v%Size() == 5)
      ASSERT(v%front == 9)
      ASSERT(v%back == 3)
      ASSERT(all(v%data == [9,7,6,4,3]))

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

      call ftlSort(l, Greater)

      ASSERT(l%Size() == 5)
      ASSERT(l%front == 9)
      ASSERT(l%back == 3)

      it = l%Begin()
      ASSERT(it%value == 9)
      call it%Inc()
      ASSERT(it%value == 7)
      call it%Inc()
      ASSERT(it%value == 6)
      call it%Inc()
      ASSERT(it%value == 4)
      call it%Inc()
      ASSERT(it%value == 3)
      call it%Inc()
      ASSERT(it == l%End())

   end subroutine


   pure logical function Greater(n,m)
      integer, intent(in) :: n,m
      Greater = (n > m)
   end function


end module
