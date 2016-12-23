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

      call testAllOf
      call testAnyOf
      call testNoneOf

      call testCount
      call testCountIf

      call testSortVector
      call testSortList

   end subroutine


   subroutine testAllOf
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New()
      ASSERT(ftlAllOf(v,IsEven))

      call v%New([2,4,6,8,10,12,13])
      ASSERT(.not.ftlAllOf(v,IsEven))

      it = v%End() - 1
      ASSERT(ftlAllOf(v%Begin(),it,IsEven))

   end subroutine


   subroutine testAnyOf
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New()
      ASSERT(.not.ftlAnyOf(v,IsEven))

      call v%New([2,4,6,8,10,12,13])
      ASSERT(ftlAnyOf(v,IsUneven))

      it = v%End() - 1
      ASSERT(.not.ftlAnyOf(v%Begin(),it,IsUneven))

   end subroutine


   subroutine testNoneOf
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New()
      ASSERT(ftlNoneOf(l,IsEven))

      call l%New([2,4,6,8,10,12,13])
      ASSERT(.not.ftlNoneOf(l,IsUneven))

      it = l%End()
      call it%Dec()
      call it%Dec()
      ASSERT(ftlNoneOf(l%Begin(),it,IsUneven))

   end subroutine


   subroutine testCount
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New([6,2,895,6,3,6,43,2,6,3])

      ASSERT(ftlCount(v,6) == 4)
      ASSERT(ftlCount(v,2) == 2)
      ASSERT(ftlCount(v,895) == 1)
      ASSERT(ftlCount(v,3) == 2)
      ASSERT(ftlCount(v,43) == 1)

      it = v%End() - 2

      ASSERT(ftlCount(v%Begin(),it,6) == 3)
      ASSERT(ftlCount(v%Begin(),it,2) == 2)
      ASSERT(ftlCount(v%Begin(),it,895) == 1)
      ASSERT(ftlCount(v%Begin(),it,3) == 1)
      ASSERT(ftlCount(v%Begin(),it,43) == 1)

   end subroutine


   subroutine testCountIf
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([6,2,895,6,3,6,43,2,6,3])

      ASSERT(ftlCountIf(l,IsEven) == 6)
      ASSERT(ftlCountIf(l,IsUneven) == 4)

      it = l%End()
      call it%Dec()
      call it%Dec()

      ASSERT(ftlCountIf(l%Begin(),it,IsEven) == 5)
      ASSERT(ftlCountIf(l%Begin(),it,IsUneven) == 3)

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


   ! example unary predicates:

   pure logical function IsEven(n)
      integer, intent(in) :: n
      IsEven = (mod(n,2) == 0)
   end function

   pure logical function IsUneven(n)
      integer, intent(in) :: n
      IsUneven = (mod(n,2) == 1)
   end function

   ! example comparator:

   pure logical function Greater(n,m)
      integer, intent(in) :: n,m
      Greater = (n > m)
   end function


end module
