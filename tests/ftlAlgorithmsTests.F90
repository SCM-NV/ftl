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

      call testForEach

      call testFind
      call testFindIf
      call testFindIfNot

      call testCount
      call testCountIf

      call testMismatch

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
      ASSERT(ftlAnyOf(v,IsOdd))

      it = v%End() - 1
      ASSERT(.not.ftlAnyOf(v%Begin(),it,IsOdd))

   end subroutine


   subroutine testNoneOf
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New()
      ASSERT(ftlNoneOf(l,IsEven))

      call l%New([2,4,6,8,10,12,13])
      ASSERT(.not.ftlNoneOf(l,IsOdd))

      it = l%End()
      call it%Dec()
      call it%Dec()
      ASSERT(ftlNoneOf(l%Begin(),it,IsOdd))

   end subroutine


   subroutine testForEach
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New([1,2,3,4,5])
      call ftlForEach(v, Square)

      ASSERT(all(v%data == [1,4,9,16,25]))

      it = v%Begin() + 2
      call ftlForEach(it, v%End(), Square)

      ASSERT(all(v%data == [1,4,81,256,625]))

   end subroutine


   subroutine testFind
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFind(v, 93)

      ASSERT(it%value == 93)
      ASSERT(it - v%Begin() == 2)

      it = ftlFind(v%Begin() + 3, v%End(), 93)

      ASSERT(it%value == 93)
      ASSERT(it - v%Begin() == 4)

   end subroutine


   subroutine testFindIf
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFindIf(v, IsEven)

      ASSERT(it%value == 8)
      ASSERT(it - v%Begin() == 1)

      it = ftlFindIf(v%Begin()+2, v%End(), IsEven)

      ASSERT(it == v%End())

   end subroutine


   subroutine testFindIfNot
      type(ftlVectorInt) :: v
      type(ftlVectorIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFindIfNot(v, IsOdd)

      ASSERT(it%value == 8)
      ASSERT(it - v%Begin() == 1)

      it = ftlFindIfNot(v%Begin()+2, v%End(), IsOdd)

      ASSERT(it == v%End())

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
      ASSERT(ftlCountIf(l,IsOdd) == 4)

      it = l%End()
      call it%Dec()
      call it%Dec()

      ASSERT(ftlCountIf(l%Begin(),it,IsEven) == 5)
      ASSERT(ftlCountIf(l%Begin(),it,IsOdd) == 3)

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


   subroutine testMismatch
      type(ftlVectorInt) :: u, v
      type(ftlVectorIntIterator) :: it(2)

      call u%New([9,6,3,6,1])
      call v%New([9,6,3,4,2])

      it = ftlMismatch(u%Begin(), u%End(), v%Begin())

      ASSERT(it(1)%value == 6)
      ASSERT(it(1) == u%End() - 2)
      ASSERT(it(2)%value == 4)
      ASSERT(it(2) == v%End() - 2)

      call u%New([10,7,4,6,1])
      call v%New([9,6,3,4,2])

      it = ftlMismatch(u, v, Greater)

      ASSERT(it(1)%value == 1)
      ASSERT(it(1) == u%End() - 1)
      ASSERT(it(2)%value == 2)
      ASSERT(it(2) == v%End() - 1)

   end subroutine


   ! example unary predicates:

   pure logical function IsEven(n)
      integer, intent(in) :: n
      IsEven = (mod(n,2) == 0)
   end function

   pure logical function IsOdd(n)
      integer, intent(in) :: n
      IsOdd = (mod(n,2) == 1)
   end function

   ! example comparator:

   pure logical function Greater(n,m)
      integer, intent(in) :: n,m
      Greater = (n > m)
   end function

   ! example unary subroutines:

   subroutine Square(n)
      integer, intent(inout) :: n
      n = n**2
   end subroutine


end module
