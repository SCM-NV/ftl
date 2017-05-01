! Copyright (c) 2016, 2017  Robert Rüger
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

module ftlAlgorithmsTestsModule

   use ftlTestToolsModule
   use ftlDynArrayIntModule
   use ftlDynArrayIntAlgorithmsModule
   use Point2DModule
   use ftlDynArrayPoint2DModule
   use ftlDynArrayPoint2DAlgorithmsModule
   use ftlListIntModule
   use ftlListIntAlgorithmsModule
   use ftlStringModule
   use ftlStringAlgorithmsModule

   implicit none
   private
   public :: ftlAlgorithmsTests

contains


   subroutine ftlAlgorithmsTests

      write (*,'(A)') 'Running ftlAlgorithms tests ...'

      ! Non-modifying sequence operations:

      call testAllOf
      call testAnyOf
      call testNoneOf

      call testForEach

      call testFind
      call testFindIf
      call testFindIfNot

      call testFirstOf

      call testCount
      call testCountIf
      call testCountIfDerivedType

      call testMismatch
      call testEqual

      call testIsPermutationDynArray
      call testIsPermutationList

      call testSearch

      ! Modifying sequence operations:

      call testIterSwap

      call testCopy

      call testGenerate

      ! Partitions:

      call testIsPartitioned
      call testPartition

      ! Sorting operations:

      call testSortDynArray
      call testSortList
      call testSortString
      call testIsSorted

      ! Heap:

      call testMakeHeap
      call testPushHeap
      call testPopHeap
      call testIsHeap

   end subroutine


   subroutine testAllOf
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New()
      ASSERT(ftlAllOf(v,IsEven))

      call v%New([2,4,6,8,10,12,13])
      ASSERT(.not.ftlAllOf(v,IsEven))

      it = v%End() - 1
      ASSERT(ftlAllOf(v%Begin(),it,IsEven))

   end subroutine


   subroutine testAnyOf
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

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
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New([1,2,3,4,5])
      call ftlForEach(v, Square)

      ASSERT(all(v%data == [1,4,9,16,25]))

      it = v%Begin() + 2
      call ftlForEach(it, v%End(), Square)

      ASSERT(all(v%data == [1,4,81,256,625]))

   end subroutine


   subroutine testFind
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFind(v, 93)

      ASSERT(it%value == 93)
      ASSERT(it - v%Begin() == 2)

      it = ftlFind(v%Begin() + 3, v%End(), 93)

      ASSERT(it%value == 93)
      ASSERT(it - v%Begin() == 4)

   end subroutine


   subroutine testFindIf
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFindIf(v, IsEven)

      ASSERT(it%value == 8)
      ASSERT(it - v%Begin() == 1)

      it = ftlFindIf(v%Begin()+2, v%End(), IsEven)

      ASSERT(it == v%End())

   end subroutine


   subroutine testFindIfNot
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New([3,8,93,5,93,67])
      it = ftlFindIfNot(v, IsOdd)

      ASSERT(it%value == 8)
      ASSERT(it - v%Begin() == 1)

      it = ftlFindIfNot(v%Begin()+2, v%End(), IsOdd)

      ASSERT(it == v%End())

   end subroutine


   subroutine testFirstOf
      type(ftlListInt) :: l, k
      type(ftlListIntIterator) :: it

      call l%New([5,6,98,3,4,6])
      call k%New([12,65,4])

      it = ftlFirstOf(l,k)

      ASSERT(it%value == 4)
      call it%Inc()
      call it%Inc()
      ASSERT(it == l%End())

   end subroutine


   subroutine testCount
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

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


   subroutine testCountIfDerivedType
      type(ftlDynArrayPoint2D) :: v

      call v%New()

      call v%PushBack(Point2D( 2.0, 0.5))
      call v%PushBack(Point2D(-1.3, 3.5))
      call v%PushBack(Point2D( 2.0,-0.9))
      call v%PushBack(Point2D( 1.3, 2.5))
      call v%PushBack(Point2D(-1.0,-0.5))
      call v%PushBack(Point2D( 4.0,-5.3))
      call v%PushBack(Point2D(-1.0, 0.5))
      call v%PushBack(Point2D( 1.0,-0.9))

      ASSERT(ftlCountIf(v,IsInFirstQuadrant) == 2)

   end subroutine
   pure logical function IsInFirstQuadrant(p)
      type(Point2D), intent(in) :: p
      IsInFirstQuadrant = (p%x > 0.0 .and. p%y > 0.0)
   end function


   subroutine testIterSwap
      type(ftlDynArrayInt) :: v

      call v%New([2,1,3,4,5,6])
      call ftlIterSwap(v%Begin(), v%Begin()+1)

      ASSERT(all(v%data == [1,2,3,4,5,6]))
      ASSERT(v%front == 1)

   end subroutine


   subroutine testCopy
      type(ftlDynArrayInt) :: src, dest
      type(ftlDynArrayIntIterator) :: it

      src = [1,2,3,4,5,6]
      dest = [9,8,7,6,5,4,3,2,1]
      it = ftlCopy(src, Begin(dest))

      ASSERT(all(dest%data == [1,2,3,4,5,6,3,2,1]))
      ASSERT(it%value == 3)

   end subroutine


   subroutine testGenerate
      type(ftlDynArrayInt) :: v

      call v%New(5)
      call ftlGenerate(v, FortyTwo)

      ASSERT(v%front == 42)
      ASSERT(v%back == 42)
      ASSERT(all(v%data == [42,42,42,42,42]))

   end subroutine


   subroutine testMismatch
      type(ftlDynArrayInt) :: u, v
      type(ftlDynArrayIntIterator) :: it(2)

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


   subroutine testEqual
      type(ftlListInt) :: l, k

      call l%New([4,5,6,7,8,9,0])
      call k%New([6,7,8,9,0])

      ASSERT(.not.ftlEqual(l,k))
      ASSERT(ftlEqual(ftlAdvance(l%Begin(),2),l%End(),k%Begin()))

      call l%New([41,52,63,74])
      call k%New([61,72,83,94])

      ASSERT(.not.ftlEqual(l,k))
      ASSERT(ftlEqual(l,k,LastDigitMatches))

   end subroutine


   subroutine testIsPermutationDynArray
      type(ftlDynArrayInt) :: u, v

      call u%New([12,7,5,9,4,6,0,99])
      call v%New([6,9,99,5,7,12,0,4])

      ASSERT(ftlIsPermutation(u,v))

      call u%New([1,2,3,4,5,9,8,7,6,5])
      call v%New([1,2,3,4,5,5,6,7,8,9])

      ASSERT(ftlIsPermutation(u,v))

      call u%New([1,2,3,4,5])

      ASSERT(.not.ftlIsPermutation(u,v))

      call u%New([0,0,0,1,2,3,4,5])
      call v%New([1,1,1,1,2,3,4,5])

      ASSERT(.not.ftlIsPermutation(u,v))
      ASSERT(ftlIsPermutation(u%Begin()+3,u%End(),v%Begin()+3))
      ASSERT(ftlIsPermutation(ftlAdvance(u%Begin(),3),u%End(),ftlAdvance(v%Begin(),3)))

   end subroutine


   subroutine testIsPermutationList
      type(ftlDynArrayInt) :: k, l

      call k%New([12,7,5,9,4,6,0,99])
      call l%New([6,9,99,5,7,12,0,4])

      ASSERT(ftlIsPermutation(k,l))

      call k%New([1,2,3,4,5,9,8,7,6,5])
      call l%New([1,2,3,4,5,5,6,7,8,9])

      ASSERT(ftlIsPermutation(k,l))

      call k%New([1,2,3,4,5])

      ASSERT(.not.ftlIsPermutation(k,l))

      call k%New([0,0,0,1,2,3,4,5])
      call l%New([1,1,1,1,2,3,4,5])

      ASSERT(.not.ftlIsPermutation(k,l))
      ASSERT(ftlIsPermutation(ftlAdvance(k%Begin(),3),k%End(),ftlAdvance(l%Begin(),3)))

   end subroutine


   subroutine testSearch
      type(ftlListInt) :: l, k
      type(ftlListIntIterator) :: it

      call l%New([376,658,422,875,324,467,589])

      call k%New([422,875,324])
      it = ftlSearch(l,k)

      ASSERT(it%value == 422)
      ASSERT(ftlDistance(l%Begin(),it) == 2)

      call k%New()
      it = ftlSearch(l,k)

      ASSERT(it == l%Begin())

      call k%New([85,34,47])
      it = ftlSearch(l,k)

      ASSERT(it == l%End())

      ASSERT(LastDigitMatches(546,56))
      ASSERT(.not.LastDigitMatches(546,25))

      it = ftlSearch(l,k,LastDigitMatches)

      ASSERT(it%value == 875)
      ASSERT(ftlDistance(l%Begin(),it) == 3)

   end subroutine


   subroutine testIsPartitioned
      type(ftlListInt) :: l

      call l%New([1,3,5,7,9,2,4,6,8])
      ASSERT(.not.ftlIsPartitioned(l, IsEven))
      ASSERT(ftlIsPartitioned(l, IsOdd))

      call l%New([2,4,6,8,0,1,3,5,7,9])
      ASSERT(ftlIsPartitioned(l, IsEven))
      ASSERT(.not.ftlIsPartitioned(l, IsOdd))

   end subroutine


   subroutine testPartition
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it
      integer :: n, i

      do n = 1, 100
         call v%New([ (RandomInt(), i = 1, 10+mod(n,20)) ])
         it = ftlPartition(v, IsEven)
         ASSERT(ftlIsPartitioned(v, IsEven))
         if (it /= v%End()) ASSERT(.not.IsEven(it%value))
         if (it /= v%Begin()) then
            call it%Dec()
            ASSERT(IsEven(it%value))
         endif
      end do

   end subroutine


   subroutine testSortDynArray
      type(ftlDynArrayInt) :: v
      integer :: i, n

      call v%New([9,6,3,4,7])

      call ftlSort(v)

      ASSERT(v%Size() == 5)
      ASSERT(v%front == 3)
      ASSERT(v%back == 9)
      ASSERT(all(v%data == [3,4,6,7,9]))
      ASSERT(ftlIsSorted(v))

      call ftlSort(v, Greater)

      ASSERT(v%Size() == 5)
      ASSERT(v%front == 9)
      ASSERT(v%back == 3)
      ASSERT(all(v%data == [9,7,6,4,3]))
      ASSERT(ftlIsSorted(v, Greater))

      do n = 1, 100
         call v%New([ (RandomInt(), i = 1, 100+mod(31*n,900)) ])
         call ftlSort(v)
         ASSERT(ftlIsSorted(v))
         call ftlSort(v,Greater)
         ASSERT(ftlIsSorted(v,Greater))
      enddo

      do n = 1, 100
         call v%New([ (RandomInt(), i = 1, 100+mod(29*n,900)) ])
         call ftlSort(v,Greater)
         ASSERT(ftlIsSorted(v,Greater))
         call ftlSort(v)
         ASSERT(ftlIsSorted(v))
      enddo

   end subroutine


   subroutine testSortList
      type(ftlListInt) :: l
      type(ftlListIntIterator) :: it

      call l%New([9,6,3,4,7])

      call ftlSort(l)

      ASSERT(l%Size() == 5)
      ASSERT(l%front == 3)
      ASSERT(l%back == 9)
      ASSERT(ftlIsSorted(l))

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
      ASSERT(ftlIsSorted(l, Greater))

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


   subroutine testSortString
      type(ftlString) :: s

      s = 'zxymmlk'
      call ftlSort(s)

      ASSERT(s == 'klmmxyz')

   end subroutine


   subroutine testIsSorted
      type(ftlDynArrayInt) :: v

      call v%New()

      ASSERT(ftlIsSorted(v))
      ASSERT(ftlIsSorted(v,Greater))

      call v%New([5,7,8,9,12,46,7,3])

      ASSERT(.not.ftlIsSorted(v%Begin(),v%End()))
      ASSERT(ftlIsSorted(v%Begin(),v%End()-2))
      ASSERT(ftlIsSortedUntil(v) == v%End()-2)

      call v%New([9,8,7,6,5,4,3,2,99,199])

      ASSERT(.not.ftlIsSorted(v%Begin(),v%End(),Greater))
      ASSERT(ftlIsSorted(v%Begin(),v%End()-2,Greater))
      ASSERT(ftlIsSortedUntil(v,Greater) == v%End()-2)

   end subroutine


   subroutine testMakeHeap
      type(ftlDynArrayInt) :: v
      integer :: n, i

      do n = 1, 100
         call v%New([ (RandomInt(), i = 1, 10+mod(n,20)) ])
         call ftlMakeHeap(v)
         ASSERT(ftlIsHeap(v))
      enddo

      do n = 1, 100
         call v%New([ (RandomInt(), i = 1, 10+mod(n,20)) ])
         call ftlMakeHeap(v,Greater)
         ASSERT(ftlIsHeap(v,Greater))
      enddo

   end subroutine


   subroutine testPushHeap
      type(ftlDynArrayInt) :: v
      integer :: i

      call v%New([ (RandomInt(), i = 1, 10) ])
      call ftlMakeHeap(v)
      ASSERT(ftlIsHeap(v))
      do i = 1, 100
         call v%PushBack(RandomInt())
         call ftlPushHeap(v)
         ASSERT(ftlIsHeap(v))
      enddo

      call ftlMakeHeap(v,Greater)
      ASSERT(ftlIsHeap(v,Greater))
      do i = 1, 100
         call v%PushBack(RandomInt())
         call ftlPushHeap(v,Greater)
         ASSERT(ftlIsHeap(v,Greater))
      enddo

   end subroutine


   subroutine testPopHeap
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: endOfHeap
      integer :: i, root

      call v%New([ (RandomInt(), i = 1, 100) ])
      call ftlMakeHeap(v)
      endOfHeap = v%End()
      do while (endOfHeap /= v%Begin())
         root = v%front
         call ftlPopHeap(v%Begin(),endOfHeap)
         call endOfHeap%Dec()
         ASSERT(endOfHeap%value == root)
         ASSERT(ftlIsHeap(v%Begin(),endOfHeap))
      enddo
      ASSERT(ftlIsSorted(v))

      call v%New([ (RandomInt(), i = 1, 100) ])
      call ftlMakeHeap(v,Greater)
      endOfHeap = v%End()
      do while (endOfHeap /= v%Begin())
         root = v%front
         call ftlPopHeap(v%Begin(),endOfHeap,Greater)
         call endOfHeap%Dec()
         ASSERT(endOfHeap%value == root)
         ASSERT(ftlIsHeap(v%Begin(),endOfHeap,Greater))
      enddo
      ASSERT(ftlIsSorted(v,Greater))

   end subroutine


   subroutine testIsHeap
      type(ftlDynArrayInt) :: v
      type(ftlDynArrayIntIterator) :: it

      call v%New()

      ASSERT(ftlIsHeap(v))

      call v%New([5])

      ASSERT(ftlIsHeap(v))

      call v%New([9,5,4,1,1,3,2,6])

      ASSERT(.not.ftlIsHeap(v))

      it = ftlIsHeapUntil(v)

      ASSERT(it%value == 6)
      ASSERT(it+1 == v%End())

      call v%New([1,4,2,5,8,3,4])

      ASSERT(ftlIsHeap(v,Greater))
      ASSERT(ftlIsHeapUntil(v,Greater) == v%End())

      call v%PushBack(1)
      call v%PushBack(7)

      ASSERT(.not.ftlIsHeap(v))

      it = ftlIsHeapUntil(v,Greater)

      ASSERT(it%value == 1)
      ASSERT(it+2 == v%End())

   end subroutine


end module
