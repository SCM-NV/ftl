! Copyright (c) 2018  Robert Rüger
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

module ftlHashSetTestsModule

   use ftlTestToolsModule
   use ftlHashSetIntModule
   use ftlHashSetStringModule

   implicit none
   private
   public :: ftlHashSetTests

contains


   subroutine ftlHashSetTests

      write (*,'(A)') 'Running ftlHashSet tests ...'

      ! Tests of the ftlHashSet container itself:

      call testNewDefault
      call testNewCopyOther
      call testAssignment
      call testInsertAndHas
      call testRehash
      call testIterators
      call testFind
      call testEraseValue
      call testEraseIterators

      call testStringSpecialization

      call testArrayFinalization

   end subroutine


   subroutine testNewDefault
      type(ftlHashSetInt) :: set

      call set%New(100)

      ASSERT(set%Empty())
      ASSERT(set%Size() == 0)
      ASSERT(size(set) == 0)
      ASSERT(set%BucketCount() == 100)

   end subroutine


   subroutine testNewCopyOther
      type(ftlHashSetInt) :: set, copy

      call set%New(10)
      call set%Insert(42)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)
      call set%Insert(13)
      ! Insert something already in the set:
      call set%Insert(13)
      call set%Insert(12)
      call set%Insert(11)

      call copy%New(set)
      call set%Delete()

      ASSERT(copy%Size() == 14)
      ASSERT(size(copy) == 14)
      ASSERT(copy%Has(42))
      ASSERT(copy%Has( 1))
      ASSERT(copy%Has( 2))
      ASSERT(copy%Has( 3))
      ASSERT(copy%Has( 4))
      ASSERT(copy%Has( 5))
      ASSERT(copy%Has( 6))
      ASSERT(copy%Has(12))
      ASSERT(copy%Has( 9))
      ASSERT(copy%Has( 7))
      ASSERT(copy%Has(10))
      ASSERT(copy%Has( 8))
      ASSERT(copy%Has(11))
      ASSERT(copy%Has(13))

      call copy%New(set)

      ASSERT(copy%Size() == 0)
      ASSERT(copy%Empty())

   end subroutine


   subroutine testAssignment
      type(ftlHashSetInt) :: set, copy

      call set%New(10)
      call set%Insert(42)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)
      call set%Insert(13)
      ! Insert something already in the set:
      call set%Insert(13)
      call set%Insert(12)
      call set%Insert(11)

      copy = set
      call set%Delete()

      ASSERT(copy%Size() == 14)
      ASSERT(size(copy) == 14)
      ASSERT(copy%Has(42))
      ASSERT(copy%Has( 1))
      ASSERT(copy%Has( 2))
      ASSERT(copy%Has( 3))
      ASSERT(copy%Has( 4))
      ASSERT(copy%Has( 5))
      ASSERT(copy%Has( 6))
      ASSERT(copy%Has(12))
      ASSERT(copy%Has( 9))
      ASSERT(copy%Has( 7))
      ASSERT(copy%Has(10))
      ASSERT(copy%Has( 8))
      ASSERT(copy%Has(11))
      ASSERT(copy%Has(13))

      copy = set

      ASSERT(copy%Size() == 0)
      ASSERT(copy%Empty())

   end subroutine


   subroutine testInsertAndHas
      type(ftlHashSetInt) :: set

      call set%New(10)

      call set%Insert(42)

      ASSERT(set%Size() == 1)
      ASSERT(size(set) == 1)

      ! add > 10 elements to have overfull buckets
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)
      ! try inserting things already in the set ...
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)

      ASSERT(set%Size() == 13)
      ASSERT(size(set) == 13)

      ! check contents.

      ASSERT( 1 .in. set)
      ASSERT( 2 .in. set)
      ASSERT( 3 .in. set)
      ASSERT( 4 .in. set)
      ASSERT( 5 .in. set)
      ASSERT( 6 .in. set)
      ASSERT( 7 .in. set)
      ASSERT( 8 .in. set)
      ASSERT( 9 .in. set)
      ASSERT(10 .in. set)
      ASSERT(11 .in. set)
      ASSERT(12 .in. set)
      ASSERT(.not.(13 .in. set))
      ASSERT(.not.(14 .in. set))
      ASSERT(.not.(15 .in. set))
      ASSERT(.not.(16 .in. set))
      ASSERT(.not.(17 .in. set))
      ASSERT(42 .in. set)

   end subroutine


   subroutine testRehash
      type(ftlHashSetInt) :: set

      call set%New(10)
      call set%SetMaxLoadFactor(1.0)

      ASSERT(set%BucketCount() == 10)

      call set%Insert( 0)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)
      call set%Insert(13)

      ASSERT(size(set) == 14)
      ASSERT(set%LoadFactor() < 1.0)
      ASSERT(set%BucketCount() > 10)

      call set%Rehash(5)

      ASSERT(size(set) == 14)
      ASSERT(set%BucketCount() == 5)

   end subroutine


   subroutine testIterators
      type(ftlHashSetInt) :: set
      type(ftlHashSetIntIterator) :: it
      integer :: i

      call set%New(10)

      call set%Insert( 0)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)

      call set%Rehash(10) ! make sure we have overfull buckets

      ASSERT(size(set) == 12)
      ASSERT(set%LoadFactor() > 1.0)
      ASSERT(set%BucketCount() == 10)

      it = set%Begin()
      do i = 1, 12
         ASSERT(it /= set%End())
         ASSERT(associated(it%value))
         ASSERT(set%Has(it%value))
         call it%Inc()
      enddo

      ASSERT(it == set%End())

   end subroutine


   subroutine testFind
      type(ftlHashSetInt) :: set

      call set%New(10)

      call set%Insert( 0)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)

      call set%Rehash(10) ! make sure we have overfull buckets

      ASSERT(set%Find( 0) /= set%End())
      ASSERT(set%Find( 1) /= set%End())
      ASSERT(set%Find( 2) /= set%End())
      ASSERT(set%Find( 3) /= set%End())
      ASSERT(set%Find( 4) /= set%End())
      ASSERT(set%Find( 5) /= set%End())
      ASSERT(set%Find( 6) /= set%End())
      ASSERT(set%Find( 7) /= set%End())
      ASSERT(set%Find( 8) /= set%End())
      ASSERT(set%Find( 9) /= set%End())
      ASSERT(set%Find(10) /= set%End())
      ASSERT(set%Find(11) /= set%End())

      ASSERT(set%Find(1337) == set%End())

   end subroutine


   subroutine testEraseValue
      type(ftlHashSetInt) :: set

      call set%New(10)

      ASSERT(set%Empty())
      ASSERT(size(set) == 0)

      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)
      call set%Insert(12)
      call set%Insert(13)

      ASSERT(size(set) == 13)

      call set%Erase(10)
      ASSERT(.not.set%Has(10))
      ASSERT(size(set) == 12)

      ! test double erase

      call set%Erase(10)
      ASSERT(.not.set%Has(10))
      ASSERT(size(set) == 12)

      call set%Rehash(8) ! make sure we have overfull buckets

      ! erase all the rest

      call set%Erase(1)
      call set%Erase(2)
      call set%Erase(3)
      call set%Erase(4)
      call set%Erase(5)
      call set%Erase(6)
      call set%Erase(7)
      call set%Erase(8)
      call set%Erase(9)
      ! 10 already erased
      call set%Erase(11)
      call set%Erase(12)
      call set%Erase(13)

      ASSERT(.not.set%Has(1))
      ASSERT(.not.set%Has(2))
      ASSERT(.not.set%Has(3))
      ASSERT(.not.set%Has(4))
      ASSERT(.not.set%Has(5))
      ASSERT(.not.set%Has(6))
      ASSERT(.not.set%Has(7))
      ASSERT(.not.set%Has(8))
      ASSERT(.not.set%Has(9))
      ASSERT(.not.set%Has(10))
      ASSERT(.not.set%Has(11))
      ASSERT(.not.set%Has(12))
      ASSERT(.not.set%Has(13))

      ASSERT(set%Empty())
      ASSERT(size(set) == 0)

   end subroutine


   subroutine testEraseIterators
      type(ftlHashSetInt) :: set
      type(ftlHashSetIntIterator) :: it

      call set%New(10)

      call set%Insert( 0)
      call set%Insert( 1)
      call set%Insert( 2)
      call set%Insert( 3)
      call set%Insert( 4)
      call set%Insert( 5)
      call set%Insert( 6)
      call set%Insert( 7)
      call set%Insert( 8)
      call set%Insert( 9)
      call set%Insert(10)
      call set%Insert(11)

      call set%Rehash(10) ! make sure we have overfull buckets

      ASSERT(size(set) == 12)

      ASSERT(set%Has(5))
      call set%Erase(5)
      ASSERT(.not.(5 .in. set))

      ASSERT(size(set) == 11)

      ASSERT(set%Has(6))
      it = set%Find(6)
      call set%Erase(it)
      ASSERT(.not.set%Has(6))

      ASSERT(size(set) == 10)

      it = set%Begin()
      call it%Inc()
      call it%Inc()
      call set%Erase(it, set%End())

      ASSERT(size(set) == 2)

   end subroutine


   subroutine testStringSpecialization
      type(ftlHashSetString) :: set

      call set%New(10)

      call set%Insert('this')
      call set%Insert('is')
      call set%Insert('a')
      call set%Insert('test')
      call set%Insert('with')
      call set%Insert('variable')
      call set%Insert('length')
      call set%Insert('keys!')

      ASSERT(set%Has('this'))
      ASSERT(set%Has('is'))
      ASSERT(set%Has('a'))
      ASSERT(set%Has('test'))
      ASSERT(set%Has('with'))
      ASSERT(set%Has('variable'))
      ASSERT(set%Has('length'))
      ASSERT(set%Has('keys!'))
      ASSERT(.not.set%Has('not there'))

      ASSERT(set%Find('with') /= set%End())
      call set%Erase('with')
      ASSERT(set%Find('with') == set%End())

   end subroutine


   subroutine testArrayFinalization
      type(ftlHashSetString) :: set(2)

      call set(1)%New(10)
      call set(1)%Insert('this')
      call set(1)%Insert('is')
      call set(1)%Insert('a')
      call set(1)%Insert('test')
      ASSERT('this' .in. set(1))

      call set(2)%New(10)
      call set(2)%Insert('bla blub')
      call set(2)%Insert('here')
      call set(2)%Insert('another')
      call set(2)%Insert('test')
      ASSERT('here' .in. set(2))

      ! This subroutine should not leak. Check with 'make memtest'

   end subroutine

end module
