! Copyright (c) 2026 Software for Chemistry & Materials BV
! SPDX-License-Identifier: LGPL-3.0-or-later

#include "ftlTestTools.inc"

module ftlLRUCacheTestsModule
   use ftlTestToolsModule
   use ftlLRUCacheIntModule
   use ftlLRUCacheStringModule
   use ftlStringModule
   implicit none
   private
   public :: ftlLRUCacheTests
contains

   subroutine ftlLRUCacheTests
      write (*,'(A)') 'Running ftlLRUCache tests ...'
      call testSlotsAndAcquisitions
      call testReleaseOrderAndPeek
      call testStringKeys
      call testStringSpecialization
      call testClearAndReinitialize
   end subroutine

   ! Deliberate hash collisions and a caller-owned payload arena exercise
   ! repeated slot replacement independently of the payload's representation.
   subroutine testSlotsAndAcquisitions
      type(ftlLRUCacheInt) :: cache, copied
      integer :: slot, same, a, b, c, i, key, payload(3)
      logical :: found, coherent

      call cache%New(0)
      call cache%Acquire(1,slot,found)
      ASSERT(slot == 0)
      ASSERT(.not.found)
      ASSERT(cache%Size() == 0)

      call cache%New(3)
      ! All four keys collide in the seven-bucket table.
      call cache%Acquire(1,a,found)
      call cache%Release(a)
      call cache%Acquire(8,b,found)
      call cache%Release(b)
      call cache%Acquire(15,c,found)
      call cache%Release(c)
      call cache%Acquire(1,slot,found)
      ASSERT(found)
      ASSERT(slot == a)
      call cache%Release(slot)
      call cache%Acquire(22,slot,found)
      ASSERT(slot == b)
      ASSERT(.not.cache%Has(8))
      ASSERT(cache%Has(1).and.cache%Has(15).and.cache%Has(22))
      call cache%Release(slot)

      call cache%Acquire(1,a,found)
      call cache%Acquire(15,b,found)
      call cache%Acquire(22,c,found)
      call cache%Acquire(29,slot,found)
      ASSERT(slot == 0)
      ASSERT(.not.found)
      call cache%Acquire(1,same,found)
      ASSERT(found.and.same == a)
      call cache%Release(same)
      call cache%Release(b)
      call cache%Acquire(29,slot,found)
      ASSERT(slot == b)
      ASSERT(cache%Has(1))
      call cache%Release(slot)
      call cache%Release(c)
      call cache%Release(a)
      copied = cache
      call cache%Clear()
      ASSERT(cache%Size() == 0)
      ASSERT(copied%Has(1).and.copied%Has(29))

      coherent = .true.
      do i = 1, 10000
         key = modulo(i*i+17*i,11)
         call cache%Acquire(key,slot,found)
         if (found) then
            coherent = coherent .and. payload(slot) == key*13
         else
            payload(slot) = key*13
         endif
         call cache%Release(slot)
      enddo
      ASSERT(coherent)
      ASSERT(cache%Size() == 3)

      call cache%New(1)
      call cache%Acquire(1,a,found)
      call cache%Acquire(2,slot,found)
      ASSERT(slot == 0)
      call cache%Release(a)
      call cache%Acquire(2,slot,found)
      ASSERT(slot == a)
      call cache%Release(slot)
      call cache%Delete()
      ASSERT(cache%Capacity() == 0)
   end subroutine

   subroutine testReleaseOrderAndPeek
      type(ftlLRUCacheInt) :: cache
      integer :: a, b, slot, nested
      logical :: found

      call cache%New(2)
      call cache%Acquire(1,a,found)
      call cache%Release(a)
      call cache%Acquire(2,b,found)
      call cache%Release(b)
      ASSERT(cache%Has(1))
      call cache%Acquire(3,slot,found)
      ASSERT(.not.found.and.slot == a)
      ASSERT(.not.cache%Has(1))
      call cache%Release(slot)

      call cache%Acquire(2,b,found)
      call cache%Acquire(2,nested,found)
      ASSERT(found.and.nested == b)
      call cache%Acquire(3,a,found)
      call cache%Release(b)
      call cache%Acquire(4,slot,found)
      ASSERT(.not.found.and.slot == 0)
      call cache%Release(a)
      call cache%Acquire(4,slot,found)
      ASSERT(.not.found.and.slot == a)
      call cache%Release(slot)
      call cache%Release(nested)
      ! The last release makes key 2 newer than key 4, independently of when
      ! key 2 was first acquired. The nested release did not unpin it early.
      call cache%Acquire(5,slot,found)
      ASSERT(slot == a)
      ASSERT(cache%Has(2).and..not.cache%Has(4))
      call cache%Release(slot)
   end subroutine

   subroutine testStringKeys
      type(ftlLRUCacheString) :: cache, copied
      type(ftlString) :: key
      integer :: a, b, slot
      logical :: found

      call cache%New(2)
      key = 'alpha'
      call cache%Acquire(key,a,found)
      ASSERT(.not.found.and.a /= 0)
      call cache%Release(a)
      key = 'beta'
      call cache%Acquire(key,b,found)
      ASSERT(.not.found.and.b /= a)
      call cache%Release(b)
      ASSERT(cache%Has(ftlString('alpha')))
      call cache%Acquire(ftlString('alpha'),slot,found)
      ASSERT(found.and.slot == a)
      call cache%Release(slot)

      copied = cache
      call cache%Acquire(ftlString('gamma'),slot,found)
      ASSERT(.not.found.and.slot == b)
      call cache%Release(slot)
      ASSERT(.not.cache%Has(ftlString('beta')))
      ASSERT(copied%Has(ftlString('beta')))
      call cache%Clear()
      ASSERT(cache%Size() == 0.and.cache%Capacity() == 2)
      ASSERT(copied%Has(ftlString('alpha')))
      call cache%Acquire(ftlString('delta'),slot,found)
      ASSERT(.not.found.and.slot /= 0)
      call cache%Release(slot)
      call cache%Delete()
      ASSERT(.not.cache%Has(ftlString('delta')))
   end subroutine

   subroutine testStringSpecialization
      type(ftlLRUCacheString) :: cache
      character(len=:), allocatable :: key
      character(len=8) :: padded
      integer :: a, b, nested, slot
      logical :: found

      call cache%New(2)
      key = 'alpha'
      call cache%Acquire(key,a,found)
      ASSERT(.not.found.and.a /= 0)
      ASSERT(cache%Has(key).and.cache%Has(ftlString(key)))
      call cache%Acquire(ftlString('alpha'),nested,found)
      ASSERT(found.and.nested == a)
      call cache%Release(a)

      call cache%Acquire('beta',b,found)
      ASSERT(.not.found.and.b /= a)
      call cache%Acquire('gamma',slot,found)
      ASSERT(.not.found.and.slot == 0)
      ASSERT(.not.cache%Has('gamma'))
      call cache%Release(nested)
      call cache%Acquire('gamma',slot,found)
      ASSERT(.not.found.and.slot == a)
      call cache%Release(slot)
      call cache%Release(b)
      ASSERT(.not.cache%Has('alpha'))
      ASSERT(cache%Has('beta').and.cache%Has(ftlString('gamma')))

      call cache%New(0)
      call cache%Acquire('disabled',slot,found)
      ASSERT(.not.found.and.slot == 0)
      ASSERT(.not.cache%Has('disabled'))

      call cache%New(3)
      call cache%Acquire('',slot,found)
      ASSERT(.not.found.and.slot /= 0)
      call cache%Release(slot)
      ASSERT(cache%Has('').and.cache%Has(ftlString('')))
      padded = 'key'
      call cache%Acquire(padded,slot,found)
      ASSERT(.not.found.and.slot /= 0)
      call cache%Release(slot)
      ASSERT(cache%Has(padded).and.cache%Has(ftlString(padded)))
      ASSERT(.not.cache%Has('key'))
   end subroutine

   subroutine testClearAndReinitialize
      type(ftlLRUCacheInt) :: cache
      integer :: slot
      logical :: found

      ASSERT(cache%Size() == 0.and.cache%Capacity() == 0)
      ASSERT(.not.cache%Has(1))
      call cache%Clear()
      call cache%Delete()
      call cache%New(2)
      call cache%Acquire(-17,slot,found)
      ASSERT(.not.found.and.slot /= 0)
      call cache%Release(slot)
      ASSERT(cache%Has(-17))
      call cache%Clear()
      ASSERT(cache%Size() == 0.and.cache%Capacity() == 2)
      ASSERT(.not.cache%Has(-17))
      call cache%New(0)
      ASSERT(cache%Size() == 0.and.cache%Capacity() == 0)
      ASSERT(cache%EstimateBytes(0) == 0)
   end subroutine

end module
