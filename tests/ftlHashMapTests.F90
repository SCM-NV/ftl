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

module ftlHashMapTestsModule

   use ftlTestToolsModule
   use ftlHashMapStrIntModule

   implicit none
   private
   public :: ftlHashMapTests

contains


   subroutine ftlHashMapTests

      write (*,'(A)') 'Running ftlHashMap tests ...'

      ! Tests of the ftlHashMap container itself:

      call testNewDefault
      call testSetAndGet
      call testErase

   end subroutine


   subroutine testNewDefault
      type(ftlHashMapStrInt) :: um

      call um%New(100)

      ASSERT(um%Empty())
      ASSERT(um%Size() == 0)
      ASSERT(size(um) == 0)
      ASSERT(um%BucketCount() == 100)

#ifdef FTL_NO_FINALIZERS
      call um%Delete()
#endif

   end subroutine


   subroutine testSetAndGet
      type(ftlHashMapStrInt) :: um
      integer :: i
      integer, pointer :: ptr

      call um%New(10)

      call um%Set('foo ', 42)

      ASSERT(um%Size() == 1)
      ASSERT(size(um) == 1)

      ! add > 10 elements to have overfull buckets
      call um%Set('bar ',  1)
      call um%Set('test',  2)
      call um%Set('blub',  3)
      call um%Set('jipi',  4)
      call um%Set('fort',  5)
      call um%Set('ran ',  6)
      call um%Set('is m',  7)
      call um%Set('y fa',  8)
      call um%Set('vour',  9)
      call um%Set('ite ', 10)
      call um%Set('lang', 11)
      call um%Set('not ', 12)
      call um%Set('rly!', 13)

      ASSERT(um%Size() == 14)
      ASSERT(size(um) == 14)

      ! retrieve all entries again

      ptr => um%Get('foo ')

      ASSERT(associated(ptr))
      ASSERT(ptr == 42)

      ASSERT(um%Get('bar ') ==  1)
      ASSERT(um%Get('test') ==  2)
      ASSERT(um%Get('blub') ==  3)
      ASSERT(um%Get('jipi') ==  4)
      ASSERT(um%Get('fort') ==  5)
      ASSERT(um%Get('ran ') ==  6)
      ASSERT(um%Get('not ') == 12)
      ASSERT(um%Get('vour') ==  9)
      ASSERT(um%Get('is m') ==  7)
      ASSERT(um%Get('ite ') == 10)
      ASSERT(um%Get('y fa') ==  8)
      ASSERT(um%Get('lang') == 11)
      ASSERT(um%Get('rly!') == 13)

      ! try writing to pointers we got from Get()

      ptr = 82
      ASSERT(um%Get('foo ') == 82)

      ! try getting into a value type directly

      i = um%Get('blub')
      ASSERT(i == 3)
      i = 1337
      ASSERT(um%Get('blub') == 3)

      ! try setting the value for a key that already exists

      call um%Set('bar ',  -1)
      call um%Set('test',  -2)
      call um%Set('blub',  -3)
      call um%Set('jipi',  -4)
      call um%Set('fort',  -5)
      call um%Set('ran ',  -6)
      call um%Set('not ', -12)
      call um%Set('vour',  -9)
      call um%Set('is m',  -7)
      call um%Set('y fa',  -8)
      call um%Set('ite ', -10)
      call um%Set('lang', -11)
      call um%Set('rly!', -13)

      ASSERT(um%Get('bar ') ==  -1)
      ASSERT(um%Get('test') ==  -2)
      ASSERT(um%Get('blub') ==  -3)
      ASSERT(um%Get('jipi') ==  -4)
      ASSERT(um%Get('fort') ==  -5)
      ASSERT(um%Get('ran ') ==  -6)
      ASSERT(um%Get('not ') == -12)
      ASSERT(um%Get('vour') ==  -9)
      ASSERT(um%Get('is m') ==  -7)
      ASSERT(um%Get('y fa') ==  -8)
      ASSERT(um%Get('ite ') == -10)
      ASSERT(um%Get('lang') == -11)
      ASSERT(um%Get('rly!') == -13)

#ifdef FTL_NO_FINALIZERS
      call um%Delete()
#endif

   end subroutine


   subroutine testErase
      type(ftlHashMapStrInt) :: um

      call um%New(10)

      ASSERT(um%Empty())
      ASSERT(size(um) == 0)

      call um%Set('bar ',  1)
      call um%Set('test',  2)
      call um%Set('blub',  3)
      call um%Set('jipi',  4)
      call um%Set('fort',  5)
      call um%Set('ran ',  6)
      call um%Set('is m',  7)
      call um%Set('y fa',  8)
      call um%Set('vour',  9)
      call um%Set('ite ', 10)
      call um%Set('lang', 11)
      call um%Set('not ', 12)
      call um%Set('rly!', 13)

      ASSERT(size(um) == 13)

      call um%Erase('bar ')
      ASSERT(.not.um%Has('bar '))
      ASSERT(size(um) == 12)

      ! test double erase

      call um%Erase('bar ')
      ASSERT(.not.um%Has('bar '))
      ASSERT(size(um) == 12)

      ! erase all the rest

      call um%Erase('test')
      call um%Erase('blub')
      call um%Erase('jipi')
      call um%Erase('fort')
      call um%Erase('ran ')
      call um%Erase('is m')
      call um%Erase('y fa')
      call um%Erase('vour')
      call um%Erase('ite ')
      call um%Erase('lang')
      call um%Erase('not ')
      call um%Erase('rly!')

      ASSERT(.not.um%Has('test'))
      ASSERT(.not.um%Has('blub'))
      ASSERT(.not.um%Has('jipi'))
      ASSERT(.not.um%Has('fort'))
      ASSERT(.not.um%Has('ran '))
      ASSERT(.not.um%Has('is m'))
      ASSERT(.not.um%Has('y fa'))
      ASSERT(.not.um%Has('vour'))
      ASSERT(.not.um%Has('ite '))
      ASSERT(.not.um%Has('lang'))
      ASSERT(.not.um%Has('not '))
      ASSERT(.not.um%Has('rly!'))

      ASSERT(um%Empty())
      ASSERT(size(um) == 0)

#ifdef FTL_NO_FINALIZERS
      call um%Delete()
#endif

   end subroutine


end module
