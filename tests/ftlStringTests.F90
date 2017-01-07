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

module ftlStringTestsModule

   use ftlTestToolsModule
   use ftlStringModule

   implicit none
   private
   public :: ftlStringTests

contains


   subroutine ftlStringTests

      write (*,'(A)') 'Running ftlString tests ...'

      call testNewDefault
      call testAssignFStr
      call testAssignOther

      call testFortranStandardMethods

      call testHash

   end subroutine


   subroutine testNewDefault
      type(ftlString) :: s

      call s%New()

   end subroutine


   subroutine testAssignFStr
      type(ftlString) :: s

      s = 'test'

      ASSERT(s%fstr == 'test')

   end subroutine


   subroutine testAssignOther
      type(ftlString) :: s1, s2

      s1 = 'testme'
      s2 = s1

      ASSERT(s1%fstr == 'testme')
      ASSERT(s2%fstr == 'testme')
      ASSERT(s1 == s2)

      s2 = 'theitcrowd'

      ASSERT(s1%fstr == 'testme')
      ASSERT(s2%fstr == 'theitcrowd')
      ASSERT(s1 /= s2)

   end subroutine


   subroutine testFortranStandardMethods
      type(ftlString) :: s1, s2

      s1 = '  this is a test with 4 trailing spaces and 2 leading spaces    '

      ASSERT(len(s1) == 64)
      ASSERT(len_trim(s1) == 60)

      s1 = trim(s1)

      ASSERT(len(s1) == 60)
      ASSERT(len_trim(s1) == 60)

   end subroutine


   subroutine testHash

      use ftlHashModule

      ASSERT(ftlHash(ftlString('testhash')) == ftlHash('testhash'))
      ASSERT(ftlHash(ftlString('another test')) == ftlHash('another test'))
      ASSERT(ftlHash(ftlString('and another')) == ftlHash('and another'))
      ASSERT(ftlHash(ftlString('number 4')) == ftlHash('number 4'))
      ASSERT(ftlHash(ftlString('the last')) == ftlHash('the last'))

   end subroutine


end module
