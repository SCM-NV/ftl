! Copyright (c) 2017  Robert Rüger
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

module ftlRegexTestsModule

   use ftlTestToolsModule
   use ftlRegexModule

   implicit none
   private
   public :: ftlRegexTests

contains


   subroutine ftlRegexTests

      write (*,'(A)') 'Running ftlRegex tests ...'

      call testCompileFlags
      call testCaptureGroups

   end subroutine


   subroutine testCompileFlags
      type(ftlRegex) :: r
      type(ftlRegexMatch) :: m

      ! the temporary ftlRegexes in these expressions leak with gfortran 6.3.1:

      !ASSERT(     ('blub atest' .matches. ftlRegex('a')))
      !ASSERT(.not.('blub atest' .matches. ftlRegex('A')))
      !ASSERT(     ('blub atest' .matches. ftlRegex('A', icase=.true.)))

      ! I believe that's a compiler bug. here is a workaround, so that we don't have a leaking testsuite ...

      call r%New('a')
      ASSERT('blub atest' .matches. r)
      call r%New('A')
      ASSERT(.not.('blub atest' .matches. r))
      call r%New('A', icase=.true.)
      ASSERT('blub atest' .matches. r)

      call r%New('([[:digit:]]+)\.([[:digit:]]+)')
      m = r%Match('some value: 12.436')
      ASSERT(m%matches)
      ASSERT(m%text == '12.436')
      ASSERT(size(m%group) == 2)
      ASSERT(m%group(1)%text == '12')
      ASSERT(m%group(2)%text == '436')

      call r%New('([[:digit:]]+)\.([[:digit:]]+)', nosub=.true.)
      m = r%Match('some value: 12.436')
      ASSERT(m%matches)
      ASSERT(m%text == '12.436')
      ASSERT(size(m%group) == 0)

   end subroutine


   subroutine testCaptureGroups
      type(ftlRegex) :: r
      type(ftlRegexMatch) :: m

      call r%New('(\w+)\s*=\s*(\w+)')

      ASSERT('something = other' .matches. r)

      m = r%Match('occupations option=value')
      ASSERT(m%text == 'option=value')
      ASSERT(m%begin == 13)
      ASSERT(m%end == 25)
      ASSERT(size(m%group) == 2)
      ASSERT(m%group(1)%text == 'option')
      ASSERT(m%group(1)%begin == 13)
      ASSERT(m%group(1)%end == 19)
      ASSERT(m%group(2)%text == 'value')
      ASSERT(m%group(2)%begin == 20)
      ASSERT(m%group(2)%end == 25)

   end subroutine


end module
