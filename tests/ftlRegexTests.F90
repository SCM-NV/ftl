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

      call testCaptureGroups

   end subroutine


   subroutine testCaptureGroups
      type(ftlRegex) :: r
      type(ftlRegexMatch) :: m

      call r%New('(\w{1,})\s{0,}=\s{0,}(\w{1,})')

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
