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

program tests

   use ftlTestToolsModule
   use ftlStringTestsModule
   use ftlRegexTestsModule
   use ftlDynArrayTestsModule
   use ftlListTestsModule
   use ftlHashMapTestsModule
   use ftlSharedPtrTestsModule
   use ftlAlgorithmsTestsModule

   ! dummy assertion to test that assertions themselves work ...
   ASSERT(.false.)

   call ftlRegexTests
   call ftlStringTests
   call ftlDynArrayTests
   call ftlListTests
   call ftlHashMapTests
   call ftlSharedPtrTests
   call ftlAlgorithmsTests

   write (*,'(A,I0,A,I0)') 'Failed assertions: ',num_failed,'/',num_asserts
   if (num_failed > 0) then
      write (*,'(A)') 'TEST FAILED'
      error stop
   else
      write (*,'(A)') 'TEST PASSED'
   endif

end program
