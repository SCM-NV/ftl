#include "ftlTestTools.inc"

program tests

   use ftlTestToolsModule
   use ftlVectorTestsModule
   use ftlListTestsModule
   use ftlSortTestsModule

   ! dummy assertion to test that assertions themselves work ...
   ASSERT(.false.)

   call ftlVectorTests
   call ftlListTests
   call ftlSortTests

   write (*,'(A,I0,A,I0)') 'Failed assertions: ',num_failed,'/',num_asserts
   if (num_failed > 0) then
      write (*,'(A)') 'TEST FAILED'
   else
      write (*,'(A)') 'TEST PASSED'
   endif

end program
