#include "stdTestTools.inc"

program tests

   use stdTestToolsModule
   use stdVectorTestsModule

   ! dummy assertion to test that assertions themselves work ...
   ASSERT(.false.)

   call stdVectorTests

   write (*,'(A,I0,A,I0)') 'Failed assertions: ',num_failed,'/',num_asserts
   if (num_failed > 0) then
      write (*,'(A)') 'TEST FAILED'
   else
      write (*,'(A)') 'TEST PASSED'
   endif

end program
