#include "ftlTestTools.inc"

module ftlAlgorithmsPerformanceTestsModule

   use ftlTestToolsModule
   use ftlVectorIntModule
   use ftlVectorIntAlgorithmsModule

   implicit none
   private
   public :: ftlAlgorithmsPerformanceTests

contains


   subroutine ftlAlgorithmsPerformanceTests

      write (*,'(A)') 'Running ftlAlgorithms performance tests ...'

      call testSortVectorInt(10000)
      call testSortVectorInt(100000)
      call testSortVectorInt(1000000)
      call testSortVectorInt(10000000)
      call testSortVectorInt(100000000)

   end subroutine


   subroutine testSortVectorInt(n)
      integer, intent(in) :: n

      type(ftlVectorInt) :: v
      integer :: i
      real :: start, finish

      call v%New([ (RandomInt(), i=1,n) ])

      call cpu_time(start)
      call ftlSort(v)
      call cpu_time(finish)
      ASSERT(ftlIsSorted(v))

      write (*,'(A,I10,A,f7.3,A)') 'Sorted ftlVectorInt: ',n,' elements in ',(finish-start),' s'

   end subroutine


end module
