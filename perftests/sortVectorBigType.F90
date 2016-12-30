subroutine sortVectorBigType(n)

   use BigTypeModule
   use BigTypeHelpers
   use ftlVectorBigTypeModule
   use ftlVectorBigTypeAlgorithmsModule

   integer, intent(in) :: n

   type(ftlVectorBigType) :: v
   real :: start, finish

   call v%New(n)
   call ftlForEach(v, RandomizeInts)

   call cpu_time(start)
   call ftlSort(v, FirstIntSmaller, useSelectionSort=.true.)
   call cpu_time(finish)

   write (*,'(A,I10,A,f7.3,A)') 'Sorted ftlVectorBigType: ',n,' elements in ',(finish-start),' s'

end subroutine


program main

   call sortVectorBigType(100)
   call sortVectorBigType(1000)
   call sortVectorBigType(10000)
   call sortVectorBigType(100000)
   call sortVectorBigType(1000000)

end program
