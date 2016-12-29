subroutine sortVectorInt(n)

   use ftlTestToolsModule
   use ftlVectorIntModule
   use ftlVectorIntAlgorithmsModule

   integer, intent(in) :: n

   type(ftlVectorInt) :: v
   real :: start, finish

   call v%New(n)
   call ftlGenerate(v%Begin(), v%End(), RandomInt)

   call cpu_time(start)
   call ftlSort(v)
   call cpu_time(finish)

   write (*,'(A,I10,A,f7.3,A)') 'Sorted ftlVectorInt: ',n,' elements in ',(finish-start),' s'

end subroutine


program main

   call sortVectorInt(10000)
   call sortVectorInt(100000)
   call sortVectorInt(1000000)
   call sortVectorInt(10000000)
   call sortVectorInt(100000000)

end program
