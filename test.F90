program stdVectorTest

   use stdVectorIntModule
   use stdVectorRealModule

   implicit none

   type(stdVectorInt) :: a
   type(stdVectorIntIterator) :: i

   call a%New()
   a = [5,5,76,3]
   call a%Insert(2, [1,1,1])
   call a%Print()

   i = a%Begin() + 4
   write (*,*) i%value


end program
