program stdVectorTest

   use stdVectorIntModule
   use stdVectorRealModule

   implicit none

   type(stdVectorInt) :: a
   type(stdVectorReal) :: v

   call a%New()
   call a%New(3)
   call a%New([5,3])
   call a%Resize(5,16)
   call a%Insert(a%size+1, [5,7,98])
   call a%Erase(2,5)
   call a%Print()

   call v%New()
   call v%New(3)
   call v%New([5.0,3.0])
   call v%Resize(5,16.0)
   call v%Insert(a%size+1, [5.0,7.0,98.0])
   call v%Erase(2,5)
   call v%Print()

end program
