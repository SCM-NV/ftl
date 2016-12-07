program stdVectorTest

   use stdVectorIntModule
   use stdVectorRealModule

   implicit none

   type(stdVectorInt) :: a,b
   type(stdVectorIntIterator) :: i,j
   integer :: blub

   call a%New()
   a = [5,5,76,3]
   i = a%Begin() + 2
   call a%Insert(2, [100,101,102])
   call a%PushBack(4)
   call a%PushBack(8)
   call a%PushBack(-4)
   call a%PushBack(4)
   call a%PushBack(-6)
   call a%PushBack(7)
   call a%PushBack(1)
   call a%PushBack(1)
   call a%PushBack(6)
   call a%PushBack(-1)
   blub = a%PopBack()
   call a%Print()
   write (*,*) 'sizes = ', a%size(), size(a%data), size(a)

   call i%Print()
   i = a%Begin() + 2
   j = i
   call j%Print()

   b = a

end program
