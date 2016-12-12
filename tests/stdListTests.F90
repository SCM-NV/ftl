#include "stdTestTools.inc"

module stdListTestsModule

   use stdTestToolsModule
   use stdListIntModule

   implicit none
   private
   public :: stdListTests

contains


   subroutine stdListTests

      write (*,'(A)') 'Running stdList tests ...'

      ! Tests of the stdList container itself:

      call testNewDefault
      call testNewCopyOther
      call testNewFromArray

   end subroutine


   subroutine testNewDefault
      type(stdListInt) :: l

      call l%New()

      ASSERT(l%Empty())
      ASSERT(l%Size() == 0)
      ASSERT(Size(l) == 0)
      ASSERT(l%Begin() == l%End())
      ASSERT(.not.(l%Begin() /= l%End()))

   end subroutine


   subroutine testNewCopyOther
      type(stdListInt) :: l,o

      call o%New([5,13,41,97,17,10,88])
      call l%New(o)

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)
      ASSERT(.not.associated(o%front,l%front))
      ASSERT(.not.associated(o%back,l%back))

   end subroutine


   subroutine testNewFromArray
      type(stdListInt) :: l

      call l%New([5,13,41,97,17,10,88])

      ASSERT(.not.l%Empty())
      ASSERT(l%Size() == 7)
      ASSERT(Size(l) == 7)
      ASSERT(l%front == 5)
      ASSERT(l%back == 88)

   end subroutine


end module
