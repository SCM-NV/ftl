module BigTypeModule

   implicit none
   private

   type, public :: BigType
      integer             :: someInts(100)
      real                :: someReals(50:50)
      character(len=1000) :: someChars
   end type

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther
   end interface

contains

   pure logical function EqualOther(self, other) result(equal)
      type(BigType), intent(in) :: self, other

      equal = all(self%someInts == other%someInts)   .and. \
              all(self%someReals == other%someReals) .and. \
              self%someChars == other%someChars

   end function

end module


module BigTypeHelpers

   implicit none

contains

   logical function FirstIntSmaller(a, b)
      use BigTypeModule
      type(BigType), intent(in) :: a, b
      FirstIntSmaller = (a%someInts(1) < b%someInts(1))
   end function


   subroutine RandomizeInts(a)
      use BigTypeModule
      use ftlTestToolsModule
      type(BigType), intent(inout) :: a
      integer :: i
      do i = 1, 100
         a%someInts(i) = RandomInt()
      enddo
   end subroutine

end module
