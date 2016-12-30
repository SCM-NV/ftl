module Point2DModule

   implicit none
   private

   type, public :: Point2D
      real :: x, y
   end type

   public :: operator(==)
   interface operator(==)
      module procedure EqualOther
   end interface

contains

   pure logical function EqualOther(self, other)
      type(Point2D), intent(in) :: self, other
      EqualOther = (self%x == other%x) .and. (self%y == other%y)
   end function

end module
