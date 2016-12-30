module BigTypeModule

   implicit none
   private

   type, public :: BigType
      integer              :: someInts(1000)
      real                 :: someReals(100:100)
      character(len=10000) :: someChars
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
