module BigTypeModule

   implicit none
   private

   type, public :: BigType
      integer              :: someInts(1000)
      real                 :: someReals(100:100)
      character(len=10000) :: someChars
   end type

end module
