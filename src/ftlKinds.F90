module ftlKindsModule

   use, intrinsic :: iso_fortran_env

   implicit none
   private

#ifdef FTL_REAL_KIND
   integer, parameter, public :: FTL_KREAL = FTL_REAL_KIND
#else
   integer, parameter, public :: FTL_KREAL = real64
#endif

end module
