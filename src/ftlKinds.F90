module ftlKindsModule

   use, intrinsic :: iso_fortran_env

   implicit none
   public

#ifdef FTL_REAL_KIND
   integer, parameter :: FTL_KREAL = FTL_REAL_KIND
#else
   integer, parameter :: FTL_KREAL = real64
#endif

end module
