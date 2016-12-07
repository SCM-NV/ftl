module SingleOrbitalTransitionModule

   implicit none

   private

   public SingleOrbitalTransitionType

   integer, parameter :: UNDEFINED_ORBITAL = -HUGE(0)
   real   , parameter :: CRAP = -HUGE(0.0)

   type SingleOrbitalTransitionType
      integer :: orgOrb = UNDEFINED_ORBITAL
      integer :: newOrb = UNDEFINED_ORBITAL
      real    :: KSdE   = CRAP
      real    :: TDM(3) = CRAP
   contains
      procedure, public :: OS => OscillatorStrengthPrivate
   endtype


contains


   real function OscillatorStrengthPrivate(self) result(OS)
      class(SingleOrbitalTransitionType), intent(in) :: self

      OS = 2.0/3.0 * self%KSdE * sqrt(sum(self%TDM **2))

   endfunction


endmodule
