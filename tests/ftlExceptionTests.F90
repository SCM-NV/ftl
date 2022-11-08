! Copyright (c) 2022  Software for Chemistry & Materials BV
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License along
! with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.


#include "ftlTestTools.inc"
#include "ftlException.inc"

module ftlExceptionTestsModule

   use ftlExceptionModule
   use ftlTestToolsModule

   use, intrinsic :: ieee_arithmetic

   implicit none
   private
   public :: ftlExceptionTests

   type, extends(ftlException) :: MathDomainError; end type
   type, extends(ftlException) :: PermissionError; end type

   logical :: uncaughtExceptionComingUp = .false.

contains


   ! Define subroutines and functions that can throw exceptions ...


   subroutine SquareRootSubroutine(r, s)
      real, intent(in)  :: r
      real, intent(out) :: s

      if (r >= 0) then
         s = sqrt(r)
      else
         s = ieee_value(s, ieee_quiet_nan) ! not necessary, but will silence compiler warnings
         FTL_THROW(MathDomainError, 'square root argument must be >= 0')
         ASSERT(.false.) ! should not get here
      endif

   end subroutine


   subroutine RunCommandSubroutine(cmd)
      character(*), intent(in) :: cmd

      if (cmd == "rm -rf /") then
         FTL_THROW(PermissionError, "not allowed to delete root directory")
      else
         FTL_TRY
            ASSERT(.not.allocated(FTL_tmpexc_global))
            ! attempt to delete something
         FTL_EXCEPT(exc)
            ! some error handling here
         FTL_END_EXCEPT
      endif

   end subroutine


   real function SquareRootFunction(r) result(s)
      real, intent(in) :: r

      if (r >= 0) then
         s = sqrt(r)
      else
         s = ieee_value(s, ieee_quiet_nan) ! not necessary, but will silence compiler warnings
         FTL_THROW(MathDomainError, 'square root argument must be >= 0')
         ASSERT(.false.) ! should not get here
      endif

   end function


   ! Debug handler for uncaught exceptions:
   ! It just checks that we were expecting an uncaught exception!
   ! After it has run, we are no longer expecting an uncaught exception

   subroutine DebugUncaughtExceptionHandler(exc)
      class(ftlException), intent(in) :: exc

      ASSERT(uncaughtExceptionComingUp)
      if (.not.uncaughtExceptionComingUp) then
         write (*, "(A,A)") "Uncaught exception when not expecting one: ", exc%message
      endif
      uncaughtExceptionComingUp = .false.

   end subroutine


   ! Actual tests below ...


   subroutine ftlExceptionTests

      write (*,'(A)') 'Running ftlException tests ...'

      ! install our own handler for uncaught exceptions, so they don't stop our unit tests
      ftlUncaughtExceptionHandler => DebugUncaughtExceptionHandler

      call testConstructor
      call testIfortBug

      ! tests with throwing subroutines
      call testNoThrowSub
      call testThrowSub
      call testTryAndCatchSub
      call testTryAndNoCatchSub
      call testIgnoreSub
      call testReturnInTopLevelTry
      call testABCD
      call testABCD_no_throwup
      call testABCD_no_throwup_tryInD

      ! tests with throwing functions
      call testNoThrowFunc
      ! ...

   end subroutine


   subroutine testConstructor

      type(ftlException) :: base_exc
      type(MathDomainError) :: my_exc
      class(ftlException), allocatable :: exc

      base_exc = ftlException('some basic error', __FILE__, __LINE__)
      ASSERT(base_exc%message == 'some basic error')

      my_exc = MathDomainError('my custom error', __FILE__, __LINE__)
      ASSERT(my_exc%message == 'my custom error')

      exc = MathDomainError('some derived error', __FILE__, __LINE__)
      ASSERT(exc%message == 'some derived error')

   end subroutine


   subroutine testIfortBug
      class(ftlException), allocatable :: exc1, exc2

      ASSERT(.not.allocated(exc1))

      exc1 = MathDomainError("first error", __FILE__, __LINE__)
      exc2 = ftlException("some generic error", __FILE__, __LINE__)

      ASSERT(allocated(exc1))
      ASSERT(exc1%message == 'first error')
      ASSERT(allocated(exc2))
      ASSERT(exc2%message == 'some generic error')

#if defined(__INTEL_COMPILER)
      exc2 = exc1; deallocate(exc1)
#else
      call move_alloc(exc1, exc2)
#endif

      ASSERT(.not.allocated(exc1))
      ASSERT(allocated(exc2))
      ASSERT(exc2%message == 'first error')

      exc1 = PermissionError("second error", __FILE__, __LINE__)
      !    ^
      !    |
      ! ifort would throw an error here if we had used move_alloc earlier:
      !
      !     forrtl: severe (122): invalid attempt to assign into a pointer that is not associated
      !
      ! Seems like a compiler bug to me ...?

      ASSERT(allocated(exc1))
      ASSERT(exc1%message == 'second error')

   end subroutine


   subroutine testNoThrowSub
      real :: s

      call SquareRootSubroutine(4.0, s)
      ASSERT(s == 2.0)

   end subroutine


   subroutine testThrowSub
      real :: s

      uncaughtExceptionComingUp = .true. ! let the uncaught exception handler know that one is coming up ...

      call SquareRootSubroutine(-4.0, s) ! will throw MathDomainError

      ASSERT(.not.uncaughtExceptionComingUp) ! make sure the debug handler caught it

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testTryAndCatchSub
      real :: s

      FTL_TRY

         ! testing of internal machinery:
         ASSERT(FTL_nestedTryBlocks == 1)

         call SquareRootSubroutine(-4.0, s) FTL_MAYTHROW ! will throw MathDomainError
         ASSERT(.false.) ! should not get here

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ASSERT(exc%message == 'square root argument must be >= 0')
            ASSERT(exc%line == 52)
            ASSERT(scan(exc%file, 'ftlExceptionTests.F90') /= 0)
            ASSERT(ieee_class(s) == ieee_quiet_nan) ! just because the subroutine was nice when it threw

            ! testing of internal machinery:
            ASSERT(FTL_nestedTryBlocks == 0)

      FTL_END_EXCEPT

   end subroutine


   subroutine testTryAndNoCatchSub
      real :: s

      uncaughtExceptionComingUp = .true. ! let the uncaught exception handler know that one is coming up ...

      FTL_TRY

         ! testing of internal machinery:
         ASSERT(FTL_nestedTryBlocks == 1)

         call SquareRootSubroutine(-4.0, s) FTL_MAYTHROW ! will throw MathDomainError
         ASSERT(.false.) ! should not get here

      FTL_EXCEPT(exc)

         class is (PermissionError) ! not the right kind of exception to catch ...
            ASSERT(.false.) ! should not get here

         ! Will go into uncaught exception handler instead! The default handler prints
         ! some info and then call ERROR STOP. Luckily we installed a custom handler that
         ! does not do this, otherwise our unit tests would be shut down at this point ...

      FTL_END_EXCEPT

      ASSERT(.not.uncaughtExceptionComingUp) ! make sure the debug handler caught it

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testIgnoreSub
      real :: s
      logical :: caughtAndIgnored = .false.

      FTL_TRY

         ! testing of internal machinery:
         ASSERT(FTL_nestedTryBlocks == 1)

         call SquareRootSubroutine(-4.0, s) FTL_MAYTHROW ! will throw MathDomainError
         ASSERT(.false.) ! should not get here

      FTL_EXCEPT(exc)

         ! Catching ALL exceptions is easily done by catching the ftlException base class, which always matches.
         ! In this test we just ignore the failure, but of course you could do something else ...

         class is (ftlException)
            caughtAndIgnored = .true. ! just for proving that we were here
            continue

         ! NOTE:
         !
         ! class default
         !    continue
         !
         !   ^--- does NOT compile, as this is used in FTL_END_EXCEPT to go into the uncaught exception handler

      FTL_END_EXCEPT

      ASSERT(caughtAndIgnored)

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testReturnInTopLevelTry
      real :: s

      uncaughtExceptionComingUp = .true. ! let the uncaught exception handler know that one is coming up ...

      FTL_TRY

         call SquareRootSubroutine(-4.0, s) ! will throw MathDomainError
         return
         ! We forgot the FTL_MAYTHROW on the last subroutine call and then maliciously returned from the try block in
         ! an attempt to silently ignore the thrown MathDomainError. So the user is trying extra hard to break things
         ! here. There is no way we can get him into the proper exception handling code below now, but we can still
         ! manage to get at least into the UncaughtExceptionHandler ...

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ! Unfortunately, there is no way to get the user here ...
            ASSERT(.false.)

      FTL_END_EXCEPT

      ! UncaughtExceptionHandler at least got the exception that the user tried to hide from us!
      ASSERT(.not.uncaughtExceptionComingUp)

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testABCD

      ! A = this subroutine has the exception handling
      ! B = testABCD_B propagates the exception up the call stack
      ! C = called from B: SquareRootSubroutine can throw a MathDomainError
      ! D = called from B: RunCommandSubroutine can throw a PermissionError

      logical :: mathErrorCaught = .false.

      FTL_TRY

         call testABCD_B() FTL_MAYTHROW

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ASSERT(exc%message == 'square root argument must be >= 0')
            mathErrorCaught = .true.

         class is (ftlException)
            ASSERT(.false.)

      FTL_END_EXCEPT

      ASSERT(mathErrorCaught)

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testABCD_B
      real :: s

      call SquareRootSubroutine(-1.0, s) FTL_THROWUP ! will throw MathDomainError
      call RunCommandSubroutine("rm -rf /") FTL_THROWUP ! will throw PermissionError

   end subroutine


   subroutine testABCD_no_throwup

      logical :: mathErrorCaught = .false.

      FTL_TRY

         call testABCD_B_no_throwup() FTL_MAYTHROW

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ASSERT(exc%message == 'square root argument must be >= 0')
            mathErrorCaught = .true.

         class is (ftlException)
            ASSERT(.false.)

      FTL_END_EXCEPT

      ASSERT(mathErrorCaught)

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testABCD_B_no_throwup
      real :: s

      call SquareRootSubroutine(-1.0, s) ! will throw MathDomainError, but FTL_THROWUP is missing here!!!
      call RunCommandSubroutine("rm -rf /") ! will throw PermissionError, but FTL_THROWUP is missing here!!!

   end subroutine


   subroutine testABCD_no_throwup_tryInD

      logical :: mathErrorCaught = .false.

      uncaughtExceptionComingUp = .true. ! let the uncaught exception handler know that one is coming up ...

      FTL_TRY

         call testABCD_B_no_throwup_tryInD() FTL_MAYTHROW

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ASSERT(exc%message == 'square root argument must be >= 0')
            mathErrorCaught = .true.

         class is (ftlException)
            ASSERT(.false.)

      FTL_END_EXCEPT

      ASSERT(.not.mathErrorCaught)
      ASSERT(.not.uncaughtExceptionComingUp)

      ! testing of internal machinery:
      ASSERT(.not.allocated(FTL_tmpexc_global))
      ASSERT(FTL_nestedTryBlocks == 0)

   end subroutine


   subroutine testABCD_B_no_throwup_tryInD
      real :: s

      call SquareRootSubroutine(-1.0, s) ! will throw MathDomainError, but FTL_THROWUP is missing here
      call RunCommandSubroutine("rm somefile") ! will not throw, but go into a try block simulating the deletion of the file

   end subroutine


   subroutine testNoThrowFunc
      real :: s

      s = SquareRootFunction(4.0)
      ASSERT(s == 2.0)

   end subroutine


end module
