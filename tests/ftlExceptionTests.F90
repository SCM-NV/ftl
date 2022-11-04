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


   subroutine ftlExceptionTests

      write (*,'(A)') 'Running ftlException tests ...'

      ! install our own handler for uncaught exceptions, so they don't stop our unit tests
      ftlUncaughtExceptionHandler => DebugUncaughtExceptionHandler

      call testConstructor

      ! tests with throwing subroutines
      call testNoThrowSub
      call testThrowSub
      call testTryAndCatchSub
      call testTryAndNoCatchSub
      call testIgnoreSub

      ! tests with throwing functions
      call testNoThrowFunc
      ! ...

   end subroutine


   subroutine testConstructor

      type(ftlException) :: base_exc
      type(MathDomainError) :: my_exc
      class(ftlException), allocatable :: exc

      base_exc = ftlException('some basic error')
      ASSERT(base_exc%message == 'some basic error')

      my_exc = MathDomainError('my custom error')
      ASSERT(my_exc%message == 'my custom error')

      exc = MathDomainError('some derived error')
      ASSERT(exc%message == 'some derived error')

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

   end subroutine


   subroutine testTryAndCatchSub
      real :: s

      FTL_TRY

         call SquareRootSubroutine(-4.0, s) FTL_MAYTHROW ! will throw MathDomainError
         ASSERT(.false.) ! should not get here

      FTL_EXCEPT(exc)

         class is (MathDomainError)
            ASSERT(exc%message == 'square root argument must be >= 0')
            ASSERT(ieee_class(s) == ieee_quiet_nan) ! just because the subroutine was nice when it threw

      FTL_END_EXCEPT

   end subroutine


   subroutine testTryAndNoCatchSub
      real :: s

      uncaughtExceptionComingUp = .true. ! let the uncaught exception handler know that one is coming up ...

      FTL_TRY

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

   end subroutine


   subroutine testIgnoreSub
      real :: s
      logical :: caughtAndIgnored = .false.

      FTL_TRY

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

   end subroutine


   subroutine testNoThrowFunc
      real :: s

      s = SquareRootFunction(4.0)
      ASSERT(s == 2.0)

   end subroutine


   ! Define a subroutine and function that can throw an exception ...

   subroutine SquareRootSubroutine(r, s)
      real, intent(in)  :: r
      real, intent(out) :: s

      if (r >= 0) then
         s = sqrt(r)
      else
         s = ieee_value(s, ieee_quiet_nan) ! not necessary, but will silence compiler warnings
         FTL_THROW(MathDomainError('square root argument must be >= 0'))
         ASSERT(.false.) ! should not get here
      endif

   end subroutine


   real function SquareRootFunction(r) result(s)
      real, intent(in) :: r

      if (r >= 0) then
         s = sqrt(r)
      else
         s = ieee_value(s, ieee_quiet_nan) ! not necessary, but will silence compiler warnings
         FTL_THROW(MathDomainError('square root argument must be >= 0'))
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


end module
