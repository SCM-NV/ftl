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



module ftlExceptionModule

   use, intrinsic :: iso_fortran_env

   implicit none
   private

   ! exception base class from which all other exceptions must inherit
   type, public :: ftlException
      character(len=:), allocatable :: message
   end type

   ! the default handler for uncaught exceptions
   public :: ftlUncaughtExceptionDefaultHandler

   ! interface for any custom uncaught exception handlers
   interface
      subroutine ftlUncaughtExceptionHandlerInterface(exc)
         import
         implicit none
         class(ftlException), intent(in) :: exc
      end subroutine
   end interface

   ! the currently active exception handler
   procedure(ftlUncaughtExceptionHandlerInterface), pointer, public, save :: ftlUncaughtExceptionHandler => ftlUncaughtExceptionDefaultHandler

   ! global temporary that we will use to transfer the exception from the (throwing) callee to the caller
   class(ftlException), allocatable, public, save :: FTL_tmpexc_global

   ! counter for how many try blocks have been nested across the call stack
   integer, public, protected, save :: FTL_nestedTryBlocks = 0

   ! internally used class whose finalizer is used to trigger the clean-up when leaving a try block
   type, public :: ftlTryBlockGuard
      private
      logical :: guarding = .false.
   contains
      procedure, public :: Acquire
      final             :: Release
   end type

contains


   subroutine Acquire(self)
      class(ftlTryBlockGuard), intent(out) :: self
      FTL_nestedTryBlocks = FTL_nestedTryBlocks + 1
      self%guarding = .true.
   end subroutine

   subroutine Release(self)
      type(ftlTryBlockGuard), intent(inout) :: self
      if (self%guarding) then
         FTL_nestedTryBlocks = FTL_nestedTryBlocks - 1
         self%guarding = .false.
         if (FTL_nestedTryBlocks == 0 .and. allocated(FTL_tmpexc_global)) then
            ! We have exited the last try block, but still have an exception pending in the global buffer?
            ! It's unhandled ... let's go into the handler now.
            block
               class(ftlException), allocatable :: exc
#if defined(__INTEL_COMPILER)
               exc = FTL_tmpexc_global
               deallocate(FTL_tmpexc_global)
#else
               call move_alloc(FTL_tmpexc_global, exc)
#endif
               call ftlUncaughtExceptionHandler(exc)
            end block
         endif
      endif
   end subroutine


   subroutine ftlUncaughtExceptionDefaultHandler(exc)
#if defined(__INTEL_COMPILER)
      use ifcore, only: tracebackqq
#endif
      class(ftlException), intent(in) :: exc

      write (error_unit, "(A)") "Uncaught ftlException!"

      ! TODO: print type of exception for standard types at least?

      if (allocated(exc%message)) then
         write (error_unit, "(A,A)") "Exception message: ", exc%message
      endif

#if defined(__INTEL_COMPILER)
      call tracebackqq("Exception origin:",-1)
      ERROR STOP "ERROR STOP after uncaught ftlException!"
#elif defined(__GFORTRAN__)
      write (error_unit, "(A)") "Exception origin:"
      call backtrace
      ERROR STOP "after uncaught ftlException!"
#endif

   end subroutine


end module
