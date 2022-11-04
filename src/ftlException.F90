! Copyright (c) 2016, 2017  Robert Rüger
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
   procedure(ftlUncaughtExceptionHandlerInterface), pointer, public :: ftlUncaughtExceptionHandler => ftlUncaughtExceptionDefaultHandler

   ! global temporary that we will use to transfer the exception from the (throwing) callee to the caller
   class(ftlException), allocatable, public, save :: FTL_tmpexc_global

   ! FIXME: single logical will break in case of nested try blocks.
   !        replace with stack of logicals?
   logical, public, save :: FTL_inTryBlock = .false.

contains


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
