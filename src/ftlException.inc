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


#ifndef FTL_EXCEPTION_INCLUDED
#define FTL_EXCEPTION_INCLUDED

! Workaround for what seems like an ifort bug.
! See subroutine testIfortBug in ftlExceptionsTest.F90 ...
#if defined(__INTEL_COMPILER)
#define FTL_MOVE_EXCEPTION(from, to)\
   to = from; deallocate(from)
#else
#define FTL_MOVE_EXCEPTION(from, to)\
   call move_alloc(from, to)
#endif

#define FTL_THROW(cls, msg)\
   if (FTL_nestedTryBlocks > 0) then;\
      if (.not.allocated(FTL_tmpexc_global)) FTL_tmpexc_global = cls(msg, __FILE__, __LINE__);\
   else;\
      call ftlUncaughtExceptionHandler(cls(msg, __FILE__, __LINE__));\
   endif;\
   return

#define FTL_RETHROW(e)\
   if (FTL_nestedTryBlocks > 0) then;\
      if (.not.allocated(FTL_tmpexc_global)) FTL_tmpexc_global = e;\
   else;\
      call ftlUncaughtExceptionHandler(e);\
   endif;\
   return

#define FTL_TRY\
   block;\
      class(ftlException), allocatable :: FTL_exc_local;\
      if (allocated(FTL_tmpexc_global)) then;\
         call ftlUncaughtExceptionHandler(FTL_tmpexc_global);\
         deallocate(FTL_tmpexc_global);\
      endif;\
      FTL_TRY_BLOCK: block;\
         type(ftlTryBlockGuard) :: FTL_tbguard;\
         call FTL_tbguard%Acquire

#define FTL_MAYTHROW\
            ; if (allocated(FTL_tmpexc_global)) then;\
               FTL_MOVE_EXCEPTION(FTL_tmpexc_global, FTL_exc_local);\
               exit FTL_TRY_BLOCK;\
            endif

#define FTL_THROWUP\
   ; if (allocated(FTL_tmpexc_global)) return

#define FTL_EXCEPT(exc)\
         continue FTL_MAYTHROW;\
      end block FTL_TRY_BLOCK;\
      associate(exc => FTL_exc_local);\
      if (allocated(FTL_exc_local)) then;\
         select type (FTL_exc_local)

#define FTL_END_EXCEPT\
         class default;\
            FTL_RETHROW(FTL_exc_local);\
         end select;\
      end if;\
      end associate;\
   end block

#endif
