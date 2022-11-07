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

#define FTL_THROW(e)\
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

#if defined(__INTEL_COMPILER)
#define FTL_MAYTHROW\
            ; if (allocated(FTL_tmpexc_global)) then;\
               FTL_exc_local = FTL_tmpexc_global;\
               deallocate(FTL_tmpexc_global);\
               exit FTL_TRY_BLOCK;\
            endif
#else
#define FTL_MAYTHROW\
            ; if (allocated(FTL_tmpexc_global)) then;\
               call move_alloc(FTL_tmpexc_global, FTL_exc_local);\
               exit FTL_TRY_BLOCK;\
            endif
#endif

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
            FTL_THROW(FTL_exc_local);\
         end select;\
      end if;\
      end associate;\
   end block

#endif
