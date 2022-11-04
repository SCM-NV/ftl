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


#ifndef FTL_EXCEPTION_INCLUDED
#define FTL_EXCEPTION_INCLUDED

#define FTL_THROW(e)\
   if (FTL_inTryBlock) then;\
      FTL_tmpexc_global = e;\
   else;\
      call ftlUncaughtExceptionHandler(e);\
   endif;\
   return;\

#define FTL_TRY\
   block;\
      class(ftlException), allocatable :: FTL_exc_local;\
      if (allocated(FTL_tmpexc_global)) deallocate(FTL_tmpexc_global);\
      FTL_inTryBlock = .true.;\
      do;\

#define FTL_MAYTHROW\
         ; call move_alloc(FTL_tmpexc_global, FTL_exc_local);\
         if (allocated(FTL_exc_local)) exit

#define FTL_EXCEPT(exc)\
         exit;\
      end do;\
      FTL_inTryBlock = .false.;\
      associate(exc => FTL_exc_local);\
      if (allocated(FTL_exc_local)) then;\
         select type (FTL_exc_local)

#define FTL_END_EXCEPT\
         class default;\
            call ftlUncaughtExceptionHandler(FTL_exc_local);\
         end select;\
      end if;\
      end associate;\
   end block

#endif
