! Copyright (c) 2018  Robert Rüger
! Copyright (c) 2018  Software for Chemistry & Materials BV
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
