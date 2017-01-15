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


#define FTL_TEMPLATE_TYPE character
#define FTL_TEMPLATE_TYPE_NAME
#define FTL_TEMPLATE_TYPE_PROVIDES_COMPARATOR_SMALLER
#define FTL_SKIP_IMPLEMENTATION
#include "ftlString.F90"
#undef FTL_SKIP_IMPLEMENTATION
#define FTL_INSTANTIATE_TEMPLATE
#include "ftlAlgorithms.F90_template"
