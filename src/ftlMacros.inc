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


#ifndef FTL_MACROS_INCLUDED
#define FTL_MACROS_INCLUDED

#if defined(__GFORTRAN__) || defined(NAGFOR)
#define PASTE(a) a
#define CAT(a,b) PASTE(a)b
#define CAT3(a,b,c) CAT(a,b)c
#define CAT4(a,b,c,d) CAT3(a,b,c)d
#else
#define PCAT(a,b,c,d) a ## b ## c ## d
#define CAT(a,b) PCAT(a,b,,)
#define CAT3(a,b,c) PCAT(a,b,c,)
#define CAT4(a,b,c,d) PCAT(a,b,c,d)
#endif

#endif
