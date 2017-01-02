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



!   ftlHash
!   =======
!
! This module implements hashing functions for common Fortran intrisic types, e.g. integers, and character strings.
! The hashing functions are used in ftlUnorderedMap. Derived types will have to supply their own ftlHash
! functions, though these can be implemented using the hashes of the intrinsic types defined in this module.


module ftlHashModule

   implicit none
   private

   public :: ftlHash
   interface ftlHash
      module procedure ftlHashInteger
      module procedure ftlHashReal
      module procedure ftlHashCharacterString
      module procedure ftlHashLogical
   end interface

contains


   ! Note: These hash functions are probably pretty bad. I didn't really test any of this ...


   pure integer function ftlHashInteger(i) result(hash)
      integer, intent(in) :: i

      character(len=32) :: str

      write (str,*) i
      hash = ftlHashCharacterString(trim(str))

   end function


   integer function ftlHashReal(r) result(hash)
      real, intent(in) :: r

      ! TODO: handle +inf, -inf and NaN. didn't test this ...

      real :: rPosZero
      character(len=32) :: str

      rPosZero = r
      if (rPosZero == 0.0) rPosZero = 0.0 ! this turns a potential -0.0 into +0.0 (they also compare equal with ==)

      write (str,*) rPosZero
      hash = ftlHashCharacterString(trim(str))

   end function


   pure integer function ftlHashCharacterString(str) result(hash)
      character(len=*), intent(in) :: str

      integer :: i

      hash = 1299709
      do i = 1, len(str)
         hash = 101 * hash + ichar(str(i:i))
      enddo
      hash = abs(hash)

   end function


   pure integer function ftlHashLogical(l) result(hash)
      logical, intent(in) :: l

      if (l) then
         hash = 1
      else
         hash = 0
      endif

   end function


end module
