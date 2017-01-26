! Copyright (c) 2017  Robert Rüger
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


!   ftlRegex
!   ========
!
! TODO: description

module ftlRegexModule

   use iso_c_binding

   implicit none
   private

   type, public :: ftlRegex
      private

      character(len=:,kind=C_char), allocatable :: pattern

      type(C_ptr)                     :: preg = C_NULL_ptr
      character(kind=C_char), pointer :: regdata(:) => null()

   contains
      private

      procedure         :: NewRaw
      generic  , public :: New => NewRaw

      procedure, public :: Delete
#ifndef FTL_NO_FINALIZERS
      final             :: Finalizer
#endif

      procedure         :: PrintError

   end type


   ! Interfaces for the functions in POSIX regex.h

   interface

      function C_regcomp(preg, pattern, flags) result(status) bind(C,name="regcomp")
         import
         type(C_ptr)           , intent(in), value :: preg
         character(kind=C_char), intent(in)        :: pattern(*)
         integer(C_int)        , intent(in), value :: flags
         integer(C_int)                            :: status
      end function

      subroutine C_regfree(reg) bind(C,name="regfree")
         import
         type(C_ptr), intent(in), value :: reg
      end subroutine

      function C_regerror(errcode, reg, errbuf, errbuf_size) result(regerror) bind(C,name="regerror")
         import
         integer(C_int)        , intent(in) , value :: errcode
         type(C_ptr)           , intent(in) , value :: reg
         character(kind=C_char), intent(out)        :: errbuf
         integer(C_size_t)     , intent(in) , value :: errbuf_size
         integer(C_size_t)                          :: regerror
      end function

   end interface


contains



   subroutine NewRaw(self, pattern, flags)
      class(ftlRegex) , intent(inout)           :: self
      character(len=*), intent(in)              :: pattern
      integer         , intent(in)   , optional :: flags(:)

      integer(C_int) :: status

      call self%Delete()

      self%pattern = pattern // C_NULL_char

      allocate(self%regdata(64))
      self%preg = c_loc(self%regdata(1))
      status = C_regcomp(self%preg, self%pattern, 1)
      if (status /= 0) then
         call self%PrintError(status)
         stop 'ERROR compiling regex in ftlRegex%New()'
      endif

   end subroutine



   subroutine Delete(self)
      class(ftlRegex), intent(inout) :: self

      if (c_associated(self%preg)) then
         call C_regfree(self%preg)
         self%preg = C_NULL_ptr
         deallocate(self%regdata)
         deallocate(self%pattern)
      endif

   end subroutine

#ifndef FTL_NO_FINALIZERS
   subroutine Finalizer(self)
      type(ftlRegex), intent(inout) :: self
      call self%Delete()
   end subroutine
#endif



   subroutine PrintError(self, status)
      use, intrinsic :: iso_fortran_env, only: error_unit
      class(ftlRegex), intent(in) :: self
      integer(C_int) , intent(in) :: status

      character(len=1024,kind=C_char) :: errbuf
      integer(C_size_t) :: errlen

      errlen = C_regerror(status, self%preg, errbuf, int(len(errbuf), C_size_t))
      write (error_unit,'(2A)') 'ftlRegex ERROR: ', errbuf(:errlen-1)

   end subroutine


end module
