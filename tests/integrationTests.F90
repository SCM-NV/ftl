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


#include "ftlTestTools.inc"

module integrationTestsModule

   use ftlTestToolsModule
   use ftlStringModule
   use ftlDynArrayStringModule

   implicit none
   private
   public :: integrationTests

contains


   subroutine integrationTests

      write (*,'(A)') 'Running integration tests ...'

      call integrationTest1

   end subroutine


   subroutine integrationTest1
      integer :: unit, iostat, lineIdx, wordIdx
      type(ftlString) :: contents
      type(ftlDynArrayString) :: contentsLine
      type(ftlString), allocatable :: words(:)
      type(ftlString) :: newline, space

      newline = FTL_STRING_NEWLINE
      space = ' '

      open (unit=unit, file='tests/assets/intTest1.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) call contents%ReadUntilEOF(unit)
      close(unit)

      contentsLine = contents%SplitLines()
      lineIdx = 1
      do while (lineIdx <= size(contentsLine))
         if (contentsLine%data(lineIdx)%StartsWith('### REMOVE')) then
            call contentsLine%Erase(lineIdx)
            words = contentsLine%data(lineIdx)%Split(' ')
            do wordIdx = 1, size(words)
               if (words(wordIdx)%IsInt()) then
                  words(wordIdx) = ftlString(int(words(wordIdx)) + 1)
                  exit
               endif
            enddo
            contentsLine%data(lineIdx) = space%Join(words)
         endif
         lineIdx = lineIdx + 1
      end do

      contents = newline%Join(contentsLine%data)
      write (*,*) contents

   end subroutine


end module
