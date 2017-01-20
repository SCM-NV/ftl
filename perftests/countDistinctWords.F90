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


subroutine countDistictWords(filename)

   use ftlStringModule
   use ftlHashMapFtlStrIntModule

   implicit none

   character(len=*), intent(in) :: filename

   integer :: iostat, i
   type(ftlString) :: contents
   type(ftlString) :: charsToRemove
   type(ftlString), allocatable :: words(:)
   type(ftlHashMapFtlStrInt) :: wordOcc
   real :: start, finish

   call cpu_time(start)

   ! Step 1: Read the entire book into an ftlString.
   open (unit=19, file=filename, status='old', action='read', iostat=iostat)
   if (iostat == 0) then
      call contents%ReadUntilEOF(19)
   else
      stop 'Unable to open file.'
   endif
   close(19)

   ! Step 2: Replace all that doesn't belong into a proper word with spaces and lowercase everything.
   charsToRemove = FTL_STRING_PUNCTUATION // FTL_STRING_WHITESPACE(2:) ! <- 1 is space itself, no need to replace it ...
   do i = 1, len(charsToRemove); associate(c => charsToRemove%At(i))
      contents = contents%Replace(c, ' ')
   end associate; enddo
   contents = contents%Lower()

   ! Step 3: Split the ftlString up into an array of words.
   words = contents%Split()
   write (*,*) 'Number of words: ', size(words)

   ! Step 4: Count the number of distinct words.
   call wordOcc%New(1000) ! <-- guess there are at least 1000 distinct words in the book
   do i = 1, size(words); associate(word => words(i))
      if (word .in. wordOcc) then
         call wordOcc%Set(word, wordOcc%Get(word) + 1)
      else
         call wordOcc%Set(word, 1)
      endif
   end associate; enddo
   write (*,*) 'Number of distinct words: ', size(wordOcc)

   call cpu_time(finish)
   write (*,'(A,f7.3,A)') 'Counted all distinct words in '//filename//' in ',(finish-start),' s'

end subroutine


program main

   call countDistictWords('tests/assets/frankenstein.txt')
   call countDistictWords('tests/assets/mobydick.txt')

end program
