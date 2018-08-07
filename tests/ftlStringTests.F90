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


#include "ftlTestTools.inc"

module ftlStringTestsModule

   use ftlTestToolsModule
   use ftlStringModule
   use ftlDynArrayStringModule

   implicit none
   private
   public :: ftlStringTests

contains


   subroutine ftlStringTests

      write (*,'(A)') 'Running ftlString tests ...'

      call testNewDefault
      call testAssignRaw
      call testAssignOther
      call testAssignRawArrays
      call testAssignOtherArrays
      call testRaw

      call testSliceOnOwnRaw

      call testAllocated

      !call testDerivedTypeIO

      call testIteratorWriting

      call testHash
      call testSwap
      call testMove

      call testStringFromNumeric
      call testStringToNumeric

      ! Overloaded operators:
      call testComparison
      call testConcat
      call testIn

      ! File reading:
      call testReadLine
      call testReadUntilEOF

      call testFortranStandardMethods

      ! Python string methods:
      call testCenter
      call testCount
      call testPartition
      call testSplit
      call testSplitLines
      call testJoin
      call testStartsWith
      call testEndsWith
      call testStrip
      call testRStrip
      call testLStrip
      call testUpperLower
      call testIsSpace
      call testReplace

      ! Other string methods:
      call testCountWords

      ! Tests of ftlString iterators:

      call testNewItDefault
      call testNewItCopyOther

      call testInc
      call testDec

      call testAdvanceReverseDiff
      call testLogicalOperations

      ! Tests of ftl Containers containing ftlStrings:
      call testSplitWordsIntoDynArray
      call testDynArrayStringSpecialization

   end subroutine


   subroutine testNewDefault
      type(ftlString) :: s

      call s%New()

   end subroutine


   subroutine testAssignRaw
      type(ftlString) :: s
      character(len=:), allocatable :: c
      character(len=2) :: flc
      character(len=10) :: lflc

      s = 'test'

      ASSERT(s%raw == 'test')

      c = s

      ASSERT(c == 'test')

      flc = s%raw

      ASSERT(flc == 'te')

      lflc = 'blabla'
      s = lflc

      ASSERT(s == 'blabla    ')

   end subroutine


   subroutine testAssignOther
      type(ftlString) :: s1, s2, uninit

      s1 = s2 ! assignment of uninitialized strings (should not cause a debug build to freak out)

      ASSERT(.not.s1%Allocated())
      ASSERT(.not.s2%Allocated())

      s1 = 'testme'
      s2 = s1

      ASSERT(s1%raw == 'testme')
      ASSERT(s2%raw == 'testme')
      ASSERT(s1 == s2)

      s2 = 'theitcrowd'

      ASSERT(s1%raw == 'testme')
      ASSERT(s2%raw == 'theitcrowd')
      ASSERT(s1 /= s2)

      s1 = uninit

      ASSERT(.not.s1%Allocated())

   end subroutine


   subroutine testAssignRawArrays
      type(ftlString), allocatable :: words(:)
      type(ftlString), allocatable :: tab(:,:)

      allocate(words(2))
      words = ['hello','world']

      ASSERT(words(1) == 'hello')
      ASSERT(words(2) == 'world')

      allocate(tab(2,3))
      tab      = 'none'
      tab(:,1) = ['hello','world']
      tab(:,2) = ['a','b']

      ASSERT(tab(1,1) == 'hello')
      ASSERT(tab(2,1) == 'world')
      ASSERT(tab(1,2) == 'a')
      ASSERT(tab(2,2) == 'b')
      ASSERT(tab(1,3) == 'none')
      ASSERT(tab(2,3) == 'none')

   end subroutine


   subroutine testAssignOtherArrays
      type(ftlString) :: s
      type(ftlString), allocatable :: words1(:), words2(:)
      type(ftlString), allocatable :: tab1(:,:), tab2(:,:)

      s = 'hello fortran world'
      words1 = s%Split()

      words2 = words1

      ASSERT(allocated(words2))
      ASSERT(size(words2) == 3)
      ASSERT(words2(1) == 'hello')
      ASSERT(words2(2) == 'fortran')
      ASSERT(words2(3) == 'world')

      allocate(tab1(3,3))
      tab1      = ftlString('none')
      tab1(:,1) = ftlString('first column')
      tab1(:,2) = words1

      ASSERT(tab1(1,1) == 'first column')
      ASSERT(tab1(2,1) == 'first column')
      ASSERT(tab1(3,1) == 'first column')
      ASSERT(tab1(1,2) == 'hello')
      ASSERT(tab1(2,2) == 'fortran')
      ASSERT(tab1(3,2) == 'world')
      ASSERT(tab1(1,3) == 'none')
      ASSERT(tab1(2,3) == 'none')
      ASSERT(tab1(3,3) == 'none')

      tab2 = tab1

      ASSERT(allocated(tab2))
      ASSERT(size(tab2,1) == 3)
      ASSERT(size(tab2,2) == 3)
      ASSERT(tab2(1,1) == 'first column')
      ASSERT(tab2(2,1) == 'first column')
      ASSERT(tab2(3,1) == 'first column')
      ASSERT(tab2(1,2) == 'hello')
      ASSERT(tab2(2,2) == 'fortran')
      ASSERT(tab2(3,2) == 'world')
      ASSERT(tab2(1,3) == 'none')
      ASSERT(tab2(2,3) == 'none')
      ASSERT(tab2(3,3) == 'none')

   end subroutine


   subroutine testRaw
      type(ftlString) :: s, t

      s = 'this test'

      ASSERT(Raw(s,4) == 'this')
      ASSERT(Raw(s,6) == 'this t')

      t = Raw(s,20)

      ASSERT(size(t) == 20)
      ASSERT(t == 'this test           ')

   end subroutine


   subroutine testSliceOnOwnRaw
      type(ftlString) :: s

      ! Slicing on the own raw it a very, very dangerous thing to do:
      ! s%raw will be deallocated because of the assignment before we read the slice from s%raw.
      ! In other words: ftlString%NewFromRaw is not aware that self%raw and raw alias!
      ! There is some magic to make this work nonetheless ...

      s = 'holladiewaldfee'
      ASSERT(s == 'holladiewaldfee')

      s = s
      ASSERT(s == 'holladiewaldfee')

      s = s%raw(:)
      ASSERT(s == 'holladiewaldfee')

      s = s%raw(2:)
      ASSERT(s == 'olladiewaldfee')
      s = s%raw(2:)
      ASSERT(s == 'lladiewaldfee')
      s = s%raw(2:)
      ASSERT(s == 'ladiewaldfee')
      s = s%raw(2:)
      ASSERT(s == 'adiewaldfee')
      s = s%raw(2:)
      ASSERT(s == 'diewaldfee')
      s = s%raw(2:)
      ASSERT(s == 'iewaldfee')

   end subroutine


   subroutine testAllocated
      type(ftlString) :: s1
      type(ftlString), allocatable :: s2

      ASSERT(.not.s1%Allocated())
      s1 = 'test'
      ASSERT(s1%Allocated())

      ASSERT(.not.allocated(s2))
      allocate(s2)
      ASSERT(allocated(s2))
      ASSERT(.not.s2%Allocated())
      s2 = 'test2'
      ASSERT(allocated(s2))
      ASSERT(s2%Allocated())

   end subroutine



   !subroutine testDerivedTypeIO
   !   type(ftlString) :: s

   !   ! TODO: make a test that doesn't print to stdout and is checked automatically ...

   !   s = 'this is the test string that should always look the same on stdout'

   !   write (*,'(A)') 'Testing ftlString unformatted derived-type IO:'
   !   write (*,*) s%raw
   !   write (*,*) s

   !   write (*,'(A)') 'Testing ftlString formatted derived-type IO:'
   !   write (*,'(A)')  s%raw
   !   write (*,'(DT)') s

   !end subroutine


   subroutine testIteratorWriting
      type(ftlString) :: s
      type(ftlStringIterator) :: it

      s = 'TEfT'
      it = Begin(s)

      ASSERT(it%value == 'T')

      call it%Inc()

      ASSERT(it%value == 'E')

      call it%Inc()

      ASSERT(it%value == 'f')

      it%value = 'S'

      ASSERT(it%value == 'S')
      ASSERT(s == 'TEST')

   end subroutine


   subroutine testHash

      use ftlHashModule

      ASSERT(ftlHash(ftlString('testhash')) == ftlHash('testhash'))
      ASSERT(ftlHash(ftlString('another test')) == ftlHash('another test'))
      ASSERT(ftlHash(ftlString('and another')) == ftlHash('and another'))
      ASSERT(ftlHash(ftlString('number 4')) == ftlHash('number 4'))
      ASSERT(ftlHash(ftlString('the last')) == ftlHash('the last'))

   end subroutine


   subroutine testSwap
      type(ftlString) :: s1, s2, uninit

      s1 = 'one string'
      s2 = 'other string'

      call ftlSwap(s1, s2)

      ASSERT(s1 == 'other string')
      ASSERT(s2 == 'one string')

      call ftlSwap(s1, uninit)

      ASSERT(.not.s1%Allocated())
      ASSERT(uninit == 'other string')

   end subroutine


   subroutine testMove
      type(ftlString) :: s1, s2, uninit
      character(len=:), allocatable :: r

      s1 = 'one string'
      ASSERT(.not.s2%Allocated())

      call ftlMove(s1, s2)

      ASSERT(.not.s1%Allocated())
      ASSERT(s2 == 'one string')

      call ftlMove(uninit, s2)

      ASSERT(.not.s2%Allocated())
      ASSERT(.not.uninit%Allocated())

      r = 'this is a raw allocatable string'
      call ftlMove(r, s1)

      ASSERT(.not.allocated(r))
      ASSERT(s1%Allocated())
      ASSERT(s1 == 'this is a raw allocatable string')

      call ftlMove(s1, r)

      ASSERT(allocated(r))
      ASSERT(.not.s1%Allocated())
      ASSERT(r == 'this is a raw allocatable string')

   end subroutine


   subroutine testStringFromNumeric
      type(ftlString) :: s

      s = ftlString(42)
      ASSERT(s == '42')
      ASSERT(s%IsInt())
      ASSERT(int(s) == 42)

      s = ftlString(42,'(I4)')
      ASSERT(s == '  42')
      ASSERT(s%IsInt())
      ASSERT(int(s) == 42)

      s = ftlString(2.0)
      ! not sure what the default output will look like, but it shouldn't contain trailing or leading whitespace ...
      ASSERT(trim(adjustl(s)) == s)
      ASSERT(s%IsReal())
      ASSERT(real(s) == 2.0)

      s = ftlString(2.0,'(F3.1)')
      ASSERT(s == '2.0')
      ASSERT(s%IsReal())
      ASSERT(real(s) == 2.0)

      s = ftlString((1.0,2.3))
      ! not sure what the default output will look like, but it shouldn't contain trailing or leading whitespace ...
      ASSERT(trim(adjustl(s)) == s)
      ASSERT(s%IsComplex())
      ASSERT(complex(s) == (1.0,2.3))

      s = ftlString(.true.)
      ASSERT(s == 'True')
      s = ftlString(.false.)
      ASSERT(s == 'False')

      s = ftlString(.true.,'(L3)')
      ASSERT(s == '  T')
      s = ftlString(.false.,'(L3)')
      ASSERT(s == '  F')

   end subroutine


   subroutine testStringToNumeric
      type(ftlString) :: s

      s = '25'
      ASSERT(s%IsInt())
      ASSERT(int(s) == 25)

      s = '-12'
      ASSERT(s%IsInt())
      ASSERT(int(s) == -12)

      s = '   -12'
      ASSERT(s%IsInt())
      ASSERT(int(s) == -12)

      s = 'not a number'
      ASSERT(.not.s%IsInt())

      s = '1e6'
#if defined(__GFORTRAN__) || defined(NAGFOR)
      ASSERT(.not.s%IsInt())
#else
      ASSERT(s%IsInt())
#endif

      s = '1.0'
      ASSERT(s%IsReal())
      ASSERT(real(s) == 1.0)

      s = '-1.e6'
      ASSERT(s%IsReal())
      ASSERT(real(s) == -1.0e6)

      s = '1'
      ASSERT(s%IsReal())
      ASSERT(real(s) == 1.0)

      s = 'not a number'
      ASSERT(.not.s%IsReal())

      s = '(0.0,1.0)'
      ASSERT(s%IsComplex())
      ASSERT(complex(s) == (0.0,1.0))

      s = 'T'
      ASSERT(s%IsLogical())
      ASSERT(s%ToLogical() .eqv. .true.)

      s = 'True'
      ASSERT(s%IsLogical())
      ASSERT(s%ToLogical() .eqv. .true.)

      s = 'F'
      ASSERT(s%IsLogical())
      ASSERT(s%ToLogical() .eqv. .false.)

      s = 'False'
      ASSERT(s%IsLogical())
      ASSERT(s%ToLogical() .eqv. .false.)

      s = 'N'
      ASSERT(.not.s%IsLogical())

   end subroutine


   subroutine testComparison
      type(ftlString) :: s1, s2

      s1 = 'test'
      s2 = 'foobar'

      ASSERT(s1 == 'test')
      ASSERT('test' == s1)
      ASSERT(.not.(s1 == s2))
      ASSERT(s1 /= s2)
      ASSERT('bar' /= s1)

      ! cases with trailing spaces:

      s1 = 'test '
      s2 = 'test  '

      ASSERT(.not.(s1 == 'test'))
      ASSERT(s1 /= 'test')
      ASSERT(.not.('test' == s1))
      ASSERT('test' /= s1)
      ASSERT(s1 /= s2)
      ASSERT(.not.(s1 == s2))

   end subroutine


   subroutine testConcat
      type(ftlString)               :: s1, s2, s3, s4
      character(len=:), allocatable :: c

      s1 = 'this'
      s2 = 'is'
      s3 = 'testing'
      c = 'some old value'

      c = s1//' '//s2//' '//s3
      ASSERT(c == 'this is testing')

      c = 'woho '//s1//' '//s2//' '//s3
      ASSERT(c == 'woho this is testing')

      s4 = 'woho '//s1//' '//s2//' '//s3
      ASSERT(s4 == 'woho this is testing')

      s4 = s1+' '+s2+' '+s3
      ASSERT(s4 == 'this is testing')

      s4 = 'woho '+s1+' '+s2+' '+s3
      ASSERT(s4 == 'woho this is testing')

      c = ' bla'
      s4 = 'test' + c
      ASSERT(s4 == 'test bla')

   end subroutine


   subroutine testIn
      type(ftlString) :: s1, s2, s3

      s1 = 'this is a string containing the word string'
      s2 = 'string'

      ASSERT(s2 .in. s1)
      ASSERT('word' .in. s1)
      ASSERT(s2 .in. 'this is a string test!')

      s3 = 'this is a ... not containing the particular word'

      ASSERT(.not.(s2 .in. s3))

   end subroutine


   subroutine testReadLine
      type(ftlString) :: line
      integer, parameter :: unit = 29
      integer :: iostat

      open (unit=unit, file='tests/assets/astronomer.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) then
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'When I heard the learned astronomer,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'When the proofs, the figures, were ranged in columns before me,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'When I was shown the charts and diagrams, to add, divide,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == '   and measure them,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'When I sitting heard the astronomer where he lectured with')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == '   much applause in the lecture-room,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'How soon unaccountable I became tired and sick,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'Till rising and gliding out I wandered off by myself,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'In the mystical moist night-air, and from time to time,')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat)
         ASSERT(line == 'Looked up in perfect silence at the stars.')
         ASSERT(iostat == 0)
         call line%ReadLine(unit, iostat) ! reading past the end of the file
         ASSERT(line == '')
         ASSERT(is_iostat_end(iostat))
         call line%ReadLine(unit)
         ASSERT(line == '')
         ASSERT(is_iostat_end(iostat))
      endif
      close(unit)

      open (unit=unit, file='tests/assets/emptyfile.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) then
         call line%ReadLine(unit, iostat) ! reading past the end of the file
         ASSERT(line == '')
         ASSERT(is_iostat_end(iostat))
         call line%ReadLine(unit)
         ASSERT(line == '')
         ASSERT(is_iostat_end(iostat))
      endif
      close(unit)

   end subroutine


   subroutine testReadUntilEOF
      type(ftlString) :: contents
      integer, parameter :: unit = 29
      integer :: iostat

      open (unit=unit, file='tests/assets/frankenstein.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) then
         call contents%ReadUntilEOF(unit)
         ASSERT('Geneva' .in. contents)
         ASSERT('darkness and distance.' .in. contents)
         ASSERT(.not.('Uilleann pipes' .in. contents))
      endif
      close(unit)

      open (unit=unit, file='tests/assets/singleline.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) then
         call contents%ReadUntilEOF(unit)
         ASSERT(contents == 'This is just one single line.')
      endif
      close(unit)

      open (unit=unit, file='tests/assets/emptyfile.txt', status='old', action='read', iostat=iostat)
      ASSERT(iostat == 0)
      if (iostat == 0) then
         call contents%ReadUntilEOF(unit)
         ASSERT(contents == '')
      endif
      close(unit)

   end subroutine


   subroutine testFortranStandardMethods
      type(ftlString) :: s1, s2

      s1 = '  this is a test with 4 trailing spaces and 2 leading spaces    '

      ASSERT(len(s1) == 64)
      ASSERT(len_trim(s1) == 60)

      s1 = trim(s1)

      ASSERT(len(s1) == 60)
      ASSERT(len_trim(s1) == 60)

      s1 = '   TEST '

      ASSERT(adjustl(s1) == 'TEST    ')
      ASSERT(len(adjustl(s1)) == 8)
      ASSERT(adjustr(s1) == '    TEST')
      ASSERT(len(adjustl(s1)) == 8)

      s1 = 'ABC'

      ASSERT(repeat(s1,3) == 'ABCABCABC')
      ASSERT(len(repeat(s1,3)) == 9)

      s2 = 'ABC1234ABC567ABC890'

      ASSERT(index(s2,s1,.true.) == 14)
      ASSERT(index(s2,s1) == 1)
      ASSERT(scan(s2,s1,.true.) == 16)
      ASSERT(scan(s2,s1) == 1)
      ASSERT(verify(s2,s1,.true.) == 19)
      ASSERT(verify(s2,s1) == 4)

      ASSERT(index(s2,s1%raw,.true.) == 14)
      ASSERT(index(s2,s1%raw) == 1)
      ASSERT(scan(s2,s1%raw,.true.) == 16)
      ASSERT(scan(s2,s1%raw) == 1)
      ASSERT(verify(s2,s1%raw,.true.) == 19)
      ASSERT(verify(s2,s1%raw) == 4)

   end subroutine


   subroutine testCenter
      type(ftlString) :: s

      s = 'x'

      ASSERT(s%Center(9) == '    x    ')
      ASSERT(s%Center(10) == '    x     ')
      ASSERT(s%Center(2,'a') == 'xa')
      ASSERT(s%Center(3,'a') == 'axa')

      s = 'longstring'

      ASSERT(s%Center(4) == 'longstring')
      ASSERT(s%Center(10) == 'longstring')
      ASSERT(s%Center(11) == 'longstring ')

   end subroutine


   subroutine testCount
      type(ftlString) :: s, sub

      s = 'testtesttest'
      sub = 'test'
      ASSERT(s%Count(sub) == 3)
      ASSERT(s%Count('hallelujathisisalong string') == 0)

      s = 'test This is our test sentence for testing replacing substrings.test'
      ASSERT(s%Count('testing') == 1)

      s = 'ABABAB'
      ASSERT(s%Count('AB') == 3)
      ASSERT(s%Count('ABAB') == 1) ! counts non overlapping ABABs!

      s = 'test'
      ASSERT(s%Count('') == 5) ! replicating a Python behavior

      ! tests with slicing
      s = 'testtesttest'
      ASSERT(s%Count('test',start=2,end=13) == 2)
      ASSERT(s%Count('test',start=2) == 2)
      ASSERT(s%Count('test',end=10) == 2)
      ASSERT(s%Count('test',start=5,end=9) == 1)
      ASSERT(s%Count('test',start=6,end=5) == 0)

   end subroutine


   subroutine testPartition
      type(ftlString) :: s, sep
      type(ftlString) :: part(3)

      s = 'option=value'

      part = s%Partition('=')

      ASSERT(part(1) == 'option')
      ASSERT(part(2) == '=')
      ASSERT(part(3) == 'value')

      part = s%Partition('?')

      ASSERT(part(1) == 'option=value')
      ASSERT(part(2) == '')
      ASSERT(part(3) == '')

      part = s%Partition('value')

      ASSERT(part(1) == 'option=')
      ASSERT(part(2) == 'value')
      ASSERT(part(3) == '')

      part = s%Partition('o')

      ASSERT(part(1) == '')
      ASSERT(part(2) == 'o')
      ASSERT(part(3) == 'ption=value')

      sep = '='
      part = s%Partition(sep)

      ASSERT(part(1) == 'option')
      ASSERT(part(2) == '=')
      ASSERT(part(3) == 'value')

   end subroutine


   subroutine testSplit
      type(ftlString) :: s
      type(ftlString), allocatable :: words(:)

      s = 'This is a simple sentence.'
      words = s%Split()

      ASSERT(size(words) == 5)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a')
      ASSERT(words(4) == 'simple')
      ASSERT(words(5) == 'sentence.')

      s = achar(9)//'This is a   simple sentence with weird whitespacing    issues.  '//achar(9)//' '
      words = s%Split()

      ASSERT(size(words) == 9)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a')
      ASSERT(words(4) == 'simple')
      ASSERT(words(5) == 'sentence')
      ASSERT(words(6) == 'with')
      ASSERT(words(7) == 'weird')
      ASSERT(words(8) == 'whitespacing')
      ASSERT(words(9) == 'issues.')

      s = 'bla,blub,test'
      words = s%Split(',')

      ASSERT(size(words) == 3)
      ASSERT(words(1) == 'bla')
      ASSERT(words(2) == 'blub')
      ASSERT(words(3) == 'test')

      s = ',bla,,blub,test,'
      words = s%Split(',')

      ASSERT(size(words) == 6)
      ASSERT(words(1) == '')
      ASSERT(words(2) == 'bla')
      ASSERT(words(3) == '')
      ASSERT(words(4) == 'blub')
      ASSERT(words(5) == 'test')
      ASSERT(words(6) == '')

      s = ',,,,,'
      words = s%Split(',')

      ASSERT(size(words) == 6)
      ASSERT(words(1) == '')
      ASSERT(words(2) == '')
      ASSERT(words(3) == '')
      ASSERT(words(4) == '')
      ASSERT(words(5) == '')
      ASSERT(words(6) == '')

      s = 'bla<>blub<>test'
      words = s%Split('<>')

      ASSERT(size(words) == 3)
      ASSERT(words(1) == 'bla')
      ASSERT(words(2) == 'blub')
      ASSERT(words(3) == 'test')

      s = '<>bla<><>blub<>test<>'
      words = s%Split('<>')

      ASSERT(size(words) == 6)
      ASSERT(words(1) == '')
      ASSERT(words(2) == 'bla')
      ASSERT(words(3) == '')
      ASSERT(words(4) == 'blub')
      ASSERT(words(5) == 'test')
      ASSERT(words(6) == '')

      s = '<><><><><>'
      words = s%Split('<>')

      ASSERT(size(words) == 6)
      ASSERT(words(1) == '')
      ASSERT(words(2) == '')
      ASSERT(words(3) == '')
      ASSERT(words(4) == '')
      ASSERT(words(5) == '')
      ASSERT(words(6) == '')

      ! Tests with maxsplit:

      s = 'This is a test.'
      words = s%Split(maxsplit=2)

      ASSERT(size(words) == 3)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a test.')

      s = 'This is a test.   '
      words = s%Split(maxsplit=2)

      ASSERT(size(words) == 3)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a test.   ')

      s = 'This is a test.   '
      words = s%Split(maxsplit=20)

      ASSERT(size(words) == 4)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a')
      ASSERT(words(4) == 'test.')

      s = '   This is a test.   '
      words = s%Split(maxsplit=2)

      ASSERT(size(words) == 3)
      ASSERT(words(1) == 'This')
      ASSERT(words(2) == 'is')
      ASSERT(words(3) == 'a test.   ')

      s = 'bla,blub,test'
      words = s%Split(',', maxsplit=1)

      ASSERT(size(words) == 2)
      ASSERT(words(1) == 'bla')
      ASSERT(words(2) == 'blub,test')

      s = ',bla,,blub,test,'
      words = s%Split(',',maxsplit=3)

      ASSERT(size(words) == 4)
      ASSERT(words(1) == '')
      ASSERT(words(2) == 'bla')
      ASSERT(words(3) == '')
      ASSERT(words(4) == 'blub,test,')

      s = ',,,,,'
      words = s%Split(',',maxsplit=20)

      ASSERT(size(words) == 6)
      ASSERT(words(1) == '')
      ASSERT(words(2) == '')
      ASSERT(words(3) == '')
      ASSERT(words(4) == '')
      ASSERT(words(5) == '')
      ASSERT(words(6) == '')

      ! Test that splitting empty strings behaves like in Python:

      s = ''
      words = s%Split(',')
      ASSERT(size(words) == 1)
      ASSERT(words(1) == '')

      s = ''
      words = s%Split()
      ASSERT(size(words) == 0)

      s = FTL_STRING_WHITESPACE
      words = s%Split()
      ASSERT(size(words) == 0)

      s = ''
      words = s%Split(',',maxsplit=20)
      ASSERT(size(words) == 1)
      ASSERT(words(1) == '')

      s = ''
      words = s%Split(maxsplit=20)
      ASSERT(size(words) == 0)

      s = FTL_STRING_WHITESPACE
      words = s%Split(maxsplit=20)
      ASSERT(size(words) == 0)

   end subroutine


   subroutine testSplitLines
      type(ftlString) :: s
      type(ftlString), allocatable :: lines(:)

      s = 'bla'//achar(10)//'blub blub'//achar(13)//achar(10)//'test'
      lines = s%SplitLines()

      ASSERT(size(lines) == 3)
      ASSERT(lines(1) == 'bla')
      ASSERT(lines(2) == 'blub blub')
      ASSERT(lines(3) == 'test')

      s = 'bla'//achar(10)//'blub blub'//achar(13)//achar(10)//'test'//achar(10)
      lines = s%SplitLines()

      ASSERT(size(lines) == 3)
      ASSERT(lines(1) == 'bla')
      ASSERT(lines(2) == 'blub blub')
      ASSERT(lines(3) == 'test')

      s = 'bla'//achar(10)//'blub blub'//achar(13)//achar(10)//'test'//achar(10)//achar(10)
      lines = s%SplitLines()

      ASSERT(size(lines) == 4)
      ASSERT(lines(1) == 'bla')
      ASSERT(lines(2) == 'blub blub')
      ASSERT(lines(3) == 'test')
      ASSERT(lines(4) == '')

      s = achar(13)//'bla'//achar(10)//'blub'//achar(13)//'blub'//achar(13)//achar(10)//'test'//achar(10)//achar(10)
      lines = s%SplitLines()

      ASSERT(size(lines) == 6)
      ASSERT(lines(1) == '')
      ASSERT(lines(2) == 'bla')
      ASSERT(lines(3) == 'blub')
      ASSERT(lines(4) == 'blub')
      ASSERT(lines(5) == 'test')
      ASSERT(lines(6) == '')

      s = 'just one line'
      lines = s%SplitLines()

      ASSERT(size(lines) == 1)
      ASSERT(lines(1) == 'just one line')

      s = ''
      lines = s%SplitLines()
      ASSERT(size(lines) == 0)

   end subroutine


   subroutine testJoin
      type(ftlString) :: sep
      type(ftlString), allocatable :: words(:)

      allocate(words(5))
      words(1) = 'test'
      words(2) = 'this'
      words(3) = 'stuff'
      words(4) = ''
      words(5) = 'thoroughly'

      sep = ':'

      ASSERT(sep%Join(words(1:0)) == '')
      ASSERT(sep%Join(words(1:1)) == 'test')
      ASSERT(sep%Join(words(1:2)) == 'test:this')
      ASSERT(sep%Join(words(1:3)) == 'test:this:stuff')
      ASSERT(sep%Join(words(1:4)) == 'test:this:stuff:')
      ASSERT(sep%Join(words(1:5)) == 'test:this:stuff::thoroughly')

      sep = '=='

      ASSERT(sep%Join(words(1:0)) == '')
      ASSERT(sep%Join(words(1:1)) == 'test')
      ASSERT(sep%Join(words(1:2)) == 'test==this')
      ASSERT(sep%Join(words(1:3)) == 'test==this==stuff')
      ASSERT(sep%Join(words(1:4)) == 'test==this==stuff==')
      ASSERT(sep%Join(words(1:5)) == 'test==this==stuff====thoroughly')

      sep = ''

      ASSERT(sep%Join(words(1:0)) == '')
      ASSERT(sep%Join(words(1:1)) == 'test')
      ASSERT(sep%Join(words(1:2)) == 'testthis')
      ASSERT(sep%Join(words(1:3)) == 'testthisstuff')
      ASSERT(sep%Join(words(1:4)) == 'testthisstuff')
      ASSERT(sep%Join(words(1:5)) == 'testthisstuffthoroughly')

      ! test free Join() method

      ASSERT(Join(':',words(1:0)) == '')
      ASSERT(Join(':',words(1:1)) == 'test')
      ASSERT(Join(':',words(1:2)) == 'test:this')
      ASSERT(Join(':',words(1:3)) == 'test:this:stuff')
      ASSERT(Join(':',words(1:4)) == 'test:this:stuff:')
      ASSERT(Join(':',words(1:5)) == 'test:this:stuff::thoroughly')

   end subroutine


   subroutine testStartsWith
      type(ftlString) :: s
      type(ftlString), allocatable :: prefixes(:)

      s = 'Teststartswith'
      ASSERT(s%StartsWith('Test'))

      s = 'Teststartswith'
      ASSERT(.not.s%StartsWith('Teststartswith but is toolong'))

      s = '  Teststartswith'
      ASSERT(.not.s%StartsWith('Test'))

      s = 'another test of startsWith'
      ASSERT(s%StartsWith(ftlString('another')))

      ! the following two lines leak memory with gfortran, see: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=79053
      !ASSERT(s%StartsWith([ftlString('Test'),ftlString('anot')]))
      !ASSERT(.not.s%StartsWith([ftlString('Test'),ftlString('not there')]))

      ! this is a workaround that does not leak ...
      allocate(prefixes(2))
      prefixes(1) = 'Test'
      prefixes(2) = 'anot'
      ASSERT(s%StartsWith(prefixes))

      prefixes(1) = 'Test'
      prefixes(2) = 'not there'
      ASSERT(.not.s%StartsWith(prefixes))

   end subroutine


   subroutine testEndsWith
      type(ftlString) :: s
      type(ftlString), allocatable :: postfixes(:)

      s = 'Testendswith'
      ASSERT(s%EndsWith('with'))

      s = 'Testendswith'
      ASSERT(.not.s%EndsWith('Testendswith but is toolong'))

      s = 'Testendswith    '
      ASSERT(.not.s%EndsWith('with'))

      s = 'another test of endsWith'
      ASSERT(s%EndsWith(ftlString('endsWith')))

      allocate(postfixes(2))
      postfixes(1) = 'Test'
      postfixes(2) = 'With'
      ASSERT(s%EndsWith(postfixes))

      postfixes(1) = 'Test'
      postfixes(2) = 'not there'
      ASSERT(.not.s%EndsWith(postfixes))

   end subroutine


   subroutine testStrip
      type(ftlString) :: s, stripme

      s = '   hohoho   '
      ASSERT(s%Strip(' ') == 'hohoho')
      ASSERT(s%Strip()    == 'hohoho')

      s = '   '
      ASSERT(s%Strip(' ') == '')

      s = 'helloworld'
      ASSERT(s%Strip(' ') == 'helloworld')

      s = ''
      ASSERT(s%Strip(' ') == '')

      s = './!@#$%^&*()-_ hello world  _)(*&^%$#@!'
      stripme = './!@#$%^&*()-_ '
      ASSERT(s%Strip('./!@#$%^&*()-_ ') == 'hello world')
      ASSERT(s%Strip(stripme) == 'hello world')

   end subroutine


   subroutine testRStrip
      type(ftlString) :: s, stripme

      s = '   hohoho   '
      ASSERT(s%RStrip(' ') == '   hohoho')
      ASSERT(s%RStrip()    == '   hohoho')

      s = '   '
      ASSERT(s%RStrip(' ') == '')

      s = 'helloworld'
      ASSERT(s%RStrip(' ') == 'helloworld')

      s = ''
      ASSERT(s%RStrip(' ') == '')

      s = './!@#$%^&*()-_ hello world  _)(*&^%$#@!'
      stripme = './!@#$%^&*()-_ '
      ASSERT(s%RStrip('./!@#$%^&*()-_ ') == './!@#$%^&*()-_ hello world')
      ASSERT(s%RStrip(stripme) == './!@#$%^&*()-_ hello world')

   end subroutine


   subroutine testLStrip
      type(ftlString) :: s, stripme

      s = '   hohoho   '
      ASSERT(s%LStrip(' ') == 'hohoho   ')
      ASSERT(s%LStrip()    == 'hohoho   ')

      s = '   '
      ASSERT(s%LStrip(' ') == '')

      s = 'helloworld'
      ASSERT(s%LStrip(' ') == 'helloworld')

      s = ''
      ASSERT(s%LStrip(' ') == '')

      s = './!@#$%^&*()-_ hello world  _)(*&^%$#@!'
      stripme = './!@#$%^&*()-_ '
      ASSERT(s%LStrip('./!@#$%^&*()-_ ') == 'hello world  _)(*&^%$#@!')
      ASSERT(s%LStrip(stripme) == 'hello world  _)(*&^%$#@!')

   end subroutine


   subroutine testUpperLower
      type(ftlString) :: s

      s = 'This is .A. [test]!'
      ASSERT(s%Upper() == 'THIS IS .A. [TEST]!')
      ASSERT(s%Lower() == 'this is .a. [test]!')

      s = FTL_STRING_DIGITS//FTL_STRING_LOWERCASE//FTL_STRING_WHITESPACE//FTL_STRING_PUNCTUATION
      ASSERT(s%Upper() == FTL_STRING_DIGITS//FTL_STRING_UPPERCASE//FTL_STRING_WHITESPACE//FTL_STRING_PUNCTUATION)

      s = FTL_STRING_DIGITS//FTL_STRING_UPPERCASE//FTL_STRING_WHITESPACE//FTL_STRING_PUNCTUATION
      ASSERT(s%Lower() == FTL_STRING_DIGITS//FTL_STRING_LOWERCASE//FTL_STRING_WHITESPACE//FTL_STRING_PUNCTUATION)

   end subroutine


   subroutine testIsSpace
      type(ftlString) :: s

      s = ''
      ASSERT(.not.s%IsSpace())

      s = ' '
      ASSERT(s%IsSpace())

      s = '      test   '
      ASSERT(.not.s%IsSpace())

      s = FTL_STRING_WHITESPACE
      ASSERT(s%IsSpace())

      s = FTL_STRING_PRINTABLE
      ASSERT(.not.s%IsSpace())

   end subroutine


   subroutine testReplace
      type(ftlString) :: s

      s = 'test This is our test sentence for testtesting replacing substrings.test'

      ASSERT(s%Replace('test','TEST')    ==  'TEST This is our TEST sentence for TESTTESTing replacing substrings.TEST')
      ASSERT(s%Replace('test','TEST', 2) ==  'TEST This is our TEST sentence for testtesting replacing substrings.test')
      ASSERT(s%Replace('e','_')          ==  't_st This is our t_st s_nt_nc_ for t_stt_sting r_placing substrings.t_st')

      ASSERT(s%Replace('testing','testinating') == 'test This is our test sentence for testtestinating replacing substrings.test')
      ASSERT(s%Replace('test','') == ' This is our  sentence for ing replacing substrings.')
      ASSERT(s%Replace('t','__') == '__es__ This is our __es__ sen__ence for __es____es__ing replacing subs__rings.__es__')
      ASSERT(s%Replace('t','__',2) == '__es__ This is our test sentence for testtesting replacing substrings.test')

      s = 'ABABAB'

      ASSERT(s%Replace('A','B') == 'BBBBBB')
      ASSERT(s%Replace('AB','B') == 'BBB')
      ASSERT(s%Replace('BA','AB') == 'AABABB')

   end subroutine


   subroutine testCountWords
      type(ftlString) :: s

      s = ''
      ASSERT(s%CountWords() == 0)

      s = FTL_STRING_WHITESPACE
      ASSERT(s%CountWords() == 0)

      s = 'this is a test'
      ASSERT(s%CountWords() == 4)

      s = 'this is a test with trailing spaces   '
      ASSERT(s%CountWords() == 7)

      s = '   this is a test with leading spaces   '
      ASSERT(s%CountWords() == 7)

      s = '   this is a test with leading and trailing spaces   '
      ASSERT(s%CountWords() == 9)

      s = achar(9)//'test containing '//achar(9)//'tabs'//achar(9)
      ASSERT(s%CountWords() == 3)

   end subroutine


   subroutine testNewItDefault
      type(ftlStringIterator) :: it

      call it%New()
      ASSERT(.not.associated(it%value))

   end subroutine


   subroutine testNewItCopyOther
      type(ftlString) :: s
      type(ftlStringIterator) :: it1, it2

      call s%New('howdy, lets test!')
      it1 = s%Begin()
      call it2%New(it1)

      ASSERT(associated(it1%value,it2%value))
      ASSERT(it2%value == 'h')

   end subroutine


   subroutine testInc
      type(ftlString) :: s
      type(ftlStringIterator) :: it

      call s%New('still testing')
      it = s%Begin()

      !ASSERT(associated(it%value,s%At(1)))
      ASSERT(it%value == 's')

      call it%Inc()

      !ASSERT(associated(it%value,s%At(2)))
      ASSERT(it%value == 't')

   end subroutine


   subroutine testDec
      type(ftlString) :: s
      type(ftlStringIterator) :: it

      call s%New('and still testing')
      it = s%End()
      call it%Dec()

      !ASSERT(associated(it%value,s%At(17)))
      ASSERT(it%value == 'g')

      call it%Dec()

      !ASSERT(associated(it%value,s%At(16)))
      ASSERT(it%value == 'n')

   end subroutine


   subroutine testAdvanceReverseDiff
      type(ftlString) :: s
      type(ftlStringIterator) :: it1, it2

      call s%New('... still testing!')
      it1 = s%Begin()
      it2 = it1 + 4

      ASSERT(it2 - it1 == 4)
      !ASSERT(associated(it2%value,s%At(5)))
      ASSERT(it2%value == 's')

      it2 = it2 - 2
      ASSERT(it2 - it1 == 2)
      !ASSERT(associated(it2%value,s%At(3)))
      ASSERT(it2%value == '.')

   end subroutine


   subroutine testLogicalOperations
      type(ftlString) :: s
      type(ftlStringIterator) :: it1, it2

      call s%New('howdy')
      it1 = s%Begin() + 2
      ASSERT(it1%value == 'w')
      it2 = s%Begin()

      ASSERT(it2%value == 'h')
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(     (it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(.not.(it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 'o')
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(     (it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(.not.(it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 'w')
      ASSERT(     (it2 == it1))
      ASSERT(.not.(it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(     (it2 <= it1))
      ASSERT(.not.(it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 'd')
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(.not.(it2 <= it1))
      ASSERT(     (it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2%value == 'y')
      ASSERT(.not.(it2 == it1))
      ASSERT(     (it2 /= it1))
      ASSERT(.not.(it2 <  it1))
      ASSERT(.not.(it2 <= it1))
      ASSERT(     (it2 >  it1))
      ASSERT(     (it2 >= it1))

      call it2%Inc()

      ASSERT(it2 == s%End())

   end subroutine


   subroutine testSplitWordsIntoDynArray
      type(ftlString) :: s, snew
      type(ftlDynArrayString) :: v
      type(ftlDynArrayStringIterator) :: it

      s = 'This is a sentence which we are going to split up into words. Then we will put it into a ftlDynArray.'

      ! Let's remove everything after sentence in the first sentence ...
      v = s%Split()
      ASSERT(size(v) == 21)
      call v%Erase(5,14)
      ASSERT(size(v) == 12)
      ASSERT(v%data(4) == 'sentence')
      v%data(4) = v%data(4) // '.' ! add the dot we deleted along with "words."
      ASSERT(v%data(4) == 'sentence.')

      snew = v%front
      it = v%Begin() + 1
      do while (it /= v%End())
         snew = snew // ' ' // it%value
         call it%Inc()
      enddo

      ASSERT(snew == 'This is a sentence. Then we will put it into a ftlDynArray.')

   end subroutine


   subroutine testDynArrayStringSpecialization
      type(ftlDynArrayString) :: v

      call v%New(['hello','world'])

      ASSERT(size(v) == 2)
      ASSERT(v%front == 'hello')
      ASSERT(v%back  == 'world')

      call v%New()
      call v%PushBack('hello')

      ASSERT(size(v) == 1)
      ASSERT(v%front == 'hello')
      ASSERT(v%back  == 'hello')
      ASSERT(v%data(1) == 'hello')

      call v%PushBack('world')
      call v%PushBack('of')
      call v%PushBack('fortran')

      ASSERT(size(v) == 4)
      ASSERT(v%front == 'hello')
      ASSERT(v%back  == 'fortran')
      ASSERT(v%data(1) == 'hello')
      ASSERT(v%data(2) == 'world')
      ASSERT(v%data(3) == 'of')
      ASSERT(v%data(4) == 'fortran')

      call v%Resize(6, 'two more')

      ASSERT(size(v) == 6)
      ASSERT(v%front == 'hello')
      ASSERT(v%back  == 'two more')
      ASSERT(v%data(1) == 'hello')
      ASSERT(v%data(2) == 'world')
      ASSERT(v%data(3) == 'of')
      ASSERT(v%data(4) == 'fortran')
      ASSERT(v%data(5) == 'two more')
      ASSERT(v%data(6) == 'two more')

      v = ['a','b','c']

      ASSERT(size(v) == 3)
      ASSERT(v%front == 'a')
      ASSERT(v%back  == 'c')
      ASSERT(v%data(1) == 'a')
      ASSERT(v%data(2) == 'b')
      ASSERT(v%data(3) == 'c')

      call v%Insert(2, 'blub')

      ASSERT(size(v) == 4)
      ASSERT(v%front == 'a')
      ASSERT(v%back  == 'c')
      ASSERT(v%data(1) == 'a')
      ASSERT(v%data(2) == 'blub')
      ASSERT(v%data(3) == 'b')
      ASSERT(v%data(4) == 'c')

      call v%Insert(2, ['hallo','world'])

      ASSERT(size(v) == 6)
      ASSERT(v%front == 'a')
      ASSERT(v%back  == 'c')
      ASSERT(v%data(1) == 'a')
      ASSERT(v%data(2) == 'hallo')
      ASSERT(v%data(3) == 'world')
      ASSERT(v%data(4) == 'blub')
      ASSERT(v%data(5) == 'b')
      ASSERT(v%data(6) == 'c')

      call v%Resize(10, 'end')

      ASSERT(size(v) == 10)
      ASSERT(v%front == 'a')
      ASSERT(v%back  == 'end')
      ASSERT(v%data( 1) == 'a')
      ASSERT(v%data( 2) == 'hallo')
      ASSERT(v%data( 3) == 'world')
      ASSERT(v%data( 4) == 'blub')
      ASSERT(v%data( 5) == 'b')
      ASSERT(v%data( 6) == 'c')
      ASSERT(v%data( 7) == 'end')
      ASSERT(v%data( 8) == 'end')
      ASSERT(v%data( 9) == 'end')
      ASSERT(v%data(10) == 'end')

   end subroutine


end module
