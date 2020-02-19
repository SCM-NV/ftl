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

module ftlRegexTestsModule

   use ftlStringModule
   use ftlTestToolsModule
   use ftlRegexModule

   implicit none
   private
   public :: ftlRegexTests

contains


   subroutine ftlRegexTests

      write (*,'(A)') 'Running ftlRegex tests ...'

      call testCompileFlags
      call testComparison
      call testCaptureGroups
      call testNumMatches
      call testMatch
      call testReplace
      call testReplaceGroupSub

      ! Tests for issues reported on GitHub:
      call testIssue4

      call testArrayFinalization

   end subroutine


   subroutine testCompileFlags
      type(ftlRegex) :: r
      type(ftlRegexMatch) :: m

      ! The temporary ftlRegexes in these expressions leak with gfortran 6.3.1:

      !ASSERT(     ('blub atest' .matches. ftlRegex('a')))
      !ASSERT(.not.('blub atest' .matches. ftlRegex('A')))
      !ASSERT(     ('blub atest' .matches. ftlRegex('A', icase=.true.)))

      ! Apparently they don't have finalization of temporaries implemented at the moment, see:
      ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=37336#c27

      ! Here is a workaround, so that we don't have a leaking testsuite ...

      call r%New('a')
      ASSERT('blub atest' .matches. r)
      call r%New('A')
      ASSERT(.not.('blub atest' .matches. r))
      call r%New('A', icase=.true.)
      ASSERT('blub atest' .matches. r)

      call r%New('([[:digit:]]+)\.([[:digit:]]+)')
      m = r%MatchFirst('some value: 12.436')
      ASSERT(m%matches)
      ASSERT(m%text == '12.436')
      ASSERT(size(m%group) == 2)
      ASSERT(m%group(1)%text == '12')
      ASSERT(m%group(2)%text == '436')

      call r%New('([[:digit:]]+)\.([[:digit:]]+)', nosub=.true.)
      m = r%MatchFirst('some value: 12.436')
      ASSERT(m%matches)
      ASSERT(size(m%group) == 0)

   end subroutine


   subroutine testComparison
      type(ftlRegex) :: r1, r2

      call r1%New('pattern')
      call r2%New('pattern')

      ASSERT(r1 == r2)
      ASSERT(.not.(r1 /= r2))

      call r2%New('other')

      ASSERT(r1 /= r2)
      ASSERT(.not.(r1 == r2))

      call r2%New('pattern', icase=.true.)

      ASSERT(r1 /= r2)
      ASSERT(.not.(r1 == r2))

      r2 = r1

      ASSERT(r1 == r2)
      ASSERT(.not.(r1 /= r2))

   end subroutine


   subroutine testCaptureGroups
      type(ftlRegex) :: r
      type(ftlRegexMatch) :: m

      call r%New('(\w+)\s*=\s*(\w+)')

      ASSERT('something = other' .matches. r)

      m = r%MatchFirst('occupations option=value')
      ASSERT(m%text == 'option=value')
      ASSERT(m%begin == 13)
      ASSERT(m%end == 25)
      ASSERT(size(m%group) == 2)
      ASSERT(m%group(1)%text == 'option')
      ASSERT(m%group(1)%begin == 13)
      ASSERT(m%group(1)%end == 19)
      ASSERT(m%group(2)%text == 'value')
      ASSERT(m%group(2)%begin == 20)
      ASSERT(m%group(2)%end == 25)

   end subroutine


   subroutine testNumMatches
      type(ftlRegex) :: r

      call r%New('a')

      ASSERT(r%NumMatches('aaa') == 3)
      ASSERT(r%NumMatches('a test sentence containing 3 times the letter a') == 3)
      ASSERT(r%NumMatches('bbb') == 0)

   end subroutine


   subroutine testMatch
      type(ftlString) :: line
      type(ftlRegex) :: r
      type(ftlRegexMatch), allocatable :: m(:)

      line = 'keyword option1=value option2=othervalue'
      call r%New('(\w+)\s*=\s*(\w+)')
      m = r%Match(line)

      ASSERT(m(1)%text == 'option1=value')
      ASSERT(m(1)%begin == 9)
      ASSERT(m(1)%end == 22)
      ASSERT(size(m(1)%group) == 2)
      ASSERT(m(1)%group(1)%text == 'option1')
      ASSERT(m(1)%group(1)%begin == 9)
      ASSERT(m(1)%group(1)%end == 16)
      ASSERT(m(1)%group(2)%text == 'value')
      ASSERT(m(1)%group(2)%begin == 17)
      ASSERT(m(1)%group(2)%end == 22)

      ASSERT(m(2)%text == 'option2=othervalue')
      ASSERT(m(2)%begin == 23)
      ASSERT(m(2)%end == 41)
      ASSERT(size(m(2)%group) == 2)
      ASSERT(m(2)%group(1)%text == 'option2')
      ASSERT(m(2)%group(1)%begin == 23)
      ASSERT(m(2)%group(1)%end == 30)
      ASSERT(m(2)%group(2)%text == 'othervalue')
      ASSERT(m(2)%group(2)%begin == 31)
      ASSERT(m(2)%group(2)%end == 41)

   end subroutine


   subroutine testReplace
      type(ftlString) :: line
      type(ftlRegex) :: r

      call r%New('\s*=\s*')

      line = 'keyword option1  = value option2 =othervalue'
      ASSERT(r%Replace(line, '=') == 'keyword option1=value option2=othervalue')
      ASSERT(r%Replace(line, '') == 'keyword option1value option2othervalue')

      line = 'c   = = holla  = '
      ASSERT(r%Replace(line, '') == 'cholla')

      line = '   = = = '
      ASSERT(r%Replace(line, '') == '')

   end subroutine


   subroutine testReplaceGroupSub
      type(ftlString) :: line
      type(ftlRegex) :: r

      call r%New('(\w+)\s*=\s*(\w+)')
      line = 'Element: mass=12 Z=6 symbol=C name=Carbon'

      ASSERT(r%Replace(line, '\2->\1', doGroupSub=.true.) == 'Element: 12->mass 6->Z C->symbol Carbon->name')
      ASSERT(r%Replace(line, '\2', doGroupSub=.true.) == 'Element: 12 6 C Carbon')
      ASSERT(r%Replace(line, 'XXX', doGroupSub=.true.) == 'Element: XXX XXX XXX XXX')
      ASSERT(r%Replace(line, '\2->\1\1', doGroupSub=.true.) == 'Element: 12->massmass 6->ZZ C->symbolsymbol Carbon->namename')

   end subroutine


   subroutine testIssue4
      type(ftlString) :: line
      type(ftlRegex) :: r
      type(ftlRegexMatch), allocatable :: m(:)

      line = 'keyword option1=value option2=othervalue'
      call r%New('(\w+)\s*=\s*(\w+)')
      m = r%Match(line)

      ! m(1)%text now holds 'option1=value'
      ASSERT(m(1)%text == 'option1=value')
      ! m(2)%text now holds 'option2=othervalue'
      ASSERT(m(2)%text == 'option2=othervalue')
      ! m(:)%group is also populated with the contents of the capture groups.
      ! e.g. m(1)%group(2)%text holds 'value'
      ASSERT(m(1)%group(2)%text == 'value')

   end subroutine


   subroutine testArrayFinalization
      type(ftlRegex), allocatable :: r(:)

      ! the following leaks 6 bytes per element with gfortran 7 developer version. bug or me not unterstanding the standard?
      !r = [ ftlRegex('hello'), ftlRegex('world') ]

      ! leak free workaround:
      allocate(r(2))
      call r(1)%New('hello')
      call r(2)%New('world')

      ASSERT('hello world' .matches. r(1))
      ASSERT('hello world' .matches. r(2))

      ! Here the rank-1 array finalizer should be called on r and both regexes should be cleaned up propery.
      ! Check this this 'make memcheck' ...

   end subroutine


end module
