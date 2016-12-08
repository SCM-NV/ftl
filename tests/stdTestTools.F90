module stdTestToolsModule

   implicit none

   integer, protected, save :: num_asserts = 0
   integer, protected, save :: num_failed  = 0

contains

   subroutine assert(cond, str, file, line)
      logical         , intent(in) :: cond
      character(len=*), intent(in) :: str
      character(len=*), intent(in) :: file
      integer         , intent(in) :: line

      num_asserts = num_asserts + 1

      if (.not.cond) then
         write (*,'(A,A,A,I0,A,A)') 'Failed assertion at ',file,'(',line,'): ',str
         num_failed = num_failed + 1

         if (str == '.false.') then
            write (*,'(A)') '(This was a dummy assertion to test if assertions themselves work ...)'
            num_asserts = num_asserts - 1
            num_failed = num_failed - 1
         endif
      endif

   end subroutine

end module
