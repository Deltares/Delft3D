module test_ec_module_simple
   use precision
   use assertions_gtest
   use cwd

   implicit none

contains
   !$f90tw TESTCODE(TEST, test_ec_module_simple, test_ec_simple, test_ec_simple,
   subroutine test_ec_simple() bind(C)
      character(len=1024) :: current_directory
      integer :: status

      status = getCWD(current_directory)

      print *, "Current working directory: ", trim(current_directory)
      print *, "Status: ", status
   end subroutine test_ec_simple
   !$f90tw)

end module test_ec_module_simple
