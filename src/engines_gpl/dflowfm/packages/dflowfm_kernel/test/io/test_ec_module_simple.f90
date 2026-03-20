module test_ec_module_simple
   use precision
   use assertions_gtest
   use cwd
   use fm_external_forcings, only: adduniformtimerelation_objects
   use m_meteo, only: initialize_ec_module, ecInstancePtr, item_lateraldischarge, ec_gettimespacevalue

   implicit none

contains
   !$f90tw TESTCODE(TEST, test_ec_module_simple, test_ec_simple, test_ec_simple,
   subroutine test_ec_simple() bind(C)
      character(len=1024) :: current_directory
      integer :: status
      logical :: success
      real(dp), dimension(3) :: test_array = [1.0_dp, 2.0_dp, 3.0_dp]

      status = getCWD(current_directory)

      print *, "Current working directory: ", trim(current_directory)
      print *, "Status: ", status
      call initialize_ec_module()

      success = adduniformtimerelation_objects('lateral_discharge', '', 'lateral', '9', 'discharge', 'ec_module/FlowFM_lateral_sources.bc', 1, &
                                               1, test_array)

      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results
         indices  = [5.0_dp, 60.0_dp, 65.0_dp, 105.0_dp, 155.0_dp, 300.0_dp]
         allocate(results(size(indices)))
         
         do i = 1, size(indices)
            success = ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, 20000101, 0.0_dp, 1, indices(i))
            results(i) = test_array(1)
         end do

          print *, "indices    results"
          do i = 1, size(indices)
            print '(F10.1, F12.1)', indices(i), results(i)
          end do
      end block

   end subroutine test_ec_simple
   !$f90tw)

end module test_ec_module_simple
