module test_ec_module_block
   use precision, only: dp
   use assertions_gtest
   use fm_external_forcings, only: adduniformtimerelation_objects
   use m_meteo, only: initialize_ec_module, ecInstancePtr, item_lateraldischarge, ec_gettimespacevalue
   use m_file_helpers, only: create_file
   

   implicit none

   character(len=*), parameter :: BC_FILENAME = "test.bc"
contains

   subroutine setup_block_from()
      call create_file(BC_FILENAME, [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name                  = 9", &
                       "    function              = timeseries", &
                       "    timeInterpolation     = block-from", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2000-01-01 00:00:00", &
                       "    quantity              = lateral_discharge", &
                       "    unit                  = m³/s", &
                       "    0 0", &
                       "    10 100", &
                       "    20 20", &
                       "    30 300", &
                       "    40 0"])
   end subroutine setup_block_from

   !$f90tw TESTCODE(TEST, test_ec_module_block, test_ec_block_from, test_ec_block_from,
   subroutine test_ec_block_from() bind(C)
      logical :: success
      real(dp), dimension(3) :: test_array = [1.0_dp, 2.0_dp, 3.0_dp]

      call initialize_ec_module()
      call setup_block_from()
      success = adduniformtimerelation_objects('lateral_discharge', '', 'lateral', '9', 'discharge', BC_FILENAME, 1, &
                                               1, test_array)
      call f90_expect_true(success, "adduniformtimerelation_objects failed to add object with block-from interpolation")
      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         indices = [5.0_dp, 15.0_dp, 25.0_dp, 35.0_dp, 45.0_dp]
         allocate (results(size(indices)))
         expected_results = [0.0_dp, 100.0_dp, 20.0_dp, 300.0_dp, 0.0_dp]
         do i = 1, size(indices)
            success = ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, 20000101, 0.0_dp, 1, indices(i))
            call f90_expect_true(success, "ec_gettimespacevalue failed")
            results(i) = test_array(1)
         end do
         call f90_expect_near(results, expected_results, 1e-8_dp, "results do not match expected values")
      end block

   end subroutine test_ec_block_from
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_module_block, test_ec_block_from_skip, test_ec_block_from_skip,
   subroutine test_ec_block_from_skip() bind(C)
      integer :: status
      logical :: success
      real(dp), dimension(3) :: test_array = [1.0_dp, 2.0_dp, 3.0_dp]

      call setup_block_from()
      call initialize_ec_module()
      success = adduniformtimerelation_objects('lateral_discharge', '', 'lateral', '9', 'discharge', BC_FILENAME, 1, &
                                               1, test_array)
      call f90_expect_true(success, "adduniformtimerelation_objects failed to add object with block-from interpolation")
      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         indices = [5.0_dp, 25.0_dp]
         allocate (results(size(indices)))
         expected_results = [0.0_dp, 20.0_dp]
         do i = 1, size(indices)
            success = ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, 20000101, 0.0_dp, 1, indices(i))
            call f90_expect_true(success, "ec_gettimespacevalue failed")
            results(i) = test_array(1)
         end do
         call f90_expect_near(results, expected_results, 1e-8_dp, "results do not match expected values")
      end block

   end subroutine test_ec_block_from_skip
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_module_block, test_ec_block_to, test_ec_block_to,
   subroutine test_ec_block_to() bind(C)
      logical :: success
      real(dp), dimension(3) :: test_array = [1.0_dp, 2.0_dp, 3.0_dp]

      call create_file(BC_FILENAME, [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[forcing]", &
                       "    name                  = 10", &
                       "    function              = timeseries", &
                       "    timeInterpolation     = block-to", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2000-01-01 00:00:00", &
                       "    quantity              = lateral_discharge", &
                       "    unit                  = m³/s", &
                       "    0 0", &
                       "    10 100", &
                       "    20 20", &
                       "    30 300", &
                       "    40 0"])
      call initialize_ec_module()
      success = adduniformtimerelation_objects('lateral_discharge', '', 'lateral', '10', 'discharge', BC_FILENAME, 1, &
                                               1, test_array)
      call f90_expect_true(success, "adduniformtimerelation_objects failed to add block-to forcing")
      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         indices = [5.0_dp, 15.0_dp, 25.0_dp, 35.0_dp]
         allocate (results(size(indices)))
         expected_results = [100.0_dp, 20.0_dp, 300.0_dp, 0.0_dp]
         do i = 1, size(indices)
            success = ec_gettimespacevalue(ecInstancePtr, item_lateraldischarge, 20000101, 0.0_dp, 1, indices(i))
            call f90_expect_true(success, "ec_gettimespacevalue failed")
            results(i) = test_array(1)
         end do
         call f90_expect_near(results, expected_results, 1e-8_dp, "results do not match expected values")
      end block

   end subroutine test_ec_block_to
   !$f90tw)

end module test_ec_module_block
