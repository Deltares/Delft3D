module test_ec_module_nesting
   use precision
   use assertions_gtest
   use m_meteo, only: initialize_ec_module, ec_addtimespacerelation,ec_gettimespacevalue, ecInstancePtr, item_waterlevelbnd
   use fm_external_forcings, only: addtimespacerelation_boundaries
   implicit none

contains
   !$f90tw TESTCODE(TEST, test_ec_module_nesting, test_basic, test_basic,
   subroutine test_basic() bind(C)
      integer :: iresult
      logical :: is_successful
      character(len=256) :: qid = "waterlevelbnd"
      real(kind=dp), allocatable :: xbndz(:), ybndz(:)
      integer, allocatable :: kdz(:)
      integer :: kx
      real(kind=dp) :: dt_nodal = 21600.0_dp
      real(kind=dp), allocatable :: xy2bndz(:, :)
      xbndz = [310.0_dp, 310.0_dp, 310.0_dp, 310.0_dp, 310.0_dp, 310.0_dp]
      ybndz = [1050.0_dp, 1070.0_dp, 1090.0_dp, 1110.0_dp, 1130.0_dp, 1150.0_dp]
      kdz = [1, 1, 1, 1, 1, 1]
      kx = 1
      allocate (xy2bndz(2, 6))
      xy2bndz(1, :) = 240
      xy2bndz(2, :) = ybndz
      call initialize_ec_module()
      ! call init_new("nesting/TestFine_bnd.ext", iresult)
      !   is_successful = addtimespacerelation_boundaries(qid, "nesting/TestFine_bnd.ext", filetype=9, method=3, &
      !                                                               operand='O', forcing_file='nesting/TestCoarse_his.nc')

      is_successful = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, 'nesting/boundary_left.pli', &
                         filetype=9, method=3, operand='O', xyen=xy2bndz, forcingfile='nesting/TestCoarse_his.nc', dtnodal=dt_nodal)

      call f90_expect_true(is_successful, "Add time spacerelation failed!")

      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         results = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
         indices = [5.0_dp, 15.0_dp, 25.0_dp, 35.0_dp, 45.0_dp]
         do i = 1, size(indices)
            is_successful = ec_gettimespacevalue(ecInstancePtr, item_waterlevelbnd, 20010101, 0.0_dp, 1, indices(i), target_array=results)
            print *, results
         end do
      end block


   end subroutine test_basic
   !$f90tw)
end module test_ec_module_nesting
