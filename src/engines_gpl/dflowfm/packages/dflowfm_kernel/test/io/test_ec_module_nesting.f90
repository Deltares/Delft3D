module test_ec_module_nesting
   use precision
   use assertions_gtest
   use m_meteo, only: initialize_ec_module, ec_addtimespacerelation, ec_gettimespacevalue, ecInstancePtr, item_waterlevelbnd
   use fm_external_forcings, only: addtimespacerelation_boundaries
   implicit none

contains

   function test_arrays_on_slope(xarray, yarray, expected_slope) result(deviation_out)
      real(dp), dimension(:), intent(in) :: xarray
      real(dp), dimension(:), intent(in) :: yarray
      real(dp), intent(in) :: expected_slope
      real(dp) :: deviation_out

      integer :: n, j
      real(dp) :: sum_x, sum_y, sum_xy, sum_x2
      real(dp) :: calculated_slope, slope_deviation

      n = size(xarray)

      if (n < 2) then
         deviation_out = -1000.0_dp  ! Indicate invalid deviation
         return
      end if

      ! Calculate sums for linear regression
      sum_x = 0.0_dp
      sum_y = 0.0_dp
      sum_xy = 0.0_dp
      sum_x2 = 0.0_dp

      do j = 1, n
         sum_x = sum_x + xarray(j)
         sum_y = sum_y + yarray(j)
         sum_xy = sum_xy + xarray(j)*yarray(j)
         sum_x2 = sum_x2 + xarray(j)*xarray(j)
      end do

      ! Calculate slope using least squares linear regression: m = (n * Σ(xy) - Σx * Σy) / (n * Σ(x²) - (Σx)²)
      calculated_slope = (real(n, dp)*sum_xy - sum_x*sum_y)/(real(n, dp)*sum_x2 - sum_x*sum_x)

      slope_deviation = abs(calculated_slope - expected_slope)
      deviation_out = slope_deviation

      ! all f90_expect_true(slope_matches, trim(error_msg))
   end function test_arrays_on_slope


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
      xbndz = [105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp, 105.0_dp]
      ybndz = [255.0_dp, 265.0_dp, 275.0_dp, 285.0_dp, 295.0_dp, 305.0_dp, 315.0_dp, 325.0_dp, 335.0_dp, 345.0_dp]
      kdz = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      kx = 1
      allocate (xy2bndz(2, 10))
      xy2bndz(1, :) = 70.0_dp
      xy2bndz(2, :) = ybndz
      call initialize_ec_module()
      !   is_successful = addtimespacerelation_boundaries(qid, "nesting/TestFine_bnd.ext", filetype=9, method=3, &
      !                                                               operand='O', forcing_file='nesting/TestCoarse_his.nc')

      is_successful = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, 'nesting/boundary_left2.pli', &
                         filetype=9, method=3, operand='O', xyen=xy2bndz, forcingfile='nesting/TestCoarse_his.nc', dtnodal=dt_nodal)

      call f90_expect_true(is_successful, "Add time spacerelation failed!")

      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         real(dp) :: slope, intercept, tolerance, deviation

         ! Original boundary condition was something along: WaterLevel = Y * 10^-4 + 0.21 
         ! This will be shifted a bit (plus physical processes) but after both interpolations (writing, reading) 
         ! should still be close to a linear relationship with the same slope.

         slope = -1.0e-4_dp
         intercept = 0.21_dp
         tolerance = 4.0e-5_dp  ! Adjust tolerance as needed

         results = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp, 9.0_dp, 10.0_dp]
         indices = [ 35.0_dp]
         do i = 1, size(indices)
            is_successful = ec_gettimespacevalue(ecInstancePtr, item_waterlevelbnd, 20010101, &
                              0.0_dp, 1, indices(i), target_array=results)
            deviation = 0.0_dp
            if (is_successful) then
               deviation =  test_arrays_on_slope(ybndz, results, slope)
            end if
            print *, i, indices(i), "deviation =", deviation
            call f90_assert_near(deviation, tolerance, 1.0e-5_dp, "Results do not match expected linear slope within tolerance!")
            
         end do

      end block

   end subroutine test_basic
   !$f90tw)
end module test_ec_module_nesting
