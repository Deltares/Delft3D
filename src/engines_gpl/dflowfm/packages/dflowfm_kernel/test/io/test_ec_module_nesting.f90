module test_ec_module_nesting
   use precision
   use assertions_gtest
   use m_meteo, only: initialize_ec_module, ec_addtimespacerelation, ec_gettimespacevalue, ecInstancePtr, item_waterlevelbnd
   use fm_external_forcings, only: addtimespacerelation_boundaries
   use m_file_helpers, only: initialize_his_2d_scalar_quantity, create_file
   implicit none

   character(len=*), parameter :: PLI_FILENAME = "boundary_l2.pli"
   character(len=*), parameter :: NC_FILENAME = "boundary_l2.nc"
   real(dp) :: slope = -0.002_dp, intercept = 0.4_dp

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
   end function test_arrays_on_slope

   subroutine initialize_netcdf() 
      use precision, only: dp
      use cwd

      implicit none

      ! Dimensions
      integer, parameter :: nstations = 3
      integer, parameter :: ntimes = 3  ! Only showing first few values in this example

      ! Variables
      character(len=256) :: station_names(nstations)
      real(kind=dp) :: station_x(nstations)
      real(kind=dp) :: station_y(nstations)
      real(kind=dp) :: time_values(ntimes)
      real(kind=dp) :: waterlevel(ntimes, nstations)
      character(len=*), parameter :: reference_time = "seconds since 2001-01-01 00:00:00 +00:00"

      character(len=1024) :: current_directory

      integer :: i, j
      integer :: status

      ! ========================================
      ! Initialize station data
      ! ========================================

      ! Station names (3 shown in CDL, 3 more implied)
      station_names(1) = "left_0001"  ! Inferred from data
      station_names(2) = "left_0002"  ! Inferred from data
      station_names(3) = "left_0003"  ! Inferred from data

      ! Station coordinates from CDL
      station_x = [101.0_dp, 101.0_dp, 101.0_dp]
      station_y = [250.0_dp, 320.0_dp, 350.0_dp]

      ! ========================================
      ! Initialize time values
      ! ========================================

      ! Time values: 0 to 10 seconds in 5-second intervals (3 values total)
      do i = 1, ntimes
         time_values(i) = real(i - 1, dp)*5.0_dp
      end do

      ! ========================================
      ! Initialize waterlevel data
      ! ========================================

      waterlevel = 0.0_dp
      ! Time step 1 (t=0):
      waterlevel(1, :) = [0.0_dp, 0.0_dp, 0.0_dp]
      ! Time step 2 (t=5s)
      waterlevel(2, :) = station_y * slope + intercept  ! Linear relationship with y-coordinate
      ! Time step 3 (t=10s)
      waterlevel(3, :) = station_y * slope + intercept  ! Linear relationship with y-coordinate

      call initialize_his_2d_scalar_quantity( &
         file_name=NC_FILENAME, &
         station_names=station_names, &
         station_x=station_x, &
         station_y=station_y, &
         time_values=time_values, &
         quantity_name="waterlevel", &
         quantities=waterlevel, &
         reference_time=reference_time)
   end subroutine initialize_netcdf

   !$f90tw TESTCODE(TEST, test_ec_module_nesting, test_2d_space_interpolation, test_2d_space_interpolation,
   subroutine test_2d_space_interpolation() bind(C)
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
      call initialize_netcdf()
      call create_file(PLI_FILENAME, [ &
                       "left", &
                       "        3 2", &
                       "        101         250", &
                       "        101         320", &
                       "        101         350"])

      is_successful = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, PLI_FILENAME, &
                         filetype=9, method=3, operand='O', xyen=xy2bndz, forcingfile=NC_FILENAME, dtnodal=dt_nodal)
      call f90_expect_true(is_successful, "Add time spacerelation failed!")

      block
         integer :: i
         real(dp), dimension(:), allocatable :: indices, results, expected_results
         real(dp) :: deviation

         results = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp, 9.0_dp, 10.0_dp]
         is_successful = ec_gettimespacevalue(ecInstancePtr, item_waterlevelbnd, 20010101, &
                                                0.0_dp, 1, 5.0_dp, target_array=results)
         deviation = 0.0_dp
         if (is_successful) then
            deviation = test_arrays_on_slope(ybndz, results, slope)
         end if
         call f90_assert_near(deviation, 0.0_dp, 1.0e-5_dp, "Results do not match expected linear slope within tolerance!")
      end block
   end subroutine test_2d_space_interpolation
   !$f90tw)
end module test_ec_module_nesting
