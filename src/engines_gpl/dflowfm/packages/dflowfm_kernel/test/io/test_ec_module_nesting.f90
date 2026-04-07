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

   subroutine test_netcdf_waterlevel_write()
      !> Sample program demonstrating how to use initialize_his_waterlevel
      !> This creates a NetCDF file matching the structure of TestCoarse_his.nc

      use precision, only: dp
      use m_file_helpers

      implicit none

      ! Dimensions
      integer, parameter :: nstations = 6
      integer, parameter :: ntimes = 61  ! Only showing first few values in this example

      ! Variables
      character(len=256) :: station_names(nstations)
      real(kind=dp) :: station_x(nstations)
      real(kind=dp) :: station_y(nstations)
      real(kind=dp) :: time_values(ntimes)
      real(kind=dp) :: waterlevel(ntimes, nstations)
      character(len=*), parameter :: reference_time = "seconds since 2001-01-01 00:00:00 +00:00"
      character(len=*), parameter :: output_file = "TestCoarse_his_sample.nc"

      integer :: i, j

      ! ========================================
      ! Initialize station data
      ! ========================================

      ! Station names (3 shown in CDL, 3 more implied)
      station_names(1) = "left_0001"
      station_names(2) = "left_0002"
      station_names(3) = "left_0003"
      station_names(4) = "right_0001"  ! Inferred from data
      station_names(5) = "right_0002"  ! Inferred from data
      station_names(6) = "right_0003"  ! Inferred from data

      ! Station coordinates from CDL
      station_x = [50.0_dp, 50.0_dp, 50.0_dp, 101.0_dp, 101.0_dp, 101.0_dp]
      station_y = [150.0_dp, 220.0_dp, 250.0_dp, 250.0_dp, 320.0_dp, 350.0_dp]

      ! ========================================
      ! Initialize time values
      ! ========================================

      ! Time values: 0 to 300 seconds in 5-second intervals (61 values total)
      do i = 1, ntimes
         time_values(i) = real(i - 1, dp)*5.0_dp
      end do

      ! ========================================
      ! Initialize waterlevel data
      ! ========================================

      ! Initialize all to zero first
      waterlevel = 0.0_dp

      ! Time step 1 (t=0): all zeros
      waterlevel(1, :) = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

      ! Time step 2 (t=5s)
      waterlevel(2, :) = [0.03337553_dp, 0.03230887_dp, 0.03185172_dp, &
                          0.01863646_dp, 0.0181476_dp, 0.01763203_dp]

      ! Time step 3 (t=10s)
      waterlevel(3, :) = [0.08054169_dp, 0.07814353_dp, 0.07711574_dp, &
                          0.04953192_dp, 0.04813552_dp, 0.04690922_dp]

      ! Time step 4 (t=15s)
      waterlevel(4, :) = [0.1257198_dp, 0.1222559_dp, 0.1207714_dp, &
                          0.08543247_dp, 0.08289338_dp, 0.08102004_dp]

      ! Time step 5 (t=20s)
      waterlevel(5, :) = [0.1601923_dp, 0.1561175_dp, 0.1543712_dp, &
                          0.1196105_dp, 0.1159541_dp, 0.1136337_dp]

      ! Time step 6 (t=25s)
      waterlevel(6, :) = [0.1816541_dp, 0.1773752_dp, 0.1755414_dp, &
                          0.1471071_dp, 0.1426016_dp, 0.0_dp]  ! Last value missing in CDL, using 0

      ! For remaining time steps, use a simple pattern (or keep as zero)
      ! In a real scenario, you would have all 61 time steps of data
      do i = 7, ntimes
         do j = 1, nstations
            ! Simple increasing pattern for demonstration
            waterlevel(i, j) = waterlevel(i - 1, j) + 0.01_dp
         end do
      end do

      ! ========================================
      ! Create NetCDF file
      ! ========================================

      write (*, '(A)') "Creating NetCDF history file: "//output_file

      call initialize_his_waterlevel( &
         file_name=output_file, &
         station_names=station_names, &
         station_x=station_x, &
         station_y=station_y, &
         time_values=time_values, &
         waterlevel=waterlevel, &
         reference_time=reference_time)

      write (*, '(A)') "NetCDF file created successfully!"
      write (*, '(A,I0,A)') "  Stations: ", nstations, " stations"
      write (*, '(A,I0,A)') "  Time steps: ", ntimes, " time points"
      write (*, '(A)') "  Reference time: "//reference_time
      write (*, '(A)') ""
      write (*, '(A)') "To view the file, run:"
      write (*, '(A)') "  ncdump "//output_file

   end subroutine test_netcdf_waterlevel_write

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
         indices = [35.0_dp]
         do i = 1, size(indices)
            is_successful = ec_gettimespacevalue(ecInstancePtr, item_waterlevelbnd, 20010101, &
                                                 0.0_dp, 1, indices(i), target_array=results)
            deviation = 0.0_dp
            if (is_successful) then
               deviation = test_arrays_on_slope(ybndz, results, slope)
            end if
            print *, i, indices(i), "deviation =", deviation
            call f90_assert_near(deviation, tolerance, 1.0e-5_dp, "Results do not match expected linear slope within tolerance!")

         end do

      end block

   end subroutine test_basic
   !$f90tw)
end module test_ec_module_nesting
