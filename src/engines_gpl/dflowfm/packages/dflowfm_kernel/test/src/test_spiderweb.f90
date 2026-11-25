module test_spiderweb
   use assertions_gtest, only: f90_assert_true, f90_expect_eq, f90_expect_near, f90_expect_true
   use m_ec_module, only: ecFreeInstance
   use m_file_helpers, only: create_file
   use m_flowtimes, only: refdate_mjd, tunit, tzone
   use m_meteo, only: ecInstancePtr, ec_gettimespacevalue
   use m_teardown, only: t_teardown
   use precision, only: dp
   implicit none(type, external)
   private

   integer, parameter :: N_QUANTITIES = 3
   integer, parameter :: REFERENCE_DATE = 20000101
   character(len=*), parameter :: TIME_UNIT = 'hours since 2000-01-01 00:00:00 +00:00'

contains

   !$gtest
   subroutine test_spiderweb__evaluate_pressure_at_eye() bind(C)
      use m_meteo, only: item_apwxwy_p
      use m_wind, only: air_pressure, ec_pwxwy_x, ec_pwxwy_y

      character(len=*), parameter :: SPW_FILE = 'test_spiderweb_pressure_at_eye.spw'
      real(dp), dimension(2), parameter :: EYE = [4.0_dp, 52.0_dp] !< The eye is in the netherlands!
      real(dp), parameter :: BACKGROUND_PRESSURE = 101000.0_dp
      real(dp), parameter :: EXPECTED_PRESSURE_DROP = 1500.0_dp
      integer, parameter :: N_ROWS = 1, N_COLS = 4
      real(dp), dimension(N_ROWS * N_COLS), parameter :: RING_SPEED = 20.0_dp, RING_DIRECTION = 45.0_dp, RING_DROP = 400.0_dp
      real(dp), dimension(N_ROWS * N_COLS * N_QUANTITIES), parameter :: RING_DATA = [RING_SPEED, RING_DIRECTION, RING_DROP]
      type(t_teardown) :: teardown
      logical :: success

      allocate (air_pressure(1), ec_pwxwy_x(1), ec_pwxwy_y(1))
      teardown = t_teardown(teardown_evaluate_pressure_at_eye)
      call create_spiderweb_file( &
         SPW_FILE, N_ROWS, N_COLS, radius=100000.0_dp, &
         t0=0.0_dp, t1=24.0_dp, &
         eye_t0=[EYE, 1000.0_dp], eye_t1=[EYE, 3000.0_dp], &
         data_t0=RING_DATA, data_t1=RING_DATA)

      call initialize_spiderweb(SPW_FILE, 'airpressure_windx_windy', target_xy=EYE, success=success)
      if (.not. f90_assert_true(success, 'could not register spiderweb pressure and wind')) then
         return
      end if
      air_pressure = BACKGROUND_PRESSURE
      ec_pwxwy_x = 5.0_dp
      ec_pwxwy_y = -5.0_dp

      ! Evaluate wind and pressure drop at T0 + six hours (that's a quarter of the way between T0 and T1).
      success = ec_gettimespacevalue(ecInstancePtr, item_apwxwy_p, REFERENCE_DATE, tzone, tunit, 6.0_dp * 3600.0_dp)
      if (.not. f90_assert_true(success, 'spiderweb evaluation must succeed at the eye')) then
         return
      end if

      call f90_expect_eq(ec_pwxwy_x(1), 0.0_dp, 'x wind must be zero at the eye')
      call f90_expect_eq(ec_pwxwy_y(1), 0.0_dp, 'y wind must be zero at the eye')
      ! A quarter of the way from 1000 Pa to 3000 Pa gives a 1500 Pa drop.
      call f90_expect_near(air_pressure(1), BACKGROUND_PRESSURE - EXPECTED_PRESSURE_DROP, 1.0e-8_dp, &
                           'pressure at the eye must include the time-interpolated drop')
   end subroutine test_spiderweb__evaluate_pressure_at_eye

   !> teardown for test_spiderweb__evaluate_pressure_at_eye
   subroutine teardown_evaluate_pressure_at_eye()
      use m_wind, only: air_pressure, ec_pwxwy_x, ec_pwxwy_y

      if (associated(ecInstancePtr)) then
         call f90_expect_true(ecFreeInstance(ecInstancePtr), 'EC instance cleanup should succeed')
      end if
      deallocate (air_pressure, ec_pwxwy_x, ec_pwxwy_y)
   end subroutine teardown_evaluate_pressure_at_eye

   !$gtest
   subroutine test_spiderweb__evaluate_wind_north_of_eye() bind(C)
      use m_mathconstants, only: PI
      use m_meteo, only: item_windxy_x
      use m_wind, only: wx, wy

      character(len=*), parameter :: SPW_FILE = 'test_spiderweb_wind_north_of_eye.spw'
      real(dp), parameter :: METRES_PER_DEGREE = 6378137.0_dp * PI / 180.0_dp  !< Arclength in meters of one "earth-degree".
      real(dp), parameter :: TOLERANCE = 1.0e-8_dp
      real(dp), dimension(3), parameter :: EYE = 0.0_dp !< The eye is at null island.
      real(dp), dimension(2), parameter :: TARGET_XY = [0.0_dp, 0.25_dp] !< Target is a quarter degree to the north of null island.
      integer, parameter :: N_ROWS = 2, N_COLS = 8
      real(dp), dimension(N_ROWS * N_COLS), parameter :: SPEED_T0 = 20.0_dp, SPEED_T1 = 60.0_dp
      real(dp), dimension(N_ROWS * N_COLS), parameter :: RING_DIRECTION = 30.0_dp, RING_DROP = 0.0_dp
      type(t_teardown) :: teardown
      logical :: success

      allocate (wx(1), wy(1))
      teardown = t_teardown(teardown_evaluate_wind_north_of_eye)
      ! Spiderweb differs in speed at T0 and T1, but are otherwise fully uniform.
      call create_spiderweb_file( &
         SPW_FILE, N_ROWS, N_COLS, radius=METRES_PER_DEGREE, &
         t0=0.0_dp, t1=24.0_dp, &
         eye_t0=EYE, eye_t1=EYE, &
         data_t0=[SPEED_T0, RING_DIRECTION, RING_DROP], &
         data_t1=[SPEED_T1, RING_DIRECTION, RING_DROP])

      call initialize_spiderweb(SPW_FILE, 'windxy', target_xy=TARGET_XY, success=success)
      if (.not. f90_assert_true(success, 'could not register spiderweb wind')) then
         return
      end if
      wx = 0.0_dp
      wy = 0.0_dp

      ! Get windxy at time halfway between T0 and T1 (twelve hours after T0)
      success = ec_gettimespacevalue(ecInstancePtr, item_windxy_x, REFERENCE_DATE, tzone, tunit, 12.0_dp * 3600.0_dp)
      if (.not. f90_assert_true(success, 'spiderweb evaluation must succeed north of the eye')) then
         return
      end if

      ! Wind speed: Halfway in time gives 40 m/s on the first ring. Halfway to that ring gives 20 m/s. 
      ! Wind from direction: Uniformly 30 degrees on the spiderweb. So interpolated value is also 30 degrees.
      call f90_expect_near(wx(1), -10.0_dp, TOLERANCE, 'interpolated x wind')
      call f90_expect_near(wy(1), -10.0_dp * sqrt(3.0_dp), TOLERANCE, 'interpolated y wind')
   end subroutine test_spiderweb__evaluate_wind_north_of_eye

   !> teardown for test_spiderweb__evaluate_wind_north_of_eye
   subroutine teardown_evaluate_wind_north_of_eye()
      use m_wind, only: wx, wy

      if (associated(ecInstancePtr)) then
         call f90_expect_true(ecFreeInstance(ecInstancePtr), 'EC instance cleanup should succeed')
      end if
      deallocate (wx, wy)
   end subroutine teardown_evaluate_wind_north_of_eye

   !> Write a spiderweb file with two snapshots.
   subroutine create_spiderweb_file(file_name, n_rows, n_cols, radius, t0, t1, eye_t0, eye_t1, data_t0, data_t1)
      use string_module, only: int2str, real2stringLeft

      character(len=*), intent(in) :: file_name
      integer, intent(in) :: n_rows !< Number of 'rings' of the spiderweb.
      integer, intent(in) :: n_cols !< Number of 'spokes' of the spiderweb.
      real(dp), intent(in) :: radius !< Radius of the spiderweb.
      real(dp), intent(in) :: t0, t1 !< Snapshot times in hours relative to the reference date.
      real(dp), dimension(3), intent(in) :: eye_t0, eye_t1 !< x, y, and pressure drop at the eye.
      real(dp), dimension(n_cols * n_rows * N_QUANTITIES), intent(in) :: data_t0, data_t1 !< Speed, direction, and pressure drop blocks.

      integer, parameter :: HEADER_SIZE = 15, NUMBER_WIDTH = 32
      character(len=*), parameter :: REAL_FMT = 'g32.17e3'
      integer :: line_length, snapshot_size
      character(len=NUMBER_WIDTH) :: number
      character(len=:), dimension(:), allocatable :: lines

      line_length = max(64, 40 + len(TIME_UNIT), (NUMBER_WIDTH + 1) * n_cols)
      snapshot_size = 4 + N_QUANTITIES * n_rows
      allocate (character(len=line_length) :: lines(HEADER_SIZE + 2 * snapshot_size))

      ! Write header
      call real2stringLeft(number, '('//REAL_FMT//')', radius)
      lines(:HEADER_SIZE) = [character(len=80) :: &
         'FileVersion = 1.03', &
         'filetype = meteo_on_spiderweb_grid', &
         'NODATA_value = -999', &
         'grid_unit = degree', &
         'spw_rad_unit = m', &
         'n_rows = '//trim(int2str(n_rows)), &
         'n_cols = '//trim(int2str(n_cols)), &
         'spw_radius = '//trim(number), &
         'n_quantity = '//trim(int2str(N_QUANTITIES)), &
         'quantity1 = wind_speed', &
         'quantity2 = wind_from_direction', &
         'quantity3 = p_drop', &
         'unit1 = m s-1', &
         'unit2 = degree', &
         'unit3 = Pa']

      ! Write timeseries
      call write_snapshot(lines(HEADER_SIZE + 1:HEADER_SIZE + snapshot_size), t0, eye_t0, data_t0)
      call write_snapshot(lines(HEADER_SIZE + snapshot_size + 1:), t1, eye_t1, data_t1)

      call create_file(file_name, lines)

   contains

      !> Write the snapshot lines to the spiderweb files.
      subroutine write_snapshot(snapshot, hours, eye, data)
         character(len=*), dimension(:), intent(out) :: snapshot
         real(dp), intent(in) :: hours
         real(dp), dimension(3), intent(in) :: eye
         real(dp), dimension(n_cols * n_rows * N_QUANTITIES), intent(in) :: data
         integer :: line, quantity, row, first, last
         character(len=NUMBER_WIDTH) :: time_string

         call real2stringLeft(time_string, '('//REAL_FMT//')', hours)
         snapshot(1) = 'TIME = '//trim(time_string)//' '//TIME_UNIT
         write (snapshot(2), '(a,'//REAL_FMT//')') 'x_spw_eye = ', eye(1)
         write (snapshot(3), '(a,'//REAL_FMT//')') 'y_spw_eye = ', eye(2)
         write (snapshot(4), '(a,'//REAL_FMT//')') 'p_drop_spw_eye = ', eye(3)
         line = 5
         do quantity = 1, N_QUANTITIES
            do row = 1, n_rows
               first = ((quantity - 1) * n_rows + row - 1) * n_cols + 1
               last = first + n_cols - 1
               write (snapshot(line), '(*('//REAL_FMT//',:,1x))') data(first:last)
               line = line + 1
            end do
         end do
      end subroutine write_snapshot
   end subroutine create_spiderweb_file

   !> Initialize the ec-module and add a spiderweb time-space relation.
   subroutine initialize_spiderweb(filename, quantity, target_xy, success)
      use m_meteo, only: ec_addtimespacerelation, initialize_ec_module
      use m_sferic, only: jsferic
      use time_module, only: ymd2modified_jul
      use timespace_parameters, only: OPERAND_OVERRIDE, SPIDERWEB, WEIGHTFACTORS

      character(len=*), intent(in) :: filename, quantity
      real(dp), dimension(2), intent(in) :: target_xy
      logical, intent(out) :: success

      success = ymd2modified_jul(REFERENCE_DATE, refdate_mjd)
      if (.not. success) then
         return
      end if
      tzone = 0.0_dp
      jsferic = 1
      call initialize_ec_module()
      success = ec_addtimespacerelation( &
         quantity, target_xy(1:1), target_xy(2:2), [1], 1, filename, &
         SPIDERWEB, WEIGHTFACTORS, OPERAND_OVERRIDE)
   end subroutine initialize_spiderweb

end module test_spiderweb
