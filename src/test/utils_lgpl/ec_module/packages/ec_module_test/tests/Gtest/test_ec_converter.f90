module test_ec_converter
   use assertions_gtest
   use ec_module_helper, only: ec_connection_builder, spiderweb_data
   use m_ec_converter, only: ecConverterPerformConversions
   use m_ec_typedefs, only: tEcConnection
   use m_mathconstants, only: PI
   use precision, only: dp
   use time_class, only: c_time
   use time_module, only: ymd2modified_jul
   implicit none
   private

contains

   !> Fill each source quantity with a constant value across the grid.
   function make_uniform_spiderweb(n_rows, n_cols, radius, eye, values) result(snapshot)
      integer, intent(in) :: n_rows, n_cols
      real(dp), intent(in) :: radius
      real(dp), dimension(2), intent(in) :: eye
      real(dp), dimension(:), intent(in) :: values
      type(spiderweb_data) :: snapshot

      snapshot = spiderweb_data(n_rows=n_rows, n_cols=n_cols, radius=radius, eye=eye, &
                               data=spread(values, dim=1, ncopies=n_rows * n_cols))
   end function make_uniform_spiderweb

   !> Return the value at the first evaluation point of a target item.
   pure function target_value(connection, target_item_index) result(value)
      type(tEcConnection), intent(in) :: connection
      integer, intent(in) :: target_item_index
      real(dp) :: value

      value = connection%targetItemsPtr(target_item_index)%ptr%targetFieldPtr%arr1dPtr(1)
   end function target_value

   !$gtest
   subroutine test_spiderweb_at_eye() bind(C)
      integer, parameter :: N_ROWS = 2, N_COLS = 5
      real(dp), dimension(2), parameter :: EYE = [4.0_dp, 52.0_dp]
      real(dp), parameter :: RADIUS = 100000.0_dp
      real(dp), parameter :: BACKGROUND_PRESSURE = 101000.0_dp
      real(dp), parameter :: WIND_SPEED = 20.0_dp, WIND_DIRECTION = 45.0_dp
      real(dp), parameter :: OFF_EYE_PRESSURE_DROP = 400.0_dp
      real(dp), parameter :: EYE_PRESSURE_DROP_T0 = 1000.0_dp, EYE_PRESSURE_DROP_T1 = 3000.0_dp
      real(dp), parameter :: EXPECTED_PRESSURE_DROP = 1500.0_dp
      real(dp) :: t0
      type(c_time) :: time
      type(spiderweb_data) :: spw_t0, spw_t1
      type(ec_connection_builder) :: builder
      type(tEcConnection), pointer :: connection
      logical :: success

      ! Arrange
      success = ymd2modified_jul(20261126, t0)

      ! T0: Spiderweb with uniform `windspeed` and `winddirection`. `p_drop` is uniform accept at the eye.
      spw_t0 = make_uniform_spiderweb(N_ROWS, N_COLS, RADIUS, EYE, [WIND_SPEED, WIND_DIRECTION, OFF_EYE_PRESSURE_DROP])
      spw_t0%data(:N_COLS, 3) = EYE_PRESSURE_DROP_T0

      ! T1: Same spiderweb as T0, except `p_drop` is higher at the eye.
      spw_t1 = spw_t0
      spw_t1%data(:N_COLS, 3) = EYE_PRESSURE_DROP_T1

      connection => builder%build_spiderweb_connection( &
         source_names=[character(len=13) :: 'windspeed', 'winddirection', 'p_drop'], &
         target_name='airpressure_windx_windy', target_xy=EYE, &
         t0=t0, t1=t0 + 1.0_dp, spw_t0=spw_t0, spw_t1=spw_t1, &
         initial_values=[5.0_dp, -5.0_dp, BACKGROUND_PRESSURE])
      if (.not. f90_assert_true(associated(connection), builder%get_error_message())) then
         return
      end if

      ! Evaluate target items a quarter day after T0.
      call time%set(t0 + 0.25_dp)

      ! Act
      success = ecConverterPerformConversions(connection, time)

      ! Assert
      if (.not. f90_assert_true(success, 'spiderweb conversion must succeed at the eye')) then
         return
      end if
      call f90_expect_eq(target_value(connection, 1), 0.0_dp, 'x wind must be zero at the eye')
      call f90_expect_eq(target_value(connection, 2), 0.0_dp, 'y wind must be zero at the eye')
      ! A quarter of the way from 1000 Pa to 3000 Pa gives a 1500 Pa drop.
      call f90_expect_near(target_value(connection, 3), BACKGROUND_PRESSURE - EXPECTED_PRESSURE_DROP, 1.0e-8_dp, &
                           'pressure at the eye must include the time-interpolated drop')
   end subroutine test_spiderweb_at_eye

   !$gtest
   subroutine test_spiderweb_wind_only_off_eye() bind(C)
      integer, parameter :: N_ROWS = 3, N_COLS = 9
      real(dp), parameter :: METRES_PER_DEGREE = 6378137.0_dp * PI / 180.0_dp !< The arc length of a one degree angle on earth's surface.
      real(dp), parameter :: TOLERANCE = 1.0e-8_dp
      real(dp), dimension(2), parameter :: EYE = [0.0_dp, 0.0_dp]
      real(dp), dimension(2), parameter :: TARGET_XY = [0.0_dp, 0.25_dp] !< Target is a quarter degree to the north of the eye.
      real(dp), parameter :: WIND_DIRECTION = 30.0_dp
      real(dp), parameter :: RING_WIND_SPEED_T0 = 20.0_dp, RING_WIND_SPEED_T1 = 60.0_dp
      real(dp) :: t0
      type(c_time) :: time
      type(spiderweb_data) :: spw_t0, spw_t1
      type(ec_connection_builder) :: builder
      type(tEcConnection), pointer :: connection
      logical :: success

      ! Arrange
      success = ymd2modified_jul(20261120, t0)

      ! T0: Uniform `winddirection`. `windspeed` is uniform except at the eye, where it is zero.
      spw_t0 = make_uniform_spiderweb(N_ROWS, N_COLS, METRES_PER_DEGREE, EYE, [WIND_DIRECTION, RING_WIND_SPEED_T0])
      spw_t0%data(:N_COLS, 2) = 0.0_dp

      ! T1: Same as T0, except the `windspeed` is higher by some constant factor.
      spw_t1 = make_uniform_spiderweb(N_ROWS, N_COLS, METRES_PER_DEGREE, EYE, [WIND_DIRECTION, RING_WIND_SPEED_T1])
      spw_t1%data(:N_COLS, 2) = 0.0_dp

      connection => builder%build_spiderweb_connection( &
         source_names=[character(len=13) :: 'winddirection', 'windspeed'], &
         target_name='windxy', target_xy=TARGET_XY, &
         t0=t0, t1=t0 + 1.0_dp, spw_t0=spw_t0, spw_t1=spw_t1, &
         initial_values=[0.0_dp, 0.0_dp])
      if (.not. f90_assert_true(associated(connection), builder%get_error_message())) then
         return
      end if

      ! Evaluate target items at time halfway between t0 and t1
      call time%set(t0 + 0.5_dp)  ! At this time, the wind speed is (RING_WIND_SPEED_T0 + RING_WIND_SPEED_T1) / 2 (m/s)

      ! Act
      success = ecConverterPerformConversions(connection, time)

      ! Assert
      if (.not. f90_assert_true(success, 'windxy conversion failed')) then
         return
      end if
      ! Halfway between the eye and first ring gives windspeed 20 m/s. The wind direction is 30 degrees everywhere.
      call f90_expect_near(target_value(connection, 1), -10.0_dp, TOLERANCE, 'interpolated x wind') ! -20 * sin(30 deg)
      call f90_expect_near(target_value(connection, 2), -10.0_dp * sqrt(3.0_dp), TOLERANCE, 'interpolated y wind') ! -20 * cos(30 deg)
   end subroutine test_spiderweb_wind_only_off_eye

end module test_ec_converter
