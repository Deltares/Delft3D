module test_unstruc_model
   use assertions_gtest
   use precision, only: dp
   use unstruc_model, only: set_time_interval, read_solver_sequence
   implicit none

   integer, parameter :: MESSAGE_BUFFER_SIZE = 256
contains

   !$gtest
   subroutine test_abort_threshold_restored_on_scope_exit() bind(C)
      use unstruc_messages, only: threshold_abort
      integer :: original_threshold

      original_threshold = threshold_abort
      call temporary_abort_threshold(.true.)
      call f90_expect_eq(threshold_abort, original_threshold, "Early return must restore the threshold")
      call temporary_abort_threshold(.false.)
      call f90_expect_eq(threshold_abort, original_threshold, "Repeated changes must restore the entry threshold")
   end subroutine test_abort_threshold_restored_on_scope_exit

   subroutine temporary_abort_threshold(return_early)
      use m_message_helpers, only: t_abort_threshold_guard
      use messagehandling, only: LEVEL_WARN, LEVEL_FATAL
      logical, intent(in) :: return_early
      type(t_abort_threshold_guard) :: abort_threshold

      call abort_threshold%set(LEVEL_WARN)
      if (return_early) then
         return
      end if
      call abort_threshold%set(LEVEL_FATAL)
   end subroutine temporary_abort_threshold

   !$gtest
   subroutine test_resetfullflowmodel_cleans_boundary_storage() bind(C)
      use m_resetfullflowmodel, only: resetfullflowmodel
      use fm_external_forcings_data, only: kbndz, xbndz, ybndz, xy2bndz, zbndz, zbndz0, kdz, zkbndz, kbanz
      integer :: partial

      call resetfullflowmodel()
      do partial = 0, 1
         allocate (xbndz(1), ybndz(1), xy2bndz(2, 1), zbndz(1), zbndz0(1), kdz(1), zkbndz(2, 1), kbanz(2, 1))
         if (partial == 0) then
            allocate (kbndz(6, 1))
         end if
         call resetfullflowmodel()
         call f90_expect_false(allocated(kbndz) .or. allocated(xbndz) .or. allocated(ybndz) .or. &
                               allocated(xy2bndz) .or. allocated(zbndz) .or. allocated(zbndz0) .or. &
                               allocated(kdz) .or. allocated(zkbndz) .or. allocated(kbanz), &
                               "Full reset must release complete and partial boundary storage")
      end do
      call resetfullflowmodel()
   end subroutine test_resetfullflowmodel_cleans_boundary_storage

   !$gtest
   subroutine test_resetfullflowmodel_releases_output_sets() bind(C)
      use m_resetfullflowmodel, only: resetfullflowmodel
      use m_statistical_output, only: realloc
      use fm_statistical_output, only: out_variable_set_his, out_variable_set_map, out_variable_set_clm

      call resetfullflowmodel()
      call realloc(out_variable_set_his)
      call realloc(out_variable_set_map)
      call realloc(out_variable_set_clm)
      call resetfullflowmodel()
      call resetfullflowmodel()
      call f90_expect_false(allocated(out_variable_set_his%statout), "History output must not retain old data pointers")
      call f90_expect_false(allocated(out_variable_set_map%statout), "Map output must not retain old data pointers")
      call f90_expect_false(allocated(out_variable_set_clm%statout), "Class-map output must not retain old data pointers")
   end subroutine test_resetfullflowmodel_releases_output_sets

   !$gtest
   subroutine test_polygon_reinitialization_after_cleanup() bind(C)
      use m_polygon, only: increasepol, m_polygon_destructor, xpl, ypl, zpl, nampli
      use m_alloc, only: allocSize, realloc
      integer :: ierr

      ierr = m_polygon_destructor()
      call f90_expect_eq(ierr, 0)
      call increasepol(2, 0)
      call f90_expect_true(min(allocSize(xpl), allocSize(ypl), allocSize(zpl)) >= 2, &
                           "Polygon storage must be reusable after destruction")
      call realloc(nampli, 1, fill="old polygon", keepExisting=.false.)
      ierr = m_polygon_destructor()
      call f90_expect_eq(ierr, 0)
      call f90_expect_false(allocated(nampli), "Polygon names must be released with their geometry")
      call increasepol(0, 0)
      if (f90_assert_true(allocated(xpl) .and. allocated(ypl) .and. allocated(zpl), &
                          "A zero-point request must allocate the coordinates")) then
         call f90_expect_eq(size(xpl) + size(ypl) + size(zpl), 0, "A zero-point request must allocate no points")
      end if
   end subroutine test_polygon_reinitialization_after_cleanup

   !$gtest
   subroutine test_repeated_inidat_preserves_storage_capacity() bind(C)
      use m_inidat, only: inidat, jaSkipCmdLineArgs
      use m_splines, only: xsp
      use m_landboundary, only: xlan

      integer :: saved_skip_args, num_splines, num_points, num_land_points

      saved_skip_args = jaSkipCmdLineArgs
      jaSkipCmdLineArgs = 1
      call inidat()
      num_splines = size(xsp, 1)
      num_points = size(xsp, 2)
      num_land_points = size(xlan)

      call inidat()
      jaSkipCmdLineArgs = saved_skip_args

      call f90_expect_eq(size(xsp, 1), num_splines, "Repeated initialization must not grow spline capacity")
      call f90_expect_eq(size(xsp, 2), num_points, "Repeated initialization must not grow spline point capacity")
      call f90_expect_eq(size(xlan), num_land_points, "Repeated initialization must not grow land-boundary capacity")
   end subroutine test_repeated_inidat_preserves_storage_capacity

   !$gtest
   subroutine test_resize_spline_capacity_grows_only_the_requested_dimension() bind(C)
      use m_missing, only: dxymis
      use m_splines, only: resize_spline_capacity, spline_capacity, points_per_spline_capacity, &
                          xsp, ysp, xsp2, ysp2, lensp, lensp2

      real(kind=dp), parameter :: point = 42.0_dp
      real(kind=dp), allocatable, dimension(:, :) :: saved_xsp, saved_ysp, saved_xsp2, saved_ysp2
      integer, allocatable, dimension(:) :: saved_lensp, saved_lensp2
      integer :: saved_spline_capacity, saved_points_per_spline_capacity, num_splines, num_points
      integer, dimension(2) :: grown_shape

      saved_spline_capacity = spline_capacity
      saved_points_per_spline_capacity = points_per_spline_capacity
      call move_alloc(xsp, saved_xsp)
      call move_alloc(ysp, saved_ysp)
      call move_alloc(xsp2, saved_xsp2)
      call move_alloc(ysp2, saved_ysp2)
      call move_alloc(lensp, saved_lensp)
      call move_alloc(lensp2, saved_lensp2)

      call resize_spline_capacity(1, 1)
      if (f90_assert_true(allocated(xsp) .and. allocated(ysp) .and. allocated(xsp2) .and. allocated(ysp2) .and. &
                          allocated(lensp) .and. allocated(lensp2), "A small request must allocate spline storage")) then
         num_splines = size(xsp, 1)
         num_points = size(xsp, 2)
         xsp(1, 1) = point

         call resize_spline_capacity(spline_capacity, points_per_spline_capacity)
         call f90_expect_true(all(shape(xsp) == [num_splines, num_points]), "An exact fit must not grow storage")

         call resize_spline_capacity(num_splines, num_points + 1)
         grown_shape = shape(xsp)
         call f90_expect_eq(grown_shape(1), num_splines, "Growing point storage must not grow the spline count")
         if (f90_assert_true(grown_shape(2) >= num_points + 1, "Point storage must fit the new point")) then
            call f90_expect_near(xsp(1, num_points + 1), dxymis, 0.0_dp, "New coordinate entries must be initialized")
         end if
         call f90_expect_near(xsp(1, 1), point, 0.0_dp, "Growing storage must preserve existing points")
         call f90_expect_true(all(shape(ysp) == grown_shape) .and. all(shape(xsp2) == grown_shape) .and. &
                              all(shape(ysp2) == grown_shape) .and. size(lensp) == num_splines .and. &
                              size(lensp2) == num_splines, "Live and backup spline buffers must stay in sync")
      end if

      call move_alloc(saved_xsp, xsp)
      call move_alloc(saved_ysp, ysp)
      call move_alloc(saved_xsp2, xsp2)
      call move_alloc(saved_ysp2, ysp2)
      call move_alloc(saved_lensp, lensp)
      call move_alloc(saved_lensp2, lensp2)
      spline_capacity = saved_spline_capacity
      points_per_spline_capacity = saved_points_per_spline_capacity
   end subroutine test_resize_spline_capacity_grows_only_the_requested_dimension

   !$gtest
   subroutine test_zeronet_preserves_saved_edge_lengths() bind(C)
      use gridoperations, only: savenet, restore
      use m_alloc, only: realloc
      use m_inidat, only: inidat, jaSkipCmdLineArgs
      use m_missing, only: dmiss
      use m_zeronet, only: zeronet
      use network_data, only: dxe, dxe0
      use m_dimens, only: lmax

      real(kind=dp), parameter :: prescribed_length = 250.0_dp
      integer :: saved_skip_args

      saved_skip_args = jaSkipCmdLineArgs
      jaSkipCmdLineArgs = 1
      call inidat()
      jaSkipCmdLineArgs = saved_skip_args
      call realloc(dxe, lmax, fill=dmiss, keepExisting=.false.)
      dxe(1) = prescribed_length
      call savenet()

      call zeronet()
      call f90_expect_near(dxe(1), dmiss, 0.0_dp, "The temporary network must not inherit prescribed lengths")
      call restore()
      call f90_expect_near(dxe(1), prescribed_length, 0.0_dp, "Restoring the network must recover its prescribed lengths")

      deallocate (dxe, dxe0)
   end subroutine test_zeronet_preserves_saved_edge_lengths

   !$gtest
   subroutine test_set_time_interval__default_simulation_start_stop() bind(C)
      character(len=*), parameter :: INTERVAL_NAME = "FooInterval"
      real(kind=dp), parameter :: SIMULATION_START = 7.0_dp, SIMULATION_STOP = 42.0_dp

      real(kind=dp) :: interval_input(3)
      real(kind=dp) :: start, step, end_

      interval_input = [7.0_dp, 0.0_dp, 0.0_dp] ! step, start, end

      call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .true., interval_name=INTERVAL_NAME)

      call f90_expect_double_eq(start, SIMULATION_START)
      call f90_expect_double_eq(step, 7.0_dp)
      call f90_expect_double_eq(end_, SIMULATION_STOP)
   end subroutine test_set_time_interval__default_simulation_start_stop

   !$gtest
   subroutine test_solver_sequence_periods() bind(C)
      use m_flowparameters, only: solver_sequence, FLOW_SOLVER_FM, FLOW_SOLVER_FROZEN_1D2D
      use m_flowtimes, only: dt_user, tstart_user, tstop_user, tfac

      integer :: unit, status
      character(len=*), parameter :: filename = 'test_solver_sequence_periods.ini'

      tfac = 3600.0_dp
      dt_user = 1800.0_dp
      tstart_user = 0.0_dp
      tstop_user = 10800.0_dp
      open(newunit=unit, file=filename, status='replace')
      write(unit, '(a)') '[General]', 'fileType = solverSequence', 'fileVersion = 1.0', &
         '[Period]', 'tStart = 0', '[Period]', 'tStart = 1', 'flowSolver = generic1d2d3d', &
         'RestartFile = state_map.nc', 'RestartDateTime = 20250101010000', &
         'useRestartBedlevel = true', 'useRestartBedComp = true', &
         '[Period]', 'tStart = 2', 'flowSolver = frozen1d2d'
      close(unit)

      call read_solver_sequence(filename, status)
      call F90_EXPECT_EQ(status, 0)
      if (status == 0) then
         call F90_EXPECT_EQ(size(solver_sequence), 3)
         call F90_EXPECT_DOUBLE_EQ(solver_sequence(2)%tstart, 3600.0_dp)
         call F90_EXPECT_EQ(solver_sequence(1)%solver, FLOW_SOLVER_FROZEN_1D2D)
         call F90_EXPECT_EQ(solver_sequence(2)%solver, FLOW_SOLVER_FM)
         call F90_EXPECT_EQ(solver_sequence(3)%solver, FLOW_SOLVER_FROZEN_1D2D)
         call F90_EXPECT_TRUE(trim(solver_sequence(2)%restart_file) == 'state_map.nc')
         call F90_EXPECT_TRUE(trim(solver_sequence(2)%restart_date_time) == '20250101010000')
         call F90_EXPECT_FALSE(solver_sequence(1)%use_restart_bedlevel)
         call F90_EXPECT_TRUE(solver_sequence(2)%use_restart_bedlevel)
         call F90_EXPECT_FALSE(solver_sequence(3)%use_restart_bedlevel)
         call F90_EXPECT_FALSE(solver_sequence(1)%use_restart_bedcomp)
         call F90_EXPECT_TRUE(solver_sequence(2)%use_restart_bedcomp)
         call F90_EXPECT_FALSE(solver_sequence(3)%use_restart_bedcomp)
         deallocate(solver_sequence)
      end if
      open(newunit=unit, file=filename, status='old')
      close(unit, status='delete')
   end subroutine test_solver_sequence_periods

   !$gtest
   subroutine test_solver_sequence_off_grid_period() bind(C)
      use m_flowparameters, only: solver_sequence
      use m_flowtimes, only: dt_user, tstart_user, tstop_user, tfac

      integer :: unit, status
      character(len=*), parameter :: filename = 'test_solver_sequence_invalid.ini'

      tfac = 3600.0_dp
      dt_user = 1800.0_dp
      tstart_user = 0.0_dp
      tstop_user = 10800.0_dp
      open(newunit=unit, file=filename, status='replace')
      write(unit, '(a)') '[General]', 'fileType = solverSequence', 'fileVersion = 1.0', &
         '[Period]', 'tStart = 0', '[Period]', 'tStart = 0.75'
      close(unit)

      call read_solver_sequence(filename, status)
      call F90_EXPECT_TRUE(status /= 0)
      call F90_EXPECT_FALSE(allocated(solver_sequence))
      open(newunit=unit, file=filename, status='old')
      close(unit, status='delete')
   end subroutine test_solver_sequence_off_grid_period

   !$gtest
   subroutine test_set_time_interval__dont_read_interval_input() bind(C)
      real(kind=dp), parameter :: SIMULATION_START = 7.0_dp, SIMULATION_STOP = 42.0_dp

      real(kind=dp) :: interval_input(3)
      real(kind=dp) :: start, step, end_

      interval_input = [7.0_dp, 14.0_dp, 35.0_dp] ! step, start, end

      call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .false.)

      call f90_expect_double_eq(start, SIMULATION_START)
      call f90_expect_double_eq(end_, SIMULATION_STOP)
   end subroutine test_set_time_interval__dont_read_interval_input

   !$gtest
   subroutine test_set_time_interval__start_stop_out_of_bounds() bind(C)
      use MessageHandling, only: SetMessageHandling, GetMessageCount, GetMessage_MH, LEVEL_WARN
      character(len=*), parameter :: INTERVAL_NAME = "FooInterval"
      real(kind=dp), parameter :: SIMULATION_START = 14.0_dp, SIMULATION_STOP = 35.0_dp

      integer :: log_level
      character(len=MESSAGE_BUFFER_SIZE) :: message

      real(kind=dp) :: interval_input(3)
      real(kind=dp) :: start, step, end_

      ! Arrange
      interval_input = [7.0_dp, 7.0_dp, 42.0_dp] ! step, start, end
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)

      ! Act
      call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .true., interval_name=INTERVAL_NAME)

      ! Assert
      call f90_expect_double_eq(start, 14.0_dp)
      call f90_expect_double_eq(step, 7.0_dp)
      call f90_expect_double_eq(end_, 35.0_dp)

      call f90_expect_eq(GetMessageCount(), 2, "Expected two messages to be written to the log")

      ! First log line is about the out-of-bounds interval start
      log_level = GetMessage_MH(1, message)
      call f90_expect_eq(log_level, LEVEL_WARN, "Log level of interval start message must be WARNING")
      call f90_expect_true(index(message, "Setting FooInterval start time to TStart") > 0, "Unexpected interval start log line")

      ! Second log line is about the out-of-bounds interval end
      log_level = GetMessage_MH(2, message)
      call f90_expect_eq(log_level, LEVEL_WARN, "Log level of interval stop message must be WARNING")
      call f90_expect_true(index(message, "Setting FooInterval stop time to TStop") > 0, "Unexpected interval stop log line")
   end subroutine test_set_time_interval__start_stop_out_of_bounds
end module test_unstruc_model
