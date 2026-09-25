module test_unstruc_model
    use assertions_gtest
    use precision, only: dp
    use unstruc_model, only: set_time_interval, read_solver_sequence
    use iso_c_binding, only: c_null_char
    implicit none

    integer, parameter :: MESSAGE_BUFFER_SIZE = 256
contains

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_set_time_interval__default_simulation_start_stop, test_set_time_interval__default_simulation_start_stop,
    subroutine test_set_time_interval__default_simulation_start_stop() bind(C)
        character(len=*), parameter :: INTERVAL_NAME = "FooInterval"
        real(kind=dp), parameter :: SIMULATION_START = 7.0_dp, SIMULATION_STOP = 42.0_dp

        real(kind=dp) :: interval_input(3)
        real(kind=dp) :: start, step, end_

        interval_input = [7.0_dp, 0.0_dp, 0.0_dp] ! step, start, end

        call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .true., interval_name=INTERVAL_NAME)

        call F90_EXPECT_DOUBLE_EQ(start, SIMULATION_START)
        call F90_EXPECT_DOUBLE_EQ(step, 7.0_dp)
        call F90_EXPECT_DOUBLE_EQ(end_, SIMULATION_STOP)
    end subroutine test_set_time_interval__default_simulation_start_stop
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_solver_sequence_periods, test_solver_sequence_periods,
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
            deallocate(solver_sequence)
        end if
        open(newunit=unit, file=filename, status='old')
        close(unit, status='delete')
    end subroutine test_solver_sequence_periods
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_solver_sequence_off_grid_period, test_solver_sequence_off_grid_period,
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
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_set_time_interval__dont_read_interval_input, test_set_time_interval__dont_read_interval_input,
    subroutine test_set_time_interval__dont_read_interval_input() bind(C)
        real(kind=dp), parameter :: SIMULATION_START = 7.0_dp, SIMULATION_STOP = 42.0_dp

        real(kind=dp) :: interval_input(3)
        real(kind=dp) :: start, step, end_

        interval_input = [7.0_dp, 14.0_dp, 35.0_dp] ! step, start, end

        call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .false.)

        call F90_EXPECT_DOUBLE_EQ(start, SIMULATION_START)
        call F90_EXPECT_DOUBLE_EQ(end_, SIMULATION_STOP)
    end subroutine test_set_time_interval__dont_read_interval_input
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_set_time_interval__start_stop_out_of_bounds, test_set_time_interval__start_stop_out_of_bounds,
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
        call F90_EXPECT_DOUBLE_EQ(start, 14.0_dp)
        call F90_EXPECT_DOUBLE_EQ(step, 7.0_dp)
        call F90_EXPECT_DOUBLE_EQ(end_, 35.0_dp)

        call F90_EXPECT_EQ(GetMessageCount(), 2, "Expected two messages to be written to the log" // c_null_char)
        
        ! First log line is about the out-of-bounds interval start
        log_level = GetMessage_MH(1, message)
        call F90_EXPECT_EQ(log_level, LEVEL_WARN, "Log level of interval start message must be WARNING" // c_null_char)
        call F90_EXPECT_TRUE(index(message, "Setting FooInterval start time to TStart") > 0, "Unexpected interval start log line" // c_null_char)
        
        ! Second log line is about the out-of-bounds interval end
        log_level = GetMessage_MH(2, message)
        call F90_EXPECT_EQ(log_level, LEVEL_WARN, "Log level of interval stop message must be WARNING" // c_null_char)
        call F90_EXPECT_TRUE(index(message, "Setting FooInterval stop time to TStop") > 0, "Unexpected interval stop log line" // c_null_char)
    end subroutine test_set_time_interval__start_stop_out_of_bounds
    !$f90tw)
end module test_unstruc_model