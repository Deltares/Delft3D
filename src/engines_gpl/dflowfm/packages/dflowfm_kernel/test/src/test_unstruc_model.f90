module test_unstruc_model
    use assertions_gtest
    use precision, only: dp
    use unstruc_model, only: set_time_interval, read_solver_sequence
    implicit none

    integer, parameter :: MESSAGE_BUFFER_SIZE = 256
contains

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
            'RestartFile = state_map.nc', 'RestartDateTime = 20250101010000', 'RstIgnoreBl = 0', &
            'RstIgnoreBedComp = 0', &
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
    subroutine test_solver_sequence_positive_restart_options() bind(C)
        use m_flowparameters, only: solver_sequence
        use m_flowtimes, only: dt_user, tstart_user, tstop_user, tfac

        integer :: unit, status
        character(len=*), parameter :: filename = 'test_solver_sequence_positive_restart.ini'

        tfac = 3600.0_dp
        dt_user = 1800.0_dp
        tstart_user = 0.0_dp
        tstop_user = 10800.0_dp
        open(newunit=unit, file=filename, status='replace')
        write(unit, '(a)') '[General]', 'fileType = solverSequence', 'fileVersion = 1.0', &
            '[Period]', 'tStart = 0', 'RstIgnoreBl = 1', 'RstIgnoreBedComp = 1', &
            'useRestartBedlevel = true', 'useRestartBedComp = true', &
            '[Period]', 'tStart = 1', 'RstIgnoreBl = 0', 'RstIgnoreBedComp = 0', &
            'useRestartBedlevel = false', 'useRestartBedComp = false'
        close(unit)

        call read_solver_sequence(filename, status)
        call F90_EXPECT_EQ(status, 0)
        if (status == 0) then
            call F90_EXPECT_TRUE(solver_sequence(1)%use_restart_bedlevel)
            call F90_EXPECT_TRUE(solver_sequence(1)%use_restart_bedcomp)
            call F90_EXPECT_FALSE(solver_sequence(2)%use_restart_bedlevel)
            call F90_EXPECT_FALSE(solver_sequence(2)%use_restart_bedcomp)
            deallocate(solver_sequence)
        end if
        open(newunit=unit, file=filename, status='old')
        close(unit, status='delete')
    end subroutine test_solver_sequence_positive_restart_options

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
