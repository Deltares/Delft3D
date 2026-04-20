module test_unstruc_model
    use assertions_gtest
    use precision, only: dp
    use unstruc_model, only: set_time_interval
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