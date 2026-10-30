module test_unstruc_model
    use assertions_gtest
    use precision, only: dp
    use unstruc_model, only: set_time_interval
    implicit none

    integer, parameter :: MESSAGE_BUFFER_SIZE = 256
contains

    !$gtest test
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

    !$gtest test
    subroutine test_set_time_interval__dont_read_interval_input() bind(C)
        real(kind=dp), parameter :: SIMULATION_START = 7.0_dp, SIMULATION_STOP = 42.0_dp

        real(kind=dp) :: interval_input(3)
        real(kind=dp) :: start, step, end_

        interval_input = [7.0_dp, 14.0_dp, 35.0_dp] ! step, start, end

        call set_time_interval(interval_input, start, step, end_, SIMULATION_START, SIMULATION_STOP, .false.)

        call f90_expect_double_eq(start, SIMULATION_START)
        call f90_expect_double_eq(end_, SIMULATION_STOP)
    end subroutine test_set_time_interval__dont_read_interval_input

    !$gtest test
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
