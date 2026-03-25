module test_unstruc_model
    use assertions_gtest
    use precision
    use unstruc_model, only: set_time_interval
    use iso_c_binding, only: c_null_char
    implicit none

    integer, parameter :: MESSAGE_BUFFER_SIZE = 1024
    character(len=*), parameter :: MESSAGE_FMT = "(A1024)"

contains

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_set_time_interval__default_simulation_start_stop, test_set_time_interval__default_simulation_start_stop,
    subroutine test_set_time_interval__default_simulation_start_stop() bind(C)
        implicit none
        real(kind=dp) :: interval_input(3)
        real(kind=dp) :: start, step, stop
        character(len=*), parameter :: interval_name = "FooInterval"
        real(kind=dp), parameter :: simulation_start = 7.0_dp, simulation_stop = 42.0_dp

        interval_input = [7.0_dp, 0.0_dp, 0.0_dp] ! step, start, stop

        call set_time_interval(interval_input, start, step, stop, simulation_start, simulation_stop, .true., interval_name=interval_name)

        call F90_ASSERT_DOUBLE_EQ(start, 7.0_dp)
        call F90_ASSERT_DOUBLE_EQ(step, 7.0_dp)
        call F90_ASSERT_DOUBLE_EQ(stop, 42.0_dp)
    end subroutine test_set_time_interval__default_simulation_start_stop
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_unstruc_model, test_set_time_interval__start_stop_out_of_bounds, test_set_time_interval__start_stop_out_of_bounds,
    subroutine test_set_time_interval__start_stop_out_of_bounds() bind(C)
        use MessageHandling, only: SetMessageHandling
        implicit none
        integer :: unit, iostat
        character(len=MESSAGE_BUFFER_SIZE) :: log_line

        real(kind=dp) :: interval_input(3)
        real(kind=dp) :: start, step, stop
        character(len=*), parameter :: interval_name = "FooInterval"
        real(kind=dp), parameter :: simulation_start = 14.0_dp, simulation_stop = 35.0_dp

        ! Arrange
        interval_input = [7.0_dp, 7.0_dp, 42.0_dp] ! step, start, stop
        open (unit=unit, status="scratch")
        call SetMessageHandling(write2screen=.false., lunMessages=unit)  ! Write logs to scratch file.

        ! Act
        call set_time_interval(interval_input, start, step, stop, simulation_start, simulation_stop, .true., interval_name=interval_name)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(start, 14.0_dp)
        call F90_ASSERT_DOUBLE_EQ(step, 7.0_dp)
        call F90_ASSERT_DOUBLE_EQ(stop, 35.0_dp)

        rewind(unit)
        ! First log line is about the out-of-bounds interval start
        read(unit, MESSAGE_FMT, iostat=iostat) log_line
        call F90_ASSERT_EQ(iostat, 0, "First log line read failure" // c_null_char)
        call F90_ASSERT_TRUE(index(log_line, "Setting FooInterval start time to TStart") > 0, "Unexpected interval start log line" // c_null_char)
        call F90_ASSERT_TRUE(index(log_line, "WARNING") > 0, "Log line about interval start time must contain WARNING" // c_null_char)
        
        ! Second log line is about the out-of-bounds interval end
        read(unit, MESSAGE_FMT, iostat=iostat) log_line
        call F90_ASSERT_EQ(iostat, 0, "Second log line read failure" // c_null_char)
        call F90_ASSERT_TRUE(index(log_line, "Setting FooInterval stop time to TStop") > 0, "Unexpected interval stop log line" // c_null_char)
        call F90_ASSERT_TRUE(index(log_line, "WARNING") > 0, "Log line about interval stop time must contain WARNING" // c_null_char)
        close(unit)
    end subroutine test_set_time_interval__start_stop_out_of_bounds
    !$f90tw)
end module test_unstruc_model