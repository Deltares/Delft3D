module test_unstruc_model
    use assertions_gtest
    use precision, only: dp
    use unstruc_model, only: set_time_interval
    use messagehandling, only: getMaxErrorLevel, LEVEL_ERROR
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

    !$gtest
    !> Reject the mass balance interval that skips its first scheduled output time.
    subroutine test_mba_bad_step() bind(C)
        call read_mba_test_mdu(100.0_dp, 1800.0_dp)

        call f90_expect_eq(getMaxErrorLevel(), LEVEL_ERROR)
    end subroutine test_mba_bad_step

    !$gtest
    !> Accept integer and decimal intervals aligned with a fractional user timestep.
    subroutine test_mba_aligned() bind(C)
        use m_flowtimes, only: ti_mba

        call read_mba_test_mdu(120.0_dp, 1800.0_dp)
        call f90_expect_lt(getMaxErrorLevel(), LEVEL_ERROR)
        call f90_expect_double_eq(ti_mba, 120.0_dp)

        call read_mba_test_mdu(100.08_dp, 1800.0_dp)
        call f90_expect_lt(getMaxErrorLevel(), LEVEL_ERROR)
        call f90_expect_double_eq(ti_mba, 100.08_dp)
    end subroutine test_mba_aligned

    !$gtest
    !> Reject an unaligned final output time even when the interval is aligned.
    subroutine test_mba_bad_end() bind(C)
        call read_mba_test_mdu(120.0_dp, 1800.1_dp)

        call f90_expect_eq(getMaxErrorLevel(), LEVEL_ERROR)
    end subroutine test_mba_bad_end

    !$gtest
    !> Skip output time validation when mass balance output is disabled.
    subroutine test_mba_disabled() bind(C)
        use m_flowtimes, only: ti_mba

        call read_mba_test_mdu(0.0_dp, 1800.1_dp)

        call f90_expect_lt(getMaxErrorLevel(), LEVEL_ERROR)
        call f90_expect_double_eq(ti_mba, 0.0_dp)
    end subroutine test_mba_disabled

    !$gtest
    !> Validate the effective interval after reading the legacy DtMassBalance setting.
    subroutine test_mba_legacy_step() bind(C)
        call read_mba_test_mdu(100.0_dp, 1800.0_dp, legacy=.true.)

        call f90_expect_eq(getMaxErrorLevel(), LEVEL_ERROR)
    end subroutine test_mba_legacy_step

    !> Read a minimal MDU with a fractional user timestep and mass balance settings.
    subroutine read_mba_test_mdu(interval, stop_time, legacy)
        use unstruc_model, only: readMDUFile
        use m_resetfullflowmodel, only: resetFullFlowModel
        use messagehandling, only: resetMaxErrorLevel, LEVEL_FATAL
        use unstruc_messages, only: threshold_abort
        use dfm_error, only: DFM_NOERR
        use m_unstruc_model_data, only: md_dt_waqbal, md_dt_waqproc

        real(kind=dp), intent(in) :: interval
        real(kind=dp), intent(in) :: stop_time
        logical, optional, intent(in) :: legacy

        character(len=*), parameter :: filename = 'test_mba_interval.mdu'
        integer :: mdu_unit, ierr, previous_threshold
        logical :: use_legacy
        real(kind=dp) :: previous_dt_waqbal, previous_dt_waqproc

        call resetFullFlowModel()
        call resetMaxErrorLevel()
        previous_threshold = threshold_abort
        threshold_abort = LEVEL_FATAL
        previous_dt_waqbal = md_dt_waqbal
        previous_dt_waqproc = md_dt_waqproc
        md_dt_waqbal = 0.0_dp
        md_dt_waqproc = 0.0_dp
        use_legacy = .false.
        if (present(legacy)) then
            use_legacy = legacy
        end if

        open (newunit=mdu_unit, file=filename, status='replace', action='write')
        write (mdu_unit, '(a)') '[model]', 'Program = D-Flow FM', &
            '[time]', 'RefDate = 20000218', 'TUnit = S', 'TStart = 1200', 'DtUser = 0.24'
        write (mdu_unit, '(a,f16.8)') 'TStop = ', stop_time
        write (mdu_unit, '(a)') '[output]', 'OutputDir = .', 'HisInterval = 0', 'MapInterval = 0', 'RstInterval = 0'
        if (interval > 0.0_dp) then
            write (mdu_unit, '(a)') 'MbaFile = test_mba.ini'
        end if
        if (use_legacy) then
            write (mdu_unit, '(a)') '[processes]'
            write (mdu_unit, '(a,f16.8)') 'DtMassBalance = ', interval
        else
            write (mdu_unit, '(a,f16.8)') 'MbaInterval = ', interval
        end if
        close (mdu_unit)

        call readMDUFile(filename, ierr)
        threshold_abort = previous_threshold
        md_dt_waqbal = previous_dt_waqbal
        md_dt_waqproc = previous_dt_waqproc

        open (newunit=mdu_unit, file=filename, status='old')
        close (mdu_unit, status='delete')
        call f90_expect_eq(ierr, DFM_NOERR)
    end subroutine read_mba_test_mdu
end module test_unstruc_model
