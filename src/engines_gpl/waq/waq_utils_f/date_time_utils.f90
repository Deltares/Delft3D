!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module date_time_utils

    use m_waq_precision
    implicit none
    private
    integer :: simulation_start_time_scu    ! Simulation start time in system clock units
    integer :: simulation_stop_time_scu     ! Simulation stop time in system clock units
    integer :: system_time_factor_seconds   ! System timer factor in seconds
    real(kind = 8) :: base_julian_time      ! Time base in Julian time

    interface compute_reference_day
        module procedure compute_refday_from_string
        module procedure compute_refday_from_integers
    end interface

    public :: simulation_start_time_scu, simulation_stop_time_scu, system_time_factor_seconds, base_julian_time, &
            convert_string_to_time_offset, convert_period_to_timer, convert_relative_time, report_time, &
            compute_reference_day

contains


    subroutine convert_string_to_time_offset(time_string, time_offset, format_ddhhmmss, format_yyydddhh, is_error)
        !! Detects absolute time string and converts to amount of sytem time after time offset
        !!
        !! Output in time_offset can have the following forms:
        !!   - in seconds if both flags are false, maximum 68 year
        !!   - in YYYDDDHH if format_yyydddhh is true, maximum 21475 year
        !!   - in DDHHMMSS if dtflq1 is true, maximum  2147 days = 5.88 year !

        use timers       !   performance timers
        use m_string_utils, only : string_equals
        use time_module, only : julian_with_leapyears

        character*(*), intent(in) :: time_string       ! String containing the time data
        integer   (4), intent(out) :: time_offset      ! Calculated time offset
        logical, intent(in) :: format_ddhhmmss         ! True if format should be DDHHMMSS
        logical, intent(in) :: format_yyydddhh         ! True if format should be YYYDDDHH
        integer   (4), intent(inout) :: is_error       ! 1 if string is no timer

        integer :: mod      !  modulo operation
        integer(4) :: ikey     !  return value of 'zoek'
        integer(4) :: iyear, imonth, iday, ihour, iminut, isecnd, idate, itime
        real(8) :: julian_time    !  to compute distance from otime
        real(8) :: time_factor    !  system clock in days
        real(8) :: calculated_offset    !  help variable

        integer(4) :: timer_handle = 0
        if (timon) call timstrt("convert_string_to_time_offset", timer_handle)

        is_error = 0
        time_offset = 0

        ! search for presence of special keywords start and stop
        if (string_equals('START', time_string)) then
            time_offset = simulation_start_time_scu
        else if (string_equals('STOP', time_string)) then
            time_offset = simulation_stop_time_scu
        else
            if (time_string(5:5) /= '/' .or. time_string(8:8) /= '/' .or. &
                    time_string(11:11) /= '-' .or. time_string(14:14) /= ':' .or. &
                    time_string(17:17) /= ':') then
                is_error = 1 ! string is no timer
                return
            end if

            read (time_string(1:4), '(i4)') iyear
            read (time_string(6:7), '(i2)') imonth
            read (time_string(9:10), '(i2)') iday
            read (time_string(12:13), '(i2)') ihour
            read (time_string(15:16), '(i2)') iminut
            read (time_string(18:19), '(i2)') isecnd
            idate = iyear * 10000 + imonth * 100 + iday
            itime = ihour * 10000 + iminut * 100 + isecnd
            julian_time = julian_with_leapyears(idate, itime)
            time_factor = system_time_factor_seconds / 864.0d+02
            ! this should support
            if (system_time_factor_seconds < 0) time_factor = -1.0d+00 / system_time_factor_seconds / 864.0d+02
            ! time steps < 1 second
            ! check if time will fit in the integer
            calculated_offset = (julian_time - base_julian_time) / time_factor

            ! no format maximum is 2**31 which is 2.147.483.648 or around 2.1E9 scu
            if (abs(calculated_offset) > 2.147e9) then
                time_offset = -999
                return
            endif

            ! time will be set in format if necessary
            ! for the round-off error
            time_offset = calculated_offset + 0.5
            ! YYDDDHH format
            if (format_yyydddhh) then
                ! the hours
                time_offset = time_offset / 3600
                ! years of 365 days assumed
                time_offset = (time_offset / 8760) * 100000 + (mod(time_offset, 8760) / 24) * 100 &
                        + mod(time_offset, 24)
            endif

            ! DDHHMMSS format
            if (format_ddhhmmss) then
                time_offset = (time_offset / 86400) * 1000000 + (mod(time_offset, 86400) / 3600) * 10000 &
                        + (mod(time_offset, 3600) / 60) * 100 + mod(time_offset, 60)
            endif
        endif

        if (timon) call timstop(timer_handle)

    end subroutine convert_string_to_time_offset

    subroutine convert_period_to_timer(input_string, timer_value, use_date_format, use_hour_format, is_error)
        !! Converts a time period string to a system timer value with formatting options.

        character*(*), intent(in) :: input_string               !! String containing the time data
        integer(kind = int_wp), intent(out) :: timer_value      !! system timer
        logical, intent(in) :: use_date_format, use_hour_format !! Formatting options TRUE if date format or hour format
        integer(kind = int_wp), intent(out) :: is_error
        integer(kind = int_wp) :: year, month, day, hour, minute, second, total_seconds

        is_error = 1
        if (input_string(5:5) /= '/' .or. input_string(8:8) /= '/' .or. &
                input_string(11:11) /= '-' .or. input_string(14:14) /= ':' .or. &
                input_string(17:17) /= ':') return

        read (input_string(1:4), '(i4)') year
        read (input_string(6:7), '(i2)') month
        read (input_string(9:10), '(i2)') day
        read (input_string(12:13), '(i2)') hour
        read (input_string(15:16), '(i2)') minute
        read (input_string(18:19), '(i2)') second

        total_seconds = year * 31536000 + month * 2592000 + day * 86400 + &
                hour * 3600 + minute * 60 + second

        if (system_time_factor_seconds < 0) then
            timer_value = -total_seconds * system_time_factor_seconds
        else
            timer_value = total_seconds / system_time_factor_seconds
        endif

        if (use_hour_format) then
            timer_value = timer_value / 3600
            timer_value = (timer_value / 8760) * 100000 + (mod(timer_value, 8760) / 24) * 100  &
                    + mod(timer_value, 24)
        else if (use_date_format) then
            timer_value = (timer_value / 86400) * 1000000 + (mod(timer_value, 86400) / 3600) * 10000  &
                    + (mod(timer_value, 3600) / 60) * 100 + mod(timer_value, 60)
        endif
        is_error = 0
    end subroutine convert_period_to_timer

    subroutine convert_relative_time(breakpoint, time_scale_factor, use_date_format, use_yydddhh_format)
        !! Converts a relative integer time in DDHHMMSS or YYDDDHH format to seconds.
        !!DDDDHHMMSS allows up to 2146 days or some 5.8 years
        !!YYDDDHH allows more than needed (NB: a year is 365 days !)
        !!If date is false, result is multiplied by ifact.
        !!Absolute offset time should be chosen such that all relative
        !!times fit into 68 years from the absolute offset !

        integer(kind = int_wp), intent(inout) :: breakpoint         !! Breakpoint time to convert
        integer(kind = int_wp), intent(in) :: time_scale_factor     !! Scale factor between time units
        logical, intent(in) :: use_date_format                    !! True if using 'date' format (DDHHMMSS)
        logical, intent(in) :: use_yydddhh_format                 !! True if using YYDDDHH format

        integer(kind = int_wp) :: seconds, minutes, hours, days, years

        if (use_date_format) then
            if (use_yydddhh_format) then
                hours = mod(breakpoint, 100)
                days = mod(breakpoint / 100, 1000)
                years = breakpoint / 100000
                breakpoint = 3600 * hours + 86400 * days + 31536000 * years
            else
                seconds = mod(breakpoint, 100)
                minutes = mod(breakpoint / 100, 100)
                hours = mod(breakpoint / 10000, 100)
                days = breakpoint / 1000000
                breakpoint = seconds + 60 * minutes + 3600 * hours + 86400 * days
            endif
        else
            breakpoint = time_scale_factor * breakpoint
        endif

    end subroutine convert_relative_time

    subroutine report_time(output_unit, time_in_scu, format_flag, percentage_complete)
        ! Writes time to the output unit in a specified format according format_flag fromat.

        INTEGER(kind = int_wp), intent(in) :: output_unit   !! Unit number output
        INTEGER(kind = int_wp), intent(in) :: time_in_scu   !! Time in system clock units
        INTEGER(kind = int_wp), intent(in) :: format_flag   !! Output format indicator
        !! (0 = integer format) (1 = dd:hh:mm:ss) (2 = yy:ddd:hh:mm:ss)
        REAL(kind = real_wp) :: percentage_complete        !! Percentage of the simulation completed
        !! IF >= 0 then percentage to be printed

        IF (format_flag == 0) THEN
            IF (percentage_complete >= 0.0) THEN
                WRITE (output_unit, 2030) time_in_scu, percentage_complete
            ELSE
                WRITE (output_unit, 2000) time_in_scu
            ENDIF
        ELSEIF (format_flag == 1) THEN
            IF (time_in_scu < 31536000) THEN
                IF (percentage_complete >= 0.0) THEN
                    WRITE (output_unit, 2040)     time_in_scu / 86400, &
                            MOD(time_in_scu, 86400) / 3600, &
                            MOD(time_in_scu, 3600) / 60, &
                            MOD(time_in_scu, 60), &
                            percentage_complete
                ELSE
                    WRITE (output_unit, 2010)     time_in_scu / 86400, &
                            MOD(time_in_scu, 86400) / 3600, &
                            MOD(time_in_scu, 3600) / 60, &
                            MOD(time_in_scu, 60)
                ENDIF
            ELSE
                IF (percentage_complete >= 0.0) THEN
                    WRITE (output_unit, 2050)     time_in_scu / 31536000, &
                            MOD(time_in_scu, 31536000) / 86400, &
                            MOD(time_in_scu, 86400) / 3600, &
                            MOD(time_in_scu, 3600) / 60, &
                            MOD(time_in_scu, 60), &
                            percentage_complete
                ELSE
                    WRITE (output_unit, 2020)     time_in_scu / 31536000, &
                            MOD(time_in_scu, 31536000) / 86400, &
                            MOD(time_in_scu, 86400) / 3600, &
                            MOD(time_in_scu, 3600) / 60, &
                            MOD(time_in_scu, 60)
                ENDIF
            ENDIF
        ELSEIF (format_flag == 2) THEN
            IF (percentage_complete >= 0.0) THEN
                WRITE (output_unit, 2050)     time_in_scu / 31536000, &
                        MOD(time_in_scu, 31536000) / 86400, &
                        MOD(time_in_scu, 86400) / 3600, &
                        MOD(time_in_scu, 3600) / 60, &
                        MOD(time_in_scu, 60), &
                        percentage_complete
            ELSE
                WRITE (output_unit, 2020)     time_in_scu / 31536000, &
                        MOD(time_in_scu, 31536000) / 86400, &
                        MOD(time_in_scu, 86400) / 3600, &
                        MOD(time_in_scu, 3600) / 60, &
                        MOD(time_in_scu, 60)
            ENDIF
        ENDIF

        RETURN
        2000 FORMAT ('  TIME = ', I12, ' .')
        2010 FORMAT ('  TIME = ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')
        2020 FORMAT ('  TIME = ', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')
        2030 FORMAT ('  TIME = ', I12, ' . ', F6.2, '% Completed')
        2040 FORMAT ('  TIME = ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S . ', F6.2, '% Completed')
        2050 FORMAT ('  TIME = ', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S . ', F6.2, '% Completed')
    end subroutine report_time

    subroutine compute_refday_from_string(date_str, ref_day)
        !! Compute reference day, varying from 1 till 365 (or 366 for leap years)
        character(len = *), intent(in) :: date_str             !!  refdate
        integer(kind = int_wp), intent(out) :: ref_day         !!  ref_day

        integer(kind = int_wp) :: iyear              !  year of the time offset
        integer(kind = int_wp) :: imonth             !  month of the time offset
        integer(kind = int_wp) :: iday               !  day of the time offset

        read(date_str, '(i4,i2,i2)') iyear, imonth, iday
        call compute_refday_from_integers(iyear, imonth, iday, ref_day)

    end subroutine compute_refday_from_string


    subroutine compute_refday_from_integers(iyear, imonth, iday, ref_day)
        !! Compute reference day, varying from 1 till 365 (or 366 for leap years)
        integer(kind = int_wp), intent(in) :: iyear              !!  year of the time offset
        integer(kind = int_wp), intent(in) :: imonth             !!  month of the time offset
        integer(kind = int_wp), intent(in) :: iday               !!  day of the time offset
        integer(kind = int_wp), intent(out) :: ref_day           !!  reference day

        integer(kind = int_wp), dimension(12) :: days_in_month       !  # days in each month
        logical :: is_leap_year           !  is iyear a leap year, yes or no

        ref_day = 0
        call checkLeapYear(iyear, is_leap_year)

        days_in_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
        if (is_leap_year) then
            days_in_month(2) = 29
        endif

        ref_day = sum(days_in_month(1:imonth - 1)) + iday

    end subroutine compute_refday_from_integers


    subroutine checkLeapYear(iyear, is_leap_year)
        !! Check if year is a leap year
        integer(kind = int_wp), intent(in) :: iyear    !!  year of the time offset
        logical, intent(out) :: is_leap_year           !!  is iyear a leap year, yes or no

        if (mod(iyear, 4) == 0)   is_leap_year = .TRUE.
        if (mod(iyear, 100) == 0) is_leap_year = .FALSE.
        if (mod(iyear, 400) == 0) is_leap_year = .TRUE.

    end subroutine checkLeapYear

end module date_time_utils