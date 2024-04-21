!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
! Module to validate time-series data, containing several routines to check different
! properies of data.
! Will return an errormessage if data does not adhere to required format.

module m_time_validation
    use m_waq_precision
    use m_waq_data_structure
    use m_error_status

    implicit none

    public :: validate_time_series_strictly_increasing, validate_time_settings
    private :: validate_divisibility, validate_start_time, validate_time_coincidence

contains
    ! Routine to check if a time series in input is strictly increasing. If not, write an error message.
    ! Input:
    ! - lun (file unit for error message)
    ! - data_block (data to validate)
    ! - ierror (error number to be increased)
    ! Output:
    ! - ierror
    subroutine validate_time_series_strictly_increasing(lun, data_block, ierror)

        integer(kind = int_wp), intent(in) :: lun        ! logical unit number for logging error message, if required

        type(t_data_block), intent(in) :: data_block  ! data block containing time series to validate
        integer(kind = int_wp), intent(inout) :: ierror     ! local error count
        character(:), allocatable :: errformat   ! format for error message
        integer(kind = int_wp) :: i

        errformat = "(/' ERROR: time value ',I0.1,' not larger than previous time value ',I0.1, '.')"
        do i = 2, size(data_block%times)
            if (data_block%times(i) <= data_block%times(i - 1)) then
                write (lun, errformat) data_block%times(i), data_block%times(i - 1)
                ierror = ierror + 1
            end if
        end do
    end subroutine validate_time_series_strictly_increasing


    ! Subroutine to validate if all time settings are valid.
    ! If not, an error message is logged to the *.lst file.
    ! Input:
    ! - lun (file unit for error message)
    ! - start, stop and time step of entire simulation, monitoring, map and history output
    ! - ierror (error number to be increased)
    ! Output:
    ! - ierror
    subroutine validate_time_settings(lun, status, &
            itstrt, itstop, idt, &
            imstrt, imstop, imstep, &
            idstrt, idstop, idstep, &
            ihstrt, ihstop, ihstep)

        use m_srstop

        integer(4), intent(in) :: lun                      ! logical unit number of logging file (*.lst)
        integer(4), intent(in) :: itstrt, itstop, idt    ! start, stop and time step of entire simulation
        integer(4), intent(in) :: imstrt, imstop, imstep ! start, stop and time step of monitoring output
        integer(4), intent(in) :: idstrt, idstop, idstep ! start, stop and time step of map output
        integer(4), intent(in) :: ihstrt, ihstop, ihstep ! start, stop and time step of history output

        type(error_status), intent(inout) :: status !< current error status

        ! Check if total simulation time is multiple of simulation delta t
        call validate_divisibility(itstop - itstrt, idt, status, lun, 1)

        ! Check if monitoring time step is multiple of simulation time step
        call validate_divisibility(imstep, idt, status, lun, 2)

        ! Check if map (output) time step is multiple of simulation time step
        call validate_divisibility(idstep, idt, status, lun, 3)

        ! Check if history time step is multiple of simulation time step
        call validate_divisibility(ihstep, idt, status, lun, 4)

        ! Check if start of monitoring timing is at least start of simulation
        call validate_start_time(imstrt, itstrt, status, lun, 5)

        ! Check if start of map (output) timing is at least start of simulation
        call validate_start_time(idstrt, itstrt, status, lun, 6)

        ! Check if start of history timing is at least start of simulation
        call validate_start_time(ihstrt, itstrt, status, lun, 7)

        ! Check if monitoring times coincide with simulation times
        call validate_time_coincidence(imstrt, imstep, itstrt, idt, status, lun, 1)

        ! Check if map (output) times coincide with simulation times
        call validate_time_coincidence(idstrt, idstep, itstrt, idt, status, lun, 2)

        ! Check if history times coincide with simulation times
        call validate_time_coincidence(ihstrt, ihstep, itstrt, idt, status, lun, 3)

        if (status%ierr > 0) then
            call srstop(1)
        end if

    end subroutine validate_time_settings


    ! Validates if the dividend is divisible by (i.e. is a multiple of) the divisor.
    ! If not, an appropriate error message is sent to the *.lst file.
    ! The number of errors is increased by one which will cause the program to stop.
    subroutine validate_divisibility(dividend, divisor, status, log_unit, ierror_message)

        integer(4), intent(in) :: dividend     ! number to divide (expected to be an exact multiple of divisor)
        integer(4), intent(in) :: divisor      ! number by which the division will be validated (wrt which the dividend must be a multiple)
        integer(4), intent(in) :: log_unit     ! unit number of logging file (*.lst)
        integer(4), intent(in) :: ierror_message ! index of error message if needed
        type(error_status), intent(inout) :: status !< current error status

        if (mod(dividend, divisor) /= 0) then
            call status%increase_error_count()
            write (log_unit, time_settings_error_messages(ierror_message)) dividend, divisor
        endif
    end subroutine validate_divisibility


    ! Validates if the start time for output is at least equal to the start time of the simulation.
    ! If not, an appropriate error message is sent to the *.lst file.
    ! The number of errors is increased by one which will cause the program to stop.
    subroutine validate_start_time(output_time, sim_time, status, log_unit, ierror_message)

        integer(4), intent(in) :: output_time  ! start of time for output
        integer(4), intent(in) :: sim_time     ! start of time for simulation
        integer(4), intent(in) :: log_unit     ! logical unit number of logging file (*.lst)
        integer(4), intent(in) :: ierror_message ! index of error message if needed
        type(error_status), intent(inout) :: status !< current error status

        if (output_time < sim_time) then
            call status%increase_error_count()
            write (log_unit, time_settings_error_messages(ierror_message)) output_time, sim_time
        endif
    end subroutine validate_start_time


    ! Validates if the time steps for output will coincide with time steps for the simulation.
    ! If not, an appropriate error message is sent to the *.lst file.
    ! The number of errors is increased by one which will cause the program to stop.
    subroutine validate_time_coincidence(out_start_time, out_time_step, &
            sim_start_time, sim_time_step, &
            status, log_unit, ierror_message)

        integer(4), intent(in) :: out_start_time, out_time_step ! start time and time step for output
        integer(4), intent(in) :: sim_start_time, sim_time_step ! start time and time step for simulation
        integer(4), intent(in) :: log_unit                      ! unit number of logging file (*.lst)
        integer(4), intent(in) :: ierror_message                  ! index of error message if needed
        type(error_status), intent(inout) :: status !< current error status

        if (mod(out_start_time - sim_start_time, sim_time_step) /=0) then
            call status%increase_error_count()
            write (log_unit, time_coincidence_error_messages(ierror_message)) out_start_time, &
                    out_time_step, sim_start_time, sim_time_step
        endif
    end subroutine

    ! Error messages for validation of time settings containing two arguments
    function time_settings_error_messages(ierror) result(message)

        integer, intent(in) :: ierror     ! index of the error message
        character(:), allocatable :: message ! message corresponding to the index ierror

        select case (ierror)
        case (1)
            message = "(/'ERROR: simulation total time (', I0.1, &
                    ') must be a multiple of simulation time step (', I0.1,').')"
        case (2)
            message = "(/'ERROR: monitoring time step (', I0.1, &
                    ') must be a multiple of simulation time step (', I0.1,').')"
        case (3)
            message = "(/'ERROR: map time step (', I0.1, &
                    ') must be a multiple of simulation time step (', I0.1,').')"
        case (4)
            message = "(/'ERROR: history time step (', I0.1, &
                    ') must be a multiple of simulation time step (', I0.1,').')"
        case (5)
            message = "(/'ERROR: monitoring (', I0.1, &
                    ') must not start earlier than simulation (', I0.1,').')"
        case (6)
            message = "(/'ERROR: map output (', I0.1, &
                    ') must not start earlier than simulation (', I0.1,').')"
        case (7)
            message = "(/'ERROR: history (', I0.1, &
                    ') must not start earlier than simulation (', I0.1,').')"
        case default
            message = "Undefined error!"
        end select
    end function time_settings_error_messages

    ! Error messages for validation of time settings containing four arguments
    function time_coincidence_error_messages(ierror) result(message)

        integer, intent(in) :: ierror  ! index of the error message
        character(:), allocatable :: message ! message corresponding to the index ierror

        select case (ierror)
        case (1)
            message = "(/'ERROR: monitoring output times (', I0.1, &
                    '+ k*', I0.1, &
                    ') will not be reached during the simulation (', &
                    I0.1,'+ n*', I0.1,').')"
        case (2)
            message = "(/'ERROR: map output times (', I0.1, &
                    '+ k*', I0.1, &
                    ') will not be reached during the simulation (', &
                    I0.1,'+ n*', I0.1,').')"
        case (3)
            message = "(/'ERROR: history output times (', I0.1, &
                    '+ k*', I0.1, &
                    ') will not be reached during the simulation (', &
                    I0.1,'+ n*', I0.1,').')"
        case default
            message = "Undefined error!"
        end select
    end function time_coincidence_error_messages

end module m_time_validation
