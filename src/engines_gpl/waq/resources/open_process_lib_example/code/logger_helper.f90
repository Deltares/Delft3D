!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

!> module that helps with logging messages
module m_logger_helper

    implicit none

    private
    public :: write_log_message, set_log_unit_number, get_log_unit_number, &
            write_error_message_with_values, write_error_message, stop_with_error

    ! Module variables with descriptive names and initialization.
    integer, save :: log_unit_number = 0  ! Log file unit number, initialized to 0.

contains

    subroutine write_log_message(message)
        !! Writes a message to the log based
        character(len = *), intent(in) :: message  ! The message to be logged.

        write(log_unit_number, '(A)') message
    end subroutine write_log_message

    subroutine set_log_unit_number(file_unit)
        integer, intent(in) :: file_unit  ! The new log file unit number.

        log_unit_number = file_unit

    end subroutine set_log_unit_number

    !> Retrieves the current log file unit number.
    subroutine get_log_unit_number(file_unit)
        integer :: file_unit  ! The current log file unit number. ! , intent(inout)
        file_unit = log_unit_number
    end subroutine get_log_unit_number

    subroutine write_error_message(message)

        integer :: file_unit
        character(len=*) message

        call get_log_unit_number(file_unit)
        if (file_unit /= 0) then
            write (file_unit, '(/,1x,a,/)') message
        else
            write (*, '(/,1x,a,/)') message
        end if
        call stop_with_error()
    end subroutine write_error_message

    subroutine write_error_message_with_values(name, value, iseg, module)
        character(len=*) :: name
        real :: value
        integer :: iseg
        character(len=*) :: module
        integer :: file_unit

        ! message to screen
        write (*, *) ' '
        write (*, *) ' coefficient value out of range'
        write (*, *) ' coefficient name:', name
        write (*, *) ' coefficient value', value
        if (iseg > 0) write (*, *) ' in segment number:', iseg
        write (*, *) ' in subroutine', module

        ! message to monitor or report file
        call get_log_unit_number(file_unit)
        if (file_unit > 0) then
            write (file_unit, *) ' '
            write (file_unit, *) ' coefficient value out of range'
            write (file_unit, *) ' coefficient name:', name
            write (file_unit, *) ' coefficient value', value
            if (iseg > 0) write (file_unit, *) ' in segment number:', iseg
            write (file_unit, *) ' in subroutine ', module
        end if
        call stop_with_error()
    end subroutine write_error_message_with_values

    !> stops execution if possible and logs an error
    subroutine stop_with_error()
        write (*, *) 'Stopped with error code : 1'
        stop 1
    end

end module m_logger_helper
