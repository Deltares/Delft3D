!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

!> module containing logic related to the file_logger class
module m_file_logger
    use m_waq_precision
    use m_base_logger
    use m_log_level
    use m_logger
    use m_logger_helper, only: stop_with_error !< replace with error handling

    implicit none

    private
    public :: file_logger

    !> Type to handle logging to a file
    type, extends(base_logger) :: file_logger
        character(:), allocatable :: log_file_path            !< path of file to log towards
        integer(kind=int_wp), private :: file_unit_number = 0 !< number of the file handle
        logical, private :: file_open = .false.               !< if the initialize call has been done
    contains

        procedure :: write_message

        final :: destruct_logger

        procedure, private :: open_file_if_needed

    end type file_logger

contains

    !> Destructor
    subroutine destruct_logger(this)
        type(file_logger) :: this !< instance of this logger

        if (this%file_open) then
            close (this%file_unit_number)
            this%file_open = .false.
        end if
    end subroutine destruct_logger

    !> writes the message to file
    subroutine write_message(this, message)
        class(file_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log

        call this%open_file_if_needed()
        write (this%file_unit_number, FMT='(A)') message
    end subroutine write_message

    !> makes sure that the file is open for writing
    !! \exception can lead to a program stop (if opening of file fails)
    subroutine open_file_if_needed(this)
        class(file_logger) :: this  !< instance of this logger

        integer(kind=int_wp) :: error_code

        if (this%file_open) then
            return
        end if

        open (newunit=this%file_unit_number, file=this%log_file_path, iostat=error_code)

        if (this%file_unit_number == 0 .or. error_code /= 0) then
            write(*, *) "Could not open the file " // this%log_file_path // " for logging."
            call stop_with_error()
        end if

        this%file_open = .true.
    end subroutine

end module m_file_logger
