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

!> module containing logic related to the base_logger class
module m_base_logger
    use m_waq_precision
    use m_log_level
    use m_logger
    use iso_c_binding

    implicit none

    private
    public :: base_logger

    !> abstract base class for common logic of logger instances
    type, extends(logger), abstract :: base_logger

    contains
        procedure :: log_error, log_warning, log_info, log_debug
        procedure(log_message_interface), deferred :: write_message

        procedure, private :: log_message
    end type base_logger

    abstract interface

        !> signature/interface of the log_.. procedures
        subroutine log_message_interface(this, message)
            import base_logger
            class(base_logger), intent(in) :: this    !< instance of this logger
            character(len=*), intent(in) :: message   !< message to log
        end subroutine
    end interface

contains
    !> Log an error message
    subroutine log_error(this, message, add_new_line)
        class(base_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log
        logical, optional, intent(in) :: add_new_line !< add a new line before message

        call this%log_message(repeat("*", 150), error_level, add_new_line)
        call this%log_message("Error:"//message, error_level)
        call this%log_message(repeat("*", 150), error_level)
    end subroutine log_error

    !> Log a warning message
    subroutine log_warning(this, message, add_new_line)
        class(base_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log
        logical, optional, intent(in) :: add_new_line !< add a new line before message

        call this%log_message("Warning:"//message, warning_level)
    end subroutine log_warning

    !> Log an info message
    subroutine log_info(this, message, add_new_line)
        class(base_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log
        logical, optional, intent(in) :: add_new_line !< add a new line before message

        call this%log_message(message, info_level, add_new_line)
    end subroutine log_info

    !> Log an debug message
    subroutine log_debug(this, message, add_new_line)
        class(base_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log
        logical, optional, intent(in) :: add_new_line !< add a new line before message

        call this%log_message("Debug:"//message, debug_level, add_new_line)
    end subroutine log_debug

    !> Log a message to file (optionally with a new line above it)
    subroutine log_message(this, message, max_log_level, add_new_line)
        class(base_logger) :: this                        !< instance of this logger
        character(len=*), intent(in) :: message           !< message to log
        integer(kind=int_wp), intent(in) :: max_log_level !< log level
        logical, optional :: add_new_line                     !< add a new line before message

        logical :: add_new_line_
        character(:), allocatable :: message_

        if (this%log_level < max_log_level) then
            return
        end if

        message_ = message

        if (present(add_new_line)) then
            if (add_new_line) then
                message_ = NEW_LINE('a')//message
            end if
        end if

        call this%write_message(message_)
    end subroutine log_message

end module m_base_logger
