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

!> module containing logic related to the composite_logger class
module m_composite_logger
    use m_waq_precision
    use m_log_level
    use m_logger_wrapper
    use m_logger

    implicit none

    private
    public :: composite_logger

    !> logger that is composed of multiple logger instances
    !! all calls will be rerouted to all sub-loggers
    type, extends(logger) :: composite_logger
        class(logger_wrapper), allocatable, dimension(:) :: sub_loggers
    contains
        procedure :: log_error, log_warning, log_info, log_debug

        procedure, private :: log_with_sub_loggers

    end type composite_logger

contains

    !> Log an error message
    subroutine log_error(this, message, add_new_line)
        class(composite_logger), intent(in) :: this !< instance of this logger
        character(len=*), intent(in) :: message     !< message to log
        logical, optional, intent(in) :: add_new_line   !< add a new line before message

        call this%log_with_sub_loggers(message, error_level, add_new_line)
    end subroutine log_error

    !> Log a warning message
    subroutine log_warning(this, message, add_new_line)
        class(composite_logger), intent(in) :: this !< instance of this logger
        character(len=*), intent(in) :: message     !< message to log
        logical, optional, intent(in) :: add_new_line   !< add a new line before message

        call this%log_with_sub_loggers(message, warning_level, add_new_line)
    end subroutine log_warning

    !> Log an info message
    subroutine log_info(this, message, add_new_line)
        class(composite_logger), intent(in) :: this !< instance of this logger
        character(len=*), intent(in) :: message     !< message to log
        logical, optional, intent(in) :: add_new_line   !< add a new line before message

        call this%log_with_sub_loggers(message, info_level, add_new_line)
    end subroutine log_info

    !> Log an debug message
    subroutine log_debug(this, message, add_new_line)
        class(composite_logger), intent(in) :: this    !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log
        logical, optional, intent(in) :: add_new_line !< add a new line before message

        call this%log_with_sub_loggers(message, debug_level, add_new_line)
    end subroutine log_debug

    !> Delegates the message towards all sub-loggers
    subroutine log_with_sub_loggers(this, message, log_level, add_new_line)
        class(composite_logger), intent(in) :: this !< instance of this logger
        character(len=*), intent(in) :: message     !< message to log
        integer(kind=int_wp) :: log_level           !< log level of the message
        logical, optional, intent(in) :: add_new_line   !< add a new line before message

        class(logger), allocatable :: sub_logger
        integer(kind=int_wp) :: index

        do index = 1, size(this%sub_loggers)
            sub_logger = this%sub_loggers(index)%logger

            select case (log_level)
            case (error_level)
                call sub_logger%log_error(message, add_new_line)
            case (warning_level)
                call sub_logger%log_warning(message, add_new_line)
            case (info_level)
                call sub_logger%log_info(message, add_new_line)
            case (debug_level)
                call sub_logger%log_debug(message, add_new_line)
            end select
        end do
    end subroutine log_with_sub_loggers

end module m_composite_logger
