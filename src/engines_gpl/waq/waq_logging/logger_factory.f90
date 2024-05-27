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

!> module containing logic related to creating loggers using a factory pattern
module m_logger_factory
    use m_waq_precision
    use m_file_logger
    use m_composite_logger
    use m_screen_logger
    use m_logger_type
    use m_logger
    use m_logger_helper, only: stop_with_error !< replace with error handling

    implicit none

    private
    public :: create_logger

contains

    !> creates a logger instance based on the specified logger type
    function create_logger(logger_type, log_level, file_name) result(logger_instance)
        integer(kind=int_wp) :: logger_type     !< type of logger to create|
        integer(kind=int_wp) :: log_level       !< initial logging level
        character(len=*), optional :: file_name !< name of the file (optional: only for file logging)

        class(logger), allocatable :: logger_instance
        type(file_logger), allocatable :: file_logger_instance

        select case (logger_type)
        case (SCREEN)
            logger_instance = screen_logger(log_level=log_level)
            return
        case (FILE)
            if (present(file_name)) then
                file_logger_instance = file_logger(log_file_path=file_name, log_level=log_level)
                call move_alloc(file_logger_instance, logger_instance)
                return
            end if
        case (FILE_AND_SCREEN)
            logger_instance = create_file_screen_logger(log_level, file_name)
            return
        case default
            write (*, *) "Error: could not create logger instance. Stopping execution"
            call stop_with_error()
        end select
    end function create_logger

    !> creates a composite logger containing a file and a screen logger
    function create_file_screen_logger(log_level, file_name) result(logger_instance)
        use m_logger_wrapper

        integer(kind=int_wp) :: log_level    !< initial logging level
        character(len=*), optional :: file_name  !< name of the file (optional: only for file logging)
        class(logger), allocatable :: logger_instance
        type(composite_logger), allocatable :: composite_logger_instance

        composite_logger_instance = composite_logger()

        allocate (composite_logger_instance%sub_loggers(2))
        composite_logger_instance%sub_loggers(1)%logger = create_logger(SCREEN, log_level)
        composite_logger_instance%sub_loggers(2)%logger = create_logger(FILE, log_level, file_name)

        call move_alloc(composite_logger_instance, logger_instance)
    end function

end module m_logger_factory
