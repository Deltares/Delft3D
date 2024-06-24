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

program delwaq

    use m_delwaq1
    use m_delwaq2
    use delwaq_exe_version_module

    use m_logger_factory
    use m_logger_type
    use m_logger
    use m_log_level

    use m_command_line_help, only: show_command_line_help, show_command_line_version
    use m_string_utils, only: join_strings
    use m_cli_utils, only: get_arguments, store_command_arguments, is_command_arg_specified

    implicit none

    character(len = 256), dimension(:), allocatable :: argv
    character(:), allocatable :: id_str
    character(len=10) :: log_file_path

    class(logger), allocatable :: log

    call get_fullversionstring_delwaq(id_str)

    if (is_command_arg_specified('--help') .or. &
        is_command_arg_specified('-h') .or. &
        is_command_arg_specified('/?')) then
        call show_command_line_help(id_str)
        stop 0
    end if

    if (is_command_arg_specified('--version') .or. &
        is_command_arg_specified('-v')) then
            call show_command_line_version()
            stop 0
    end if

    ! initialize logging
    log_file_path = 'delwaq.log'
    log = create_logger(FILE, INFO_LEVEL, log_file_path)

    argv = get_arguments()
    call log%log_info('Running: '//trim(id_str))
    call log%log_info('Provided arguments: '//join_strings(argv(1:), ', '))

    if (.not. delwaq1(argv)) then
        call log%log_error('Error during delwaq pre-processing')
        stop 1
    end if

    if (.not. is_command_arg_specified('-validation_mode')) then
        if (.not. delwaq2()) then
            call log%log_error('Error during delwaq processing')
            stop 1
        end if
    end if

    call log%log_info('Normal end', .true.)
    write (*, *) ' Normal end'
end program delwaq
