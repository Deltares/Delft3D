!  Copyright (C)  Stichting Deltares, 2012-2023.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License version 3,
!  as published by the Free Software Foundation.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to registered trademarks
!  of Stichting Deltares remain the property of Stichting Deltares. All
!  rights reserved.

module m_cli_utils
    use m_waq_precision
    use m_string_utils

    implicit none
    private
    character(len=256), dimension(:), allocatable, save :: command_arguments_list

    public :: store_command_arguments, &
              get_input_filename, &
              get_arguments, &
              get_argument_by_index, &
              get_command_argument_by_name, &
              is_command_arg_specified

    interface get_command_argument_by_name
        procedure get_string_command_argument_by_name
        procedure get_int_command_argument_by_name
        procedure get_real_command_argument_by_name
    end interface

contains

    !> Gets declared command line arguments (using caching)
    function get_arguments() result(argv)
        character(len=256), allocatable :: argv(:) !< All specified arguments

        integer(4) :: i
        integer :: argc

        if (allocated(command_arguments_list)) then
            argv = command_arguments_list
            return
        end if

        argc = command_argument_count()

        allocate (argv(argc))
        do i = 1, argc
            call get_command_argument(i, argv(i))
        end do

    end function get_arguments

    !> Gives the value for an command-line argument at the provided index
    function get_argument_by_index(index, arg_value) result(success)
        integer(kind=int_wp), intent(in) :: index           !< Index of the argument
        character(:), allocatable, intent(out) :: arg_value !< Value of the argument

        logical :: success !< Could successfully find the argument

        ! local
        character(:), allocatable :: value_found
        character(:), dimension(:), allocatable :: arguments

        arguments = get_arguments()
        if (index > size(arguments)) then
            success = .false.
            return
        end if

        arg_value = trim(arguments(index))
        success = .true.
    end function

    !> Gives the index of the name in the commandline arguments
    !! (returns -1 if no match can be found)
    function get_command_arg_index_by_name(name) result(arg_index)
        character(len=*), intent(in) :: name   !< Name of the argument
        integer(kind=int_wp) :: arg_index !< Value of the argument

        ! local
        character(:), allocatable :: name_to_search
        integer(kind=int_wp) :: i

        name_to_search = trim(name)

        arg_index = index_in_array(name_to_search, get_arguments(), .true.)
    end function

    !> Checks if the provided name is specified as a command-line argument
    function is_command_arg_specified(name) result(arg_specified)
        character(len=*), intent(in) :: name   !< Name of the argument
        logical :: arg_specified      !< Value of the argument

        arg_specified = get_command_arg_index_by_name(name) > 0
    end function

    !> Searches the string value for an command-line argument
    function get_string_command_argument_by_name(name, arg_value, parsing_error) result(success)
        character(len=*), intent(in) :: name   !< Name of the argument
        character(:), allocatable, intent(out) :: arg_value !< Value of the argument
        logical, optional, intent(out) :: parsing_error !< Error has occured during parsing

        logical :: success !< Could successfully find and parse the argument

        ! local
        character(:), allocatable :: name_to_search
        integer(kind=int_wp) :: arg_index

        if (present(parsing_error)) then
            parsing_error = .false.
        end if

        arg_index = get_command_arg_index_by_name(name)

        if (arg_index == -1 .or. arg_index >= size(command_arguments_list)) then
            success = .false.
            return
        end if

        arg_value = trim(command_arguments_list(arg_index + 1))

        if (arg_value(1:1) == '-') then
            success = .false.
            if (present(parsing_error)) then
                parsing_error = .true.
            end if
            return
        end if

        success = .true.
    end function

    !> Searches the integer value for an command-line argument
    function get_int_command_argument_by_name(name, arg_value, parsing_error) result(success)
        character(len=*), intent(in) :: name            !< Name of the argument
        integer(kind=int_wp), intent(out) :: arg_value  !< Value of the argument
        logical, optional, intent(out) :: parsing_error !< Error has occured during parsing

        logical :: success !< Could successfully find and the argument

        ! local
        character(:), allocatable :: value_found
        integer(kind=int_wp) :: stat

        if (.not. get_string_command_argument_by_name(name, value_found)) then
            success = .false.
            return
        end if

        read (value_found, *, iostat=stat) arg_value

        if (stat /= 0 .and. present(parsing_error)) then
            parsing_error = .true.
        end if

        success = stat == 0
    end function

    !> Searches the real value for an command-line argument
    function get_real_command_argument_by_name(name, arg_value, parsing_error) result(success)
        character(len=*), intent(in) :: name            !< Name of the argument
        real(kind=real_wp), intent(out) :: arg_value    !< Value of the argument
        logical, optional, intent(out) :: parsing_error !< Error has occured during parsing

        logical :: success !< Could successfully find and parse the argument

        ! local
        character(:), allocatable :: value_found
        integer(kind=int_wp) :: stat

        if (.not. get_string_command_argument_by_name(name, value_found)) then
            success = .false.
            return
        end if

        read (value_found, *, iostat=stat) arg_value

        if (stat /= 0 .and. present(parsing_error)) then
            parsing_error = .true.
        end if

        success = stat == 0
    end function

    !> Stores a set of (constructed) command-line arguments in memory
    !! if no args are provided, the args will be determined by default functions
    subroutine store_command_arguments(args)
        character(len=*), dimension(:), optional, intent(in) :: args !< Override arguments to store with these arguments

        character(:), allocatable, dimension(:) :: args_to_set

        if (present(args)) then
            args_to_set = args
        else
            args_to_set = get_arguments()
        end if

        ! Deallocate existing arguments list if necessary
        if (allocated(command_arguments_list)) then
            deallocate (command_arguments_list)
        end if

        command_arguments_list = args_to_set

    end subroutine store_command_arguments

    !> read an input filename, either from the command line, a steering file, or interactively
    !! from the user. It also optionally checks the existence of a file with a specified extension
    subroutine get_input_filename(file_name, extension_check)

        use iso_fortran_env, only: input_unit, output_unit
        use m_logger_helper, only: stop_with_error
        use m_file_path_utils, only: get_file_name_without_extension

        character(:), allocatable, intent(out) :: file_name        !! file name input file
        character(len=*), intent(in) :: extension_check   !! if not empty then check existance of file with this
        !! extension when name is enterd interactive from keyboard

        character(len=9) :: steering_file_name
        character(:), allocatable :: filename_with_ext
        character(len=3) :: user_answer
        logical :: file_exists, command_found
        character(len=1) :: path_separator_unix, path_separator_windows, char_arg
        integer(kind=int_wp) :: file_unit, name_length, int_arg, error_status, quotes_index, i, j, IOERR
        real(kind=real_wp) :: real_arg

        path_separator_unix = '/'
        path_separator_windows = '\\'

        ! first argument from command line if it is not a option starting with -
        if (get_argument_by_index(1, file_name)) then
            if (file_name(1:1) /= '-') then
                file_name = get_file_name_without_extension(file_name)
                return
            end if
        end if

        ! Get filename filename from keyboard

        write (output_unit, *) ' Name of the model files ? '
        write (output_unit, *) ' DELWAQ will provide the extensions. '
        read (input_unit, '(A)') file_name

        name_length = len(file_name)

        ! TODO: could be abstracted to a separate subroutine in m_cli_utils `remove_quotes_from_filename`
        ! remove quotes if they are the first and the last
        if (file_name(1:1) == '''') then
            quotes_index = index(file_name(2:), '''')
            if (quotes_index /= 0) then
                if (quotes_index == name_length - 1) then
                    file_name(quotes_index + 1:) = ' '
                    file_name = file_name(2:)
                elseif (file_name(quotes_index + 2:) == ' ') then
                    file_name(quotes_index + 1:) = ' '
                    file_name = file_name(2:)
                end if
            end if
        end if

        ! If empty string then stop
        if (file_name == ' ') then
            write (output_unit, *) ' ERROR no filename entered!'
            call stop_with_error()
        end if

        ! check existence of output file
        if (extension_check /= ' ') then
            quotes_index = index(file_name, ' ')
            if (quotes_index == 0) quotes_index = name_length + 1
            filename_with_ext = file_name(1:quotes_index - 1)//extension_check
            inquire (FILE=filename_with_ext, EXIST=file_exists)

            if (file_exists) then
                write (output_unit, *) ' File:', filename_with_ext(1:quotes_index + 3), ' already exist.'
                write (output_unit, *) ' Do you want it to be replaced ? '
                write (output_unit, *) ' Answer yes or no ? '
                read (input_unit, '(A3)') user_answer
                if (user_answer(1:1) == 'N' .or. user_answer(1:1) == 'n') call stop_with_error()
            end if
        end if

        ! if there is remove extension
        file_name = get_file_name_without_extension(file_name)
    end subroutine get_input_filename

end module m_cli_utils
