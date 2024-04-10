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

    implicit none
    private
    character(len = 300), dimension(:), allocatable, save :: command_arguments_list
    character(len = *), parameter :: options_file_name = 'delwaq.options'

    public :: get_argument, get_number_of_arguments, store_command_arguments, get_argument_from_list, &
            count_total_arguments, retrieve_command_argument, get_input_filename

contains

    subroutine store_command_arguments(args)
        !! Stores a set of (constructed) command-line arguments This routine is used in the library version
        !! of DELWAQ to simulate the standalone features
        !! Stores command-line arguments from 'args' and additional arguments
        !! from the file 'delwaq.options' into 'command_arguments_list'.
        character(len = *), dimension(:), intent(in) :: args

        integer(kind = int_wp) :: i, arg_count, file_status, logical_unit
        logical :: opened
        logical :: file_exists
        character(len = 256) :: file_line

        ! Read and count lines from 'delwaq.options'
        arg_count = 0
        inquire(file = options_file_name, exist = file_exists)
        if (file_exists) then
            open(newunit = logical_unit, file = options_file_name)
            do
                read(logical_unit, '(a)', iostat = file_status) file_line
                if (file_status /= 0) exit
                if (file_line /= ' ') arg_count = arg_count + 1
            enddo
            close(logical_unit)
        endif

        ! Deallocate existing arguments list if necessary
        if (allocated(command_arguments_list)) then
            deallocate(command_arguments_list)
        endif

        ! Allocate memory for storing arguments
        allocate(command_arguments_list(size(args) + arg_count))

        command_arguments_list(1:size(args)) = args

        ! Store additional arguments from the options file
        if (file_exists) then
            arg_count = size(args)
            open(newunit = logical_unit, file = options_file_name)

            do
                read(logical_unit, '(a)', iostat = file_status) file_line
                if (file_status /= 0) exit
                if (file_line /= ' ') then
                    arg_count = arg_count + 1
                    command_arguments_list(arg_count) = file_line
                endif
            enddo
            close(logical_unit)
        endif

    end subroutine store_command_arguments

    function get_argument(index) result(argument_value)
        !! Returns the command line argument at the specified index.
        !! If the index is out of range, it returns a single space.

        integer(kind = int_wp), intent(in) :: index
        character(len = len(command_arguments_list(1))) :: argument_value

        if (index < 0 .or. index >= size(command_arguments_list)) then
            argument_value = ' '
        else
            argument_value = command_arguments_list(index + 1)
        endif

    end function get_argument

    integer function get_number_of_arguments()
        !! Returns the number of stored arguments in the command_arguments_list.

        if (allocated(command_arguments_list)) then
            get_number_of_arguments = size(command_arguments_list)
        else
            get_number_of_arguments = 0
        endif

    end function get_number_of_arguments

    subroutine get_argument_from_list(arg_index, argument_line)
        !! Retrieves the arg_index'th argument from the command line
        !! or stored arguments.

        integer(kind = int_wp), intent(in) :: arg_index     !! The index of the argument to retrieve
        character(len = *), intent(out) :: argument_line    !! The retrieved argument line

        integer(kind = int_wp) :: stored_arg_count, system_arg_count    ! The number of stored and system arguments
        character(len = 256) :: buffer
        integer(kind = int_wp) :: file_arg_count, io_status
        logical :: file_exists
        integer(kind = int_wp) :: logical_unit

        ! Check if there are stored arguments
        stored_arg_count = get_number_of_arguments()
        if (stored_arg_count > 0) then
            ! Use stored arguments
            argument_line = get_argument(arg_index)
        else
            ! Determine the number of arguments provided by the system
            system_arg_count = command_argument_count() + 1

            ! Get argument from command line
            call get_command_argument(arg_index, buffer)
            argument_line = buffer
            ! TODO: check why copying buffer to argument_line? also the argument_line is not used and is reassigned
            !   later

            ! Read additional arguments from file
            inquire(file = options_file_name, exist = file_exists)
            if (file_exists) then
                if (system_arg_count > 0) then
                    file_arg_count = arg_index - system_arg_count
                else
                    file_arg_count = arg_index - 1
                end if

                open(newunit = logical_unit, file = options_file_name)
                do
                    read(logical_unit, '(a)', iostat = io_status) argument_line
                    if (io_status /= 0) exit
                    if (argument_line /= ' ') then
                        if (file_arg_count == 0) then
                            exit
                        endif
                        file_arg_count = file_arg_count - 1
                    endif
                end do
                close(logical_unit)
            endif
        endif
    end subroutine get_argument_from_list

    integer function count_total_arguments() result(total_arg_count)
        !! Counts the total number of arguments by adding the number of stored
        !! arguments and additional arguments from the 'options_file_name' file.

        integer(kind = int_wp) :: system_arg_count
        logical :: options_file_exists
        integer(kind = int_wp) :: file_logical_unit, io_error_status
        character(len = 20) :: file_line

        ! Initialize the count with the number of stored arguments
        total_arg_count = get_number_of_arguments()

        if (total_arg_count == 0) then
            ! Count arguments provided by the system command line
            system_arg_count = command_argument_count() + 1  ! Includes the program name as the zeroth argument

            ! Ensure at least one argument (the program name)
            if (system_arg_count == 0) then
                total_arg_count = 1
            else
                total_arg_count = system_arg_count
            endif

            ! Check if 'options_file_name' file exists for additional arguments
            inquire(file = options_file_name, exist = options_file_exists)
            if (options_file_exists) then
                open(newunit = file_logical_unit, file = options_file_name)
                do
                    ! Read each line from the file
                    read(file_logical_unit, '(a)', iostat = io_error_status) file_line
                    if (io_error_status /= 0) exit
                    if (file_line /= ' ') then
                        total_arg_count = total_arg_count + 1
                    endif
                end do
                close(file_logical_unit)
            endif
        endif
    end function count_total_arguments

    subroutine retrieve_command_argument (command_str, arg_mode, command_found, int_arg, real_arg, char_arg, &
            error_status)
        !! count_total_arguments, gives no. of commandline arguments get_argument_from_list, gives argument

        use m_string_manipulation, only : upper_case
        USE Timers

        CHARACTER(len = *), intent(in) :: command_str        !! Command to be looked for
        integer(kind = int_wp), intent(in) :: arg_mode       !! Kind of command argument
        !! (0 , no argument asked), (1 , integer argument), (2 , real argument), (3 , character argument)
        integer(kind = int_wp), intent(out) :: int_arg       !! Integer argument
        CHARACTER(len = *), intent(out) :: char_arg          !! Character argument
        integer(kind = int_wp), intent(out) :: error_status  !! Error status
        REAL(kind = real_wp), intent(out) :: real_arg        !! Real argument
        LOGICAL, intent(out) :: command_found                !! Flag if command is found

        integer(kind = int_wp) :: start_index, end_index, arg_i, arg_index, command_len, arg_len
        character(len = 256) :: arg_value, arg_upper, command_upper
        integer(kind = int_wp) :: I, time_handle = 0
        if (timon) call timstrt("getcom", time_handle)

        command_found = .FALSE.
        error_status = 0

        start_index = 0
        command_len = LEN(command_str)
        end_index = command_len

        ! Trim and find the actual length of the command string
        DO I = 1, command_len
            IF (command_str(I:I) /= ' ' .AND. start_index == 0) THEN
                start_index = I
            ELSEIF (command_str(I:I) == ' ' .AND. start_index /= 0) THEN
                end_index = I - 1
                exit
            ENDIF
        end do

        IF (start_index == 0) go to 800

        arg_len = end_index - start_index + 1

        call upper_case(command_str(start_index:end_index), command_upper, arg_len)

        ! Iterate through command-line arguments to find a match
        DO arg_i = 2, count_total_arguments()
            CALL get_argument_from_list(arg_i - 1, arg_value)
            call upper_case(arg_value, arg_upper, arg_len)
            IF (arg_upper(1:arg_len) == command_upper(1:arg_len)) THEN
                command_found = .TRUE.
                arg_index = arg_i - 1
                exit
            ENDIF
        end do

        ! Process the argument based on the argument mode (arg_mod)
        IF (command_found) THEN
            CALL get_argument_from_list(arg_index, arg_upper)
            IF (arg_mode == 1) THEN
                IF (arg_upper(arg_len + 1:arg_len + 1) /= ' ') THEN
                    READ (arg_upper(arg_len + 1:), '(I20)', ERR = 110) int_arg
                    GOTO 111
                    110          CONTINUE
                    error_status = 3
                    111          CONTINUE
                ELSEIF (arg_index < count_total_arguments() - 1) THEN
                    CALL get_argument_from_list(arg_index + 1, arg_upper)
                    READ (arg_upper, '(I20)', ERR = 120) int_arg
                    GOTO 121
                    120          CONTINUE
                    error_status = 2
                    121          CONTINUE
                ELSE
                    error_status = 1
                ENDIF
            ELSEIF (arg_mode == 2) THEN
                IF (arg_upper(arg_len + 1:arg_len + 1) /= ' ') THEN
                    READ (arg_upper(arg_len + 1:), '(F20.0)', ERR = 130) real_arg
                    GOTO 131
                    130          CONTINUE
                    error_status = 3
                    131          CONTINUE
                ELSEIF (arg_index < count_total_arguments() - 1) THEN
                    CALL get_argument_from_list(arg_index + 1, arg_upper)
                    READ (arg_upper, '(F20.0)', ERR = 140) real_arg
                    GOTO 141
                    140          CONTINUE
                    error_status = 2
                    141          CONTINUE
                ELSE
                    error_status = 1
                ENDIF
            ELSEIF (arg_mode == 3) THEN
                IF (arg_upper(arg_len + 1:arg_len + 1) /= ' ') THEN
                    char_arg = arg_upper(arg_len + 1:)
                ELSEIF (arg_index < count_total_arguments() - 1) THEN
                    CALL get_argument_from_list(arg_index + 1, char_arg)
                    IF (char_arg(1:1) == '-') THEN
                        char_arg = ' '
                        error_status = 1
                    ENDIF
                ELSE
                    error_status = 1
                ENDIF
            ENDIF
        ENDIF
        800 CONTINUE

        if (timon) call timstop(time_handle)
        RETURN
    END SUBROUTINE retrieve_command_argument

    subroutine get_input_filename (file_name, extension_check)
        !! read an input filename, either from the command line, a steering file, or interactively
        !! from the user. It also optionally checks the existence of a file with a specified extension

        USE ISO_FORTRAN_ENV, ONLY : INPUT_UNIT, OUTPUT_UNIT
        use m_srstop, only : SRSTOP

        character(len = *), intent(out) :: file_name        !! file name input file
        character(len = *), intent(in) :: extension_check   !! if not empty then check existance of file with this
        !! extension when name is enterd interactive from keyboard

        CHARACTER(len = 9) :: steering_file_name
        CHARACTER(len = 256) :: filename_with_ext
        CHARACTER(len = 3) :: user_answer
        LOGICAL :: file_exists, command_found
        CHARACTER(len = 1) :: path_separator_unix, path_separator_windows, char_arg
        INTEGER(kind = int_wp) :: file_unit, name_length, int_arg, error_status, quotes_index, i, j, IOERR
        REAL(kind = real_wp) :: real_arg

        path_separator_unix = '/'
        path_separator_windows = '\\'

        name_length = LEN(file_name)
        ! first argument from command line if it is not a option starting with -
        CALL get_argument_from_list (1, file_name)

        ! Get filename from steering file Delft3D
        IF (file_name == ' ' .OR. file_name(1:1) == '-') THEN
            steering_file_name = ' '
            CALL retrieve_command_argument ('-waq', 0, command_found, int_arg, real_arg, char_arg, error_status)
            IF (command_found) THEN
                steering_file_name = 'runid.waq'
            ENDIF
            CALL retrieve_command_argument ('-sed', 0, command_found, int_arg, real_arg, char_arg, error_status)
            IF (command_found) THEN
                steering_file_name = 'runid.sed'
            ENDIF
            CALL retrieve_command_argument ('-eco', 0, command_found, int_arg, real_arg, char_arg, error_status)
            IF (command_found) THEN
                steering_file_name = 'runid.eco'
            ENDIF
            CALL retrieve_command_argument ('-chem', 0, command_found, int_arg, real_arg, char_arg, error_status)
            IF (command_found) THEN
                steering_file_name = 'runid.chm'
            ENDIF
            IF (steering_file_name /= ' ') THEN
                INQUIRE (FILE = steering_file_name, EXIST = file_exists)
                IF (file_exists) THEN
                    OPEN(NEWUNIT = file_unit, FILE = steering_file_name)
                    READ(file_unit, '(A)', IOSTAT = IOERR) file_name
                    IF (IOERR /= 0) file_name = ' '
                    CLOSE(file_unit)
                ENDIF
            ENDIF
        ENDIF

        ! Get filename filename from keyboard
        IF (file_name == ' ' .OR. file_name(1:1) == '-') THEN
            WRITE (OUTPUT_UNIT, *) ' Name of the model files ? '
            WRITE (OUTPUT_UNIT, *) ' DELWAQ will provide the extensions. '
            READ  (INPUT_UNIT, '(A)')   file_name

            ! TODO: could be abstracted to a separate subroutine in m_cli_utils `remove_quotes_from_filename`
            ! remove quotes if they are the first and the last
            IF (file_name(1:1) == '''') THEN
                quotes_index = INDEX (file_name(2:), '''')
                IF (quotes_index /= 0) THEN
                    IF (quotes_index == name_length - 1) THEN
                        file_name(quotes_index + 1:) = ' '
                        file_name = file_name(2:)
                    ELSEIF (file_name(quotes_index + 2:) == ' ') THEN
                        file_name(quotes_index + 1:) = ' '
                        file_name = file_name(2:)
                    ENDIF
                ENDIF
            ENDIF
            ! If empty string then stop
            IF (file_name == ' ') THEN
                WRITE (OUTPUT_UNIT, *) ' ERROR no filename entered!'
                CALL SRSTOP(1)
            ENDIF
            ! check existence of output file
            IF (extension_check /= ' ') THEN
                quotes_index = INDEX (file_name, ' ')
                IF (quotes_index == 0) quotes_index = name_length + 1
                filename_with_ext = file_name(1:quotes_index - 1) // extension_check
                INQUIRE (FILE = filename_with_ext, EXIST = file_exists)

                IF (file_exists) THEN
                    WRITE (OUTPUT_UNIT, *) ' File:', filename_with_ext(1:quotes_index + 3), ' already exist.'
                    WRITE (OUTPUT_UNIT, *) ' Do you want it to be replaced ? '
                    WRITE (OUTPUT_UNIT, *) ' Answer yes or no ? '
                    READ  (INPUT_UNIT, '(A3)') user_answer
                    IF (user_answer(1:1) == 'N' .OR.  user_answer(1:1) == 'n') CALL SRSTOP (1)
                ENDIF
            ENDIF

        ELSE

            ! if there is remove extension
            DO i = name_length, 1, -1
                IF (file_name(i:i) == path_separator_unix) exit
                IF (file_name(i:i) == path_separator_windows) exit
            end do

            IF (i == 0) i = 1
            j = INDEX (file_name(i:), '.', .TRUE.)
            IF (j > 0) file_name(i + j - 1:name_length) = ' '

        ENDIF

    end subroutine get_input_filename

end module m_cli_utils
