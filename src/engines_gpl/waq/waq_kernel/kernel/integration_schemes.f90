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

module integration_schemes
    !!     Module integration_schemes:
    !!     - Encapsulate the interface of run_integration_schemes and initialize_all_conditions:
    !!       A, J and C are now pointers to arrays
    use m_waq_precision
    use m_integration_scheme_25
    use m_integration_scheme_24
    use m_integration_scheme_23
    use m_integration_scheme_21_22
    use m_integration_scheme_18
    use m_integration_scheme_17
    use m_integration_scheme_16
    use m_integration_scheme_15
    use m_integration_scheme_13
    use m_integration_scheme_12
    use m_integration_scheme_11
    use m_integration_scheme_14
    use m_integration_scheme_7
    use m_integration_scheme_6
    use m_integration_scheme_5
    use m_integration_scheme_1
    use m_integration_scheme_0
    use m_startup_screen
    use m_cli_utils, only : retrieve_command_argument
    use m_open_waq_files

    implicit none

    integer(kind = int_wp), parameter :: PAGE_LENGTH = 64         !! pagelength for output in lines
    integer(kind = int_wp), parameter :: NUM_FILES = 50          !! number of files to be opened
    integer(kind = int_wp), parameter :: FILE_NAME_LEN = 255       !! length file names

    private
    public :: run_integration_schemes

contains

    subroutine run_integration_schemes(buffer, max_real_arr_size, max_int_arr_size, max_char_arr_size, init, &
            action, dlwqd)

        use m_grid_utils_external
        USE initialize_conditions, only : initialize_all_conditions
        USE Timers
        use delwaq2_data
        use m_waq_data_buffer
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_cli_utils, only : get_input_filename
        use m_logger, only : set_log_unit_number

        type(waq_data_buffer), target :: buffer
        integer(kind = int_wp) :: max_real_arr_size, max_int_arr_size, max_char_arr_size
        logical :: init             !!  if T boot the system if F no initialisation
        logical :: exists
        integer(kind = int_wp) :: action    !! indication of the action to be performed
        type(delwaq_data), target :: dlwqd
        type(gridpointercoll), pointer, save :: gridps          !! collection of all grid definitions
        integer(kind = int_wp) :: input_file                    !! unit nummer of the common boot-file

        ! input structure for boot-file
        integer(kind = int_wp), save :: file_unit_list(NUM_FILES)
        character*(FILE_NAME_LEN), save :: file_name_list(NUM_FILES)
        integer(kind = int_wp), save :: filtype(NUM_FILES)
        character*(FILE_NAME_LEN), save :: runid
        logical, save :: init2 = .true. ! to suppress the start-up screen

        logical :: lfound
        integer(kind = int_wp) :: idummy, ierr2
        real(kind = real_wp) :: rdummy
        character :: cdummy
        character*2 :: c2

        integer(kind = int_wp), save :: ithndl = 0

        integer(kind = int_wp), save :: indx
        integer(kind = int_wp) :: ierr
        integer(kind = int_wp) :: imr
        integer(kind = int_wp) :: imi
        integer(kind = int_wp) :: imc
        integer(kind = int_wp) :: file_unit_i
        integer(kind = int_wp) :: ierrd
        integer(kind = int_wp) :: k

        IF (INIT) THEN
            call timini ()
            ! for openda-usage, where multiple instances are launched,
            ! the time module does not work correctly.
            if (dlwqd%set_timer) timon = .true.
            timon = .true.
            if (timon) call timstrt("integration_schemes", ithndl)

            ! boot the system; read dimensions of sysn from delwaq03.wrk-file
            CALL get_input_filename(RUNID, '.mon')
            file_name_list(1) = TRIM(RUNID) // '-delwaq03.wrk'

            ! produce a user-friendly message if the 03 work file is missing,
            ! an indication that DELWAQ1 was not able to complete its job properly.
            inquire(file = file_name_list(1), exist = exists)
            if (.not. exists) then
                write(*, '(a)') 'integration_schemes cannot run - the system work file is missing'
                write(*, '(2a)') '    File name: ', trim(file_name_list(1))
                write(*, '(2a)') '    Please check if DELWAQ1 ran correctly'
                call terminate_execution(1)
            endif

            ! the file does exist, so continue processing
            CALL open_waq_files(input_file, file_name_list(1), 1, 2, IERR)
            IF (IERR > 0) GOTO 999
            read  (input_file)   in
            read  (input_file)   ii
            read  (input_file)   imr, imi, imc
            read  (input_file) (file_unit_list(k), k = 1, num_file_units)
            read  (input_file) (file_name_list(k), k = 1, num_file_units)
            read  (input_file) (filtype(k), k = 1, num_file_units)
            do file_unit_i = 1, num_file_units
                close (file_unit_list(file_unit_i))
            end do
            close(input_file)

            CALL open_waq_files(file_unit_list(19), file_name_list(19), 19, 1, IERRD)
            CALL set_log_unit_number(file_unit_list(19))

            ! Show startup screen
            IF (INIT2) THEN
                INIT2 = .FALSE.
                CALL startup_screen(file_unit_list(19))
            ENDIF

            IF (ACTION == ACTION_FULLCOMPUTATION) THEN
                WRITE(*, *)
                WRITE(*, '(A9,A)') '  runid: ', TRIM(RUNID)
                WRITE(*, *)
            ENDIF

            ! collaborative call to i0
            IERR = 0
            gridps => dlwqd%gridps
            call initialize_all_conditions(buffer, NUM_FILES, max_real_arr_size, max_int_arr_size, max_char_arr_size, &
                    PAGE_LENGTH, file_unit_list, file_name_list, filtype, gridps, dlwqd, ierr)

            IF (IERR > 0) GOTO 992
            ! end of initialisation
            WRITE (*, *)
            WRITE (*, *) ' SIMULATION STARTED '
            WRITE (*, *)
            WRITE (*, *) ' INTEGRATION ROUTINE =', intsrt
        ENDIF

        ! Store the local persistent variables
        DLWQD%II = II
        DLWQD%IN = IN

        ! branch to the appropriate integration option
        select case (intsrt)

        case(0)
            ! not transport, just processes
            call integration_scheme_0(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        case(1)
            ! backward in space and time
            call integration_scheme_1(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        case(2, 3, 4) ! deprecated
            goto 991
        case(5)     ! Flux corrected transport
            call integration_scheme_5(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        case(6)     ! Direct steady state, backward differences in space
            call integration_scheme_6(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        case(7)     ! Direct steady state, central differences in space
            call integration_scheme_7(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        case(8, 9, 10) ! deprecated
            goto 991
        case(11)     ! Horizontal explicit upwind, vertical implicit central
            call integration_scheme_11(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(12)     ! Horizontal explicit FCT   , vertical implicit central
            call integration_scheme_12(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(13)     ! Horizontal explicit upwind, vertical implicit upwind
            call integration_scheme_13(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(14)     ! Horizontal explicit FCT   , vertical implicit upwind
            call integration_scheme_14(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(15)     ! GMRES, horizontal upwind, vertical upwind
            call integration_scheme_15(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(16)     ! GMRES, horizontal upwind, vertical central
            call integration_scheme_16(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(17)     ! stationary GMRES, horizontal upwind, vertical upwind
            call integration_scheme_17(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(18)     ! stationary GMRES, horizontal upwind, vertical central
            call integration_scheme_18(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(19, 20) ! deprecated
            goto 991

        case(21)     ! Self adjusting teta method (limiter Salezac)
            call integration_scheme_21_22(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(22)     ! Self adjusting teta method (limiter Boris and Book)
            call integration_scheme_21_22(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(23)     ! Leonards QUICKEST
            call integration_scheme_23(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(24)     ! Local flexible time step method by Leonard Postma
            call integration_scheme_24(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case(25)     ! Special for emission module
            call integration_scheme_25(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        case default
            goto 990

        end select

        if (action == action_finalisation    .or. &
                action == action_fullcomputation) then
            ! print timer-results
            if (timon) then
                call timstop (ithndl)
                call timdump (TRIM(RUNID) // '-timers.out')
                call timfinalize()
            endif
        endif

        return

        990 WRITE (*, *) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED'
        CALL terminate_execution(1)
        991 WRITE (*, *) ' ERROR: INTEGRATION OPTION DEPRECATED'
        CALL terminate_execution(1)
        992 WRITE (*, *) ' ERROR : INITIALISATION FAILED'
        CALL terminate_execution(1)
        999 WRITE (*, *) ' ERROR: NO VALID SET OF MODEL-INTERMEDIATE-FILES'
        CALL terminate_execution(1)
    END SUBROUTINE run_integration_schemes

END MODULE integration_schemes
