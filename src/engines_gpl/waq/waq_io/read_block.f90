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
module m_read_block
    use m_waq_precision
    use waq_timers, only : read_time_delay
    use open_data_structure, only : read_data_ods
    use m_time_validation
    use m_error_status

    implicit none

    private
    public :: read_block

contains


    subroutine read_block(lun, lchar, filtype, inpfil, output_verbose_level, &
            iwidth, substances, constants, parameters, functions, &
            segfuncs, segments, gridps, data_block, ierr, &
            status)
        !! Reads a block of input items ( procesparameters, initial conditions )

        use open_data_structure, only : read_time_dependant_data_matrix
        use simulation_input_options, only : process_simulation_input_options
        use matrix_utils, only : compute_matrix, print_matrix
        use m_cli_utils, only : retrieve_command_argument
        use m_grid_utils_external          ! for the storage of contraction grids
        use m_waq_data_structure  ! for definition and storage of data
        use rd_token
        use timers       !   performance timers

        integer(kind = int_wp), intent(inout) :: lun(*)        !< unit numbers used
        character(len = *), intent(inout) :: lchar(*)     !< filenames
        integer(kind = int_wp), intent(inout) :: filtype(*)    !< type of binary file
        type(t_input_file), intent(inout) :: inpfil       !< input file structure with include stack and flags
        integer(kind = int_wp), intent(in) :: output_verbose_level        !< level of reporting to ascii output file
        integer(kind = int_wp), intent(in) :: iwidth        !< width of output
        type(t_waq_item), intent(inout) :: substances   !< delwaq substances list
        type(t_waq_item), intent(inout) :: constants    !< delwaq constants list
        type(t_waq_item), intent(inout) :: parameters   !< delwaq parameters list
        type(t_waq_item), intent(inout) :: functions    !< delwaq functions list
        type(t_waq_item), intent(inout) :: segfuncs     !< delwaq segment-functions list
        type(t_waq_item), intent(inout) :: segments     !< delwaq segments name list
        type(GridPointerColl), intent(in) :: GridPs       !< collection off all grid definitions
        type(t_data_block), intent(out) :: data_block   !< data block to be filled
        integer(kind = int_wp), intent(out) :: ierr          !< output error count

        type(error_status), intent(inout) :: status !< current error status

        type(t_data_block) :: data_buffer  ! data block to be read
        type(t_waq_item) :: waq_param    ! list of param items to be set in this block ( substances etc )
        type(t_waq_item) :: data_param   ! list of param items in the data
        type(t_waq_item) :: waq_loc      ! list of loc items to be set in this block (segments, boundaries, loads)
        type(t_waq_item) :: data_loc     ! list of loc items in the data
        type(t_waq_item) :: types        ! delwaq (item-) type list, not relevant here for boundaries, loads
        type(t_fdata) :: odsdata      ! funtion data block to be read
        type(t_fdata) :: fdata        ! funtion data block to be read
        integer(kind = int_wp) :: ierr2         ! local error indicator (ierr2 = 2, end of block)
        integer(kind = int_wp) :: i_base_grid   ! index of base grid
        integer(kind = int_wp) :: igrid         ! index of input grid
        integer(kind = int_wp) :: noseg         ! number of segments
        integer(kind = int_wp) :: noseg_org     ! original number of segments
        integer(kind = int_wp) :: i             ! loop counter
        integer(kind = int_wp) :: noits         ! number of scale factors / columns sybstances
        integer(kind = int_wp) :: noits_loc     ! number of scale factors locations
        integer(kind = int_wp) :: ndim1         ! first dimension matrix
        integer(kind = int_wp) :: ndim2         ! second dimension matrix
        real(kind = real_wp) :: missing_value         ! missing value
        integer(kind = int_wp) :: t_asked       ! type of token asked
        integer(kind = int_wp) :: itype         ! type of token
        character(len = 256) :: ctoken       ! character token from input
        integer(kind = int_wp) :: itoken        ! integer token from input
        real(kind = real_wp) :: rtoken        ! real token from input
        character :: cdummy       ! dummy not used
        integer(kind = int_wp) :: idummy        ! dummy not used
        real(kind = real_wp) :: rdummy        ! dummy not used
        character(len = 256) :: adummy       ! dummy not used
        character(len = 10) :: callr        ! kind of item
        character(len = 10) :: strng1       ! kind of item
        character(len = 10) :: strng2       ! kind of item
        character(len = 10) :: strng3       ! kind of item

        logical :: lfound       ! Keyword found (or not)
        logical :: lsegfuncheck ! Do check if segmentfunctions are correct
        integer(kind = INT64) :: filesize      ! Reported size of the file

        logical       is_date_format, is_ddhhmmss_format, is_yyddhh_format
        integer(kind = int_wp) :: chkflg, itfact
        integer(kind = int_wp) :: nocol         ! number of columns in input
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_block", ithndl)

        call retrieve_command_argument ('-nosegfuncheck', 0, lfound, idummy, rdummy, adummy, ierr2)
        lsegfuncheck = .not. lfound

        ! defaults and initialisation
        data_block%subject = SUBJECT_UNKNOWN
        data_block%num_parameters = 0
        data_block%num_locations = 0
        data_block%num_breakpoints = 0
        data_block%function_type = FUNCTYPE_CONSTANT
        data_block%igrid = 1
        data_block%is_external = .false.
        data_block%filetype = FILE_NONE
        data_block%filename = ' '
        data_block%iorder = ORDER_UNKNOWN
        data_block%is_parameter_named = .false.
        data_block%param_name => null()
        data_block%are_location_named = .false.
        data_block%loc_name => null()
        data_block%is_parameter_pointered = .false.
        data_block%param_pointers => null()
        data_block%are_locations_pointered = .false.
        data_block%location_pointers => null()
        data_block%is_scaled = .false.
        data_block%scale_factor = 1.0
        data_block%need_parameters_scaling = .false.
        data_block%parameter_scale_factor => null()
        data_block%are_locations_default = .false.
        data_block%need_location_scaling = .false.
        data_block%location_scale_factor => null()
        data_block%times => null()
        data_block%values => null()

        ierr2 = waq_param%initialize()
        ierr2 = data_param%initialize()
        ierr2 = waq_loc%initialize()
        ierr2 = data_loc%initialize()
        ierr2 = types%initialize()

        i_base_grid = GridPs%base_grid
        noseg = GridPs%Pointers(i_base_grid)%noseg
        noseg_org = get_original_noseg()

        ! initialise a number of variables
        ierr = 0
        missing_value = -999.0
        is_date_format = inpfil%is_date_format
        is_ddhhmmss_format = inpfil%is_ddhhmmss_format
        is_yyddhh_format = inpfil%is_yyddhh_format
        itfact = inpfil%itfact

        ! loop over the input (tokens) till input is ready or error
        do

            if (gettoken(ctoken, ierr2) /= 0) exit

            if (ctoken == 'ABSOLUTE') then
                write (lunut, 1900)
                if (data_block%function_type == 0) data_block%function_type = FUNCTYPE_BLOCK
                cycle
            endif
            if (ctoken == 'BLOCK') then
                write (lunut, 2000)
                data_block%function_type = FUNCTYPE_BLOCK
                cycle
            endif
            if (ctoken == 'LINEAR') then
                write (lunut, 2000)
                data_block%function_type = FUNCTYPE_LINEAR
                cycle
            endif
            if (ctoken == 'HARMONICS') then
                write (lunut, 2005)
                data_block%function_type = FUNCTYPE_HARMONIC
                cycle
            endif
            if (ctoken == 'FOURIERS') then
                write (lunut, 2010)
                data_block%function_type = FUNCTYPE_FOURIER
                cycle
            endif
            if (ctoken == 'TIME_DELAY') then
                call read_time_delay (ierr2)
                if (ierr2 /= 0) goto 100
                cycle
            endif
            if (ctoken == 'ODS_FILE') then
                if (data_block%function_type == FUNCTYPE_HARMONIC .or. data_block%function_type == FUNCTYPE_FOURIER) then
                    write (lunut, 2100)
                    ierr = 1
                    exit
                endif
                data_block%is_external = .true.
                data_block%filetype = FILE_ODS
                cycle
            endif
            if (ctoken == 'BINARY_FILE') then
                if (data_block%function_type == FUNCTYPE_HARMONIC .or. data_block%function_type == FUNCTYPE_FOURIER) then
                    write (lunut, 2110)
                    ierr = 1
                    exit
                endif
                data_block%is_external = .true.
                data_block%filetype = FILE_BINARY
                cycle
            endif
            if (ctoken == 'UNFORMATTED') then
                if (data_block%function_type == FUNCTYPE_HARMONIC .or. data_block%function_type == FUNCTYPE_FOURIER) then
                    write (lunut, 2110)
                    ierr = 1
                    exit
                endif
                data_block%is_external = .true.
                data_block%filetype = FILE_UNFORMATTED
                cycle
            endif
            if (ctoken == 'BIG_ENDIAN') then
                if (data_block%function_type == FUNCTYPE_HARMONIC .or. data_block%function_type == FUNCTYPE_FOURIER) then
                    write (lunut, 2110)
                    ierr = 1
                    exit
                endif
                data_block%is_external = .true.
                data_block%filetype = data_block%filetype + FILE_BIG_ENDIAN
                cycle
            endif
            if (ctoken == 'MULTIPLEHYD_FILE') then
                if (data_block%function_type == FUNCTYPE_HARMONIC .or. data_block%function_type == FUNCTYPE_FOURIER) then
                    write (lunut, 2110)
                    ierr = 1
                    exit
                endif
                if (.not. (data_block%subject == SUBJECT_FUNCTION .or. data_block%subject == SUBJECT_SEGFUNC)) then
                    write (lunut, 2270)
                    ierr = 1
                    exit
                endif
                if (data_param%no_item /= 1) then
                    write (lunut, 2280)
                    ierr = 1
                    exit
                endif

                ! handle file option, should we resolve the use of 17? = work file segment-functions
                call process_simulation_input_options(-4, lun, 17, lchar, filtype, &
                        is_date_format, is_yyddhh_format, noseg, ierr2, status, &
                        .false.)
                if (ierr2 /= 0) exit

                ierr2 = puttoken(lchar(17))
                data_block%is_external = .true.
                data_block%filetype = FILE_BINARY
                cycle
            endif
            if (ctoken == 'INITIALS') then
                callr = 'initial'
                strng1 = 'segments'
                strng2 = 'substance'
                data_block%subject = SUBJECT_INITIAL
                data_block%function_type = FUNCTYPE_CONSTANT
                chkflg = 1
                call read_items(lunut, inpfil, output_verbose_level, chkflg, callr, &
                        waq_param, data_param, substances, types, noits, &
                        ierr2, status)

                if (ierr2 /= 0) then
                    write (lunut, 2120)
                    goto 100
                endif
                cycle
            endif
            if (ctoken == 'CONSTANTS') then
                callr = 'constant'
                data_block%subject = SUBJECT_CONSTANT
                data_block%function_type = FUNCTYPE_CONSTANT
                data_block%num_locations = 1
                data_block%iorder = ORDER_PARAM_LOC
                ierr2 = waq_loc%resize(1)
                waq_loc%no_item = 1
                waq_loc%name(1) = 'constant'
                waq_loc%ipnt(1) = 1

                chkflg = 0

                call read_items(lunut, inpfil, output_verbose_level, chkflg, callr, &
                        waq_param, data_param, constants, types, noits, &
                        ierr2, status)

                if (ierr2 /= 0) then
                    write (lunut, 2120)
                    ierr = 1
                    exit
                endif

                cycle
            endif

            if (ctoken == 'FUNCTIONS') then
                callr = 'function'
                data_block%subject = SUBJECT_FUNCTION
                if (data_block%function_type == FUNCTYPE_CONSTANT) data_block%function_type = FUNCTYPE_BLOCK
                data_block%num_locations = 1
                data_block%iorder = ORDER_PARAM_LOC
                write (lunut, *) ' '
                ierr2 = waq_loc%resize(1)
                waq_loc%no_item = 1
                waq_loc%name(1) = 'constant'
                waq_loc%ipnt(1) = 1

                chkflg = 0

                call read_items(lunut, inpfil, output_verbose_level, chkflg, callr, &
                        waq_param, data_param, functions, types, noits, &
                        ierr2, status)

                if (ierr2 /= 0) then
                    write (lunut, 2120)
                    goto 100
                endif
                cycle
            endif

            if (ctoken=='PARAMETERS') then
                strng1 = 'parameter'
                chkflg = -1
                data_block%subject = SUBJECT_PARAMETER
                data_block%function_type = 0
                write (lunut, *) ' '

                call read_items(lunut, inpfil, output_verbose_level, chkflg, strng1, &
                        waq_param, data_param, parameters, types, noits, &
                        ierr2, status)

                if (ierr2 /= 0) then
                    write (lunut, 2150)
                    goto 100
                endif
                if (data_param%no_item /= 0 .and. data_block%iorder == ORDER_UNKNOWN) data_block%iorder = ORDER_PARAM_LOC
                cycle
            endif

            if (ctoken=='SEG_FUNCTIONS') then
                strng1 = 'seg-funct.'
                chkflg = 0
                data_block%subject = SUBJECT_SEGFUNC
                if (data_block%function_type == 0) data_block%function_type = 1
                write (lunut, *) ' '

                call read_items(lunut, inpfil, output_verbose_level, chkflg, strng1, &
                        waq_param, data_param, segfuncs, types, noits, &
                        ierr2, status)

                if (ierr2 /= 0) then
                    write (lunut, 2150)
                    goto 100
                endif
                if (data_param%no_item /= 0 .and. data_block%iorder == ORDER_UNKNOWN) data_block%iorder = ORDER_LOC_PARAM
                cycle
            endif

            if (ctoken  == 'SEGMENTS' .or. ctoken == 'ALL' .or. ctoken == 'INPUTGRID') then
                if (waq_loc%no_item  == -1) then
                    write (lunut, 2170)
                    ierr = 1
                    goto 100
                endif
                if (ctoken == 'ALL') then
                    waq_loc%no_item = noseg
                    write (lunut, 2020) waq_loc%no_item
                    ierr2 = waq_loc%resize(waq_loc%no_item)
                    do i = 1, waq_loc%no_item
                        waq_loc%ipnt(i) = i
                        write(waq_loc%name(i), '(''segment '',i8)') i
                    enddo
                elseif (ctoken == 'INPUTGRID') then
                    if (gettoken(ctoken, ierr2) /= 0) goto 100
                    igrid = gridps%find_column(ctoken)
                    if (igrid >= 1) then
                        data_block%igrid = igrid
                        write (lunut, 2290), trim(ctoken)
                        waq_loc%no_item = gridps%pointers(igrid)%noseg
                        write (lunut, 2300) waq_loc%no_item
                        ierr2 = waq_loc%resize(waq_loc%no_item)
                        do i = 1, waq_loc%no_item
                            waq_loc%ipnt(i) = i
                            write(waq_loc%name(i), '(''segment '',i8)') i
                        enddo
                    else
                        write (lunut, 2310), trim(ctoken)
                        ierr = 1
                        goto 100
                    endif
                else
                    callr = 'segment'
                    chkflg = 1
                    data_block%are_locations_pointered = .true.

                    call read_items(lunut, inpfil, output_verbose_level, chkflg, callr, &
                            waq_loc, data_loc, segments, types, noits_loc, &
                            ierr2, status)

                    if (ierr2 /= 0) then
                        write (lunut, 2180)
                        goto 100
                    endif
                endif
                if (data_param%no_item == 0) then
                    write (lunut, 2030) strng1, strng1
                    data_block%iorder = ORDER_LOC_PARAM
                    if (data_block%subject == SUBJECT_PARAMETER) ierr2 = puttoken('PARAMETERS')
                    if (data_block%subject == SUBJECT_SEGFUNC) ierr2 = puttoken('SEG_FUNCTIONS')
                    if (data_block%subject == SUBJECT_INITIAL) ierr2 = puttoken('INITIALS')
                else
                    write (lunut, 2040) strng1, strng1
                    data_block%iorder = ORDER_PARAM_LOC
                endif
                cycle
            endif

            if (ctoken == 'DEFAULTS') then
                if (data_block%subject == SUBJECT_PARAMETER .or. &
                        data_block%subject == SUBJECT_INITIAL   .or. &
                        data_block%subject == SUBJECT_SEGFUNC) then

                    data_block%iorder = ORDER_PARAM_LOC
                    data_block%are_locations_default = .true.
                    if (data_param%no_item > 0) then
                        ctoken = 'DATA'
                    else
                        if (data_block%subject == SUBJECT_PARAMETER) ctoken = 'PARAMETERS'
                        if (data_block%subject == SUBJECT_INITIAL) ctoken = 'INITIALS'
                        if (data_block%subject == SUBJECT_SEGFUNC) ctoken = 'SEG_FUNCTIONS'
                    endif
                    ierr2 = puttoken(ctoken)
                    ierr2 = waq_loc%resize(1)
                    waq_loc%no_item = 1
                    waq_loc%name(1) = 'defaults'
                    waq_loc%ipnt(1) = 0
                    cycle
                else
                    write (lunut, 2190)
                    ierr = 1
                    goto 100
                endif
            endif

            if (ctoken == 'DATA' .or. data_block%is_external) then
                if (data_block%subject == SUBJECT_CONSTANT) then
                    strng1 = 'constants'
                    strng2 = 'values'
                endif
                if (data_block%subject == SUBJECT_PARAMETER) then
                    strng1 = 'parameters'
                    strng2 = 'segments'
                    if (waq_loc%no_item == 0) then
                        write (lunut, 2260)
                        goto 100
                    endif
                endif
                if (data_block%subject == SUBJECT_FUNCTION) then
                    strng1 = 'functions'
                    strng2 = 'values'
                endif
                if (data_block%subject == SUBJECT_SEGFUNC) then
                    strng1 = 'seg-functs'
                    strng2 = 'segments'
                    if (waq_loc%no_item == 0) then
                        write (lunut, 2260)
                        goto 100
                    endif
                endif
                strng3 = 'breakpoint'
                if (data_block%function_type == FUNCTYPE_HARMONIC) strng3 = 'harmonic'
                if (data_block%function_type == FUNCTYPE_FOURIER) strng3 = 'fourier'

                if (data_block%filetype == FILE_ODS) then

                    ! when data_loc is not filled only

                    if (data_loc%no_item == 0) then
                        ierr2 = data_loc%resize(waq_loc%no_item)
                        data_loc%no_item = waq_loc%no_item
                        data_loc%name = waq_loc%name
                        data_loc%ipnt = waq_loc%ipnt
                        data_loc%sequence = waq_loc%sequence
                        data_loc%constant = waq_loc%constant
                    endif

                    data_block%num_parameters = waq_param%no_item
                    data_block%num_locations = waq_loc%no_item

                    call read_data_ods(lunut, ctoken, data_param, data_loc, missing_value, &
                            data_buffer, ierr2)
                    if (ierr2 /= 0) goto 100
                    call compute_matrix (lunut, data_param, data_loc, waq_param, waq_loc, &
                            missing_value, data_buffer, data_block)
                    data_block%is_external = .false.
                    deallocate(data_buffer%times, data_buffer%values)

                elseif (mod(data_block%filetype, 10) == FILE_BINARY .or. &
                        mod(data_block%filetype, 10) == FILE_UNFORMATTED) then
                    if (data_block%subject == SUBJECT_SEGFUNC) data_block%iorder = ORDER_LOC_PARAM
                    write (lunut, 2220) ctoken
                    data_block%filename = ctoken

                    if (lsegfuncheck) then
                        ! Check the size of the file (if it is binary, otherwise this is not reliable)
                        call check_file_size(ctoken, noits * noseg_org, mod(data_block%filetype, 10), filesize, ierr2)
                        if (ierr2 < 0) then
                            ierr2 = 1
                            write(lunut, 2320) ctoken
                        elseif (ierr2 > 0) then
                            ierr2 = 0        ! It is a warning, proceed at your own peril
                            call status%increase_warning_count()
                            write(lunut, 2330) ctoken, filesize, 4 * (1 + noits * noseg_org), noits, noseg
                            write(lunut, 2340)
                        endif
                    end if

                else

                    ! Check if an inner loop column header exists for the data matrix

                    nocol = noits
                    call read_header(waq_param, data_param, nocol, itfact, is_date_format, &
                            is_yyddhh_format, ierr2, status)
                    if (ierr2 /= 0) goto 100

                    ! when data_loc is not filled only

                    if (data_loc%no_item == 0) then
                        ierr2 = data_loc%resize(waq_loc%no_item)
                        data_loc%no_item = waq_loc%no_item
                        data_loc%name = waq_loc%name
                        data_loc%ipnt = waq_loc%ipnt
                        data_loc%sequence = waq_loc%sequence
                        data_loc%constant = waq_loc%constant
                    endif

                    ! read the data

                    data_block%num_parameters = waq_param%no_item
                    data_block%num_locations = waq_loc%no_item

                    data_buffer%num_parameters = nocol
                    data_buffer%num_locations = data_loc%no_item
                    data_buffer%iorder = data_block%iorder
                    data_buffer%function_type = data_block%function_type

                    call read_time_dependant_data_matrix(data_buffer, itfact, is_date_format, is_yyddhh_format, ierr2)
                    if (ierr2 /= 0) goto 100

                    call validate_time_series_strictly_increasing(lunut, data_buffer, ierr2)

                    call compute_matrix (lunut, data_param, data_loc, waq_param, waq_loc, &
                            missing_value, data_buffer, data_block)
                    deallocate(data_buffer%times, data_buffer%values)
                endif
                if (ierr2 == 1 .or. ierr2 == 4) then
                    write (lunut, 2200)
                    goto 100
                endif
                if (waq_loc%no_item == -1) write (lunut, 1910)
                data_block%num_parameters = waq_param%no_item
                data_block%is_parameter_named = .true.
                data_block%param_name => waq_param%name
                data_block%is_parameter_pointered = .true.
                data_block%param_pointers => waq_param%ipnt
                data_block%num_locations = waq_loc%no_item
                data_block%are_location_named = .true.
                data_block%loc_name => waq_loc%name
                waq_param%name => null()
                waq_param%ipnt => null()
                waq_loc%name => null()
                if (data_block%are_locations_pointered) then
                    data_block%location_pointers => waq_loc%ipnt
                    waq_loc%ipnt => null()
                endif
                call print_matrix(lunut, iwidth, data_block, strng1, strng2, &
                        strng3, output_verbose_level)
                if (ierr2 == 3) goto 50
                exit
            endif

            ! unknown keyword
            write (lunut, 2210) trim(ctoken)
            ierr = 1
            exit

        enddo ! end loop over the input

        write (lunut, 1140)

        50 continue
        goto 110

        100 continue
        if (ierr2 /= 0) then
            write (lunut, 2090)
            ierr = ierr2
        endif

        110 continue

        ierr2 = waq_param%cleanup()
        ierr2 = data_param%cleanup()
        ierr2 = waq_loc%cleanup()
        ierr2 = data_loc%cleanup()
        ierr2 = types%cleanup()

        if (timon) call timstop(ithndl)
        return

        1140 FORMAT(/' ====> input item completed <==== '//)
        1900 FORMAT(/' Absolute times (YYYY/MM/DD;HH:MM:SS) expected in next' &
                , ' time function block.')
        1910 FORMAT(/' Data are supplied as single default values', &
                ' for all segments !')
        2000 FORMAT(/' Time function is linearly interpolated.')
        2005 FORMAT(/' Time function as set of harmonics.')
        2010 FORMAT(/' Time function as Fourier series.')
        2020 FORMAT(/' Input will be given for all ', I10, ' segments.')
        2030 FORMAT(/' ', A, 's ordered in groups of ', A, 's per segment.')
        2040 FORMAT(/' ', A, 's ordered in groups of segments per ', A, '.')
        2090 FORMAT(' ERROR encountered in processing this input item !')
        2100 FORMAT(' Harmonics or Fouriers not allowed with ODS-files !')
        2110 FORMAT(' Harmonics or Fouriers not allowed with binary files !')
        2120 FORMAT(' ERROR during processing of CONSTANT or FUNCTION', &
                ' names !')
        2150 FORMAT(' ERROR during processing of PARAMETERS or', &
                ' SEG_FUNCTIONS names !')
        2170 FORMAT(' ERROR: The DEFAULT keyword was already specified,', &
                ' no SEGMENTS or ALL expected any more !')
        2180 FORMAT(' ERROR during processing of SEGMENT identifiers !')
        2190 FORMAT(' ERROR: DEFAULTS only allowed after PARAMETERS or', &
                ' SEG_FUNCTIONS keyword has been set !')
        2200 FORMAT(' ERROR reading the data block !')
        2210 FORMAT(' ERROR keyword: ', A, ' not recognized !')
        2220 FORMAT(' Input comes from binary file: ', A)
        2260 FORMAT(' ERROR: Segments not defined, use ALL for all segs !')
        2270 FORMAT(' ERROR: MULTIPLEHYD_FILE only allowed for (segment-)functions !')
        2280 FORMAT(' ERROR: MULTIPLEHYD_FILE only allowed for single item entry!')
        2290 FORMAT(' Input grid for this item is :', A)
        2300 FORMAT(' Input will be given for ', I10, ' segments.')
        2310 FORMAT(' ERROR: Input grid not defined :', A)
        2320 FORMAT(' ERROR: Binary/unformatted file does not exist or could not be opened: ', A)
        2330 FORMAT(' WARNING: Binary/unformatted file does not have the correct size: ', A, /, &
                '          The reported size is: ', I0, /, &
                '          The size should not be zero and it should be a whole multiple of ', I0, &
                ' (= 4*(1+', I0, '*', I0, '))')
        2340 FORMAT('          As on some file systems the reported size may be incorrect,', /, &
                '          this is treated as a warning')

    end subroutine read_block

    subroutine check_file_size(filename, nodata, type, filesize, ierr)

        character(len = *), intent(in) :: filename
        integer(kind = int_wp), intent(in) :: nodata
        integer(kind = int_wp), intent(in) :: type
        integer(kind = INT64), intent(out) :: filesize
        integer(kind = int_wp), intent(out) :: ierr

        integer(kind = int_wp) :: norcd, i
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: time
        real(kind = real_wp), dimension(:), allocatable :: data
        character(14) :: strng

        integer(kind = INT64) :: recordsize

        ierr = 0
        ! Check that the file exists and can be opened
        if (type == FILE_BINARY) then
            open(newunit = lun, file = filename, iostat = ierr, status = 'old', access = 'stream')
        else
            open(newunit = lun, file = filename, iostat = ierr, status = 'old', form = 'unformatted')
        endif
        if (ierr /= 0) then
            ierr = -999 ! Explicitly mark the file as unuseable
            return
        endif

        ! Check if this is a steering file
        read (lun, iostat = ierr) strng
        if (ierr /= 0) then
            ierr = 0
            strng = 'x'
        endif
        if (strng(1:14) /= 'Steering file ') then
            if (type == FILE_BINARY) then
                inquire(lun, size = filesize)
                close(lun)

                recordsize = 4 * (nodata + 1) ! single-precision reals occupy four bytes, also 4 bytes for the time
                if (mod(filesize, recordsize) /= 0) then
                    ierr = 1
                endif
            else
                rewind(lun)
                allocate(data(nodata))

                ! Determine the number of records - iostat does not seem to distinguish between partly fulfilled
                ! reads and end-of-file
                norcd = 0
                do
                    read(lun, iostat = ierr) time, data
                    if (ierr /= 0) then
                        exit
                    endif
                    norcd = norcd + 1
                enddo

                ! The last record may have been too short, so try again:
                ! - Read all the records we have been able to read
                ! - Try reading an extra number (time). This should fail
                rewind(lun)
                do i = 1, norcd
                    read(lun, iostat = ierr) time, data
                enddo

                read(lun, iostat = ierr) time

                ! If we have been able to read at least one record and the last read
                ! let to and end-of-file condition, we accept the file. Otherwise return
                ! an error.
                if (norcd > 0 .and. ierr < 0) then
                    ierr = 0
                else
                    ierr = 1
                endif
                close(lun)
                deallocate(data)
            endif
        else
            ! No check yet if it is a steering file
        endif

    end subroutine check_file_size

    ! Function to get around the name clash - information in a COMMON block
    integer function get_original_noseg()
        use m_sysn          ! System characteristics

        get_original_noseg = noseg
    end function get_original_noseg

    subroutine read_header(waq_param, data_param, nocol, itfact, is_date_format, &
            is_yyddhh_format, ierr, status)

        ! Checks if column header exists
        use m_usefor, only : compact_usefor
        use m_waq_data_structure ! for definition and storage of data
        use rd_token
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset
        use m_string_utils

        type(t_waq_item), intent(inout) :: waq_param    ! list of param items to be set in this block ( substances etc )
        type(t_waq_item), intent(inout) :: data_param   ! list of param items in the data
        integer(kind = int_wp), intent(inout) :: nocol         ! number of columns in input
        integer(kind = int_wp), intent(in) :: itfact        ! factor between clocks
        logical, intent(in) :: is_date_format       ! true if time in 'date' format
        logical, intent(in) :: is_yyddhh_format       ! true if yyetc instead of ddetc
        integer(kind = int_wp), intent(out) :: ierr          ! error indication

        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: itype          ! type of token
        character(len = 256) :: ctoken        ! character token
        integer(kind = int_wp) :: itoken         ! integer token
        real(kind = real_wp) :: rtoken         ! real token
        logical :: first         ! first loop indicator / .not. header exists
        integer(kind = int_wp) :: i              ! item index
        integer(kind = int_wp) :: k              ! shifted item index
        integer(kind = int_wp) :: icnt           ! shift in item index
        character(len = 8) :: strng         ! string to be printed
        integer(kind = int_wp) :: nitm          ! number of items in data
        integer(kind = int_wp) :: ierr2         ! local error indication
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_header", ithndl)

        ! read loop
        first = .true.
        do
            if (gettoken(ctoken, itoken, rtoken, itype, ierr) /= 0) then
                goto 9999 ! a read error
            else
                !  a string has arrived, check for date string
                if (itype == 1) then
                    call convert_string_to_time_offset (ctoken, itoken, .false., .false., ierr2)
                    if (ierr2 == 0) then
                        ! date string found, push back, exit input loop
                        push = .true.
                        exit

                    endif
                    if (first) then

                        ! a header exists
                        first = .false.
                        data_param%sequence = 0
                        nocol = 0
                        write (lunut, *)
                    endif
                    nocol = nocol + 1
                    strng = 'not used'
                    do i = 1, data_param%no_item
                        if (string_equals(ctoken(1:20), data_param%name(i))) then
                            strng = 'used'
                            data_param%sequence(i) = nocol
                        endif
                    enddo
                    write (lunut, 1000) nocol, ctoken, strng
                else
                    ! end of the list, push token back, conversion of time removed
                    push = .true.
                    exit
                endif
            endif
        enddo

        if (.not. first) then

            ! is everything resolved ?
            icnt = 0
            nitm = data_param%no_item
            do i = 1, nitm
                k = i - icnt
                if (data_param%name(k) == '&$&$SYSTEM_NAME&$&$!') cycle
                if (data_param%sequence(k) > 0) cycle
                call compact_usefor(lunut, waq_param, data_param, k, icnt)
                call status%increase_warning_count()
                if (i + icnt >= nitm) exit
            enddo

        endif

        9999 if (timon) call timstop(ithndl)
        return

        1000 format (' column:', i3, ' contains: ', a40, ' status: ', a8)

    end subroutine read_header

    subroutine read_items(lunrep, inpfil, output_verbose_level, chkflg, callr, &
            waq_item, data_item, name_item, type_item, noits, &
            ierr, status)

        ! item name retrieval, new style of parse_boundary_condition_data using t_waq_item structures instead of workspace

        use m_waq_data_structure
        use rd_token
        use timers       !   performance timers
        use m_string_utils

        integer(kind = int_wp), intent(in) :: lunrep        ! report file
        type(t_input_file), intent(inout) :: inpfil       ! input file structure with include stack
        integer(kind = int_wp), intent(in) :: output_verbose_level        ! level of reporting to ascii output file
        integer(kind = int_wp), intent(in) :: chkflg        ! check on input or add items
        character(len = 10), intent(in) :: callr        ! calling subject
        type(t_waq_item), intent(out) :: waq_item     ! list of items to be set in this block ( boundaries, loads, substances etc )
        type(t_waq_item), intent(out) :: data_item    ! list of items in the data
        type(t_waq_item), intent(inout) :: name_item    ! delwaq item list
        type(t_waq_item), intent(in) :: type_item    ! delwaq (item-) type list
        integer(kind = int_wp), intent(out) :: noits         ! number of scale factors to be read
        integer(kind = int_wp), intent(inout) :: ierr          ! cummulative error count

        type(error_status), intent(inout) :: status !< current error status

        logical       usefor, setnam, comput, signon
        integer(kind = int_wp) :: ntitm         ! number of bounds/wastes
        integer(kind = int_wp) :: nttype        ! number of bound/waste types
        integer(kind = int_wp) :: noitm         ! number of items read
        integer(kind = int_wp) :: t_asked       ! type of token asked
        integer(kind = int_wp) :: itype         ! type of token read
        integer(kind = int_wp) :: itoken        ! real token
        character(len = 256) :: ctoken       ! character token
        real(kind = real_wp) :: rtoken        ! real token
        integer(kind = int_wp) :: itmnr, ioffc, ioffi, nconst, ierr2, i, i2, ifound, namset

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_items", ithndl)

        usefor = .false.
        setnam = .false.
        comput = .false.
        signon = .false.

        waq_item%no_item = 0
        itmnr = 0
        data_item%no_item = 0
        noitm = 0
        noits = 0
        ntitm = name_item%no_item
        nttype = type_item%no_item

        ioffc = 0
        ioffi = 0
        nconst = 0

        !     get a token string (and return if something wrong was found)

        10 continue
        if (signon .or. (usefor .and. setnam)) then
            ierr = gettoken(ctoken, itoken, rtoken, itype, ierr2)
        else
            ierr = gettoken(ctoken, itoken, itype, ierr2)
        endif
        if (ierr /= 0) then
            push = .true.
            goto 9999
        endif

        ! a keyword was met
        if (abs(itype) == 1 .and. &
                (ctoken(1:5) == 'BLOCK'        .or. &
                        ctoken(1:6) == 'LINEAR'       .or. &
                        ctoken(1:4) == 'ITEM'         .or. &
                        ctoken(1:13) == 'IDENTICALITEM'.or. &
                        ctoken(1:12) == 'USEDATA_ITEM' .or. &
                        ctoken(1:7) == 'FORITEM'      .or. &
                        ctoken(1:9) == 'DATA_ITEM'    .or. &
                        ctoken(1:6) == 'CONCEN'       .or. &
                        ctoken(1:6) == 'DATA'         .or. &
                        ctoken(1:10) == 'TIME_DELAY'   .or. &
                        ctoken(1:8) == 'ODS_FILE'     .or. &
                        ctoken(1:11) == 'BINARY_FILE'  .or. &
                        ctoken(1:8) == 'ABSOLUTE'     .or. &
                        ctoken(1:4) == 'TIME'         .or. &
                        ctoken(1:9) == 'HARMONICS'    .or. &
                        ctoken(1:8) == 'FOURIERS'     .or. &
                        ctoken(1:5) == 'SCALE'        .or. &
                        ctoken(1:8) == 'DEFAULTS'     .or. &
                        ctoken(1:3) == 'ALL'          .or. &
                        ctoken(1:8) == 'SEGMENTS'     .or. &
                        ctoken(1:9) == 'CONSTANTS'    .or. &
                        ctoken(1:10) == 'PARAMETERS'   .or. &
                        ctoken(1:9) == 'FUNCTIONS'    .or. &
                        ctoken(1:9) == 'INPUTGRID'    .or. &
                        ctoken(1:13) == 'SEG_FUNCTIONS')) then
            if (usefor) then
                write (lunut, 1035) ctoken
                goto 40
            else
                push = .true.
                goto 9999
            endif
        endif

        ! computations
        if (abs(itype) == 1 .and. &
                (ctoken ==  '*'  .or. ctoken ==  '/'  .or. &
                        ctoken ==  '+'  .or. ctoken ==  '-'  .or. &
                        ctoken == 'MIN' .or. ctoken == 'MAX')) then
            if (.not. comput) then
                write (lunut, 1070)
                goto 40
            endif
            if (signon) then
                write (lunut, 1080)
                goto 40
            endif
            noitm = noitm + 1
            noits = noits + 1
            ierr2 = data_item%resize(noitm)
            data_item%no_item = noitm
            data_item%sequence(noitm) = 0
            if (ctoken ==  '*') data_item%ipnt(noitm) = -1000000
            if (ctoken ==  '/') data_item%ipnt(noitm) = -10000000
            if (ctoken ==  '+') data_item%ipnt(noitm) = -100000000
            if (ctoken ==  '-') data_item%ipnt(noitm) = -1000000000
            if (ctoken == 'MIN') data_item%ipnt(noitm) = -1100000000
            if (ctoken == 'MAX') data_item%ipnt(noitm) = -1200000000
            signon = .true.
            goto 10
        endif

        ! an item used in computations
        if (abs(itype) == 1 .and. signon) then

            do i = 1, itmnr - 1
                if (waq_item%ipnt(i) == -1300000000) cycle
                if (string_equals(ctoken(1:20), waq_item%name(i))) then
                    noits = noits - 1
                    i2 = data_item%ipnt(noitm)
                    if (i2 == -1000000)    write(lunut, 1120)i, ctoken
                    if (i2 == -10000000)   write(lunut, 1110)i, ctoken
                    if (i2 == -100000000)  write(lunut, 1100)i, ctoken
                    if (i2 == -1000000000) write(lunut, 1090)i, ctoken
                    if (i2 == -1100000000) write(lunut, 1092)i, ctoken
                    if (i2 == -1200000000) write(lunut, 1094)i, ctoken
                    data_item%ipnt(noitm) = i2 + i
                    data_item%name(noitm) = '&$&$SYSTEM_NAME&$&$!'
                    signon = .false.
                    goto 10
                endif
            enddo

            i2 = data_item%ipnt(noitm)
            if (i2 == -1000000)    write(lunut, 1130)ctoken
            if (i2 == -10000000)   write(lunut, 1140)ctoken
            if (i2 == -100000000)  write(lunut, 1150)ctoken
            if (i2 == -1000000000) write(lunut, 1160)ctoken
            if (i2 == -1100000000) write(lunut, 1162)ctoken
            if (i2 == -1200000000) write(lunut, 1164)ctoken
            data_item%sequence(noitm) = noits
            data_item%name(noitm) = ctoken
            signon = .false.
            goto 10
        endif

        ! a number is used in computations
        if (abs(itype) == 2 .or. abs(itype) == 3) then
            if (setnam .or. signon) then
                nconst = nconst + 1
                noits = noits - 1
                i2 = data_item%ipnt(noitm)
                data_item%name(noitm) = '&$&$SYSTEM_NAME&$&$!'
                data_item%constant(noitm) = rtoken
                if (signon) then
                    if (i2 == -1000000)    write(lunut, 1170)rtoken
                    if (i2 == -10000000)   write(lunut, 1180)rtoken
                    if (i2 == -100000000)  write(lunut, 1190)rtoken
                    if (i2 == -1000000000) write(lunut, 1200)rtoken
                    if (i2 == -1100000000) write(lunut, 1210)rtoken
                    if (i2 == -1200000000) write(lunut, 1220)rtoken
                    data_item%ipnt(noitm) = i2 - nconst
                    signon = .false.
                endif
                if (setnam) then
                    namset = waq_item%ipnt(itmnr)
                    if (namset > 0 .and. output_verbose_level >= 3) then
                        write (lunut, 1001) callr, itmnr, callr, namset, &
                                name_item%name(namset), rtoken
                    elseif (namset == 0 .and. output_verbose_level >= 3) then
                        write (lunut, 1001) callr, itmnr, callr, namset, &
                                'flow', rtoken
                    elseif (namset == -1300000000 .and. output_verbose_level >= 3) then
                        write (lunut, 1001) callr, itmnr, callr, namset, &
                                'Ignored', rtoken
                    elseif (output_verbose_level >= 3) then
                        write (lunut, 1011) callr, itmnr, callr, -namset, &
                                type_item%name(-namset), rtoken
                    endif
                    data_item%ipnt(noitm) = -nconst
                    data_item%sequence(noitm) = 0
                    usefor = .false.
                    setnam = .false.
                    comput = .true.
                endif
                goto 10
            endif
        endif

        ! A local redirection of the name of an item or substance
        if (abs(itype) == 1 .and. ctoken == 'USEFOR') then
            if (usefor) then
                write (lunut, 1035) ctoken
                goto 40
            else
                usefor = .true.
                setnam = .false.
                goto 10
            endif
        endif
        ! Getting the items of this block
        !       waq_item(itmnr)   is the reference to the delwaq item (NAMSET)
        !       data_item(noitm)    is the order number in the series (data)
        !       namset              is the ID number of NOITMth name
        !       ANAME/ATYPE(NAMSET) is the corresponding reserved name or type (delwaq item)
        !       ctoken              is the name that should be used.
        if (itype == 1) then
            if (usefor .and. setnam) then
                namset = waq_item%ipnt(itmnr)
                if (namset > 0 .and. output_verbose_level >= 3) then
                    write (lunut, 1000) callr, itmnr, callr, namset, &
                            name_item%name(namset), ctoken
                elseif (namset == 0 .and. output_verbose_level >= 3) then
                    write (lunut, 1000) callr, itmnr, callr, namset, &
                            'FLOW', ctoken
                elseif (namset == -1300000000 .and. output_verbose_level >= 3) then
                    write (lunut, 1000) callr, itmnr, callr, namset, &
                            'Ignored', ctoken
                elseif (output_verbose_level >= 3) then
                    write (lunut, 1010) callr, itmnr, callr, -namset, &
                            type_item%name(-namset), ctoken
                endif
                data_item%sequence(noitm) = noits
                data_item%name(noitm) = ctoken
                usefor = .false.
                setnam = .false.
                !                     it is now possible to compute
                comput = .true.
                goto 10
            endif

            ! fill in a string value if an empty string is provided
            if (chkflg      == -1 .and. &
                    ctoken(1:20) == '                    ') then
                ctoken = 'Item-'
                write (ctoken(6:12), '(i7)') noitm + 1
            endif

            ! FLOW is only valid as CONCENTR. and item number is 0
            if (string_equals(ctoken(1:20), 'FLOW                ') &
                    .and. callr == 'CONCENTR. ') then
                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = 0
                data_item%ipnt(noitm) = itmnr
                data_item%sequence(noitm) = noits
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken
                if (usefor) setnam = .true.
                if (output_verbose_level >= 3 .and. .not. usefor) &
                        write (lunut, 1020) callr, itmnr, callr, 0, 'FLOW'
                goto 10
            endif

            ! ctoken equals an item-NAME
            i2 = name_item%find(ctoken)
            if (i2 >= 1) then
                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = i2
                data_item%ipnt(noitm) = itmnr
                data_item%sequence(noitm) = noits
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken
                if (usefor) setnam = .true.
                if (output_verbose_level >= 3 .and. .not. usefor) &
                        write (lunut, 1020) callr, itmnr, callr, i2, name_item%name(i2)
                goto 10
            endif

            ! ctoken equals an item-TYPE. the index reference is set negative
            i2 = type_item%find(ctoken)
            if (i2 >= 1) then
                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = -i2
                data_item%ipnt(noitm) = itmnr
                data_item%sequence(noitm) = noits
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken

                if (usefor) setnam = .true.
                if (output_verbose_level >= 3 .and. .not. usefor) &
                        write (lunut, 1030) callr, itmnr, callr, i2, type_item%name(i2)
                goto 10
            endif

            ! name does not exist
            if (chkflg == 1) then

                ! ignore the data
                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = -1300000000
                data_item%ipnt(noitm) = 1300000000
                data_item%sequence(noitm) = noits
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken

                if (usefor) setnam = .true.
                write (lunut, 1040) callr, itmnr, trim(ctoken)
                call status%increase_warning_count()
                goto 10
            else

                ! now a new name is added to the list of names
                ntitm = ntitm + 1
                ierr2 = name_item%resize(ntitm)
                name_item%no_item = ntitm
                name_item%name(ntitm) = ctoken

                ! plus normal procedure
                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = ntitm
                data_item%ipnt(noitm) = itmnr
                data_item%sequence(noitm) = noits
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken

                if (usefor) setnam = .true.
                if (output_verbose_level >= 3 .and. .not. usefor) &
                        write (lunut, 1020) callr, itmnr, callr, &
                                ntitm, name_item%name(ntitm)
                goto 10
            endif
        endif

        ! no item name was given, but an item number
        if (itype == 2) then
            if (itoken <=  ntitm .and. itoken >= -nttype) then

                itmnr = itmnr + 1
                ierr2 = waq_item%resize(itmnr)
                waq_item%no_item = itmnr

                noitm = noitm + 1
                ierr2 = data_item%resize(noitm)
                data_item%no_item = noitm

                noits = noits + 1

                waq_item%ipnt(itmnr) = itoken
                data_item%ipnt(noitm) = itmnr
                data_item%sequence(noitm) = noits
                if (callr == 'segment') then
                    if (itoken <= 0) then
                        write (lunut, 1060) itoken
                        goto 40
                    endif
                    if (output_verbose_level >= 3 .and. .not. usefor) &
                            write (lunut, 1015) callr, itmnr, callr, itoken
                    write (ctoken, '(''Segment '',i8)') itoken
                elseif (itoken == 0 .and. callr /= 'CONCENTR. ') then
                    write (lunut, 1060) itoken
                    goto 40
                elseif (itoken > 0) then
                    if (output_verbose_level >= 3 .and. .not. usefor) &
                            write (lunut, 1020) callr, itmnr, callr, itoken, &
                                    name_item%name(itoken)
                    ctoken = name_item%name(itoken)
                elseif (itoken == 0 .and. callr == 'CONCENTR. ') then
                    if (output_verbose_level >= 3 .and. .not. usefor) &
                            write (lunut, 1020) callr, itmnr, callr, itoken, &
                                    'FLOW'
                    ctoken = 'FLOW'
                else
                    if (output_verbose_level >= 3 .and. .not. usefor) &
                            write (lunut, 1030) callr, itmnr, callr, -itoken, &
                                    type_item%name(-itoken)
                    ctoken = type_item%name(-itoken)
                endif
                waq_item%name(itmnr) = ctoken
                data_item%name(noitm) = ctoken
                if (usefor) setnam = .true.
                goto 10
            else
                write (lunut, 1060) itoken
                goto 40
            endif
        endif

        40 ierr = 1
        9999 if (timon) call timstop(ithndl)
        return

        1000 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with ID  : ', &
                A20, ' and local substitution: ', A20)
        1001 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with ID  : ', &
                A20, ' and local substitution: ', E15.6)
        1010 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' type:', I5, &
                ' with type: ', A20, ' and local substitution: ', A20)
        1011 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' type:', I5, &
                ' with type: ', A20, ' and local substitution: ', E15.6)
        1015 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5)
        1020 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with ID  : ', &
                A20)
        1030 FORMAT (' Input ', A, ' nr:', I5, ' is ', A, ' nr:', I5, ' with type: ', &
                A20)
        1035 FORMAT (' ERROR: no reserved keyword expected: ', A20)
        1040 FORMAT (' WARNING: Input ', A, ' nr:', I5, ' with name: ', A20, &
                ' is not a valid ID, data ignored')
        1060 FORMAT (' ERROR: number: ', I5, ' is not a valid item number !')
        1070 FORMAT (' ERROR: multiplication is only allowed in USEFOR', &
                ' context !')
        1080 FORMAT (' ERROR: arithmetics should be separated by items !')
        1090 FORMAT (' Subtracted by item nr: ', I6, ' Name: ', A20)
        1092 FORMAT (' Minimum value is item nr: ', I6, ' Name: ', A20)
        1094 FORMAT (' Maximum value is item nr: ', I6, ' Name: ', A20)
        1100 FORMAT (' Summed with item nr: ', I6, ' Name: ', A20)
        1110 FORMAT (' Divided by item nr: ', I6, ' Name: ', A20)
        1120 FORMAT (' Multiplied by item nr: ', I6, ' Name: ', A20)
        1130 FORMAT (' Multiplied by local substitution: ', A20)
        1140 FORMAT (' Divided by local substitution: ', A20)
        1150 FORMAT (' Summed with local substitution: ', A20)
        1160 FORMAT (' Subtracted by local substitution: ', A20)
        1162 FORMAT (' Minimum value is local substitution: ', A20)
        1164 FORMAT (' Maximum value is local substitution: ', A20)
        1170 FORMAT (' Multiplied by: ', E15.6)
        1180 FORMAT (' Divided by: ', E15.6)
        1190 FORMAT (' Summed with: ', E15.6)
        1200 FORMAT (' Subtracted by: ', E15.6)
        1210 FORMAT (' Minimum value is: ', E15.6)
        1220 FORMAT (' Maximum value is: ', E15.6)

    END SUBROUTINE read_items

end module m_read_block
