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
module inputs_block_7
    use m_waq_precision
    use m_read_block
    use m_error_status

    implicit none

    private
    public :: read_block_7_process_parameters

contains

    subroutine read_block_7_process_parameters (file_unit_list, file_name_list, filtype, inpfil, syname, &
            iwidth, output_verbose_level, gridps, constants, chkpar, &
            status)

        !! Reads block 7 of input, process parameters

        use error_handling, only : check_error
        use m_logger_helper, only : stop_with_error
        use m_string_utils, only : index_in_array
        use m_open_waq_files
        use m_grid_utils_external   ! for the storage of contraction grids
        use m_waq_data_structure  ! for definition and storage of data
        use rd_token       ! tokenized reading
        use partmem, only : alone, use_settling, layt        ! for the interface with Delpar (Tau and VertDisp)
        use timers       !   performance timers
        use m_waq_memory_dimensions
        use omp_lib

        integer(kind = int_wp), intent(inout) :: file_unit_list(*)        !< unit numbers used
        character(len = *), intent(inout) :: file_name_list(*)     !< filenames
        integer(kind = int_wp), intent(inout) :: filtype(*)    !< type of binary file
        type(t_input_file), intent(inout) :: inpfil       !< input file structure with include stack and flags
        character(len = *), intent(in) :: syname(*)    !< substance names
        integer(kind = int_wp), intent(in) :: iwidth        !< width of output
        integer(kind = int_wp), intent(in) :: output_verbose_level        !< level of reporting to ascii output file
        type(GridPointerColl), intent(in) :: GridPs       !< collection off all grid definitions
        type(t_waq_item), intent(inout) :: constants    !< delwaq constants list
        logical, intent(in) :: chkpar(2)    !< check for SURF and LENGTH

        type(error_status) :: status !< current error status


        !     local declarations

        type(t_data_column) :: proc_pars            ! all the process parameters data from file
        type(t_data_block) :: dlwqdata             ! one data block
        type(t_waq_item) :: substances           ! delwaq substances list
        type(t_waq_item) :: parameters           ! delwaq parameters list
        type(t_waq_item) :: functions            ! delwaq functions list
        type(t_waq_item) :: segfuncs             ! delwaq segment-functions list
        type(t_waq_item) :: segments             ! delwaq segments
        character(len = 255) :: ctoken               ! token from input
        character(len = 20) :: ch20                 ! name
        integer(kind = int_wp) :: itime                 ! time in scu (dummy used for constants)
        integer(kind = int_wp) :: nosss                 ! total number of segments (water and bottom)
        integer(kind = int_wp) :: ierr2                 ! error indicator
        integer(kind = int_wp) :: ierr3                 ! error indicator
        integer(kind = int_wp) :: ioerr                 ! IO - error indicator
        integer(kind = int_wp) :: inum_fast_solver_vectors                ! location of num_fast_solver_vectors
        integer(kind = int_wp) :: inothr                ! location of NOTHREADS
        integer(kind = int_wp) :: i                     ! loop counter
        integer(kind = int_wp) :: idata                 ! help variable
        logical :: taupart              ! is tau present?
        logical :: vdfpart              ! is vertical diffusion present
        integer(kind = int_wp) :: special               ! index of special parameters
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_block_7_process_parameters", ithndl)

        !        Read initial conditions

        proc_pars%maxsize = 0
        proc_pars%current_size = 0
        ierr2 = substances%initialize()
        ierr2 = substances%resize(num_substances_total)
        substances%no_item = num_substances_total
        substances%name(1:num_substances_total) = syname(1:num_substances_total)
        ierr2 = constants%initialize()
        ierr2 = parameters%initialize()
        ierr2 = functions%initialize()
        ierr2 = segfuncs%initialize()

        nosss = num_cells + num_cells_bottom
        ierr2 = segments%initialize()
        ierr2 = segments%resize(nosss)
        segments%no_item = nosss
        do i = 1, nosss
            write (segments%name(i), '(''segment '',i8)') i
        enddo

        IERR2 = 0
        num_threads = 1
        taupart = .false.
        vdfpart = .false.

        do

            if (gettoken(ctoken, ierr2) /= 0) exit

            if (ctoken == 'CONSTANTS'     .or. &
                    ctoken == 'FUNCTIONS'     .or. &
                    ctoken == 'PARAMETERS'    .or. &
                    ctoken == 'SEG_FUNCTIONS') then

                ! new file structure

                push = .true.
                call read_block (file_unit_list, file_name_list, filtype, inpfil, output_verbose_level, &
                        iwidth, substances, constants, parameters, functions, &
                        segfuncs, segments, gridps, dlwqdata, ierr2, &
                        status)

                if (ierr2 > 0) goto 30

                ! check for special constants, get directly from structure (ignore order, scaling etc this is not clean)

                if (dlwqdata%subject == SUBJECT_CONSTANT) then
                    ch20 = 'NOVEC'
                    inum_fast_solver_vectors = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%num_spatial_parameters))
                    if (inum_fast_solver_vectors > 0) then
                        num_fast_solver_vectors = nint(dlwqdata%values(inum_fast_solver_vectors, 1, 1))
                        write(file_unit, 2240)
                        write(file_unit, 2250) num_fast_solver_vectors
                    endif
                    ch20 = 'NOTHREADS'
                    inothr = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%num_spatial_parameters))
                    if (inothr > 0) then
                        num_threads = nint(dlwqdata%values(inothr, 1, 1))
                        write(file_unit, 2310)
                        write(file_unit, 2320) num_threads
                        if (num_threads > 0) call omp_set_num_threads(num_threads)
                        num_threads = omp_get_max_threads()
                    endif
                endif
                ch20 = 'TAU'
                inum_fast_solver_vectors = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%num_spatial_parameters))
                if (inum_fast_solver_vectors > 0) taupart = .true.
                ch20 = 'VERTDISPER'
                inum_fast_solver_vectors = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%num_spatial_parameters))
                if (inum_fast_solver_vectors > 0) vdfpart = .true.

                ! add to the collection
                idata = proc_pars%add(dlwqdata)

            else

                ! unrecognised keyword

                if (ctoken(1:1) /= '#') then
                    write (file_unit, 2040) trim(ctoken)
                    call status%increase_error_count()
                    goto 30
                else
                    ierr2 = 2
                    exit
                endif

            endif

        enddo
        if (.not. alone) then              ! Delwaq runs with Delpar
            if (use_settling .or. layt > 1) then
                if (taupart) then
                    write (file_unit, 2330)
                else
                    write (file_unit, 2340)
                    call status%increase_warning_count()
                endif
                if (layt > 1) then
                    if (vdfpart) then
                        write (file_unit, 2350)
                    else
                        write (file_unit, 2360)
                        call status%increase_warning_count()
                    endif
                endif
            endif
        endif

        ! write to output and report files
        num_constants = constants%no_item
        num_spatial_parameters = parameters%no_item
        num_time_functions = functions%no_item
        num_spatial_time_fuctions = segfuncs%no_item

        write (file_unit, 2050) constants%no_item
        write (file_unit, 2060) parameters%no_item
        write (file_unit, 2070) functions%no_item
        write (file_unit, 2080) segfuncs%no_item
        if (constants%no_item  > 0) write (file_unit_list(2)) (constants%name(i), i = 1, constants%no_item)
        if (parameters%no_item > 0) write (file_unit_list(2)) (parameters%name(i), i = 1, parameters%no_item)
        if (functions%no_item  > 0) write (file_unit_list(2)) (functions%name(i), i = 1, functions%no_item)
        if (segfuncs%no_item   > 0) write (file_unit_list(2)) (segfuncs%name(i), i = 1, segfuncs%no_item)

        call open_waq_files(file_unit_list(16), file_name_list(16), 16, 1, ioerr)
        write(file_unit_list(16)) ' 5.000PROCES'
        write(file_unit_list(16)) proc_pars%current_size
        do i = 1, proc_pars%current_size
            ioerr = proc_pars%data_block(i)%write(file_unit_list(16))
        enddo
        close (file_unit_list(16))

        ! evaluate constants for report in dlwqp1
        itime = 0
        do i = 1, proc_pars%current_size
            if (proc_pars%data_block(i)%subject == SUBJECT_CONSTANT) then
                ierr3 = proc_pars%data_block(i)%evaluate(gridps, itime, constants%no_item, 1, constants%constant)
            endif
        enddo

        ! if necessary, check for the parameters SURF and LENGTH
        if (chkpar(1)) then
            ch20 = 'SURF'
            special = index_in_array(ch20, parameters%name(:parameters%no_item))
            if (special <= 0) then
                special = index_in_array(ch20, segfuncs%name(:segfuncs%no_item))
                if (special <= 0) then
                    call status%increase_error_count()
                    write(file_unit, 2410)
                endif
            endif
        endif

        if (chkpar(2)) then
            ch20 = 'LENGTH'
            special = index_in_array(ch20, parameters%name(:parameters%no_item))
            if (special <= 0) then
                special = index_in_array(ch20, segfuncs%name(:segfuncs%no_item))
                if (special <= 0) then
                    call status%increase_error_count()
                    write(file_unit, 2420)
                endif
            endif
        endif

        ! proc_pars cleanup
        ierr3 = substances%cleanup()
        ierr3 = parameters%cleanup()
        ierr3 = functions%cleanup()
        ierr3 = segfuncs%cleanup()
        ierr3 = segments%cleanup()

        30 continue
        if (ierr2 > 0 .and. ierr2 /= 2) call status%increase_error_count()
        if (ierr2 == 3) call stop_with_error()
        call check_error(ctoken, iwidth, 7, ierr2, status)
        if (timon) call timstop(ithndl)

        return
        2040 FORMAT (/' ERROR, unrecognized token: ', A)
        2050 FORMAT(/' Total number of constants        : ', I4)
        2060 FORMAT(/' Total number of parameters       : ', I4)
        2070 FORMAT(/' Total number of functions        : ', I4)
        2080 FORMAT(/' Total number of segment functions: ', I4)
        2240 FORMAT (/, ' NOVEC Keyword found')
        2250 FORMAT (' Number of fast solver vectors set to :', I6)
        2310 FORMAT (/, ' NOTHREADS Keyword found')
        2320 FORMAT (' Number of threads for parallel processing set to :', I6)
        2330 FORMAT (' Tau from DELWAQ will be used for DELPAR')
        2340 FORMAT (' WARNING: TAU not found. DELPAR will try to get its own TAU or compute it!')
        2350 FORMAT (' VertDisp from DELWAQ will be used for DELPAR')
        2360 FORMAT (' WARNING: VertDisp not found. DELPAR will try to get its own VertDisp or compute it!')
        2410 FORMAT (/, ' ERROR: No parameter or segment function "SURF" found - needed for special waste loads!')
        2420 FORMAT (/, ' ERROR: No parameter or segment function "LENGTH" found - needed for special waste loads!')

    END SUBROUTINE read_block_7_process_parameters

end module inputs_block_7
