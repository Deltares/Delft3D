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

    subroutine read_block_7_process_parameters (lun, lchar, filtype, inpfil, syname, &
            iwidth, output_verbose_level, gridps, constants, chkpar, &
            status)

        !! Reads block 7 of input, process parameters

        use error_handling, only : check_error
        use m_srstop
        use m_open_waq_files
        use dlwqgrid_mod   ! for the storage of contraction grids
        use dlwq_hyd_data  ! for definition and storage of data
        use rd_token       ! tokenized reading
        use partmem, only : alone, lsettl, layt        ! for the interface with Delpar (Tau and VertDisp)
        use timers       !   performance timers
        use m_sysn

        include 'omp_lib.h'

        integer(kind = int_wp), intent(inout) :: lun(*)        !< unit numbers used
        character(len = *), intent(inout) :: lchar(*)     !< filenames
        integer(kind = int_wp), intent(inout) :: filtype(*)    !< type of binary file
        type(inputfilestack), intent(inout) :: inpfil       !< input file structure with include stack and flags
        character(len = *), intent(in) :: syname(*)    !< substance names
        integer(kind = int_wp), intent(in) :: iwidth        !< width of output
        integer(kind = int_wp), intent(in) :: output_verbose_level        !< level of reporting to ascii output file
        type(GridPointerColl), intent(in) :: GridPs       !< collection off all grid definitions
        type(t_dlwq_item), intent(inout) :: constants    !< delwaq constants list
        logical, intent(in) :: chkpar(2)    !< check for SURF and LENGTH

        type(error_status) :: status !< current error status


        !     local declarations

        type(t_dlwqdatacoll) :: proc_pars            ! all the process parameters data from file
        type(t_dlwqdata) :: dlwqdata             ! one data block
        type(t_dlwq_item) :: substances           ! delwaq substances list
        type(t_dlwq_item) :: parameters           ! delwaq parameters list
        type(t_dlwq_item) :: functions            ! delwaq functions list
        type(t_dlwq_item) :: segfuncs             ! delwaq segment-functions list
        type(t_dlwq_item) :: segments             ! delwaq segments
        character(len = 255) :: ctoken               ! token from input
        character(len = 20) :: ch20                 ! name
        integer(kind = int_wp) :: itime                 ! time in scu (dummy used for constants)
        integer(kind = int_wp) :: nosss                 ! total number of segments (water and bottom)
        integer(kind = int_wp) :: ierr2                 ! error indicator
        integer(kind = int_wp) :: ierr3                 ! error indicator
        integer(kind = int_wp) :: ioerr                 ! IO - error indicator
        integer(kind = int_wp) :: inovec                ! location of NOVEC
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
        proc_pars%cursize = 0
        ierr2 = dlwq_init(substances)
        ierr2 = dlwq_resize(substances, notot)
        substances%no_item = notot
        substances%name(1:notot) = syname(1:notot)
        ierr2 = dlwq_init(constants)
        ierr2 = dlwq_init(parameters)
        ierr2 = dlwq_init(functions)
        ierr2 = dlwq_init(segfuncs)

        nosss = noseg + nseg2
        ierr2 = dlwq_init(segments)
        ierr2 = dlwq_resize(segments, nosss)
        segments%no_item = nosss
        do i = 1, nosss
            write (segments%name(i), '(''segment '',i8)') i
        enddo

        IERR2 = 0
        nothrd = 1
        taupart = .false.
        vdfpart = .false.

        do

            if (gettoken(ctoken, ierr2) /= 0) exit

            if (ctoken == 'CONSTANTS'     .or. &
                    ctoken == 'FUNCTIONS'     .or. &
                    ctoken == 'PARAMETERS'    .or. &
                    ctoken == 'SEG_FUNCTIONS') then

                ! new file strucure

                push = .true.
                call read_block (lun, lchar, filtype, inpfil, output_verbose_level, &
                        iwidth, substances, constants, parameters, functions, &
                        segfuncs, segments, gridps, dlwqdata, ierr2, &
                        status)

                if (ierr2 > 0) goto 30

                ! check for special constants, get directly from structure (ignore order, scaling etc this is not clean)

                if (dlwqdata%subject == SUBJECT_CONSTANT) then
                    ch20 = 'NOVEC'
                    inovec = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%no_param))
                    if (inovec > 0) then
                        novec = nint(dlwqdata%values(inovec, 1, 1))
                        write(lunut, 2240)
                        write(lunut, 2250) novec
                    endif
                    ch20 = 'NOTHREADS'
                    inothr = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%no_param))
                    if (inothr > 0) then
                        nothrd = nint(dlwqdata%values(inothr, 1, 1))
                        write(lunut, 2310)
                        write(lunut, 2320) nothrd
                        if (nothrd > 0) call omp_set_num_threads(nothrd)
                        nothrd = omp_get_max_threads()
                    endif
                endif
                ch20 = 'TAU'
                inovec = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%no_param))
                if (inovec > 0) taupart = .true.
                ch20 = 'VERTDISPER'
                inovec = index_in_array(ch20, dlwqdata%param_name(:dlwqdata%no_param))
                if (inovec > 0) vdfpart = .true.

                ! add to the collection

                idata = dlwqdataCollAdd(proc_pars, dlwqdata)

            else

                ! unrecognised keyword

                if (ctoken(1:1) /= '#') then
                    write (lunut, 2040) trim(ctoken)
                    call status%increase_error_count()
                    goto 30
                else
                    ierr2 = 2
                    exit
                endif

            endif

        enddo
        if (.not. alone) then              ! Delwaq runs with Delpar
            if (lsettl .or. layt > 1) then
                if (taupart) then
                    write (lunut, 2330)
                else
                    write (lunut, 2340)
                    call status%increase_warning_count()
                endif
                if (layt > 1) then
                    if (vdfpart) then
                        write (lunut, 2350)
                    else
                        write (lunut, 2360)
                        call status%increase_warning_count()
                    endif
                endif
            endif
        endif

        ! write to output and report files

        nocons = constants%no_item
        nopa = parameters%no_item
        nofun = functions%no_item
        nosfun = segfuncs%no_item

        write (lunut, 2050) constants%no_item
        write (lunut, 2060) parameters%no_item
        write (lunut, 2070) functions%no_item
        write (lunut, 2080) segfuncs%no_item
        if (constants%no_item  > 0) write (lun(2)) (constants%name(i), i = 1, constants%no_item)
        if (parameters%no_item > 0) write (lun(2)) (parameters%name(i), i = 1, parameters%no_item)
        if (functions%no_item  > 0) write (lun(2)) (functions%name(i), i = 1, functions%no_item)
        if (segfuncs%no_item   > 0) write (lun(2)) (segfuncs%name(i), i = 1, segfuncs%no_item)

        call open_waq_files(lun(16), lchar(16), 16, 1, ioerr)
        write(lun(16)) ' 5.000PROCES'
        write(lun(16)) proc_pars%cursize
        do i = 1, proc_pars%cursize
            ioerr = dlwqdataWrite(lun(16), proc_pars%dlwqdata(i))
        enddo
        close (lun(16))

        !     evaluate constants for report in dlwqp1

        itime = 0
        do i = 1, proc_pars%cursize
            if (proc_pars%dlwqdata(i)%subject == SUBJECT_CONSTANT) then
                ierr3 = dlwqdataevaluate(proc_pars%dlwqdata(i), gridps, itime, constants%no_item, 1, constants%constant)
            endif
        enddo

        !     if necessary, check for the parameters SURF and LENGTH

        if (chkpar(1)) then
            ch20 = 'SURF'
            special = index_in_array(ch20, parameters%name(:parameters%no_item))
            if (special <= 0) then
                special = index_in_array(ch20, segfuncs%name(:segfuncs%no_item))
                if (special <= 0) then
                    call status%increase_error_count()
                    write(lunut, 2410)
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
                    write(lunut, 2420)
                endif
            endif
        endif

        !     proc_pars cleanup

        ierr3 = dlwq_cleanup(substances)
        ierr3 = dlwq_cleanup(parameters)
        ierr3 = dlwq_cleanup(functions)
        ierr3 = dlwq_cleanup(segfuncs)
        ierr3 = dlwq_cleanup(segments)

        30 continue
        if (ierr2 > 0 .and. ierr2 /= 2) call status%increase_error_count()
        if (ierr2 == 3) call srstop(1)
        call check_error(ctoken, iwidth, 7, ierr2, status)
        if (timon) call timstop(ithndl)
        return
        !
        !       Output formats
        !
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
        !
    END

end module inputs_block_7
