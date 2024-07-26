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

module initialize_conditions
    use m_waq_precision
    use m_string_utils
    use m_zlayer
    use m_segcol
    use m_expands_vol_area_for_bottom_cells, only : expands_vol_area_for_bottom_cells
    use time_dependent_variables, only : initialize_time_dependent_variables
    use workspace
    use m_delpar00
    use m_chknmr
    use m_array_manipulation, only : initialize_real_array, copy_real_array_elements, create_pointer_table
    use m_open_waq_files

    private
    public :: initialize_all_conditions

contains
    subroutine initialize_all_conditions(buffer, num_files, max_real_arr_size, max_int_arr_size, max_char_arr_size, &
            page_length, file_unit_list, file_name_list, filtype, gridps, dlwqd, ierr)

        !> Initializes all start conditions for simulation
        !>
        !> Performs:
        !>      - calls SPACE to allocate space for all arrays
        !>      - calls initialize_fixed_conditions to initialize all fixed conditions
        !>      - calls initialize_processes to initialize all processes subsystem
        !>      - calls initialize_variables for unclear reasons
        !>      - calls initialize_output to initialize the output system
        !>      - imports all grid informations and exchange tables
        !>      - calls initialize_time_dependent_variables to initialize all time dependent variables
        !>      - calls expands_vol_area_for_bottom_cells to initialize the water bed layers
        !>      - imports initial conditions

        !     LOGICAL UNITNUMBERS : file_unit_list( 2) - system intermediate file
        !                           file_unit_list(19) - monitoring output file
        !
        !     SUBROUTINES CALLED  : SPACE , initialises common blocks
        !                           initialize_fixed_conditions, initialises fixed conditions
        !                           initialize_processes, initialises proces system
        !                           initialize_output, initialises output system
        !                           initialize_time_dependent_variables, sets time functions
        !                           open_waq_files, opens files
        !                           MOVE  , copy's arrays
        !                           ZERO  , zeros an real arrays
        !
        use m_dhisys
        use m_grid_utils_external
        use variable_declaration
        use delwaq2_data
        use timers
        use workspace, only : set_array_indexes
        use string_module  ! string manipulation tools
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics
        use m_real_array_indices          ! Pointers in real array workspace
        use m_integer_array_indices          ! Pointers in integer array workspace
        use m_character_array_indices          ! Pointers in character array workspace

        !     Parameters          :

        !     kind           function         name            description
        type(waq_data_buffer), intent(inout) :: buffer        !< System total array space
        integer(kind = int_wp), intent(in) :: num_files          !< Number of files
        integer(kind = int_wp), intent(inout) :: max_real_arr_size         !< dimension   A-array
        integer(kind = int_wp), intent(inout) :: max_int_arr_size         !< dimension   J-array
        integer(kind = int_wp), intent(inout) :: max_char_arr_size         !< dimension   C-array
        integer(kind = int_wp), intent(in) :: page_length         !< pagelength of the output file
        integer(kind = int_wp), intent(inout) :: file_unit_list(num_files) !< array with unit numbers
        character(*), intent(in) :: file_name_list(num_files) !< filenames
        integer(kind = int_wp), intent(in) :: filtype(num_files) !< type of file
        type(gridpointercoll), intent(out) :: gridps        !< collection off all grid definitions
        type(delwaq_data), intent(inout) :: dlwqd         !< derived type for persistent storage
        integer(kind = int_wp), intent(inout) :: ierr          !< error count

        REAL(kind = real_wp) :: RDUMMY(1)
        LOGICAL       LDUMMY, UPDATR
        CHARACTER*200 FINAM
        INTEGER(kind = int_wp) :: SENDBUF(3)
        CHARACTER*4   cext                          ! inital conditions file extention

        INTEGER(kind = int_wp) :: IERRIO, new_lun

        LOGICAL       propor
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("initialize_all_conditions", ithandl)

        !         initialise the system

        ftype = filtype
        call set_array_indexes  (file_unit_list(19), .TRUE., buffer%rbuf, buffer%ibuf, buffer%chbuf, &
                max_real_arr_size, max_int_arr_size, max_char_arr_size)

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
            !
            !     copy common to (possible) shared array to share these values with
            !     other processes (domain decomposition)
            !
            CALL DHISYS (J(ISYSI:), J(ISYSN:))
            !
            J(ILP) = page_length
            J(ILP + 1) = 10
            J(ILP + 4) = page_length
            J(ILP + 5) = 20

            !     number of all segments - including segments at the bed

            nosss = num_cells + num_cells_bottom                ! num_cells_bottom are bed-volumes
            noqtt = num_exchanges + num_exchanges_bottom_dir
            !
            !         initialisation of info from the system file
            !
            CALL open_waq_files (file_unit_list(2), file_name_list(2), 2, 2, IERRD)
            CALL initialize_fixed_conditions (file_unit_list, C(IMNAM), C(ISNAM), J(IDUMP:), C(IDNAM), &
                    J(IDPNT:), J(IVPNT:), A(IDISP:), J(IBPNT:), C(IBNID), &
                    C(IBNAM), C(IBTYP), J(INTYP:), J(IWAST:), iwstkind, &
                    C(IWSID), C(IWNAM), C(IWTYP), A(ILENG:), A(ICONS:), &
                    A(IPARM:), J(INRFT:), J(INRH2:), C(ICNAM), C(IPNAM), &
                    C(IFNAM), C(ISFNA), C(IDINA), C(IVNAM), J(IKNMR:), &
                    C(IDANA), J(IPDMP:), J(IQDMP:), J(ISDMP:), C(IRNAM), &
                    J(IORAA:), J(NQRAA:), J(IQRAA:), J(IGNOS:), J(IGREF:), &
                    J(IGSEG:), gridps, J(IDMPB:), dlwqd)
            CLOSE (file_unit_list(2))
            !
            !     open binary system files for new input processing, if any
            !
            CALL open_waq_files (file_unit_list(41), file_name_list(41), 41, 1, IERRD)
            IF (IERRD == 0) THEN
                DO I = 1, num_unformat_files
                    READ (file_unit_list(41), *) iftyp, FINAM
                    new_lun = 800 + I
                    CALL open_waq_files (new_lun, FINAM, 3, 2 + iftyp, IOERR)
                    IF (IOERR /= 0) THEN
                        WRITE (file_unit_list(19), '(A,I3,A,A)') &
                                ' ERROR opening file on unit: ', 800 + I, ' filename: ', FINAM
                        CALL stop_with_error()
                    ENDIF
                    ICLEN = LEN(FINAM)
                    DO IC = 1, MIN(ICLEN, 200)
                        C(ILUNT + (I - 1) * 200 + IC - 1) = FINAM(IC:IC)
                    ENDDO
                ENDDO
                CLOSE(file_unit_list(41))
            ENDIF
            !
            !     initialisation of PROCES subsytem
            !
            IF (num_processes_activated > 0) THEN
                CALL open_waq_files (file_unit_list(24), file_name_list(24), 24, 2, IERRD)
                CALL initialize_processes (file_unit_list(24), file_name_list(24), file_unit_list(19), num_substances_total, process_space_int_len, &
                        num_processes_activated, num_local_vars, num_fluxes, num_defaults, J(INSVA:), &
                        J(IIFLU:), J(IPVAR:), J(IPTYP:), A(IDEFA:), A(ISTOC:), &
                        C(IPRNA:), J(IIMOD:), IERR, bloom_status_ind, &
                        bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                        A(IDSTO:), A(IVSTO:), num_dispersion_arrays_new, J(IDPNW:), num_velocity_arrays_new, &
                        J(IVPNW:), num_local_vars_exchange, J(IPGRD:), J(IPNDT:), num_vars, &
                        J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), J(IVTAG:), &
                        J(IVAGG:), num_input_ref, J(ipror:), j(iprvpt:))
                CLOSE (file_unit_list(24))
            ENDIF
            !
            !     Set variable "structure"
            !
            CALL initialize_variables (file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                    num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                    num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                    NOPRED, num_vars, J(IVARR:), J(IVIDX:), J(IVTDA:), &
                    J(IVDAG:), J(IVTAG:), J(IVAGG:), num_grids, J(IVSET:))
            !
            !     initialisation of OUTPUT subsytem
            !

            IF (num_output_files > 0) THEN
                CALL open_waq_files (file_unit_list(25), file_name_list(25), 25, 2, IERRD)
                CALL initialize_output (file_unit_list(25), file_name_list(25), file_unit_list(19), num_output_files, num_output_variables_extra, &
                        output_buffer_len, J(IIOUT:), J(IIOPO:), C(IONAM), C(IOSNM), &
                        C(IOUNI), C(IODSC), num_substances_total, C(ISSNM), C(ISUNI), &
                        C(ISDSC), file_unit_list, file_name_list, IERR)
                CLOSE (file_unit_list(25))
            ENDIF
            !
            !         initialisation of the grid layout
            !
            IF (num_cells_u_dir * num_cells_v_dir > 0) THEN
                CALL open_waq_files (file_unit_list(6), file_name_list(6), 6, 2, IERRD)
                READ  (file_unit_list(6)) (J(K), K = IGRID, IGRID + num_cells_u_dir * num_cells_v_dir - 1)
                CLOSE (file_unit_list(6))
            ENDIF
            !
            !         initialisation of exchange pointers
            !
            CALL open_waq_files (file_unit_list(8), file_name_list(8), 8, 2 + ftype(8), IERRD)

            if (num_rows * num_columns > 0) then

                !        read grid, make pointer table

                i1 = ilgra - 1
                read  (file_unit_list(8)) nmax2, mmax2, noseg2, kmax2, noq1d, noq2d, noq3d
                read  (file_unit_list(8)) (j(i1 + k), k = 1, num_columns * num_rows)
                i2 = ikbnd - 1

                call create_pointer_table(num_rows, num_columns, num_layers_grid, num_cells, num_boundary_conditions, &
                        num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, j(ilgra:), j(ixpnt:), &
                        cellpnt, flowpnt)
                finam = file_name_list(8)(1:index(file_name_list(8), '.', .true.)) // 'cco'
                call open_waq_files (file_unit_list(8), finam, 8, 2 + ftype(8), ierrd)
                read (file_unit_list(8))
                read (file_unit_list(8)) mmax2, nmax2, x0, y0, beta, np2, nlay
                do i = 1, 2 * np2 + 9
                    read(file_unit_list(8)) dummy
                enddo
                read (file_unit_list(8)) cell_x
                read (file_unit_list(8)) cell_y
            else

                ! Read pointer table with first index 4
                I1 = IXPNT - 1
                do i = 1, noqtt
                    READ (file_unit_list(8)) (J(I1 + K + (i - 1) * 4), K = 1, 4)
                enddo
            ENDIF
            CLOSE (file_unit_list(8))

            IBFLAG = 0
            IF (MOD(INTOPT, 16) >= 8) IBFLAG = 1

            !
            ! locally/per processor adapt the feature array:
            !   feature 1 == segment is active segment of own subdomain or not
            !   feature 2 == position w.r.t. the vertical direction
            !   feature 3 == segment is active segment of global domain or not
            !   feature 4 == segment belongs to own processor
            CALL CHKNMR (file_unit_list(19), nosss, J(IKNMR:))

            ! determine top of the vertical columns
            call segcol(nosss, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                    j(ixpnt:), j(iknmr:), isegcol)

            ! initial conditions
            propor = .false.
            call open_waq_files (file_unit_list(18), file_name_list(18), 18, 2, ierrd)
            ! look for the file type
            ig = scan (file_name_list(18), '.', back = .true.)
            cext = file_name_list(18)(ig:ig + 3)
            call str_lower(cext)
            ! if .rmp or .rm2 (Sobek) or .map, it is a map-file
            if (cext == '.map' .or. cext == '.rmp' .or. &
                    cext == '.rm2') then
                ! read title of simulation
                read (file_unit_list(18), iostat = ierrio) finam(1:160)
                if (ierrio /= 0) goto 50
                !  at end of third line ...
                if (finam(114:120) == 'mass/m2' .or. &
                        finam(114:120) == 'MASS/M2') propor = .true.
                ! should be nr. of substance
                read (file_unit_list(18)) idummy
                if (idummy /= num_substances_total) then
                    write (file_unit_list(19), '(a,a,/,a,i10)') &
                            ' ERROR reading initial conditions - filename: ', file_name_list(18), &
                            ' Number of substances does not match : ', idummy
                    call stop_with_error()
                endif
                ! should be nr. of comp. volumes
                read (file_unit_list(18)) idummy
                if (idummy /= nosss) then
                    write (file_unit_list(19), '(a,a,/,a,i10)') &
                            ' ERROR reading initial conditions - filename: ', file_name_list(18), &
                            ' Number of computational volumes does not match : ', idummy
                    call stop_with_error()
                endif
                do i = 1, num_substances_total
                    read (file_unit_list(18)) finam(1:20)
                enddo
            endif
            read  (file_unit_list(18), iostat = ierrio)                   & ! like the .ini, the .res and .wrk file
                    idummy, (a(k), k = iconc, iconc + num_substances_total * nosss - 1)
            50 if (ierrio /= 0) then
                write (file_unit_list(19), '(a,a)') &
                        ' ERROR reading initial conditions - filename: ', &
                        file_name_list(18), &
                        ' Too few data - file contents does not match current ' // &
                                'model'
                call stop_with_error()
            else
                read  (file_unit_list(18), iostat = ierrio) idummy
                if (ierrio == 0) then
                    write (file_unit_list(19), '(a,a)') &
                            ' ERROR reading initial conditions - filename: ', &
                            file_name_list(18), &
                            ' Too many data - file contents does not match ' // &
                                    'current model'
                    call stop_with_error()
                endif
            endif
            close (file_unit_list(18))

            ! first read of relevant time varying arrays
            IFFLAG = 1

            CALL initialize_time_dependent_variables (file_unit_list, ITSTRT, ITIMEL, A(IHARM:), A(IFARR:), &
                    J(INRHA:), J(INRH2:), J(INRFT:), IDT, A(IVOL:), &
                    A(IDIFF:), A(IAREA:), A(IFLOW:), A(IVELO:), A(ILENG:), &
                    A(IWSTE:), A(IBSET:), A(ICONS:), A(IPARM:), A(IFUNC:), &
                    A(ISFUN:), J(IBULK:), file_name_list, C(ILUNT), ftype, &
                    INTSRT, ISFLAG, IFFLAG, IVFLAG, ILFLAG, &
                    UPDATR, J(IKTIM:), J(IKNMR:), J(INISP:), A(INRSP:), &
                    J(INTYP:), J(IWORK:), .FALSE., LDUMMY, RDUMMY, &
                    .TRUE., gridps, DLWQD)

            ! Particle tracking
            call delpar00 (file_unit_list(19), file_name_list(45), num_cells, num_exchanges, a(ivol:), a(iflow:), &
                    num_spatial_time_fuctions, c(isfna:), a(isfun:))

            ! New bottomlayer processing
            IF (num_exchanges_bottom_dir > 0) &
                    CALL expands_vol_area_for_bottom_cells (file_unit_list, num_cells, num_cells_bottom, num_layers, num_grids, &
                            num_exchanges, num_exchanges_bottom_dir, J(IGREF:), J(IGSEG:), num_constants, &
                            num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, A(ICONS:), C(ICNAM:), &
                            A(IPARM:), C(IPNAM:), A(IFUNC:), C(IFNAM:), A(ISFUN:), &
                            C(ISFNA:), J(IXPNT:), A(IVOL:), A(IAREA:), A(IFLOW:), &
                            A(ILENG:))
            !

            IF (INTSRT == 6 .OR. INTSRT == 7) THEN
                NOSUBz = num_substances_total
            ELSE
                NOSUBz = num_substances_transported
            ENDIF
            call copy_real_array_elements   (A(IBSET:), A(IBOUN:), num_boundary_conditions * NOSUBz)
            call copy_real_array_elements   (A(IBSET:), A(IBSAV:), num_boundary_conditions * NOSUBz)
            call initialize_real_array   (A(IDERV:), num_substances_total * NOSSS)
            call initialize_real_array   (A(IMAS2:), num_substances_total * 5)
            call initialize_real_array   (A(IWDMP:), num_substances_total * num_waste_loads * 2)
            IF (MOD(INTOPT, 16) > 7) THEN
                call initialize_real_array(A(IDMPQ:), num_substances_transported * NDMPQ * 2)
                call initialize_real_array(A(IDMPS:), num_substances_total * num_monitoring_cells * 3)
                call initialize_real_array(A(ISMAS:), num_substances_total * NDMPAR * 6)
                call initialize_real_array(A(IFLXI:), NDMPAR * num_fluxes)
                call initialize_real_array(A(IFLXD:), num_monitoring_cells * num_fluxes)
                call initialize_real_array(A(ITRRA:), num_substances_transported * num_transects)
            ENDIF

            !         make start masses for dynamic and iterative computation

            if (intsrt ==  6 .or. intsrt ==  7 .or. &
                    intsrt == 17 .or. intsrt == 18) goto 40

            !         initial conditions coflowing substances

            do iseg = 0, nosss - 1
                volume = a(ivol + iseg)
                do i1 = iseg * num_substances_total, iseg * num_substances_total + num_substances_transported - 1
                    a(imass + i1) = a(iconc + i1) * volume
                enddo
            enddo

            !         initial conditions passive substances

            if (num_substances_transported /= num_substances_total) then                         ! if there are bed-substances
                indx = index_in_array('SURF      ', buffer%create_strings_20_array(ipnam, num_spatial_parameters))
                if (indx > 0) then                           ! and if SURF is found
                    call inact (nosss, num_substances_transported, num_substances_total, a(iconc:), a(imass:), &
                            num_spatial_parameters, indx, a(iparm:), c(imnam + 113), propor, &
                            .true.)
                else                                     ! routine inact is at end of this file !
                    indx = index_in_array('SURF      ', buffer%create_strings_20_array(isfna, num_spatial_time_fuctions))
                    if (indx > 0) then                        ! and if SURF is found
                        call inact (nosss, num_substances_transported, num_substances_total, a(iconc:), a(imass:), &
                                num_spatial_time_fuctions, indx, a(isfun:), c(imnam + 113), propor, &
                                .false.)
                    else
                        write (file_unit_list(19), '(a,a)')               & !   not found
                                ' Error reading initial conditions: ', &
                                ' horizontal surface area not found! '
                        call stop_with_error()
                    endif
                endif
            endif

            !         deal with z-layers (inactive cells at the bottom side of the water column
            call zlayer (num_cells, nosss, num_substances_transported, num_substances_total, num_layers, &
                    a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, num_exchanges, a(iarea:), num_constants, &
                    c(icnam:), a(icons:), num_spatial_parameters, c(ipnam:), a(iparm:), &
                    num_spatial_time_fuctions, c(isfna:), a(isfun:), a(iconc:), a(imass:), &
                    j(iknmr:), iknmkv, j(ixpnt:))


            !     temporary for closure error

            40 INDX = index_in_array('CLOSE_ERR ', buffer%create_strings_20_array(ICNAM, num_constants))
            IF (INDX > 0) THEN
                ICFLAG = 1
                WRITE(file_unit_list(19), *) ' Closure error correction enabled'
            ELSE
                ICFLAG = 0
                WRITE(file_unit_list(19), *) ' Closure error correction disabled'
            ENDIF

        end associate

        if (timon) call timstop (ithandl)
        RETURN
    END subroutine initialize_all_conditions

    subroutine inact (num_cells, num_substances_transported, num_substances_total, conc, amass, &
            num_spatial_parameters, iparm, parm, string, propor, &
            direct)
        !>\File
        !>         Makes mass/gridcell from mass/m2 for the passive substances

        implicit none

        integer(kind = int_wp), intent(in) :: num_cells              !< number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_transported              !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total              !< total number of substances
        real(kind = real_wp), intent(inout) :: conc (num_substances_total, num_cells) !< the concentration values
        real(kind = real_wp), intent(out) :: amass(num_substances_total, num_cells) !< the mass values
        integer(kind = int_wp), intent(in) :: num_spatial_parameters               !< number of parameters or segment functions
        integer(kind = int_wp), intent(in) :: iparm              !< selected parameter
        real(kind = real_wp), intent(in) :: parm (num_spatial_parameters * num_cells) !< parameter or segment function array
        character(1), intent(out) :: string(7)          !< model docu substring
        logical, intent(in) :: propor             !< if .true. then /m2 in the input
        logical, intent(in) :: direct             !< if .false. segments is first index

        integer(kind = int_wp) :: iseg   ! loop counter computational volumes
        integer(kind = int_wp) :: isys   ! loop counter modelled substances
        real(kind = real_wp) :: surf   ! help variable
        integer(kind = int_wp) :: indx   ! index

        string(1:7) = ['m', 'a', 's', 's', '/', 'm', '2']   ! always in the output and keep debugger happy
        if (direct) then
            indx = iparm                    ! parameter
        else
            indx = (iparm - 1) * num_cells + 1       ! segment function
        endif
        do iseg = 1, num_cells
            surf = parm(indx)
            do isys = num_substances_transported + 1, num_substances_total
                if (propor) then                                ! input / m2
                    amass(isys, iseg) = conc(isys, iseg) * surf
                else                                              ! input / gridcell
                    amass(isys, iseg) = conc(isys, iseg)
                    conc (isys, iseg) = conc(isys, iseg) / surf      ! conc  / m2
                endif
            enddo
            if (direct) then
                indx = indx + num_spatial_parameters
            else
                indx = indx + 1
            endif
        enddo

        return
    end subroutine inact

    SUBROUTINE initialize_fixed_conditions (file_unit_list, MODID, SYSID, IDUMP, DUMPID, &
            IDPNT, IVPNT, DISP, IBPNT, BNDID, &
            BNDNAM, BNDTYP, INWTYP, IWASTE, iwsknd, &
            WASTID, WSTNAM, WSTTYP, ALENG, CONST, &
            PARAM, NRFTOT, NRHARM, CONAME, PANAME, &
            FUNAME, SFNAME, DINAME, VENAME, IKNMRK, &
            DANAM, IPDMP, IQDMP, ISDMP, RANAM, &
            IORAAI, NQRAAI, IQRAAI, GRDNOS, GRDREF, &
            GRDSEG, GridPs, DMPBAL, dlwqd)

        !> Reads the DelwaQ binary system file
        !>
        !> Initialises all fixed conditions and names for the simulation from the binary system
        !>                          file at file_unit_list(2).
        !     LOGICAL UNITNUMBERS : file_unit_list( 2) - system intermediate file
        !                           file_unit_list(19) - monitoring output file

        use timers
        use m_grid_utils_external
        use delwaq2_data
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics


        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     file_unit_list     INTEGER       *     INPUT   logical unitnumbers
        !     MODID   CHAR*40       4     OUTPUT  Model and run-ID
        !     SYSID   CHAR*20   num_substances_total     OUTPUT  Systems ID
        !     IDUMP   INTEGER  num_monitoring_points     OUTPUT  Dump segment numbers
        !     DUMPID  CHAR*20  num_monitoring_points     OUTPUT  Dump-segment ID
        !     IDPNT   INTEGER   num_substances_transported     OUTPUT  Pointers to dispersion array
        !     IVPNT   INTEGER   num_substances_transported     OUTPUT  Pointers to velocity array
        !     DISP    REAL          3     OUTPUT  dispersion in 3 directions
        !     IBPNT   INTEGER  4*num_boundary_conditions    OUTPUT  1,* = timelag
        !                                         2,* = flow pointer
        !                                         3,* = segment pointer
        !                                         4,* = time on timelag
        !     BNDID   CHAR*20   num_boundary_conditions     OUTPUT  Open boundary ID's
        !     BNDNAM  CHAR*40   num_boundary_conditions     OUTPUT  Open boundary names
        !     BNDTYP  CHAR*20   num_boundary_conditions     OUTPUT  Open boundary types
        !     INWTYP  INTEGER       *     OUTPUT  Types of items
        !     IWASTE  INTEGER   num_waste_loads     OUTPUT  waste load segment numbers
        integer(kind = int_wp), intent(out) :: iwsknd(*) !  wasteload processing
        !     WASTID  CHAR*20   num_waste_loads     OUTPUT  Waste location ID
        !     WSTNAM  CHAR*40   num_waste_loads     OUTPUT  Waste location names
        !     WSTTYP  CHAR*20   num_waste_loads     OUTPUT  Waste location types
        !     ALENG   REAL        3       OUTPUT  Lengthes in 3 directions
        !     CONST   REAL     num_constants     OUTPUT  value of constants
        !     PARAM   REAL    num_spatial_parameters,num_cells  OUTPUT  value of parameters
        !     NRFTOT  INTEGER  num_items_time_fn     OUTPUT  file lengthes per item
        !     NRHARM  INTEGER  num_items_time_fn     OUTPUT  nr of harmonics per item
        !     CONAME  CHAR*20  num_constants     OUTPUT  Constant names
        !     PANAME  CHAR*20  num_spatial_parameters       OUTPUT  Parameter names
        !     FUNAME  CHAR*20  num_time_functions      OUTPUT  Function names
        !     SFNAME  CHAR*20  num_spatial_time_fuctions     OUTPUT  Segment function names
        !     DINAME  CHAR*20  num_dispersion_arrays     OUTPUT  Dispersion array names
        !     VENAME  CHAR*20  num_velocity_arrays     OUTPUT  Velocity array names
        !     DANAM   CHAR*20  NDMPAR     OUTPUT  Dump-area    ID
        !     IPDMP   INTEGER       *     OUTPUT  pointer structure dump area's
        !     IQDMP   INTEGER       *     OUTPUT  Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     OUTPUT  Segment to dumped segment pointer
        !     RANAM   CHAR*20       *     OUTPUT  transects names
        !     IORAAI  INTEGER       *     OUTPUT  option output transects
        !     NQRAAI  INTEGER       *     OUTPUT  number of exch. per transect
        !     IQRAAI  INTEGER       *     OUTPUT  exchange nunbers transect
        !
        !
        !     IN COMMON BLOCK     :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_cells   INTEGER       1     INPUT
        !     num_substances_transported   INTEGER 1   INPUT
        !     num_substances_total   INTEGER       1   INPUT
        !     num_dispersion_arrays  INTEGER       1   INPUT
        !     num_velocity_arrays  INTEGER       1     INPUT
        !     num_exchanges     INTEGER       1     INPUT
        !     num_monitoring_points  INTEGER       1     INPUT
        !     num_boundary_conditions   INTEGER       1     INPUT
        !     num_boundary_types  INTEGER       1     INPUT
        !     num_waste_loads   INTEGER       1     INPUT
        !     num_waste_load_types  INTEGER       1     INPUT   Number of waste load types
        !     num_constants  INTEGER       1     INPUT   Number of constants used
        !     num_spatial_parameters    INTEGER       1     INPUT   Number of parameters
        !     num_time_functions   INTEGER       1     INPUT   Number of functions ( user )
        !     num_spatial_time_fuctions  INTEGER       1     INPUT   Number of segment functions
        !     num_items_time_fn  INTEGER       1     INPUT   Number possible functions
        !     NDMPAR  INTEGER       1     INPUT   Number of dump area's
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     NTDMPS  INTEGER       1     INPUT   total number segments in dump area
        !     num_transects  INTEGER       1     INPUT
        !     num_transect_exchanges  INTEGER       1     INPUT

        INTEGER(kind = int_wp) :: IPDMP(*), IQDMP(*), ISDMP (*), IORAAI(*), &
                NQRAAI(*), IQRAAI(*), GRDNOS(*), GRDREF(*), &
                IDUMP (*), IDPNT (*), IVPNT (*), IBPNT (4, *), &
                IWASTE(*), NRFTOT(*), NRHARM(*), file_unit_list   (*), &
                IKNMRK(*), INWTYP(*)
        INTEGER(kind = int_wp) :: GRDSEG(num_cells + num_cells_bottom, num_grids)
        CHARACTER*40 MODID (4), BNDNAM(*), WSTNAM(*)
        CHARACTER*20 SYSID (*), DUMPID(*), BNDID (*), BNDTYP(*), &
                WASTID(*), WSTTYP(*), CONAME(*), PANAME(*), &
                FUNAME(*), SFNAME(*), DINAME(*), VENAME(*), &
                DANAM (*), RANAM (*)
        real(kind = real_wp) :: DISP  (*), ALENG (*), CONST (*), PARAM (*)
        CHARACTER*40  FILLER
        type(GridPointerColl), intent(inout) :: GridPs     !< definitions of the grids
        type(delwaq_data), intent(inout) :: dlwqd      !< derived type for persistent storage
        integer(kind = int_wp) :: dmpbal(*)  !< indicates if dump area is included in the balance
        type(t_grid) :: aGrid      ! a single grid

        integer(kind = int_wp) :: it, noqtt, nosss, k, igrid, iin, iseg, ierror, i_grid
        integer(kind = int_wp) :: isys, ix, i, idummy
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("initialize_fixed_conditions", ithandl)

        IT = 0
        ! read from the system file
        NOQTT = num_exchanges + num_exchanges_bottom_dir
        NOSSS = num_cells + num_cells_bottom
        IIN = file_unit_list(2)
        READ (IIN, END = 40, ERR = 40)  MODID (1), MODID(2)
        READ (IIN, END = 40, ERR = 40)  MODID (3), MODID(4)
        READ (IIN, END = 40, ERR = 40) (SYSID (K), K = 1, num_substances_total)
        IF (num_monitoring_points > 0) &
                READ (IIN, END = 40, ERR = 40) (IDUMP(K), DUMPID(K), K = 1, num_monitoring_points)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DANAM(K), K = 1, NDMPAR)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DMPBAL(K), K = 1, NDMPAR)
        IF (num_transects > 0) &
                READ (IIN, END = 40, ERR = 40) (RANAM(K), K = 1, num_transects)

        ! sub-grid
        DO IGRID = 1, num_grids
            READ (IIN, END = 40, ERR = 40)  GRDNOS(IGRID), GRDREF(IGRID), &
                    (GRDSEG(ISEG, IGRID), ISEG = 1, NOSSS)
        ENDDO
        !     the grid structures
        DO IGRID = 1, num_grids
            ierror = aGrid%read(iin, nosss)
            if (ierror /= 0) goto 40
            i_grid = GridPs%add(aGrid)
        ENDDO
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, num_substances_total)
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, num_substances_total)
        READ (IIN, END = 40, ERR = 40) (IKNMRK(K), K = 1, NOSSS)
        IF (num_dispersion_arrays > 0) &
                READ (IIN, END = 40, ERR = 40) (DINAME(K), K = 1, num_dispersion_arrays)
        IF (num_velocity_arrays > 0) &
                READ (IIN, END = 40, ERR = 40) (VENAME(K), K = 1, num_velocity_arrays)
        READ (IIN, END = 40, ERR = 40) (IDPNT (K), K = 1, num_substances_transported)
        READ (IIN, END = 40, ERR = 40) (IVPNT (K), K = 1, num_substances_transported)
        IF (num_boundary_conditions  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (IBPNT (2, K), K = 1, num_boundary_conditions)
            READ (IIN, END = 40, ERR = 40) (IBPNT (3, K), K = 1, num_boundary_conditions)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IPDMP(K), K = 1, NDMPAR + NTDMPQ)
            IX = NDMPAR + NTDMPQ
            READ (IIN, END = 40, ERR = 40)  (IPDMP(IX + K), K = 1, NDMPAR + NTDMPS)
        ENDIF
        IF (num_transects > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IORAAI(K), K = 1, num_transects)
            READ (IIN, END = 40, ERR = 40)  (NQRAAI(K), K = 1, num_transects)
            READ (IIN, END = 40, ERR = 40)  (IQRAAI(K), K = 1, num_transect_exchanges)
        ENDIF
        IF (num_transects > 0 .OR. NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IQDMP(K), K = 1, NOQTT)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (ISDMP(K), K = 1, NOSSS)
        ENDIF
        READ (IIN, END = 40, ERR = 40) IDUMMY, (DISP  (K), K = 1, 3)
        READ (IIN, END = 40, ERR = 40) IDUMMY, (ALENG (K), K = 1, 3)
        IF (num_boundary_conditions  > 0) THEN
            DO I = 1, num_boundary_conditions
                READ (IIN, END = 40, ERR = 40) BNDID(I), BNDNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (BNDTYP(K), K = 1, num_boundary_types)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, num_boundary_conditions)
            IT = IT + num_boundary_conditions
            !          read time lags
            READ (IIN, END = 40, ERR = 40) (IBPNT(1, K), K = 1, num_boundary_conditions)
        ENDIF
        IF (num_waste_loads  > 0) THEN
            DO I = 1, num_waste_loads
                READ (IIN, END = 40, ERR = 40) IWASTE(I), iwsknd(i), &
                        WASTID(I), WSTNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (WSTTYP(K), K = 1, num_waste_load_types)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, num_waste_loads)
            IT = IT + num_waste_loads
        ENDIF
        IF (num_constants > 0) THEN
            READ (IIN, END = 40, ERR = 40) (CONAME(K), K = 1, num_constants)
        ENDIF
        IF (num_spatial_parameters   > 0) THEN
            READ (IIN, END = 40, ERR = 40) (PANAME(K), K = 1, num_spatial_parameters)
        ENDIF
        IF (num_time_functions  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (FUNAME(K), K = 1, num_time_functions)
        ENDIF
        IF (num_spatial_time_fuctions > 0) THEN
            READ (IIN, END = 40, ERR = 40) (SFNAME(K), K = 1, num_spatial_time_fuctions)
        ENDIF
        !
        !     Time function info
        !
        READ (IIN, END = 40, ERR = 40) (NRFTOT(K), K = 1, num_items_time_fn)
        READ (IIN, END = 40, ERR = 40) (NRHARM(K), K = 1, num_items_time_fn)
        !
        !         boundary timings greater then timelag
        !
        DO I = 1, num_boundary_conditions
            IBPNT(4, I) = IBPNT(1, I) + 1
        end do
        !
        !         extract reference date and time
        !
        CALL MODIFIED_JULIAN(MODID(4))
        dlwqd%otime = otime
        dlwqd%tscale = tscale
        !
        !         completion successful
        !
        WRITE (file_unit_list(19), 2000) (MODID(K), K = 1, 4)
        if (timon) call timstop (ithandl)
        RETURN
        !
        !         unsuccessful read
        !
        40 WRITE (file_unit_list(19), 2010)
        CALL stop_with_error()
        !
        !         output formats
        !
        2000 FORMAT (' ', 20X, A40/21X, A40//21X, A40/21X, A40// &
                21X, 'Initialisation from system file completed.')
        2010 FORMAT ('   ERROR reading binary system file !!'/ &
                '   initialisation NOT successful    !!'/ &
                '   simulation impossible            !!')
        !
    CONTAINS
        SUBROUTINE MODIFIED_JULIAN(T0STRING)

            CHARACTER(LEN = *) :: T0STRING

            INTEGER(kind = int_wp) :: IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE
            INTEGER(kind = int_wp) :: IERR
            REAL(kind = dp) :: TEMP1, TEMP2

            REAL(kind = dp), PARAMETER :: MODIFICATION_OFFSET = 2400000.5D0

            TSCALE = 1.0d0

            READ(T0STRING, '(4x,i4.4,x,i2.2,x,i2.2,x,i2.2,x,i2.2,x,i2.2,7x,i8)', IOSTAT = IERR) &
                    IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, ISCALE

            IF (IERR /= 0) THEN
                IYEAR = 1900
                IMONTH = 1
                IDAY = 1
                IHOUR = 0
                IMIN = 0
                ISEC = 0
                ISCALE = 1
            ENDIF

            TEMP1 = INT ((IMONTH - 14.0D0) / 12.0D0)
            TEMP2 = IDAY - 32075.0D0 + &
                    INT (1461.0D0 * (IYEAR + 4800.0D0 + TEMP1) / 4.0D0) + &
                    INT (367.0D0 * (IMONTH - 2.0D0 - TEMP1 * 12.0D0) / 12.0D0) - &
                    INT (3.0D0 * INT ((IYEAR + 4900.0D0 + TEMP1) / 100.0D0) / &
                            4.0)
            TEMP1 = FLOAT (IHOUR) * 3600.0 + &
                    FLOAT (IMIN) * 60.0 + &
                    FLOAT (ISEC) - 43200.0
            OTIME = TEMP2 + (TEMP1 / 86400.0) - MODIFICATION_OFFSET
            TSCALE = 86400.0D0 / ISCALE
        END SUBROUTINE MODIFIED_JULIAN

    END SUBROUTINE initialize_fixed_conditions

    SUBROUTINE initialize_processes(LUNWRP, LCH, LUREP, num_substances_total, process_space_int_len, &
            num_processes_activated, num_local_vars, num_fluxes, num_defaults, PRVNIO, &
            IFLUX, PRVVAR, PRVTYP, DEFAUL, STOCHI, &
            PRONAM, IMODU, IERR, bloom_status_ind, &
            bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
            DSTO, VSTO, num_dispersion_arrays_new, IDPNW, num_velocity_arrays_new, &
            IVPNW, num_local_vars_exchange, PROGRD, PRONDT, num_vars, &
            VARARR, VARIDX, VARTDA, VARDAG, VARTAG, &
            VARAGG, num_input_ref, proref, prvpnt)
        ! Initialisation of PROCES system .
        !
        !     FILES               : LUNWRP, Proces work file
        !                           LUREP , Monitoring file
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNWRP  INTEGER       1     INPUT   Proces work file
        !     LCH     CHA*(*)       1     INPUT   Name proces work file
        !     LUREP   INTEGER       1     INPUT   Monitoring file
        !     num_substances_total   INTEGER       1     INPUT   Number of substances
        !     process_space_int_len  INTEGER       1     INPUT   Length process_space_int
        !     num_processes_activated   INTEGER       1     INPUT   Number of called processes
        !     num_local_vars   INTEGER       1     INPUT   Number of local proces params
        !     num_fluxes   INTEGER       1     INPUT   total number of fluxes
        !     num_defaults   INTEGER       1     INPUT   Number of used defaults
        !     PRVNIO  INTEGER       *     OUTPUT  Number of variables per proces
        !     IFLUX   INTEGER       *     OUTPUT  Pointer in FLUX per proces inst.
        !     process_space_int   INTEGER       *     OUTPUT  Pointer in SSA per proces inst.
        !     IPSSA   INTEGER       *     OUTPUT  Pointer to SSA per proces inst.
        !     DEFAUL  REAL          *     OUTPUT  Default proces parameters
        !     STOCHI  REAL          *     OUTPUT  Proces stochiometry
        !     PRONAM  CHA*(*)       *     OUTPUT  Name of called module
        !     IMODU   INTEGER       *     OUTPUT  Module number proces
        !     IERR    INTEGER       1     IN/OUT  Error count
        !     bloom_status_ind  INTEGER       1     INPUT   Number of Bloom module (if >0)
        !     bloom_ind  INTEGER       1     INPUT   Offset in process_space_int for Bloom
        !     num_substances_transported   INTEGER       1     INPUT   Number of active substances
        !     num_dispersion_arrays_extra   INTEGER      1     INPUT
        !     num_velocity_arrays_extra   INTEGER       1     INPUT
        !     DSTO    INTEGER num_substances_transported,*     OUTPUT  dispersion stochi matrix
        !     VSTO    INTEGER num_substances_transported,*     OUTPUT  velocity stochi matrix
        !     num_dispersion_arrays_new   INTEGER       1     INPUT   Number of new dispersion array
        !     IDPNW   INTEGER   num_substances_transported     OUTPUT  Pointers to new dispersion array
        !     num_velocity_arrays_new   INTEGER       1     INPUT
        !     IVPNW   INTEGER   num_substances_transported     OUTPUT  Pointers to new velocity array
        !     PROGRD  INTEGER   num_processes_activated     OUTPUT  Grid number for process
        !     PRONDT  INTEGER   num_processes_activated     OUTPUT  Fractional step for process

        use timers
        use process_registration

        INTEGER(kind = int_wp) :: LUNWRP, LUREP, num_substances_total, process_space_int_len, num_processes_activated, &
                num_local_vars, num_fluxes, num_defaults, bloom_status_ind, &
                bloom_ind, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                num_dispersion_arrays_new, num_velocity_arrays_new, num_vars, num_input_ref
        INTEGER(kind = int_wp) :: PRVNIO(*), IFLUX(*), PRVVAR(*), &
                PRVTYP(*), IMODU(*), IDPNW(*), &
                IVPNW(*), PROGRD(*), PRONDT(*), &
                VARARR(*), VARIDX(*), VARTDA(*), &
                VARDAG(*), VARTAG(*), VARAGG(*), &
                proref(*), prvpnt(*)
        REAL(kind = real_wp) :: DEFAUL(*), STOCHI(*), DSTO(*), &
                VSTO(*)
        CHARACTER*(*) LCH
        CHARACTER*10  PRONAM(*)
        !
        !     Local declarations
        INTEGER(kind = int_wp) :: NIPMSD, NPROCD, NOLOCD, NFLUXD, NODEFD, &
                NOTOTD, IOFF, NOSYSD, num_dispersion_arrays_extraD, num_velocity_arrays_extraD, &
                num_local_vars_exchangeD, num_dispersion_arrays_newD, num_velocity_arrays_newD, NOVARD, nrrefD
        REAL(kind = real_wp) :: VERSIO

        integer(kind = int_wp) :: k, ierr, num_local_vars_exchange, iproc, ifracs, ipdgrd

        ! Store fractional step flag in common CFRACS
        COMMON /CFRACS/ IFRACS
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("initialize_processes", ithandl)

        ! read and check version number
        READ (LUNWRP, ERR = 900, END = 900) VERSIO
        !
        !     read and check dimensions
        !
        READ (LUNWRP, ERR = 900, END = 900) NIPMSD, NPROCD, NFLUXD, &
                NOLOCD, NODEFD, NOTOTD, &
                NOSYSD, num_dispersion_arrays_extraD, num_velocity_arrays_extraD, &
                num_local_vars_exchangeD, num_dispersion_arrays_newD, num_velocity_arrays_newD, &
                NOVARD, nrrefD
        IF (NIPMSD /= process_space_int_len) THEN
            WRITE (LUREP, 2020) NIPMSD, process_space_int_len
            IERR = IERR + 1
        ENDIF
        IF (NPROCD /= num_processes_activated) THEN
            WRITE (LUREP, 2030) NPROCD, num_processes_activated
            IERR = IERR + 1
        ENDIF
        IF (NFLUXD /= num_fluxes) THEN
            WRITE (LUREP, 2040) NFLUXD, num_fluxes
            IERR = IERR + 1
        ENDIF
        IF (NOLOCD /= num_local_vars) THEN
            WRITE (LUREP, 2050) NOLOCD, num_local_vars
            IERR = IERR + 1
        ENDIF
        IF (NODEFD /= num_defaults) THEN
            WRITE (LUREP, 2060) NODEFD, num_defaults
            IERR = IERR + 1
        ENDIF
        IF (NOTOTD /= num_substances_total) THEN
            WRITE (LUREP, 2070) NOTOTD, num_substances_total
            IERR = IERR + 1
        ENDIF
        IF (NOSYSD /= num_substances_transported) THEN
            WRITE (LUREP, 2120) NOSYSD, num_substances_transported
            IERR = IERR + 1
        ENDIF
        IF (num_dispersion_arrays_extraD /= num_dispersion_arrays_extra) THEN
            WRITE (LUREP, 2130) num_dispersion_arrays_extraD, num_dispersion_arrays_extra
            IERR = IERR + 1
        ENDIF
        IF (num_velocity_arrays_extraD /= num_velocity_arrays_extra) THEN
            WRITE (LUREP, 2140) num_velocity_arrays_extraD, num_velocity_arrays_extra
            IERR = IERR + 1
        ENDIF
        IF (num_local_vars_exchangeD /= num_local_vars_exchange) THEN
            WRITE (LUREP, 2150) num_local_vars_exchangeD, num_local_vars_exchange
            IERR = IERR + 1
        ENDIF
        IF (num_dispersion_arrays_newD /= num_dispersion_arrays_new) THEN
            WRITE (LUREP, 2160) num_dispersion_arrays_newD, num_dispersion_arrays_new
            IERR = IERR + 1
        ENDIF
        IF (num_velocity_arrays_newD /= num_velocity_arrays_new) THEN
            WRITE (LUREP, 2170) num_velocity_arrays_newD, num_velocity_arrays_new
            IERR = IERR + 1
        ENDIF
        IF (NOVARD /= num_vars) THEN
            WRITE (LUREP, 2190) NOVARD, num_vars
            IERR = IERR + 1
        ENDIF
        IF (nrrefD /= num_input_ref) THEN
            WRITE (LUREP, 2200) nrrefd, num_input_ref
            IERR = IERR + 1
        ENDIF
        IF (IERR > 0) GOTO 910
        !
        READ (LUNWRP, ERR = 900, END = 900) (PRVNIO(K), K = 1, num_processes_activated)
        READ (LUNWRP, ERR = 900, END = 900) (IFLUX(K), K = 1, num_processes_activated)
        READ (LUNWRP, ERR = 900, END = 900) (PRVVAR(K), K = 1, process_space_int_len)
        READ (LUNWRP, ERR = 900, END = 900) (PRVTYP(K), K = 1, process_space_int_len)
        READ (LUNWRP, ERR = 900, END = 900) (DEFAUL(K), K = 1, num_defaults)
        READ (LUNWRP, ERR = 900, END = 900) (STOCHI(K), K = 1, num_substances_total * num_fluxes)
        READ (LUNWRP, ERR = 900, END = 900) (DSTO(K), K = 1, num_substances_transported * num_dispersion_arrays_extra)
        READ (LUNWRP, ERR = 900, END = 900) (VSTO(K), K = 1, num_substances_transported * num_velocity_arrays_extra)
        IF (num_dispersion_arrays_new > 0) THEN
            READ (LUNWRP, ERR = 900, END = 900) (IDPNW(K), K = 1, num_substances_transported)
        ENDIF
        IF (num_velocity_arrays_new > 0) THEN
            READ (LUNWRP, ERR = 900, END = 900) (IVPNW(K), K = 1, num_substances_transported)
        ENDIF
        READ (LUNWRP, ERR = 900, END = 900) (PRONAM(K), K = 1, num_processes_activated)
        READ (LUNWRP, ERR = 900, END = 900) (PROGRD(K), K = 1, num_processes_activated)
        READ (LUNWRP, ERR = 900, END = 900) (PRONDT(K), K = 1, num_processes_activated)
        READ (LUNWRP, ERR = 900, END = 900) (VARARR(K), K = 1, num_vars)
        READ (LUNWRP, ERR = 900, END = 900) (VARIDX(K), K = 1, num_vars)
        READ (LUNWRP, ERR = 900, END = 900) (VARTDA(K), K = 1, num_vars)
        READ (LUNWRP, ERR = 900, END = 900) (VARDAG(K), K = 1, num_vars)
        READ (LUNWRP, ERR = 900, END = 900) (VARTAG(K), K = 1, num_vars)
        READ (LUNWRP, ERR = 900, END = 900) (VARAGG(K), K = 1, num_vars)
        read (lunwrp, err = 900, end = 900) (proref(k), k = 1, num_processes_activated * num_input_ref)
        k = 1
        do iproc = 1, num_processes_activated
            prvpnt(iproc) = k
            k = k + prvnio(iproc)
        enddo
        !
        !     Set module numbers
        !
        DO K = 1, num_processes_activated
            CALL PRONRS (PRONAM(K), IMODU(K))
        end do
        !
        !     Report on process decomposition
        !
        IFRACS = 0
        IPDGRD = 0
        DO K = 1, num_processes_activated
            IF (PRONDT(K) > 1) THEN
                IFRACS = 1
            ENDIF
            IF (PROGRD(K) > 1) THEN
                IPDGRD = 1
            ENDIF
        ENDDO
        IF (IFRACS == 0 .AND. IPDGRD == 0) THEN
            WRITE(LUREP, 3010)
        ELSE
            WRITE(LUREP, 3020)
            DO K = 1, num_processes_activated
                WRITE(LUREP, 3000) PRONAM(K), PROGRD(K), PRONDT(K)
            ENDDO
        ENDIF
        !
        !     Check for Bloom connection
        !
        bloom_status_ind = 0
        bloom_ind = 0
        IOFF = 1
        DO K = 1, num_processes_activated
            IF (PRONAM(K)(1:6) == 'D40BLO') THEN
                bloom_status_ind = K
                bloom_ind = IOFF
                WRITE (LUREP, 2100)
            ENDIF
            IOFF = IOFF + PRVNIO(K)
        end do
        !
        goto 9999  !    RETURN
        !
        !     unsuccessful read
        !
        900 CONTINUE
        WRITE (LUREP, 2090) LCH, LUNWRP
        IERR = IERR + 1
        !
        910 CONTINUE
        9999 if (timon) call timstop (ithandl)
        RETURN

        2020 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for process_space_int_len', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2030 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_processes_activated ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2040 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_fluxes ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2050 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_local_vars ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2060 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_defaults ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2070 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_substances_total ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2090 FORMAT (' ERROR  : Reading proces work file;', A, &
                /'          on unit number ', I3)
        2100 FORMAT (' MESSAGE: Bloom fractional step switched on')
        2120 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_substances_transported ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2130 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_dispersion_arrays_extra ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2140 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_velocity_arrays_extra ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2150 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_local_vars_exchange ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2160 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_dispersion_arrays_new ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2170 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_velocity_arrays_new ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2190 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_vars ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2200 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_input_ref ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        3000 FORMAT (/' MODULE :', A, ' on grid ', I3, ', timestep multiplier:', I3)
        3010 FORMAT (/' No process decomposition active')
        3020 FORMAT (/' Process decomposition active')
        !
    end subroutine initialize_processes

    SUBROUTINE initialize_output(LUNWRO, LCH, LUREP, num_output_files, num_output_variables_extra, &
            output_buffer_len, IOUTPS, IOPOIN, OUNAM, OUSNM, &
            OUUNI, OUDSC, num_substances_total, SYSNM, SYUNI, &
            SYDSC, file_unit_list, file_name_list, IERR)
        ! Initialisation of OUTPUT system.
        !   - Reads output work file.
        !
        !     PARAMETERS          : 12
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNWRO  INTEGER       1     INPUT   Output work file
        !     LCH     CHA*(*)       1     INPUT   Name output work file
        !     LUREP   INTEGER       1     INPUT   Monitoring file
        !     num_output_files   INTEGER       1     INPUT   Number of output files
        !     num_output_variables_extra  INTEGER       1     INPUT   Number of extra output vars
        !     output_buffer_len  INTEGER       1     INPUT
        !     IOUTPS  INTEGER 7*num_output_files    OUTPUT   Output structure
        !                                            index 1 = start time
        !                                            index 2 = stop time
        !                                            index 3 = time step
        !                                            index 4 = number of vars
        !                                            index 5 = kind of output
        !                                            index 6 = format of output
        !                                            index 7 = initialize flag
        !     IOPOIN  INTEGER  num_output_variables_extra    OUTPUT   Pointer to DELWAQ array's
        !     OUNAM   CHAR*(*) num_output_variables_extra    OUTPUT   name of output variable
        !     OUSNM   CHAR*(*) num_output_variables_extra    OUTPUT   standard name of output variable
        !     OUUNI   CHAR*(*) num_output_variables_extra    OUTPUT   unit of output variable
        !     OUDSC   CHAR*(*) num_output_variables_extra    OUTPUT   description of output variable
        !     OSSNM   CHAR*(*) num_output_variables_extra    OUTPUT   standard name of substance
        !     OSUNI   CHAR*(*) num_output_variables_extra    OUTPUT   unit of substance
        !     OSDSC   CHAR*(*) num_output_variables_extra    OUTPUT   description of substance
        !     file_unit_list     INTEGER    *        INPUT   array with unit numbers
        !     file_name_list   CHAR*(*)   *        INPUT   filenames
        !     IERR    INTEGER       1    IN/OUT   cummulative error count
        !
        !     Declaration of arguments
        !
        use m_open_waq_files
        use timers
        use results

        integer(kind = int_wp) :: lunwro, lurep, num_output_files, num_output_variables_extra, output_buffer_len, num_substances_transported, &
                ierr, num_substances_total
        integer(kind = int_wp) :: ioutps(7, *), iopoin(*), file_unit_list(*)
        character*(*) lch, file_name_list(*)
        character*20  ounam(*)
        character*100 ousnm(*), sysnm(*)
        character*40  ouuni(*), syuni(*)
        character*60  oudsc(*), sydsc(*)

        ! local declarations
        integer(kind = int_wp), parameter :: luoff = 18
        integer(kind = int_wp), parameter :: luoff2 = 36
        integer(kind = int_wp) :: noutpd, nrvard, nbufmd
        real(kind = real_wp) :: versio

        integer(kind = int_wp) :: k, isrtou, ifi, idum

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("initialize_output", ithandl)

        ! read and check version number
        read (lunwro, err = 900, end = 900) versio

        ! read and check dimensions
        read (lunwro, err = 900, end = 900) noutpd, nrvard, nbufmd, ncopt
        if (noutpd /= num_output_files) then
            write (lurep, 2020) noutpd, num_output_files
            ierr = ierr + 1
        endif
        if (nrvard /= num_output_variables_extra) then
            write (lurep, 2030) nrvard, num_output_variables_extra
            ierr = ierr + 1
        endif
        if (nbufmd /= output_buffer_len) then
            write (lurep, 2040) nbufmd, output_buffer_len
            ierr = ierr + 1
        endif
        if (ierr > 0) goto 910

        read (lunwro, err = 900, end = 900) (ioutps(1, k), k = 1, num_output_files)
        read (lunwro, err = 900, end = 900) (ioutps(2, k), k = 1, num_output_files)
        read (lunwro, err = 900, end = 900) (ioutps(3, k), k = 1, num_output_files)
        read (lunwro, err = 900, end = 900) (ioutps(4, k), k = 1, num_output_files)
        read (lunwro, err = 900, end = 900) (ioutps(5, k), k = 1, num_output_files)
        read (lunwro, err = 900, end = 900) (ioutps(6, k), k = 1, num_output_files)
        if (num_output_variables_extra>0) then
            read (lunwro, err = 900, end = 900) (iopoin(k), k = 1, num_output_variables_extra)
            read (lunwro, err = 900, end = 900) (ounam (k), k = 1, num_output_variables_extra)
            read (lunwro, err = 900, end = 900) (ousnm (k), k = 1, num_output_variables_extra)
            read (lunwro, err = 900, end = 900) (ouuni (k), k = 1, num_output_variables_extra)
            read (lunwro, err = 900, end = 900) (oudsc (k), k = 1, num_output_variables_extra)
        endif
        if (num_substances_total>0) then
            read (lunwro, err = 900, end = 900) (sysnm (k), k = 1, num_substances_total)
            read (lunwro, err = 900, end = 900) (syuni (k), k = 1, num_substances_total)
            read (lunwro, err = 900, end = 900) (sydsc (k), k = 1, num_substances_total)
        endif

        ! Set initialize flag, open files: only on first subdomain
        DO K = 1, num_output_files
            ISRTOU = IOUTPS(5, K)
            IF (K <= 4) THEN
                IFI = K + LUOFF
            ELSEIF (K <= 7) THEN
                IFI = K + LUOFF2 - 4
            ELSE
                IFI = K + LUOFF2 - 2
            ENDIF

            ! Open the output-file in the correct way, depending on type of output
            IOUTPS(7, K) = 1
            IF (ISRTOU == IMON .OR. ISRTOU == IMO2 .OR. &
                    ISRTOU == IMO3 .OR. ISRTOU == IMO4) THEN

                ! Do not open the normal monitor file
                IF (K /= 1) THEN
                    CALL open_waq_files (file_unit_list(IFI), file_name_list(IFI), 19, 1, IDUM)
                ENDIF
            ELSEIF (ISRTOU == IDMP .OR. ISRTOU == IDM2) THEN
                CALL open_waq_files (file_unit_list(IFI), file_name_list(IFI), 20, 1, IDUM)
            ELSEIF (ISRTOU == IHIS .OR. ISRTOU == IHI2 .OR. &
                    ISRTOU == IHI3 .OR. ISRTOU == IHI4) THEN
                CALL open_waq_files (file_unit_list(IFI), file_name_list(IFI), 21, 1, IDUM)
            ELSEIF (ISRTOU == IMAP .OR. ISRTOU == IMA2) THEN
                CALL open_waq_files (file_unit_list(IFI), file_name_list(IFI), 22, 1, IDUM)
            ELSEIF (ISRTOU == IBAL .OR. ISRTOU == IBA2) THEN
                CALL open_waq_files (file_unit_list(IFI), file_name_list(IFI), 37, 1, IDUM)
            ENDIF
        end do

        if (timon) call timstop (ithandl)
        RETURN

        ! unsuccessful read
        900 CONTINUE
        WRITE (LUREP, 2050) LCH, LUNWRO
        IERR = IERR + 1

        910 CONTINUE
        if (timon) call timstop (ithandl)

        RETURN
        2020 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_output_files', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2030 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for num_output_variables_extra', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2040 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for output_buffer_len', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2050 FORMAT (' ERROR  : Reading output work file;', A, &
                /'          on unit number ', I3)

    end subroutine initialize_output
end module initialize_conditions
