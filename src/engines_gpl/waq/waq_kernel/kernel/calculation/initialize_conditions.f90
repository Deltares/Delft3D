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
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace

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

            nosss = noseg + nseg2                ! nseg2 are bed-volumes
            noqtt = noq + noq4
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
                DO I = 1, NUFIL
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
            IF (NPROC > 0) THEN
                CALL open_waq_files (file_unit_list(24), file_name_list(24), 24, 2, IERRD)
                CALL initialize_processes (file_unit_list(24), file_name_list(24), file_unit_list(19), NOTOT, NIPMSA, &
                        NPROC, NOLOC, NFLUX, NODEF, J(INSVA:), &
                        J(IIFLU:), J(IPVAR:), J(IPTYP:), A(IDEFA:), A(ISTOC:), &
                        C(IPRNA:), J(IIMOD:), IERR, IPBLOO, &
                        IOFFBL, NOSYS, NDSPX, NVELX, &
                        A(IDSTO:), A(IVSTO:), NDSPN, J(IDPNW:), NVELN, &
                        J(IVPNW:), NLOCX, J(IPGRD:), J(IPNDT:), NOVAR, &
                        J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), J(IVTAG:), &
                        J(IVAGG:), nrref, J(ipror:), j(iprvpt:))
                CLOSE (file_unit_list(24))
            ENDIF
            !
            !     Set variable "structure"
            !
            CALL initialize_variables (file_unit_list(19), NOCONS, NOPA, NOFUN, NOSFUN, &
                    NOSYS, NOTOT, NODISP, NOVELO, NODEF, &
                    NOLOC, NDSPX, NVELX, NLOCX, NFLUX, &
                    NOPRED, NOVAR, J(IVARR:), J(IVIDX:), J(IVTDA:), &
                    J(IVDAG:), J(IVTAG:), J(IVAGG:), NOGRID, J(IVSET:))
            !
            !     initialisation of OUTPUT subsytem
            !

            IF (NOUTP > 0) THEN
                CALL open_waq_files (file_unit_list(25), file_name_list(25), 25, 2, IERRD)
                CALL initialize_output (file_unit_list(25), file_name_list(25), file_unit_list(19), NOUTP, NRVART, &
                        NBUFMX, J(IIOUT:), J(IIOPO:), C(IONAM), C(IOSNM), &
                        C(IOUNI), C(IODSC), NOTOT, C(ISSNM), C(ISUNI), &
                        C(ISDSC), file_unit_list, file_name_list, IERR)
                CLOSE (file_unit_list(25))
            ENDIF
            !
            !         initialisation of the grid layout
            !
            IF (NX * NY > 0) THEN
                CALL open_waq_files (file_unit_list(6), file_name_list(6), 6, 2, IERRD)
                READ  (file_unit_list(6)) (J(K), K = IGRID, IGRID + NX * NY - 1)
                CLOSE (file_unit_list(6))
            ENDIF
            !
            !         initialisation of exchange pointers
            !
            CALL open_waq_files (file_unit_list(8), file_name_list(8), 8, 2 + ftype(8), IERRD)

            if (nmax * mmax > 0) then

                !        read grid, make pointer table

                i1 = ilgra - 1
                read  (file_unit_list(8)) nmax2, mmax2, noseg2, kmax2, noq1d, noq2d, noq3d
                read  (file_unit_list(8)) (j(i1 + k), k = 1, mmax * nmax)
                i2 = ikbnd - 1

                call create_pointer_table(nmax, mmax, kmax, noseg, nobnd, &
                        noq, noq1, noq2, j(ilgra:), j(ixpnt:), &
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
            call segcol(nosss, noq1, noq2, noq3, noq4, &
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
                if (idummy /= notot) then
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
                do i = 1, notot
                    read (file_unit_list(18)) finam(1:20)
                enddo
            endif
            read  (file_unit_list(18), iostat = ierrio)                   & ! like the .ini, the .res and .wrk file
                    idummy, (a(k), k = iconc, iconc + notot * nosss - 1)
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
            call delpar00 (file_unit_list(19), file_name_list(45), noseg, noq, a(ivol:), a(iflow:), &
                    nosfun, c(isfna:), a(isfun:))

            ! New bottomlayer processing
            IF (NOQ4 > 0) &
                    CALL expands_vol_area_for_bottom_cells (file_unit_list, NOSEG, NSEG2, NOLAY, NOGRID, &
                            NOQ, NOQ4, J(IGREF:), J(IGSEG:), NOCONS, &
                            NOPA, NOFUN, NOSFUN, A(ICONS:), C(ICNAM:), &
                            A(IPARM:), C(IPNAM:), A(IFUNC:), C(IFNAM:), A(ISFUN:), &
                            C(ISFNA:), J(IXPNT:), A(IVOL:), A(IAREA:), A(IFLOW:), &
                            A(ILENG:))
            !

            IF (INTSRT == 6 .OR. INTSRT == 7) THEN
                NOSUBz = NOTOT
            ELSE
                NOSUBz = NOSYS
            ENDIF
            call copy_real_array_elements   (A(IBSET:), A(IBOUN:), NOBND * NOSUBz)
            call copy_real_array_elements   (A(IBSET:), A(IBSAV:), NOBND * NOSUBz)
            call initialize_real_array   (A(IDERV:), NOTOT * NOSSS)
            call initialize_real_array   (A(IMAS2:), NOTOT * 5)
            call initialize_real_array   (A(IWDMP:), NOTOT * NOWST * 2)
            IF (MOD(INTOPT, 16) > 7) THEN
                call initialize_real_array(A(IDMPQ:), NOSYS * NDMPQ * 2)
                call initialize_real_array(A(IDMPS:), NOTOT * NDMPS * 3)
                call initialize_real_array(A(ISMAS:), NOTOT * NDMPAR * 6)
                call initialize_real_array(A(IFLXI:), NDMPAR * NFLUX)
                call initialize_real_array(A(IFLXD:), NDMPS * NFLUX)
                call initialize_real_array(A(ITRRA:), NOSYS * NORAAI)
            ENDIF

            !         make start masses for dynamic and iterative computation

            if (intsrt ==  6 .or. intsrt ==  7 .or. &
                    intsrt == 17 .or. intsrt == 18) goto 40

            !         initial conditions coflowing substances

            do iseg = 0, nosss - 1
                volume = a(ivol + iseg)
                do i1 = iseg * notot, iseg * notot + nosys - 1
                    a(imass + i1) = a(iconc + i1) * volume
                enddo
            enddo

            !         initial conditions passive substances

            if (nosys /= notot) then                         ! if there are bed-substances
                indx = index_in_array('SURF      ', buffer%create_strings_20_array(ipnam, nopa))
                if (indx > 0) then                           ! and if SURF is found
                    call inact (nosss, nosys, notot, a(iconc:), a(imass:), &
                            nopa, indx, a(iparm:), c(imnam + 113), propor, &
                            .true.)
                else                                     ! routine inact is at end of this file !
                    indx = index_in_array('SURF      ', buffer%create_strings_20_array(isfna, nosfun))
                    if (indx > 0) then                        ! and if SURF is found
                        call inact (nosss, nosys, notot, a(iconc:), a(imass:), &
                                nosfun, indx, a(isfun:), c(imnam + 113), propor, &
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
            call zlayer (noseg, nosss, nosys, notot, nolay, &
                    a(ivol:), noq1 + noq2, noq, a(iarea:), nocons, &
                    c(icnam:), a(icons:), nopa, c(ipnam:), a(iparm:), &
                    nosfun, c(isfna:), a(isfun:), a(iconc:), a(imass:), &
                    j(iknmr:), iknmkv, j(ixpnt:))


            !     temporary for closure error

            40 INDX = index_in_array('CLOSE_ERR ', buffer%create_strings_20_array(ICNAM, NOCONS))
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

    subroutine inact (noseg, nosys, notot, conc, amass, &
            nopa, iparm, parm, string, propor, &
            direct)
        !>\File
        !>         Makes mass/gridcell from mass/m2 for the passive substances

        implicit none

        integer(kind = int_wp), intent(in) :: noseg              !< number of computational volumes
        integer(kind = int_wp), intent(in) :: nosys              !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot              !< total number of substances
        real(kind = real_wp), intent(inout) :: conc (notot, noseg) !< the concentration values
        real(kind = real_wp), intent(out) :: amass(notot, noseg) !< the mass values
        integer(kind = int_wp), intent(in) :: nopa               !< number of parameters or segment functions
        integer(kind = int_wp), intent(in) :: iparm              !< selected parameter
        real(kind = real_wp), intent(in) :: parm (nopa * noseg) !< parameter or segment function array
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
            indx = (iparm - 1) * noseg + 1       ! segment function
        endif
        do iseg = 1, noseg
            surf = parm(indx)
            do isys = nosys + 1, notot
                if (propor) then                                ! input / m2
                    amass(isys, iseg) = conc(isys, iseg) * surf
                else                                              ! input / gridcell
                    amass(isys, iseg) = conc(isys, iseg)
                    conc (isys, iseg) = conc(isys, iseg) / surf      ! conc  / m2
                endif
            enddo
            if (direct) then
                indx = indx + nopa
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
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics


        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     file_unit_list     INTEGER       *     INPUT   logical unitnumbers
        !     MODID   CHAR*40       4     OUTPUT  Model and run-ID
        !     SYSID   CHAR*20   NOTOT     OUTPUT  Systems ID
        !     IDUMP   INTEGER  NODUMP     OUTPUT  Dump segment numbers
        !     DUMPID  CHAR*20  NODUMP     OUTPUT  Dump-segment ID
        !     IDPNT   INTEGER   NOSYS     OUTPUT  Pointers to dispersion array
        !     IVPNT   INTEGER   NOSYS     OUTPUT  Pointers to velocity array
        !     DISP    REAL          3     OUTPUT  dispersion in 3 directions
        !     IBPNT   INTEGER  4*NOBND    OUTPUT  1,* = timelag
        !                                         2,* = flow pointer
        !                                         3,* = segment pointer
        !                                         4,* = time on timelag
        !     BNDID   CHAR*20   NOBND     OUTPUT  Open boundary ID's
        !     BNDNAM  CHAR*40   NOBND     OUTPUT  Open boundary names
        !     BNDTYP  CHAR*20   NOBND     OUTPUT  Open boundary types
        !     INWTYP  INTEGER       *     OUTPUT  Types of items
        !     IWASTE  INTEGER   NOWST     OUTPUT  waste load segment numbers
        integer(kind = int_wp), intent(out) :: iwsknd(*) !  wasteload processing
        !     WASTID  CHAR*20   NOWST     OUTPUT  Waste location ID
        !     WSTNAM  CHAR*40   NOWST     OUTPUT  Waste location names
        !     WSTTYP  CHAR*20   NOWST     OUTPUT  Waste location types
        !     ALENG   REAL        3       OUTPUT  Lengthes in 3 directions
        !     CONST   REAL     NOCONS     OUTPUT  value of constants
        !     PARAM   REAL    NOPA,NOSEG  OUTPUT  value of parameters
        !     NRFTOT  INTEGER  NOITEM     OUTPUT  file lengthes per item
        !     NRHARM  INTEGER  NOITEM     OUTPUT  nr of harmonics per item
        !     CONAME  CHAR*20  NOCONS     OUTPUT  Constant names
        !     PANAME  CHAR*20  NOPA       OUTPUT  Parameter names
        !     FUNAME  CHAR*20  NOFUN      OUTPUT  Function names
        !     SFNAME  CHAR*20  NOSFUN     OUTPUT  Segment function names
        !     DINAME  CHAR*20  NODISP     OUTPUT  Dispersion array names
        !     VENAME  CHAR*20  NOVELO     OUTPUT  Velocity array names
        !     DANAM   CHAR*20  NDMPAR     OUTPUT  Dump-area    ID
        !     IPDMP   INTEGER       *     OUTPUT  pointer structure dump area's
        !     IQDMP   INTEGER       *     OUTPUT  Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     OUTPUT  Segment to dumped segment pointer
        !     RANAM   CHAR*20       *     OUTPUT  Raaien names
        !     IORAAI  INTEGER       *     OUTPUT  option output raaien
        !     NQRAAI  INTEGER       *     OUTPUT  number of exch. per raai
        !     IQRAAI  INTEGER       *     OUTPUT  exchange nunbers raaien
        !
        !
        !     IN COMMON BLOCK     :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSEG   INTEGER       1     INPUT   Number of segments
        !     NOSYS   INTEGER       1     INPUT   Number of active systems
        !     NOTOT   INTEGER       1     INPUT   Number of systems
        !     NODISP  INTEGER       1     INPUT   Number of dispersion array's
        !     NOVELO  INTEGER       1     INPUT   Number of velocity array's
        !     NOQ     INTEGER       1     INPUT   total number of exchanges
        !     NODUMP  INTEGER       1     INPUT   Number of dump segments
        !     NOBND   INTEGER       1     INPUT   Number of open boundaries
        !     NOBTYP  INTEGER       1     INPUT   Number of boundary types
        !     NOWST   INTEGER       1     INPUT   Number of load locations
        !     NOWTYP  INTEGER       1     INPUT   Number of waste load types
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOITEM  INTEGER       1     INPUT   Number possible functions
        !     NDMPAR  INTEGER       1     INPUT   Number of dump area's
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     NTDMPS  INTEGER       1     INPUT   total number segments in dump area
        !     NORAAI  INTEGER       1     INPUT   number of raaien
        !     NTRAAQ  INTEGER       1     INPUT   total number of exch. in raaien

        INTEGER(kind = int_wp) :: IPDMP(*), IQDMP(*), ISDMP (*), IORAAI(*), &
                NQRAAI(*), IQRAAI(*), GRDNOS(*), GRDREF(*), &
                IDUMP (*), IDPNT (*), IVPNT (*), IBPNT (4, *), &
                IWASTE(*), NRFTOT(*), NRHARM(*), file_unit_list   (*), &
                IKNMRK(*), INWTYP(*)
        INTEGER(kind = int_wp) :: GRDSEG(NOSEG + NSEG2, NOGRID)
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
        NOQTT = NOQ + NOQ4
        NOSSS = NOSEG + NSEG2
        IIN = file_unit_list(2)
        READ (IIN, END = 40, ERR = 40)  MODID (1), MODID(2)
        READ (IIN, END = 40, ERR = 40)  MODID (3), MODID(4)
        READ (IIN, END = 40, ERR = 40) (SYSID (K), K = 1, NOTOT)
        IF (NODUMP > 0) &
                READ (IIN, END = 40, ERR = 40) (IDUMP(K), DUMPID(K), K = 1, NODUMP)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DANAM(K), K = 1, NDMPAR)
        IF (NDMPAR > 0) &
                READ (IIN, END = 40, ERR = 40) (DMPBAL(K), K = 1, NDMPAR)
        IF (NORAAI > 0) &
                READ (IIN, END = 40, ERR = 40) (RANAM(K), K = 1, NORAAI)

        ! sub-grid
        DO IGRID = 1, NOGRID
            READ (IIN, END = 40, ERR = 40)  GRDNOS(IGRID), GRDREF(IGRID), &
                    (GRDSEG(ISEG, IGRID), ISEG = 1, NOSSS)
        ENDDO
        !     the grid structures
        DO IGRID = 1, NOGRID
            ierror = aGrid%read(iin, nosss)
            if (ierror /= 0) goto 40
            i_grid = GridPs%add(aGrid)
        ENDDO
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, NOTOT)
        READ (IIN, END = 40, ERR = 40) (IDUMMY, ISYS = 1, NOTOT)
        READ (IIN, END = 40, ERR = 40) (IKNMRK(K), K = 1, NOSSS)
        IF (NODISP > 0) &
                READ (IIN, END = 40, ERR = 40) (DINAME(K), K = 1, NODISP)
        IF (NOVELO > 0) &
                READ (IIN, END = 40, ERR = 40) (VENAME(K), K = 1, NOVELO)
        READ (IIN, END = 40, ERR = 40) (IDPNT (K), K = 1, NOSYS)
        READ (IIN, END = 40, ERR = 40) (IVPNT (K), K = 1, NOSYS)
        IF (NOBND  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (IBPNT (2, K), K = 1, NOBND)
            READ (IIN, END = 40, ERR = 40) (IBPNT (3, K), K = 1, NOBND)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IPDMP(K), K = 1, NDMPAR + NTDMPQ)
            IX = NDMPAR + NTDMPQ
            READ (IIN, END = 40, ERR = 40)  (IPDMP(IX + K), K = 1, NDMPAR + NTDMPS)
        ENDIF
        IF (NORAAI > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IORAAI(K), K = 1, NORAAI)
            READ (IIN, END = 40, ERR = 40)  (NQRAAI(K), K = 1, NORAAI)
            READ (IIN, END = 40, ERR = 40)  (IQRAAI(K), K = 1, NTRAAQ)
        ENDIF
        IF (NORAAI > 0 .OR. NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (IQDMP(K), K = 1, NOQTT)
        ENDIF
        IF (NDMPAR > 0) THEN
            READ (IIN, END = 40, ERR = 40)  (ISDMP(K), K = 1, NOSSS)
        ENDIF
        READ (IIN, END = 40, ERR = 40) IDUMMY, (DISP  (K), K = 1, 3)
        READ (IIN, END = 40, ERR = 40) IDUMMY, (ALENG (K), K = 1, 3)
        IF (NOBND  > 0) THEN
            DO I = 1, NOBND
                READ (IIN, END = 40, ERR = 40) BNDID(I), BNDNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (BNDTYP(K), K = 1, NOBTYP)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, NOBND)
            IT = IT + NOBND
            !          read time lags
            READ (IIN, END = 40, ERR = 40) (IBPNT(1, K), K = 1, NOBND)
        ENDIF
        IF (NOWST  > 0) THEN
            DO I = 1, NOWST
                READ (IIN, END = 40, ERR = 40) IWASTE(I), iwsknd(i), &
                        WASTID(I), WSTNAM(I)
            end do
            READ (IIN, END = 40, ERR = 40) (WSTTYP(K), K = 1, NOWTYP)
            READ (IIN, END = 40, ERR = 40) (INWTYP(K + IT), K = 1, NOWST)
            IT = IT + NOWST
        ENDIF
        IF (NOCONS > 0) THEN
            READ (IIN, END = 40, ERR = 40) (CONAME(K), K = 1, NOCONS)
        ENDIF
        IF (NOPA   > 0) THEN
            READ (IIN, END = 40, ERR = 40) (PANAME(K), K = 1, NOPA)
        ENDIF
        IF (NOFUN  > 0) THEN
            READ (IIN, END = 40, ERR = 40) (FUNAME(K), K = 1, NOFUN)
        ENDIF
        IF (NOSFUN > 0) THEN
            READ (IIN, END = 40, ERR = 40) (SFNAME(K), K = 1, NOSFUN)
        ENDIF
        !
        !     Time function info
        !
        READ (IIN, END = 40, ERR = 40) (NRFTOT(K), K = 1, NOITEM)
        READ (IIN, END = 40, ERR = 40) (NRHARM(K), K = 1, NOITEM)
        !
        !         boundary timings greater then timelag
        !
        DO I = 1, NOBND
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

    SUBROUTINE initialize_processes(LUNWRP, LCH, LUREP, NOTOT, NIPMSA, &
            NPROC, NOLOC, NFLUX, NODEF, PRVNIO, &
            IFLUX, PRVVAR, PRVTYP, DEFAUL, STOCHI, &
            PRONAM, IMODU, IERR, IPBLOO, &
            IOFFBL, NOSYS, NDSPX, NVELX, &
            DSTO, VSTO, NDSPN, IDPNW, NVELN, &
            IVPNW, NLOCX, PROGRD, PRONDT, NOVAR, &
            VARARR, VARIDX, VARTDA, VARDAG, VARTAG, &
            VARAGG, nrref, proref, prvpnt)
        ! Initialisation of PROCES system .
        !
        !     FILES               : LUNWRP, Proces work file
        !                           LUREP , Monitoring file
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNWRP  INTEGER       1     INPUT   Proces work file
        !     LCH     CHA*(*)       1     INPUT   Name proces work file
        !     LUREP   INTEGER       1     INPUT   Monitoring file
        !     NOTOT   INTEGER       1     INPUT   Number of substances
        !     NIPMSA  INTEGER       1     INPUT   Length IPMSA
        !     NPROC   INTEGER       1     INPUT   Number of called processes
        !     NOLOC   INTEGER       1     INPUT   Number of local proces params
        !     NFLUX   INTEGER       1     INPUT   total number of fluxes
        !     NODEF   INTEGER       1     INPUT   Number of used defaults
        !     PRVNIO  INTEGER       *     OUTPUT  Number of variables per proces
        !     IFLUX   INTEGER       *     OUTPUT  Pointer in FLUX per proces inst.
        !     IPMSA   INTEGER       *     OUTPUT  Pointer in SSA per proces inst.
        !     IPSSA   INTEGER       *     OUTPUT  Pointer to SSA per proces inst.
        !     DEFAUL  REAL          *     OUTPUT  Default proces parameters
        !     STOCHI  REAL          *     OUTPUT  Proces stochiometry
        !     PRONAM  CHA*(*)       *     OUTPUT  Name of called module
        !     IMODU   INTEGER       *     OUTPUT  Module number proces
        !     IERR    INTEGER       1     IN/OUT  Error count
        !     IPBLOO  INTEGER       1     INPUT   Number of Bloom module (if >0)
        !     IOFFBL  INTEGER       1     INPUT   Offset in IPMSA for Bloom
        !     NOSYS   INTEGER       1     INPUT   Number of active substances
        !     NDSPX   INTEGER       1     INPUT   Number of extra dispersion array
        !     NVELX   INTEGER       1     INPUT   Number of extra velocity array
        !     DSTO    INTEGER NOSYS,*     OUTPUT  dispersion stochi matrix
        !     VSTO    INTEGER NOSYS,*     OUTPUT  velocity stochi matrix
        !     NDSPN   INTEGER       1     INPUT   Number of new dispersion array
        !     IDPNW   INTEGER   NOSYS     OUTPUT  Pointers to new dispersion array
        !     NVELN   INTEGER       1     INPUT   Number of new velocity array
        !     IVPNW   INTEGER   NOSYS     OUTPUT  Pointers to new velocity array
        !     PROGRD  INTEGER   NPROC     OUTPUT  Grid number for process
        !     PRONDT  INTEGER   NPROC     OUTPUT  Fractional step for process

        use timers
        use process_registration

        INTEGER(kind = int_wp) :: LUNWRP, LUREP, NOTOT, NIPMSA, NPROC, &
                NOLOC, NFLUX, NODEF, IPBLOO, &
                IOFFBL, NOSYS, NDSPX, NVELX, &
                NDSPN, NVELN, NOVAR, nrref
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
                NOTOTD, IOFF, NOSYSD, NDSPXD, NVELXD, &
                NLOCXD, NDSPND, NVELND, NOVARD, nrrefD
        REAL(kind = real_wp) :: VERSIO

        integer(kind = int_wp) :: k, ierr, nlocx, iproc, ifracs, ipdgrd

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
                NOSYSD, NDSPXD, NVELXD, &
                NLOCXD, NDSPND, NVELND, &
                NOVARD, nrrefD
        IF (NIPMSD /= NIPMSA) THEN
            WRITE (LUREP, 2020) NIPMSD, NIPMSA
            IERR = IERR + 1
        ENDIF
        IF (NPROCD /= NPROC) THEN
            WRITE (LUREP, 2030) NPROCD, NPROC
            IERR = IERR + 1
        ENDIF
        IF (NFLUXD /= NFLUX) THEN
            WRITE (LUREP, 2040) NFLUXD, NFLUX
            IERR = IERR + 1
        ENDIF
        IF (NOLOCD /= NOLOC) THEN
            WRITE (LUREP, 2050) NOLOCD, NOLOC
            IERR = IERR + 1
        ENDIF
        IF (NODEFD /= NODEF) THEN
            WRITE (LUREP, 2060) NODEFD, NODEF
            IERR = IERR + 1
        ENDIF
        IF (NOTOTD /= NOTOT) THEN
            WRITE (LUREP, 2070) NOTOTD, NOTOT
            IERR = IERR + 1
        ENDIF
        IF (NOSYSD /= NOSYS) THEN
            WRITE (LUREP, 2120) NOSYSD, NOSYS
            IERR = IERR + 1
        ENDIF
        IF (NDSPXD /= NDSPX) THEN
            WRITE (LUREP, 2130) NDSPXD, NDSPX
            IERR = IERR + 1
        ENDIF
        IF (NVELXD /= NVELX) THEN
            WRITE (LUREP, 2140) NVELXD, NVELX
            IERR = IERR + 1
        ENDIF
        IF (NLOCXD /= NLOCX) THEN
            WRITE (LUREP, 2150) NLOCXD, NLOCX
            IERR = IERR + 1
        ENDIF
        IF (NDSPND /= NDSPN) THEN
            WRITE (LUREP, 2160) NDSPND, NDSPN
            IERR = IERR + 1
        ENDIF
        IF (NVELND /= NVELN) THEN
            WRITE (LUREP, 2170) NVELND, NVELN
            IERR = IERR + 1
        ENDIF
        IF (NOVARD /= NOVAR) THEN
            WRITE (LUREP, 2190) NOVARD, NOVAR
            IERR = IERR + 1
        ENDIF
        IF (nrrefD /= nrref) THEN
            WRITE (LUREP, 2200) nrrefd, nrref
            IERR = IERR + 1
        ENDIF
        IF (IERR > 0) GOTO 910
        !
        READ (LUNWRP, ERR = 900, END = 900) (PRVNIO(K), K = 1, NPROC)
        READ (LUNWRP, ERR = 900, END = 900) (IFLUX(K), K = 1, NPROC)
        READ (LUNWRP, ERR = 900, END = 900) (PRVVAR(K), K = 1, NIPMSA)
        READ (LUNWRP, ERR = 900, END = 900) (PRVTYP(K), K = 1, NIPMSA)
        READ (LUNWRP, ERR = 900, END = 900) (DEFAUL(K), K = 1, NODEF)
        READ (LUNWRP, ERR = 900, END = 900) (STOCHI(K), K = 1, NOTOT * NFLUX)
        READ (LUNWRP, ERR = 900, END = 900) (DSTO(K), K = 1, NOSYS * NDSPX)
        READ (LUNWRP, ERR = 900, END = 900) (VSTO(K), K = 1, NOSYS * NVELX)
        IF (NDSPN > 0) THEN
            READ (LUNWRP, ERR = 900, END = 900) (IDPNW(K), K = 1, NOSYS)
        ENDIF
        IF (NVELN > 0) THEN
            READ (LUNWRP, ERR = 900, END = 900) (IVPNW(K), K = 1, NOSYS)
        ENDIF
        READ (LUNWRP, ERR = 900, END = 900) (PRONAM(K), K = 1, NPROC)
        READ (LUNWRP, ERR = 900, END = 900) (PROGRD(K), K = 1, NPROC)
        READ (LUNWRP, ERR = 900, END = 900) (PRONDT(K), K = 1, NPROC)
        READ (LUNWRP, ERR = 900, END = 900) (VARARR(K), K = 1, NOVAR)
        READ (LUNWRP, ERR = 900, END = 900) (VARIDX(K), K = 1, NOVAR)
        READ (LUNWRP, ERR = 900, END = 900) (VARTDA(K), K = 1, NOVAR)
        READ (LUNWRP, ERR = 900, END = 900) (VARDAG(K), K = 1, NOVAR)
        READ (LUNWRP, ERR = 900, END = 900) (VARTAG(K), K = 1, NOVAR)
        READ (LUNWRP, ERR = 900, END = 900) (VARAGG(K), K = 1, NOVAR)
        read (lunwrp, err = 900, end = 900) (proref(k), k = 1, nproc * nrref)
        k = 1
        do iproc = 1, nproc
            prvpnt(iproc) = k
            k = k + prvnio(iproc)
        enddo
        !
        !     Set module numbers
        !
        DO K = 1, NPROC
            CALL PRONRS (PRONAM(K), IMODU(K))
        end do
        !
        !     Report on process decomposition
        !
        IFRACS = 0
        IPDGRD = 0
        DO K = 1, NPROC
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
            DO K = 1, NPROC
                WRITE(LUREP, 3000) PRONAM(K), PROGRD(K), PRONDT(K)
            ENDDO
        ENDIF
        !
        !     Check for Bloom connection
        !
        IPBLOO = 0
        IOFFBL = 0
        IOFF = 1
        DO K = 1, NPROC
            IF (PRONAM(K)(1:6) == 'D40BLO') THEN
                IPBLOO = K
                IOFFBL = IOFF
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
                /'          DELWAQ boot file for NIPMSA', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2030 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NPROC ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2040 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NFLUX ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2050 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NOLOC ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2060 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NODEF ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2070 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NOTOT ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2090 FORMAT (' ERROR  : Reading proces work file;', A, &
                /'          on unit number ', I3)
        2100 FORMAT (' MESSAGE: Bloom fractional step switched on')
        2120 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NOSYS ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2130 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NDSPX ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2140 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NVELX ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2150 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NLOCX ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2160 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NDSPN ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2170 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NVELN ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2190 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NOVAR ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        2200 FORMAT (' ERROR  : Proces work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NRREF ', &
                /'          ', I6, ' in proces,', I6, ' in boot file.')
        3000 FORMAT (/' MODULE :', A, ' on grid ', I3, ', timestep multiplier:', I3)
        3010 FORMAT (/' No process decomposition active')
        3020 FORMAT (/' Process decomposition active')
        !
    end subroutine initialize_processes

    SUBROUTINE initialize_output(LUNWRO, LCH, LUREP, NOUTP, NRVART, &
            NBUFMX, IOUTPS, IOPOIN, OUNAM, OUSNM, &
            OUUNI, OUDSC, NOTOT, SYSNM, SYUNI, &
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
        !     NOUTP   INTEGER       1     INPUT   Number of output files
        !     NRVART  INTEGER       1     INPUT   Number of extra output vars
        !     NBUFMX  INTEGER       1     INPUT   length of output buffer
        !     IOUTPS  INTEGER 7*NOUTP    OUTPUT   Output structure
        !                                            index 1 = start time
        !                                            index 2 = stop time
        !                                            index 3 = time step
        !                                            index 4 = number of vars
        !                                            index 5 = kind of output
        !                                            index 6 = format of output
        !                                            index 7 = initialize flag
        !     IOPOIN  INTEGER  NRVART    OUTPUT   Pointer to DELWAQ array's
        !     OUNAM   CHAR*(*) NRVART    OUTPUT   name of output variable
        !     OUSNM   CHAR*(*) NRVART    OUTPUT   standard name of output variable
        !     OUUNI   CHAR*(*) NRVART    OUTPUT   unit of output variable
        !     OUDSC   CHAR*(*) NRVART    OUTPUT   description of output variable
        !     OSSNM   CHAR*(*) NRVART    OUTPUT   standard name of substance
        !     OSUNI   CHAR*(*) NRVART    OUTPUT   unit of substance
        !     OSDSC   CHAR*(*) NRVART    OUTPUT   description of substance
        !     file_unit_list     INTEGER    *        INPUT   array with unit numbers
        !     file_name_list   CHAR*(*)   *        INPUT   filenames
        !     IERR    INTEGER       1    IN/OUT   cummulative error count
        !
        !     Declaration of arguments
        !
        use m_open_waq_files
        use timers
        use results

        integer(kind = int_wp) :: lunwro, lurep, noutp, nrvart, nbufmx, nosys, &
                ierr, notot
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
        if (noutpd /= noutp) then
            write (lurep, 2020) noutpd, noutp
            ierr = ierr + 1
        endif
        if (nrvard /= nrvart) then
            write (lurep, 2030) nrvard, nrvart
            ierr = ierr + 1
        endif
        if (nbufmd /= nbufmx) then
            write (lurep, 2040) nbufmd, nbufmx
            ierr = ierr + 1
        endif
        if (ierr > 0) goto 910

        read (lunwro, err = 900, end = 900) (ioutps(1, k), k = 1, noutp)
        read (lunwro, err = 900, end = 900) (ioutps(2, k), k = 1, noutp)
        read (lunwro, err = 900, end = 900) (ioutps(3, k), k = 1, noutp)
        read (lunwro, err = 900, end = 900) (ioutps(4, k), k = 1, noutp)
        read (lunwro, err = 900, end = 900) (ioutps(5, k), k = 1, noutp)
        read (lunwro, err = 900, end = 900) (ioutps(6, k), k = 1, noutp)
        if (nrvart>0) then
            read (lunwro, err = 900, end = 900) (iopoin(k), k = 1, nrvart)
            read (lunwro, err = 900, end = 900) (ounam (k), k = 1, nrvart)
            read (lunwro, err = 900, end = 900) (ousnm (k), k = 1, nrvart)
            read (lunwro, err = 900, end = 900) (ouuni (k), k = 1, nrvart)
            read (lunwro, err = 900, end = 900) (oudsc (k), k = 1, nrvart)
        endif
        if (notot>0) then
            read (lunwro, err = 900, end = 900) (sysnm (k), k = 1, notot)
            read (lunwro, err = 900, end = 900) (syuni (k), k = 1, notot)
            read (lunwro, err = 900, end = 900) (sydsc (k), k = 1, notot)
        endif

        ! Set initialize flag, open files: only on first subdomain
        DO K = 1, NOUTP
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
                /'          DELWAQ boot file for NOUTP', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2030 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NRVART', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2040 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NBUFMX', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2050 FORMAT (' ERROR  : Reading output work file;', A, &
                /'          on unit number ', I3)

    end subroutine initialize_output
end module initialize_conditions
