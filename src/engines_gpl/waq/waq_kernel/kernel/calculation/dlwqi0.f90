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

module dlwqi0_mod
    use m_waq_precision
    use m_string_utils
    use m_zlayer
    use m_segcol
    use m_dlwqtd
    use m_dlwqt0
    use m_dlwqiv
    use m_dlwqip
    use m_dlwqio
    use m_dlwqi2
    use m_delpar00
    use m_chknmr
    use m_array_manipulation, only : initialize_real_array, copy_real_array_elements, create_pointer_table
    use m_srstop
    use m_open_waq_files

contains
    subroutine dlwqi0 (buffer, nlun, imaxa, &
            imaxi, imaxc, ipage, lun, lchar, &
            filtype, gridps, dlwqd, ierr)

        !          Deltares Software Centre

        !>\File
        !>                          Initializes all start conditions for simulation
        !>
        !>                          Performs:
        !>                             - calls SPACE to allocate space for all arrays
        !>                             - calls DLWQI2 to initialize all fixed conditions
        !>                             - calls DLWQIP to initialize all processes subsystem
        !>                             - calls DLWQIV for unclear reasons
        !>                             - calls DLWQIO to initialize the output system
        !>                             - imports all grid informations and exchange tables
        !>                             - calls DLWQT0 to initialize all time dependent variables
        !>                             - calls DLWQTD to initialize the water bed layers
        !>                             - imports initial conditions


        !     LOGICAL UNITNUMBERS : LUN( 2) - system intermediate file
        !                           LUN(19) - monitoring output file
        !
        !     SUBROUTINES CALLED  : SPACE , initialises common blocks
        !                           DLWQI2, initialises fixed conditions
        !                           DLWQIP, initialises proces system
        !                           DLWQIO, initialises output system
        !                           DLWQT0, sets time functions
        !                           open_waq_files, opens files
        !                           MOVE  , copy's arrays
        !                           ZERO  , zeros an real arrays
        !
        use m_dhisys
        use dlwqgrid_mod
        use memory_mangement
        use delwaq2_data
        use timers
        use workspace
        use string_module  ! string manipulation tools
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace

        !     Parameters          :

        !     kind           function         name            description
        type(waq_data_buffer), intent(inout) :: buffer        !< System total array space
        integer(kind = int_wp), intent(in) :: nlun          !< Number of files
        integer(kind = int_wp), intent(inout) :: imaxa         !< dimension   A-array
        integer(kind = int_wp), intent(inout) :: imaxi         !< dimension   J-array
        integer(kind = int_wp), intent(inout) :: imaxc         !< dimension   C-array
        integer(kind = int_wp), intent(in) :: ipage         !< pagelength of the output file
        integer(kind = int_wp), intent(inout) :: lun    (nlun) !< array with unit numbers
        character(*), intent(in) :: lchar  (nlun) !< filenames
        integer(kind = int_wp), intent(in) :: filtype(nlun) !< type of file
        type(gridpointercoll), intent(out) :: gridps        !< collection off all grid definitions
        type(delwaq_data), intent(inout) :: dlwqd         !< derived type for persistent storage
        integer(kind = int_wp), intent(inout) :: ierr          !< error count

        !
        !     Local declaration
        !
        REAL(kind = real_wp) :: RDUMMY(1)
        LOGICAL       LDUMMY, UPDATR
        CHARACTER*200 FINAM
        INTEGER(kind = int_wp) :: SENDBUF(3)
        CHARACTER*4   cext                          ! inital conditions file extention

        INTEGER(kind = int_wp) :: IERRIO, new_lun

        LOGICAL       propor
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwqi0", ithandl)

        !         initialise the system

        ftype = filtype
        CALL SPACE  (LUN(19), .TRUE., buffer%rbuf, buffer%ibuf, buffer%chbuf, &
                IMAXA, IMAXI, IMAXC)

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
            !
            !     copy common to (possible) shared array to share these values with
            !     other processes (domain decomposition)
            !
            CALL DHISYS (J(ISYSI:), J(ISYSN:))
            !
            J(ILP) = IPAGE
            J(ILP + 1) = 10
            J(ILP + 4) = IPAGE
            J(ILP + 5) = 20

            !     number of all segments - including segments at the bed

            nosss = noseg + nseg2                ! nseg2 are bed-volumes
            noqtt = noq + noq4
            !
            !         initialisation of info from the system file
            !
            CALL open_waq_files (LUN(2), LCHAR(2), 2, 2, IERRD)
            CALL DLWQI2 (LUN, C(IMNAM), C(ISNAM), J(IDUMP:), C(IDNAM), &
                    J(IDPNT:), J(IVPNT:), A(IDISP:), J(IBPNT:), C(IBNID), &
                    C(IBNAM), C(IBTYP), J(INTYP:), J(IWAST:), iwstkind, &
                    C(IWSID), C(IWNAM), C(IWTYP), A(ILENG:), A(ICONS:), &
                    A(IPARM:), J(INRFT:), J(INRH2:), C(ICNAM), C(IPNAM), &
                    C(IFNAM), C(ISFNA), C(IDINA), C(IVNAM), J(IKNMR:), &
                    C(IDANA), J(IPDMP:), J(IQDMP:), J(ISDMP:), C(IRNAM), &
                    J(IORAA:), J(NQRAA:), J(IQRAA:), J(IGNOS:), J(IGREF:), &
                    J(IGSEG:), gridps, J(IDMPB:), dlwqd)
            CLOSE (LUN(2))
            !
            !     open binary system files for new input processing, if any
            !
            CALL open_waq_files (LUN(41), LCHAR(41), 41, 1, IERRD)
            IF (IERRD == 0) THEN
                DO I = 1, NUFIL
                    READ (LUN(41), *) iftyp, FINAM
                    new_lun = 800 + I
                    CALL open_waq_files (new_lun, FINAM, 3, 2 + iftyp, IOERR)
                    IF (IOERR /= 0) THEN
                        WRITE (LUN(19), '(A,I3,A,A)') &
                                ' ERROR opening file on unit: ', 800 + I, ' filename: ', FINAM
                        CALL SRSTOP(1)
                    ENDIF
                    ICLEN = LEN(FINAM)
                    DO IC = 1, MIN(ICLEN, 200)
                        C(ILUNT + (I - 1) * 200 + IC - 1) = FINAM(IC:IC)
                    ENDDO
                ENDDO
                CLOSE(LUN(41))
            ENDIF
            !
            !     initialisation of PROCES subsytem
            !
            IF (NPROC > 0) THEN
                CALL open_waq_files (LUN(24), LCHAR(24), 24, 2, IERRD)
                CALL DLWQIP (LUN(24), LCHAR(24), LUN(19), NOTOT, NIPMSA, &
                        NPROC, NOLOC, NFLUX, NODEF, J(INSVA:), &
                        J(IIFLU:), J(IPVAR:), J(IPTYP:), A(IDEFA:), A(ISTOC:), &
                        C(IPRNA:), J(IIMOD:), IERR, IPBLOO, &
                        IOFFBL, NOSYS, NDSPX, NVELX, &
                        A(IDSTO:), A(IVSTO:), NDSPN, J(IDPNW:), NVELN, &
                        J(IVPNW:), NLOCX, J(IPGRD:), J(IPNDT:), NOVAR, &
                        J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), J(IVTAG:), &
                        J(IVAGG:), nrref, J(ipror:), j(iprvpt:))
                CLOSE (LUN(24))
            ENDIF
            !
            !     Set variable "structure"
            !
            CALL DLWQIV (LUN(19), NOCONS, NOPA, NOFUN, NOSFUN, &
                    NOSYS, NOTOT, NODISP, NOVELO, NODEF, &
                    NOLOC, NDSPX, NVELX, NLOCX, NFLUX, &
                    NOPRED, NOVAR, J(IVARR:), J(IVIDX:), J(IVTDA:), &
                    J(IVDAG:), J(IVTAG:), J(IVAGG:), NOGRID, J(IVSET:))
            !
            !     initialisation of OUTPUT subsytem
            !

            IF (NOUTP > 0) THEN
                CALL open_waq_files (LUN(25), LCHAR(25), 25, 2, IERRD)
                CALL DLWQIO (LUN(25), LCHAR(25), LUN(19), NOUTP, NRVART, &
                        NBUFMX, J(IIOUT:), J(IIOPO:), C(IONAM), C(IOSNM), &
                        C(IOUNI), C(IODSC), NOTOT, C(ISSNM), C(ISUNI), &
                        C(ISDSC), LUN, LCHAR, IERR)
                CLOSE (LUN(25))
            ENDIF
            !
            !         initialisation of the grid layout
            !
            IF (NX * NY > 0) THEN
                CALL open_waq_files (LUN(6), LCHAR(6), 6, 2, IERRD)
                READ  (LUN(6)) (J(K), K = IGRID, IGRID + NX * NY - 1)
                CLOSE (LUN(6))
            ENDIF
            !
            !         initialisation of exchange pointers
            !
            CALL open_waq_files (LUN(8), LCHAR(8), 8, 2 + ftype(8), IERRD)

            if (nmax * mmax > 0) then

                !        read grid, make pointer table

                i1 = ilgra - 1
                read  (lun(8)) nmax2, mmax2, noseg2, kmax2, noq1d, noq2d, noq3d
                read  (lun(8)) (j(i1 + k), k = 1, mmax * nmax)
                i2 = ikbnd - 1

                call create_pointer_table(nmax, mmax, kmax, noseg, nobnd, &
                        noq, noq1, noq2, j(ilgra:), j(ixpnt:), &
                        cellpnt, flowpnt)
                finam = lchar(8)(1:index(lchar(8), '.', .true.)) // 'cco'
                call open_waq_files (lun(8), finam, 8, 2 + ftype(8), ierrd)
                read (lun(8))
                read (lun(8)) mmax2, nmax2, x0, y0, beta, np2, nlay
                do i = 1, 2 * np2 + 9
                    read(lun(8)) dummy
                enddo
                read (lun(8)) cell_x
                read (lun(8)) cell_y
            else

                !        Read pointer table with first index 4

                I1 = IXPNT - 1
                do i = 1, noqtt
                    READ (LUN(8)) (J(I1 + K + (i - 1) * 4), K = 1, 4)
                enddo
            ENDIF
            CLOSE (LUN(8))

            IBFLAG = 0
            IF (MOD(INTOPT, 16) >= 8) IBFLAG = 1

            !
            !     locally/per processor adapt the feature array:
            !        feature 1 == segment is active segment of own subdomain or not
            !        feature 2 == position w.r.t. the vertical direction
            !        feature 3 == segment is active segment of global domain or not
            !        feature 4 == segment belongs to own processor
            !

            CALL CHKNMR (LUN(19), nosss, J(IKNMR:))

            ! determine top of the vertcical columns

            call segcol(nosss, noq1, noq2, noq3, noq4, &
                    j(ixpnt:), j(iknmr:), isegcol)

            !         initial conditions

            propor = .false.
            call open_waq_files (lun(18), lchar(18), 18, 2, ierrd)
            ig = scan (lchar(18), '.', back = .true.)                ! look for the file type
            cext = lchar(18)(ig:ig + 3)
            call str_lower(cext)
            if (cext == '.map' .or. cext == '.rmp' .or. &
                    cext == '.rm2') then                               ! if .rmp or .rm2 (Sobek) or .map, it is a map-file
                read (lun(18), iostat = ierrio) finam(1:160)            ! read title of simulation
                if (ierrio /= 0) goto 50
                if (finam(114:120) == 'mass/m2' .or. &
                        finam(114:120) == 'MASS/M2') propor = .true.    !  at end of third line ...
                read (lun(18)) idummy                                 ! should be nr. of substance
                if (idummy /= notot) then
                    write (lun(19), '(a,a,/,a,i10)') &
                            ' ERROR reading initial conditions - filename: ', lchar(18), &
                            ' Number of substances does not match : ', idummy
                    call srstop(1)
                endif
                read (lun(18)) idummy                                 ! should be nr. of comp. volumes
                if (idummy /= nosss) then
                    write (lun(19), '(a,a,/,a,i10)') &
                            ' ERROR reading initial conditions - filename: ', lchar(18), &
                            ' Number of computational volumes does not match : ', idummy
                    call srstop(1)
                endif
                do i = 1, notot
                    read (lun(18)) finam(1:20)
                enddo
            endif
            read  (lun(18), iostat = ierrio)                   & ! like the .ini, the .res and .wrk file
                    idummy, (a(k), k = iconc, iconc + notot * nosss - 1)
            50 if (ierrio /= 0) then
                write (lun(19), '(a,a)') &
                        ' ERROR reading initial conditions - filename: ', &
                        lchar(18), &
                        ' Too few data - file contents does not match current ' // &
                                'model'
                call srstop(1)
            else
                read  (lun(18), iostat = ierrio) idummy
                if (ierrio == 0) then
                    write (lun(19), '(a,a)') &
                            ' ERROR reading initial conditions - filename: ', &
                            lchar(18), &
                            ' Too many data - file contents does not match ' // &
                                    'current model'
                    call srstop(1)
                endif
            endif
            close (lun(18))

            !         first read of relevant time varying arrays
            !
            IFFLAG = 1

            CALL DLWQT0 (LUN, ITSTRT, ITIMEL, A(IHARM:), A(IFARR:), &
                    J(INRHA:), J(INRH2:), J(INRFT:), IDT, A(IVOL:), &
                    A(IDIFF:), A(IAREA:), A(IFLOW:), A(IVELO:), A(ILENG:), &
                    A(IWSTE:), A(IBSET:), A(ICONS:), A(IPARM:), A(IFUNC:), &
                    A(ISFUN:), J(IBULK:), LCHAR, C(ILUNT), ftype, &
                    INTSRT, ISFLAG, IFFLAG, IVFLAG, ILFLAG, &
                    UPDATR, J(IKTIM:), J(IKNMR:), J(INISP:), A(INRSP:), &
                    J(INTYP:), J(IWORK:), .FALSE., LDUMMY, RDUMMY, &
                    .TRUE., gridps, DLWQD)

            !         Particle tracking

            call delpar00 (lchar(45), noseg, noq, a(ivol:), a(iflow:), &
                    nosfun, c(isfna:), a(isfun:))

            !
            !     New bottomlayer processing
            !
            IF (NOQ4 > 0) &
                    CALL DLWQTD (LUN, NOSEG, NSEG2, NOLAY, NOGRID, &
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
                        write (lun(19), '(a,a)')               & !   not found
                                ' Error reading initial conditions: ', &
                                ' horizontal surface area not found! '
                        call srstop(1)
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
                WRITE(LUN(19), *) ' Closure error correction enabled'
            ELSE
                ICFLAG = 0
                WRITE(LUN(19), *) ' Closure error correction disabled'
            ENDIF

        end associate

        if (timon) call timstop (ithandl)
        RETURN
    END subroutine

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
    end subroutine
end module dlwqi0_mod
