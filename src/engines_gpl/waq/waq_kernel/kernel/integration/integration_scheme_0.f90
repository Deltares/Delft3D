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
module m_integration_scheme_0
    use m_waq_precision
    use m_zercum
    use m_setset
    use m_proint
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use m_dlwqo2

    implicit none

contains


    subroutine integration_scheme_0 (buffer, lun, lchar, &
            action, dlwqd, gridps)

        !       Deltares Software Centre

        !>\file
        !>                         No tranport scheme (0)
        !>
        !>                         Performs only calculation of new concentrations due processes

        use m_dlwq18
        use m_dlwq14
        use m_dlwq13
        use m_delpar01
        use m_array_manipulation, only : copy_real_array_elements
        use data_processing, only : close_files
        use dlwqgrid_mod
        use timers
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use memory_mangement          ! module with the more recently added arrays
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_dlwqdata_save_restore

        implicit none

        !     Parameters         :

        !     kind           function         name                Descriptipon
        type(waq_data_buffer), target :: buffer           !< System total array space
        integer(kind = int_wp), intent(in) :: lun  (*)          !< array with unit numbers
        character*(*), intent(in) :: lchar(*)          !< array with file names
        integer(kind = int_wp), intent(in) :: action            !< type of action to perform
        type(delwaq_data), target :: dlwqd             !< delwaq data structure
        type(GridPointerColl) :: gridps            !< collection of all grid definitions

        !     Local declarations
        LOGICAL         IMFLAG, IDFLAG, IHFLAG
        LOGICAL         LREWIN
        REAL(kind = real_wp) :: RDUMMY(1)
        INTEGER(kind = int_wp) :: NSTEP
        INTEGER(kind = int_wp) :: IBND
        INTEGER(kind = int_wp) :: ISYS
        INTEGER(kind = int_wp) :: IERROR

        INTEGER(kind = int_wp) :: IDTOLD
        INTEGER(kind = int_wp) :: sindex

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (ACTION == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt ("integration_scheme_0", ithandl)
                goto 20
            endif

            if (ACTION == ACTION_INITIALISATION  .or. &
                    ACTION == ACTION_FULLCOMPUTATION) then

                !          some initialisation

                ithandl = 0
                ITIME = ITSTRT
                NSTEP = (ITSTOP - ITSTRT) / IDT
                IFFLAG = 0
                IAFLAG = 0
                IBFLAG = 0

                !     Dummy variables - used in DLWQD
                ITIMEL = ITIME
                lleng = 0
                ioptzb = 0
                nopred = 6
                NOWARN = 0
                tol = 0.0D0
                forester = .FALSE.
                updatr = .FALSE.

                nosss = noseg + nseg2
                noqtt = noq + noq4
                NOQT = NOQ + NOQ4
                inwtyp = intyp + nobnd

                IF (MOD(INTOPT, 16) >= 8) IBFLAG = 1
                LDUMMY = .FALSE.
                IF (NDSPN == 0) THEN
                    NDDIM = NODISP
                ELSE
                    NDDIM = NDSPN
                ENDIF
                IF (NVELN == 0) THEN
                    NVDIM = NOVELO
                ELSE
                    NVDIM = NVELN
                ENDIF
                LSTREC = ICFLAG == 1
                NOWARN = 0
                IF (ILFLAG == 0) LLENG = ILENG + 2


                !          Initialize second volume array with the first one

                nosss = noseg + nseg2
                call copy_real_array_elements   (A(IVOL:), A(IVOL2:), NOSSS)

            endif
            !
            !     Save/restore the local persistent variables,
            !     if the computation is split up in steps
            !
            !     Note: the handle to the timer (ithandl) needs to be
            !     properly initialised and restored
            !
            IF (ACTION == ACTION_INITIALISATION) THEN
                if (timon) call timstrt ("integration_scheme_0", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop (ithandl)
                RETURN
            ENDIF

            IF (ACTION == ACTION_SINGLESTEP) THEN
                call dlwqdata_restore(dlwqd)
            ENDIF

            if (timon) call timstrt ("integration_scheme_0", ithandl)

            !======================= simulation loop ============================

            10 continue

            !        Determine the volumes and areas that ran dry,
            !        They cannot have explicit processes during this time step

            call hsurf  (noseg, nopa, c(ipnam), a(iparm:), nosfun, &
                    c(isfna), a(isfun:), surface, lun(19))
            call dryfld (noseg, nosss, nolay, a(ivol:), noq1 + noq2, &
                    a(iarea:), nocons, c(icnam), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            !        user transport processes

            call dlwqtr (notot, nosys, nosss, noq, noq1, &
                    noq2, noq3, nopa, nosfun, nodisp, &
                    novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam), nocons, nofun, c(icnam), &
                    c(ipnam), c(ifnam), c(isfna), ldummy, ilflag)

            !jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

            call setset (lun(19), nocons, nopa, nofun, nosfun, &
                    nosys, notot, nodisp, novelo, nodef, &
                    noloc, ndspx, nvelx, nlocx, nflux, &
                    nopred, novar, nogrid, j(ivset:))

            !        return conc and take-over from previous step or initial condition,
            !        and do particle tracking of this step (will be back-coupled next call)

            call delpar01(itime, noseg, nolay, noq, nosys, &
                    notot, a(ivol:), surface, a(iflow:), c(isnam:), &
                    nosfun, c(isfna:), a(isfun:), a(imass:), a(iconc:), &
                    iaflag, intopt, ndmps, j(isdmp:), a(idmps:), &
                    a(imas2:))

            !        call PROCES subsystem

            call proces (notot, nosss, a(iconc:), a(ivol:), itime, &
                    idt, a(iderv:), ndmpar, nproc, nflux, &
                    j(iipms:), j(insva:), j(iimod:), j(iiflu:), j(iipss:), &
                    a(iflux:), a(iflxd:), a(istoc:), ibflag, ipbloo, &
                    ioffbl, a(imass:), nosys, &
                    itfact, a(imas2:), iaflag, intopt, a(iflxi:), &
                    j(ixpnt:), iknmkv, noq1, noq2, noq3, &
                    noq4, ndspn, j(idpnw:), a(idnew:), nodisp, &
                    j(idpnt:), a(idiff:), ndspx, a(idspx:), a(idsto:), &
                    nveln, j(ivpnw:), a(ivnew:), novelo, j(ivpnt:), &
                    a(ivelo:), nvelx, a(ivelx:), a(ivsto:), a(idmps:), &
                    j(isdmp:), j(ipdmp:), ntdmpq, a(idefa:), j(ipndt:), &
                    j(ipgrd:), j(ipvar:), j(iptyp:), j(ivarr:), j(ividx:), &
                    j(ivtda:), j(ivdag:), j(ivtag:), j(ivagg:), j(iapoi:), &
                    j(iaknd:), j(iadm1:), j(iadm2:), j(ivset:), j(ignos:), &
                    j(igseg:), novar, a, nogrid, ndmps, &
                    c(iprna:), intsrt, &
                    j(iprvpt:), j(iprdon:), nrref, j(ipror:), nodef, &
                    surface, lun(19))

            !     Call OUTPUT system

            CALL DLWQO2 (NOTOT, NOSSS, NOPA, NOSFUN, ITIME, &
                    C(IMNAM:), C(ISNAM:), C(IDNAM:), J(IDUMP:), NODUMP, &
                    A(ICONC:), A(ICONS:), A(IPARM:), A(IFUNC:), A(ISFUN:), &
                    A(IVOL:), NOCONS, NOFUN, IDT, NOUTP, &
                    LCHAR, LUN, J(IIOUT:), J(IIOPO:), A(IRIOB:), &
                    C(IOSNM:), C(IOUNI:), C(IODSC:), C(ISSNM:), C(ISUNI:), C(ISDSC:), &
                    C(IONAM:), NX, NY, J(IGRID:), C(IEDIT:), &
                    NOSYS, A(IBOUN:), J(ILP:), A(IMASS:), A(IMAS2:), &
                    A(ISMAS:), NFLUX, A(IFLXI:), ISFLAG, IAFLAG, &
                    IBFLAG, IMSTRT, IMSTOP, IMSTEP, IDSTRT, &
                    IDSTOP, IDSTEP, IHSTRT, IHSTOP, IHSTEP, &
                    IMFLAG, IDFLAG, IHFLAG, NOLOC, A(IPLOC:), &
                    NODEF, A(IDEFA:), ITSTRT, ITSTOP, NDMPAR, &
                    C(IDANA:), NDMPQ, NDMPS, J(IQDMP:), J(ISDMP:), &
                    J(IPDMP:), A(IDMPQ:), A(IDMPS:), A(IFLXD:), NTDMPQ, &
                    C(ICBUF:), NORAAI, NTRAAQ, J(IORAA:), J(NQRAA:), &
                    J(IQRAA:), A(ITRRA:), C(IRNAM:), A(ISTOC:), NOGRID, &
                    NOVAR, J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), &
                    J(IAKND:), J(IAPOI:), J(IADM1:), J(IADM2:), J(IVSET:), &
                    J(IGNOS:), J(IGSEG:), A, NOBND, NOBTYP, &
                    C(IBTYP:), J(INTYP:), C(ICNAM:), noqtt, J(IXPNT:), &
                    INTOPT, C(IPNAM:), C(IFNAM:), C(ISFNA:), J(IDMPB:), &
                    NOWST, NOWTYP, C(IWTYP:), J(IWAST:), J(INWTYP:), &
                    A(IWDMP:), iknmkv, isegcol)

            !          zero cummulative array's

            if (imflag .or. (ihflag .and. noraai > 0)) then
                call zercum (notot, nosys, nflux, ndmpar, ndmpq, &
                        ndmps, a(ismas:), a(iflxi:), a(imas2:), a(iflxd:), &
                        a(idmpq:), a(idmps:), noraai, imflag, ihflag, &
                        a(itrra:), ibflag, nowst, a(iwdmp:))
            endif

            !          simulation done ?

            if (itime < 0) goto 9999
            if (itime >= itstop) goto 20

            !        add processes

            call dlwq14 (a(iderv:), notot, nosss, itfact, a(imas2:), &
                    idt, iaflag, a(idmps:), intopt, j(isdmp:))
            itimel = itime                     ! For case 2 a(ivoll) contains the incorrect
            itime = itime + idt               ! new volume from file and mass correction
            idtold = idt

            !        set a time step

            call dlwq18 (nosys, notot, nototp, nosss, a(ivol2:), &
                    surface, a(imass:), a(iconc:), a(iderv:), idtold, &
                    ivflag, lun(19))

            !          integrate the fluxes at dump segments fill ASMASS with mass

            if (ibflag > 0) then
                call proint (nflux, ndmpar, idtold, itfact, a(iflxd:), &
                        a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            endif
            !          end of loop

            if (ACTION == ACTION_FULLCOMPUTATION) goto 10

            20 continue

            if (ACTION == ACTION_FINALISATION    .or. &
                    ACTION == ACTION_FULLCOMPUTATION) then
                !             close files, except monitor file

                call CloseHydroFiles(dlwqd%collcoll)
                call close_files(lun)

                !             write restart file

                CALL DLWQ13 (LUN, LCHAR, A(ICONC:), ITIME, C(IMNAM:), &
                        C(ISNAM:), NOTOT, NOSSS)
            endif

        end associate

        9999 if (timon) call timstop (ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime

        RETURN
    END SUBROUTINE

end module m_integration_scheme_0
