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
module m_integration_scheme_5
    use m_waq_precision
    use m_zercum
    use m_setset
    use m_proint
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use time_dependent_variables, only : initialize_time_dependent_variables
    use m_dlwqo2

    implicit none

contains


    subroutine integration_scheme_5 (buffer, file_unit_list, file_name_list, &
            action, dlwqd, gridps)

        !       Deltares Software Centre

        !>\file
        !>                         Flux corrected transport according to Boris and Book (FCT) (5)
        !>
        !>                         3D flux corrected transport according to Boris and Book.
        !>                         Explicit in time. Very fast and accurate solution. Time
        !>                         step size is limited by a stability criterion.

        !     CREATED            : june 1988 by L. Postma

        !     LOGICAL UNITS      : file_unit_list(19) , output, monitoring file
        !                          file_unit_list(20) , output, formatted dump file
        !                          file_unit_list(21) , output, unformatted hist. file
        !                          file_unit_list(22) , output, unformatted dump file
        !                          file_unit_list(23) , restart file

        !     SUBROUTINES CALLED : DLWQTR, user transport routine
        !                          PROCES, DELWAQ proces system
        !                          DLWQO2, DELWAQ output system
        !                          DLWQ13, system postpro-dump routine
        !                          DLWQ14, scales waterquality
        !                          DLWQ15, wasteload routine
        !                          DLWQ17, boundary routine
        !                          DLWQ18, integration step
        !                          DLWQ41, update volumes
        !                          initialize_time_dependent_variables, update other time functions
        !                          DLWQ50, transport
        !                          DLWQ51, flux correction
        !                          DLWQ52, makes masses and concentrations
        !                          PROINT, integration of fluxes
        !                          open_waq_files, opens files
        !                          ZERCUM, zero's the cummulative array's
        !
        use m_dlwqf8
        use m_dlwqce
        use m_dlwqb3
        use m_dlwq52
        use m_dlwq51
        use m_dlwq50
        use m_dlwq41
        use m_dlwq18
        use m_dlwq17
        use m_dlwq15
        use m_dlwq14
        use m_dlwq13
        use m_delpar01
        use m_array_manipulation, only : copy_real_array_elements
        use data_processing, only : close_files
        use m_grid_utils_external
        use timers
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use variable_declaration          ! module with the more recently added arrays
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_dlwqdata_save_restore

        implicit none

        !     Arguments           :
        !     type     kind  function         name                      description

        type(waq_data_buffer), intent(inout) :: buffer              !< System total array space
        integer(kind = int_wp), intent(inout) :: file_unit_list    (*)              !< array with unit numbers
        character (*), intent(in) :: file_name_list  (*)              !< array with file names
        integer(kind = int_wp), intent(in) :: action                  !< mode of operation
        type(delwaq_data), target :: dlwqd                   !< data area stepwise processing
        type(GridPointerColl) :: GridPs                  !< collection of all grid definitions


        !     Local declarations

        logical   imflag     !  true if monitoring took place, set in dlwqo2, used in zercum
        logical   ihflag     !  true if history    took place, set in dlwqo2, used in zercum
        logical   idflag     !  true if dump       took place, set in dlwqo2, not used
        logical   lrewin     !  true if rewind     took place, set in dlwq41, used for closure error corr.
        logical   ldumm2     !  dummy logical, parameter in initialize_time_dependent_variables
        real(kind = real_wp) :: rdummy(1)  !  dummy real in initialize_time_dependent_variables
        integer(kind = int_wp) :: nstep      !  number of time steps (does not work if idt is time varying !)
        integer(kind = int_wp) :: ierr       !  error variable
        integer(kind = int_wp) :: ibnd       !  loop counter boundaries (loop should be in a called subroutine !)
        integer(kind = int_wp) :: isys       !  loop counter substances (loop should be in a called subroutine !)
        integer(kind = int_wp) :: sindex     !  if non-zero, then the surface array is filled
        
        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt ("integration_scheme_5", ithandl)
                goto 20
            endif

            IF (ACTION == ACTION_INITIALISATION  .OR. &
                    ACTION == ACTION_FULLCOMPUTATION) THEN

                !
                !          some initialisation
                !
                ithandl = 0
                ITIME = ITSTRT
                NSTEP = (ITSTOP - ITSTRT) / IDT
                IFFLAG = 0
                IAFLAG = 0
                ibflag = 0

                !     Dummy variables - used in DLWQD
                ITIMEL = ITIME
                lleng = 0
                ioptzb = 0
                nopred = 6
                NOWARN = 0
                tol = 0.0D0
                forester = .FALSE.
                updatr = .FALSE.
                NOQT = NOQ + NOQ4

                if (btest(intopt, 3)) ibflag = 1
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
                nosss = noseg + nseg2
                NOQTT = NOQ + NOQ4
                inwtyp = intyp + nobnd
                !
                !          initialize second volume array with the first one
                !
                call copy_real_array_elements   (A(IVOL:), A(IVOL2:), nosss)
            ENDIF

            !
            !     Save/restore the local persistent variables,
            !     if the computation is split up in steps
            !
            !     Note: the handle to the timer (ithandl) needs to be
            !     properly initialised and restored
            !
            IF (ACTION == ACTION_INITIALISATION) THEN
                if (timon) call timstrt ("integration_scheme_5", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop (ithandl)
                RETURN
            ENDIF

            IF (ACTION == ACTION_SINGLESTEP) THEN
                call dlwqdata_restore(dlwqd)
            ENDIF

            if (timon) call timstrt ("integration_scheme_5", ithandl)

            !======================= simulation loop ============================

            10 continue

            !        Determine the volumes and areas that ran dry at start of time step

            call hsurf  (noseg, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call dryfld (noseg, nosss, nolay, a(ivol:), noq1 + noq2, &
                    a(iarea:), nocons, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            !        user transport processes

            call dlwqtr (notot, nosys, nosss, noq, noq1, &
                    noq2, noq3, nopa, nosfun, nodisp, &
                    novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam:), nocons, nofun, c(icnam:), &
                    c(ipnam:), c(ifnam:), c(isfna:), ldummy, ilflag)

            !jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

            call setset (file_unit_list(19), nocons, nopa, nofun, nosfun, &
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
                    j(ixpnt:), p_iknmkv, noq1, noq2, noq3, &
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
                    surface, file_unit_list(19))

            !          set new boundaries

            if (itime >= 0) then
                if (dlwqd%inopenda) then
                    do ibnd = 1, nobnd
                        do isys = 1, nosys
                            call get_openda_buffer(isys, ibnd, 1, 1, &
                                    A(ibset:+(ibnd - 1) * nosys + isys - 1))
                        enddo
                    enddo
                endif
                call dlwq17 (a(ibset:), a(ibsav:), j(ibpnt:), nobnd, nosys, &
                        notot, idt, a(iconc:), a(iflow:), a(iboun:))
            endif
            !
            !     Call OUTPUT system
            !
            CALL DLWQO2 (NOTOT, nosss, NOPA, NOSFUN, ITIME, &
                    C(IMNAM:), C(ISNAM:), C(IDNAM:), J(IDUMP:), NODUMP, &
                    A(ICONC:), A(ICONS:), A(IPARM:), A(IFUNC:), A(ISFUN:), &
                    A(IVOL:), NOCONS, NOFUN, IDT, NOUTP, &
                    file_name_list, file_unit_list, J(IIOUT:), J(IIOPO:), A(IRIOB:), &
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
                        ndmps, a(ismas:), a(iflxi:), a(imas2:), &
                        a(idmpq:), a(idmps:), noraai, imflag, ihflag, &
                        a(itrra:), ibflag, nowst, a(iwdmp:))
            endif

            !          simulation done ?

            if (itime < 0) goto 9999
            if (itime >= itstop) goto 20

            !          add processes

            call dlwq14 (a(iderv:), notot, nosss, itfact, a(imas2:), &
                    idt, iaflag, a(idmps:), intopt, j(isdmp:))

            !     get new volumes

            itimel = itime
            itime = itime + idt
            select case (ivflag)
            case (1)                 !     computation of volumes for computed volumes only
                call copy_real_array_elements   (a(ivol:), a(ivol2:), noseg)
                call dlwqb3 (a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), notot, &
                        noq, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                        a(imas2:), idt, iaflag, nosys, a(idmpq:), &
                        ndmpq, j(iqdmp:))
                updatr = .true.
            case (2)                 !     the fraudulent computation option
                call dlwq41 (file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivoll:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivol2:), dlwqd)
                if (lrewin) call copy_real_array_elements (a(ivol2:), a(ivoll:), noseg)
                call dlwqf8 (noseg, noq, j(ixpnt:), idt, iknmkv, &
                        a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               !     read new volumes from files
                call dlwq41 (file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
            end select

            !        update the info on dry volumes with the new volumes

            call dryfle (noseg, nosss, a(ivol2:), nolay, nocons, &
                    c(icnam:), a(icons:), surface, j(iknmr:), iknmkv)

            !          add the waste loads

            call dlwq15 (nosys, notot, noseg, noq, nowst, &
                    nowtyp, ndmps, intopt, idt, itime, &
                    iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                    a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                    j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                    iknmkv, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                    a(iwdmp:), 1, notot)

            !          do the transport itself

            call dlwq50 (nosys, notot, nosss, noqtt, nvdim, &
                    a(ivnew:), a(iarea:), a(iflow:), j(ixpnt:), j(ivpnw:), &
                    a(iconc:), a(iboun:), idt, a(iderv:), iaflag, &
                    a(imas2:))

            !          set the first guess in array CONC2 == ITIMR

            call dlwq18 (nosys, notot, nototp, nosss, a(ivol2:), &
                    surface, a(imass:), a(itimr:), a(iderv:), idt, &
                    ivflag, file_unit_list(19))

            !             perform the flux correction on conc2 == a(itimr:)

            call dlwq51 (nosys, notot, nosss, noq1, noq2, &
                    noq3, noqtt, nddim, nvdim, a(idisp:), &
                    a(idnew:), a(ivnew:), a(ivol2:), a(iarea:), a(iflow:), &
                    a(ileng:), j(ixpnt:), iknmkv, j(idpnw:), j(ivpnw:), &
                    a(iconc:), a(itimr:), a(iboun:), intopt, ilflag, &
                    idt, iaflag, a(imas2:), ndmpq, j(iqdmp:), &
                    a(idmpq:))
            call dlwq52 (nosys, notot, nosss, a(ivol2:), a(imass:), &
                    a(itimr:), a(iconc:))

            !          new time values, volumes excluded
            call initialize_time_dependent_variables (file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                    j(inrha:), j(inrh2:), j(inrft:), idt, a(ivol:), &
                    a(idiff:), a(iarea:), a(iflow:), a(ivelo:), a(ileng:), &
                    a(iwste:), a(ibset:), a(icons:), a(iparm:), a(ifunc:), &
                    a(isfun:), j(ibulk:), file_name_list, c(ilunt:), ftype, &
                    intsrt, isflag, ifflag, ivflag, ilflag, &
                    ldumm2, j(iktim:), j(iknmr:), j(inisp:), a(inrsp:), &
                    j(intyp:), j(iwork:), .false., ldummy, rdummy, &
                    .false., gridps, dlwqd)


            !    calculate closure error
            if (lrewin .and. lstrec) then
                call dlwqce (a(imass:), a(ivoll:), a(ivol2:), nosys, notot, &
                        noseg, file_unit_list(19))
                call copy_real_array_elements   (a(ivoll:), a(ivol:), noseg)
            else
                !     replace old by new volumes
                call copy_real_array_elements   (a(ivol2:), a(ivol:), noseg)
            endif

            !          integrate the fluxes at dump segments fill ASMASS with mass

            if (ibflag > 0) then
                call proint (nflux, ndmpar, idt, itfact, a(iflxd:), &
                        a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            endif

            !          end of loop

            if (ACTION == ACTION_FULLCOMPUTATION) goto 10

            20 continue

            if (ACTION == ACTION_FINALISATION    .or. &
                    ACTION == ACTION_FULLCOMPUTATION) then
                !         close files, except monitor file

                call CloseHydroFiles(dlwqd%collcoll)
                call close_files(file_unit_list)

                !         write restart file

                CALL DLWQ13 (file_unit_list, file_name_list, A(ICONC:), ITIME, C(IMNAM:), &
                        C(ISNAM:), NOTOT, nosss)

            endif

        end associate
        9999 if (timon) call timstop (ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime

        RETURN
    END SUBROUTINE

end module m_integration_scheme_5
