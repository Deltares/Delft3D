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
module m_integration_scheme_11
    use m_waq_precision
    use m_zercum
    use m_setset
    use m_integrate_areas_fluxes
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use time_dependent_variables, only: initialize_time_dependent_variables
    use m_write_output

    implicit none

contains

    !> Upwind explicit horizontal, central implicit vertical (11)
    !> Performs time dependent integration. Upwind explicit 1st order
    !> horizontally, central implicit vertically.\n
    !> Method has the option to treat additional velocities, like
    !> settling of suspended matter, upwind to avoid wiggles.
    !> Optional Forester filter to enhance vertical monotonicity.
    subroutine integration_scheme_11(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_dlwqf8
        use m_dlwqd2
        use m_dlwqd1
        use m_dlwqce
        use m_dlwqb3
        use m_dlwq44
        use m_dlwq42
        use m_dlwq41
        use m_dlwq17
        use m_dlwq16
        use m_dlwq15
        use m_dlwq14
        use dryfld_mod
        use m_write_restart_map_file
        use m_delpar01
        use m_array_manipulation, only: copy_real_array_elements
        use data_processing, only: close_files
        use m_grid_utils_external
        use timers
        use delwaq2_data
        use m_waq_openda_exchange_items, only: get_openda_buffer
        use variable_declaration          ! module with the more recently added arrays
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_dlwqdata_save_restore

        type(waq_data_buffer), target :: buffer                  !< System total array space
        integer(kind=int_wp), intent(inout) :: file_unit_list(*) !< Array with logical unit numbers
        character(len=*), intent(in) :: file_name_list(*)        !< Array with file names
        integer(kind=int_wp), intent(in) :: action               !< Span of the run or type of action to perform
                                                                 !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data), target :: dlwqd                       !< DELWAQ data structure
        type(GridPointerColl) :: gridps                          !< Collection of all grid definitions

        ! Local variables
        logical :: imflag, idflag, ihflag
        logical :: lrewin, ldumm2
        real(kind=real_wp) :: rdummy(1)
        integer(kind=int_wp) :: nstep
        integer(kind=int_wp) :: ibnd
        integer(kind=int_wp) :: isys
        integer(kind=int_wp) :: ierror

        integer(kind=int_wp) :: larea
        integer(kind=int_wp) :: ldisp
        integer(kind=int_wp) :: ldiff
        integer(kind=int_wp) :: lflow
        integer(kind=int_wp) :: lnoq
        integer(kind=int_wp) :: lqdmp
        integer(kind=int_wp) :: lvelo
        integer(kind=int_wp) :: lxpnt

        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt("integration_scheme_11", ithandl)
                goto 20
            end if

            if (action == ACTION_INITIALISATION .or. &
                action == ACTION_FULLCOMPUTATION) then

                ! some initialisation
                ithandl = 0
                itime = itstrt
                nstep = (itstop - itstrt)/idt
                ifflag = 0
                iaflag = 0
                ibflag = 0
                if (mod(intopt, 16) >= 8) ibflag = 1
                ldummy = .false.
                if (ndspn == 0) then
                    nddim = nodisp
                else
                    nddim = ndspn
                end if
                if (nveln == 0) then
                    nvdim = novelo
                else
                    nvdim = nveln
                end if
                lstrec = icflag == 1
                forester = btest(intopt, 6)
                nowarn = 0
                if (ilflag == 0) lleng = ileng + 2

                ! initialize second volume array with the first one
                nosss = noseg + nseg2
                call copy_real_array_elements(a(ivol:), a(ivol2:), nosss)
            end if

            ! Save/restore the local persistent variables,
            ! if the computation is split up in steps
            !
            ! Note: the handle to the timer (ithandl) needs to be
            ! properly initialised and restored
            if (action == ACTION_INITIALISATION) then
                if (timon) call timstrt("integration_scheme_11", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop(ithandl)
                return
            end if

            if (action == ACTION_SINGLESTEP) then
                call dlwqdata_restore(dlwqd)
            end if

            ! adaptations for layered bottom 08-03-2007  lp
            nosss = noseg + nseg2
            noqtt = noq + noq4
            inwtyp = intyp + nobnd

            ! set alternating set of pointers
            noqt = noq1 + noq2
            lnoq = noqtt - noqt
            ldisp = idisp + 2
            ldiff = idnew + nddim*noqt
            larea = iarea + noqt
            lflow = iflow + noqt
            lleng = ileng + noqt*2
            lvelo = ivnew + nvdim*noqt
            lxpnt = ixpnt + noqt*4
            lqdmp = iqdmp + noqt

            if (timon) call timstrt("integration_scheme_11", ithandl)

            !======================= simulation loop ============================
10          continue

            ! Determine the volumes and areas that ran dry at start of time step
            call hsurf(nosss, nopa, c(ipnam), a(iparm:), nosfun, &
                       c(isfna), a(isfun:), surface, file_unit_list(19))
            call dryfld(noseg, nosss, nolay, a(ivol:), noq1 + noq2, &
                        a(iarea:), nocons, c(icnam), a(icons:), surface, &
                        j(iknmr:), iknmkv)

            ! user transport processes
            call dlwqtr(notot, nosys, nosss, noq, noq1, &
                        noq2, noq3, nopa, nosfun, nodisp, &
                        novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                        a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                        a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                        idt, c(isnam), nocons, nofun, c(icnam), &
                        c(ipnam), c(ifnam), c(isfna), ldummy, ilflag)

            ! Temporary ? set the variables grid-setting for the DELWAQ variables
            call setset(file_unit_list(19), nocons, nopa, nofun, nosfun, &
                        nosys, notot, nodisp, novelo, nodef, &
                        noloc, ndspx, nvelx, nlocx, nflux, &
                        nopred, novar, nogrid, j(ivset:))

            ! return conc and take-over from previous step or initial condition,
            ! and do particle tracking of this step (will be back-coupled next call)
            call delpar01(itime, noseg, nolay, noq, nosys, &
                          notot, a(ivol:), surface, a(iflow:), c(isnam), &
                          nosfun, c(isfna), a(isfun:), a(imass:), a(iconc:), &
                          iaflag, intopt, ndmps, j(isdmp:), a(idmps:), &
                          a(imas2:))

            ! call PROCES subsystem
            call proces(notot, nosss, a(iconc:), a(ivol:), itime, &
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
                        c(iprna), intsrt, &
                        j(iprvpt:), j(iprdon:), nrref, j(ipror:), nodef, &
                        surface, file_unit_list(19))

            ! set new boundaries
            if (itime >= 0) then
                !           first: adjust boundaries by OpenDA
                if (dlwqd%inopenda) then
                    do ibnd = 1, nobnd
                        do isys = 1, nosys
                            call get_openda_buffer(isys, ibnd, 1, 1, &
                                                   A(ibset:+(ibnd - 1)*nosys + isys - 1))
                        end do
                    end do
                end if
                call thatcher_harleman_bc(a(ibset:), a(ibsav:), j(ibpnt:), nobnd, nosys, &
                                          notot, idt, a(iconc:), a(iflow:), a(iboun:))
            end if

            ! Call OUTPUT system
            call write_output(notot, nosss, nopa, nosfun, itime, &
                              c(imnam), c(isnam), c(idnam), j(idump:), nodump, &
                              a(iconc:), a(icons:), a(iparm:), a(ifunc:), a(isfun:), &
                              a(ivol:), nocons, nofun, idt, noutp, &
                              file_name_list, file_unit_list, j(iiout:), j(iiopo:), a(iriob:), &
                              c(iosnm), c(iouni), c(iodsc), c(issnm), c(isuni), c(isdsc), &
                              c(ionam), nx, ny, j(igrid:), c(iedit), &
                              nosys, a(iboun:), j(ilp:), a(imass:), a(imas2:), &
                              a(ismas:), nflux, a(iflxi:), isflag, iaflag, &
                              ibflag, imstrt, imstop, imstep, idstrt, &
                              idstop, idstep, ihstrt, ihstop, ihstep, &
                              imflag, idflag, ihflag, noloc, a(iploc:), &
                              nodef, a(idefa:), itstrt, itstop, ndmpar, &
                              c(idana), ndmpq, ndmps, j(iqdmp:), j(isdmp:), &
                              j(ipdmp:), a(idmpq:), a(idmps:), a(iflxd:), ntdmpq, &
                              c(icbuf), noraai, ntraaq, j(ioraa:), j(nqraa:), &
                              j(iqraa:), a(itrra:), c(irnam), a(istoc:), nogrid, &
                              novar, j(ivarr:), j(ividx:), j(ivtda:), j(ivdag:), &
                              j(iaknd:), j(iapoi:), j(iadm1:), j(iadm2:), j(ivset:), &
                              j(ignos:), j(igseg:), a, nobnd, nobtyp, &
                              c(ibtyp), j(intyp:), c(icnam), noqtt, j(ixpnt:), &
                              intopt, c(ipnam), c(ifnam), c(isfna), j(idmpb:), &
                              nowst, nowtyp, c(iwtyp), j(iwast:), j(inwtyp:), &
                              a(iwdmp:), iknmkv, isegcol)

            ! zero cummulative array's
            if (imflag .or. (ihflag .and. noraai > 0)) then
                call set_cumulative_arrays_zero(notot, nosys, nflux, ndmpar, ndmpq, &
                                                ndmps, a(ismas:), a(iflxi:), a(imas2:), &
                                                a(idmpq:), a(idmps:), noraai, imflag, ihflag, &
                                                a(itrra:), ibflag, nowst, a(iwdmp:))
            end if

            ! simulation done ?
            if (itime < 0) goto 9999
            if (itime >= itstop) goto 20

            ! add processes
            call scale_processes_derivs_and_update_balances(a(iderv:), notot, nosss, itfact, a(imas2:), &
                                                            idt, iaflag, a(idmps:), intopt, j(isdmp:))

            ! get new volumes
            itimel = itime
            itime = itime + idt
            select case (ivflag)
            case (1)                 ! computation of volumes for computed volumes only
                call copy_real_array_elements(a(ivol:), a(ivol2:), noseg)
                call dlwqb3(a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), notot, &
                            noq, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                            a(imas2:), idt, iaflag, nosys, a(idmpq:), &
                            ndmpq, j(iqdmp:))
                updatr = .true.
            case (2)                 ! the fraudulent computation option
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                            j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivoll:), &
                            j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                            updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                            lstrec, lrewin, a(ivol2:), dlwqd)
                if (lrewin) call copy_real_array_elements(a(ivol2:), a(ivoll:), noseg)
                call dlwqf8(noseg, noq, j(ixpnt:), idt, iknmkv, &
                            a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               !     read new volumes from files
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                            j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                            j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                            updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                            lstrec, lrewin, a(ivoll:), dlwqd)
            end select

            ! update the info on dry volumes with the new volumes
            call dryfle(noseg, nosss, a(ivol2:), nolay, nocons, &
                        c(icnam), a(icons:), surface, j(iknmr:), iknmkv)

            ! add the waste loads
            call dlwq15(nosys, notot, noseg, noq, nowst, &
                        nowtyp, ndmps, intopt, idt, itime, &
                        iaflag, c(isnam), a(iconc:), a(ivol:), a(ivol2:), &
                        a(iflow:), j(ixpnt:), c(iwsid), c(iwnam), c(iwtyp), &
                        j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                        iknmkv, nopa, c(ipnam), a(iparm:), nosfun, &
                        c(isfna), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                        a(iwdmp:), 1, notot)

            ! explicit part of the transport step, derivative

            call dlwq16(nosys, notot, nosss, noq1, noq2, &
                        noq3, noqt, nddim, nvdim, a(idisp:), &
                        a(idnew:), a(ivnew:), a(iarea:), a(iflow:), a(ileng:), &
                        j(ixpnt:), iknmkv, j(idpnw:), j(ivpnw:), a(iconc:), &
                        a(iboun:), intopt, ilflag, idt, a(iderv:), &
                        iaflag, a(imas2:), ndmpq, j(iqdmp:), a(idmpq:))

            ! explicit part of transport done, volumes on diagonal
            call dlwq42(nosys, notot, nototp, nosss, a(ivol2:), &
                        surface, a(imass:), a(iconc:), a(iderv:), idt, &
                        ivflag, file_unit_list(19))

            ! performs the implicit part of the transport step
            call dlwqd1(nosys, notot, nosss, noq3, lnoq, &
                        nddim, nvdim, a(ldisp:), a(ldiff:), a(lvelo:), &
                        a(larea:), a(lflow:), a(lleng:), j(lxpnt:), iknmkv, &
                        j(idpnw:), j(ivpnw:), a(iconc:), a(iboun:), intopt, &
                        ilflag, idt, a(iderv:), iaflag, a(imas2:), &
                        file_unit_list(19), ndmpq, j(lqdmp:), &
                        a(idmpq:), arhs, adiag, acodia, bcodia)

            ! Forester filter on the vertical
            if (forester) then
                call dlwqd2(file_unit_list(19), nosys, notot, nosss, noq3, &
                            kmax, a(iconc:), a(lleng:), nowarn)
            end if

            ! update the nescessary arrays
            call dlwq44(nosys, notot, nosss, a(ivol2:), a(imass:), &
                        a(iconc:), a(iderv:))

            ! new time values, volumes excluded
            call initialize_time_dependent_variables(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                                                     j(inrha:), j(inrh2:), j(inrft:), idt, a(ivol:), &
                                                     a(idiff:), a(iarea:), a(iflow:), a(ivelo:), a(ileng:), &
                                                     a(iwste:), a(ibset:), a(icons:), a(iparm:), a(ifunc:), &
                                                     a(isfun:), j(ibulk:), file_name_list, c(ilunt), ftype, &
                                                     intsrt, isflag, ifflag, ivflag, ilflag, &
                                                     ldumm2, j(iktim:), j(iknmr:), j(inisp:), a(inrsp:), &
                                                     j(intyp:), j(iwork:), .false., ldummy, rdummy, &
                                                     .false., gridps, dlwqd)

            ! calculate closure error
            if (lrewin .and. lstrec) then
                call dlwqce(a(imass:), a(ivoll:), a(ivol2:), nosys, notot, &
                            noseg, file_unit_list(19))
                call copy_real_array_elements(a(ivoll:), a(ivol:), noseg)
            else
                ! replace old by new volumes
                call copy_real_array_elements(a(ivol2:), a(ivol:), noseg)
            end if

            ! integrate the fluxes at dump segments fill asmass with mass
            if (ibflag > 0) then
                call integrate_fluxes_for_dump_areas(nflux, ndmpar, idt, itfact, a(iflxd:), &
                                                     a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            end if

            ! end of loop
            if (action == ACTION_FULLCOMPUTATION) then
                goto 10
            end if
            20 continue
            if (action == ACTION_FINALISATION .or. &
                action == ACTION_FULLCOMPUTATION) then

                ! close files, except monitor file
                call close_hydro_files(dlwqd%collcoll)
                call close_files(file_unit_list)

                ! write restart file
                call write_restart_map_file(file_unit_list, file_name_list, a(iconc:), itime, c(imnam), &
                                            c(isnam), notot, nosss)
            end if
        end associate
        9999 if (timon) call timstop(ithandl)
        dlwqd%iaflag = iaflag
        dlwqd%itime = itime
    end subroutine integration_scheme_11
end module m_integration_scheme_11
