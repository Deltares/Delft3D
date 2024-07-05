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
module m_integration_scheme_18
    use m_waq_precision
    use m_sgmres
    use m_setset
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use m_write_output
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes

    implicit none

    contains


        
    !> Iterative stationary vertical central method (18)
    !! Performs substance by substance a stationairy solution.
    !! Uses the GMRES method with Krylov sub-spaces.
    !! Horizontal fluxes are discretized upwind.
    !! Vertical fluxes are discretized central.
    !! like 6 but steady state solver
    !! is GMRES and compact storage from option 16
    subroutine integration_scheme_18(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_dlwqg3
        use m_dlwqh6
        use m_dlwqh3
        use m_dlwqh1
        use m_dlwqf5
        use m_dlwqf1
        use m_dlwq66
        use m_dlwq65
        use m_dlwq64
        use m_dlwq60
        use m_dlwq41
        use m_dlwq15
        use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
        use m_write_restart_map_file
        use m_array_manipulation, only : initialize_real_array
        use data_processing, only : close_files
        use m_grid_utils_external
        use timers
        use variable_declaration
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace


        type(waq_data_buffer), target         :: buffer              !< System total array space
        integer(kind = int_wp), intent(inout) :: file_unit_list  (*) !< Array with logical unit numbers
        character(len=*),       intent(in)    :: file_name_list(*)   !< Array with file names
        integer(kind = int_wp), intent(in)    :: action              !< Span of the run or type of action to perform
                                                                     !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data),      target        :: dlwqd               !< DELWAQ data structure
        type(GridPointerColl)                 :: gridps              !< Collection of all grid definitions

        ! Local variables
        logical          imflag, idflag, ihflag
        logical          ldummy, lstrec, lrewin
        logical, save :: litrep
        logical          update
        real(kind = dp) :: tol

        integer(kind = int_wp) :: itime
        integer(kind = int_wp) :: itimel
        integer(kind = int_wp) :: ifflag
        integer(kind = int_wp) :: iaflag
        integer(kind = int_wp) :: ibflag
        integer(kind = int_wp) :: nddim
        integer(kind = int_wp) :: nvdim
        integer(kind = int_wp) :: isys
        integer(kind = int_wp) :: icsys
        integer(kind = int_wp) :: nsys
        integer(kind = int_wp) :: inwtyp
        integer(kind = int_wp) :: istep
        integer(kind = int_wp) :: ith
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: iscale
        integer(kind = int_wp) :: nopred
        integer(kind = int_wp) :: iter
        integer(kind = int_wp) :: ioptpc
        integer(kind = int_wp) :: nosss
        integer(kind = int_wp) :: noqtt

        integer(kind = int_wp) :: ithandl
        integer(kind = int_wp), save :: ithand1 = 0 ! Leave local
        integer(kind=int_wp), pointer :: p_iknmkv(:)

        p_iknmkv(1:size(iknmkv)) => iknmkv
        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_INITIALISATION  .or. &
                    action == ACTION_FINALISATION) then
                return
            endif

            ! Initialize solver parameters
            ithandl = 0
            if (timon) call timstrt("integration_scheme_18", ithandl)

            call initialize_gmres(file_unit_list(19), nocons, c(icnam:), a(icons:), ioptpc, &
                    iter, tol, iscale, litrep, noseg, &
                    noq3, noq, novec, nomat, &
                    nolay, intsrt, intopt)

            itime = itstrt + idt
            ifflag = 0
            iaflag = 0
            ibflag = 0
            if (mod(intopt, 16) >= 8) ibflag = 1
            if (ndspn == 0) then
                nddim = nodisp
            else
                nddim = ndspn
            endif
            if (nveln == 0) then
                nvdim = novelo
            else
                nvdim = nveln
            endif
            lstrec = icflag == 1
            nosss = noseg + nseg2
            noqtt = noq + noq4
            inwtyp = intyp + nobnd

            ! Initialize pointer matrices for fast solvers
            call dlwqf1(noseg, nobnd, noq, noq1, noq2, &
                    nomat, j(ixpnt:), j(iwrk:), j(imat:), rowpnt, &
                    fmat, tmat)

            iexseg = 1     !  There is nothing to mask. This array is meant for method 21

            !======================= simulation loop ============================
            ! make closure error correction
            if (j(inrh2 + 1) >= 0 .and. ivflag == 0 .and. &
                    idt       > 0 .and. lstrec) then
                call dlwq41(file_unit_list, itstrt + idt, itstrt, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        update, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
                call dlwq65(a(ivol2:), a(ivol:), idt, noseg)
            else
                call initialize_real_array(a(ivol2:), noseg)
            endif

            ! Determine the volumes and areas that ran dry,
            ! They cannot have explicit processes during this time step
            call hsurf(noseg, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(noseg, nosss, nolay, a(ivol:), &
                    noq1 + noq2, a(iarea:), nocons, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            ! user transport processes
            call dlwqtr(notot, nosys, nosss, noq, noq1, &
                    noq2, noq3, nopa, nosfun, nodisp, &
                    novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam:), nocons, nofun, c(icnam:), &
                    c(ipnam:), c(ifnam:), c(isfna:), ldummy, ilflag)

            ! Temporary ? set the variables grid-setting for the DELWAQ variables
            call setset(file_unit_list(19), nocons, nopa, nofun, nosfun, &
                    nosys, notot, nodisp, novelo, nodef, &
                    noloc, ndspx, nvelx, nlocx, nflux, &
                    nopred, novar, nogrid, j(ivset:))

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
                    c(iprna:), intsrt, &
                    j(iprvpt:), j(iprdon:), nrref, j(ipror:), nodef, &
                    surface, file_unit_list(19))

            ! loop over the systems
            ith = 1
            iaflag = 1
            do isys = 1, nosys
                ! do the user water quality processes
                icsys = isys
                call dlwq60(a(iderv:), a(iconc:), notot, noseg, itfact, &
                        a(imas2:), isys, 1, a(idmps:), intopt, &
                        j(isdmp:))

                ! add the waste loads
                call dlwq15(nosys, notot, noseg, noq, nowst, &
                        nowtyp, ndmps, intopt, 1, itime, &
                        iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                        a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                        j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                        iknmkv, nopa, c(ipnam:), a(iparm:), nosfun, &
                        c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                        a(iwdmp:), isys, 1)

                ! fill the diagonal of the matrix, with conc-array and closure error
                call dlwqh1(noseg, notot, nobnd, isys, gm_diag(1, ith), &
                        a(ivol2:), a(iconc:))

                ! build rest of matrix like in option 16

                call dlwqg3(noseg, nobnd, noq1, noq2, noq, &
                        j(ixpnt:), nddim, nvdim, j(idpnw:), j(ivpnw:), &
                        a(iarea:), a(iflow:), a(ileng:), a(idisp:), a(idnew:), &
                        a(ivnew:), isys, intopt, ilflag, nomat, &
                        gm_amat(1, ith), j(imat:), rowpnt, gm_diag(1, ith), gm_diac(1:, ith), &
                        iscale, fmat, tmat, iknmkv)

                ! initial guess : take rhs / diagonal
                call dlwqh3(noseg, nosys, notot, nobnd, isys, &
                        a(iderv:), a(iboun:), gm_rhs(1, ith), gm_diac(1:, ith), gm_sol (1, ith))

                ! solve linear system of equations
                ! note that RHS is in A(IDERV:) for steady state otpions
                call sgmres(noseg + nobnd, gm_rhs (1, ith), gm_sol (1, ith), novec, gm_work(1, ith), &
                        noseg + nobnd, gm_hess(1, ith), novec + 1, iter, tol, &
                        nomat, gm_amat(1, ith), j(imat:), gm_diag(1, ith), rowpnt, &
                        nolay, ioptpc, nobnd, gm_trid(1, ith), iexseg (:, ith), &
                        file_unit_list(19), litrep)

                ! copy solution for this substance into concentration array, note that the array for
                ! segment dumps is not filled yet
                call dlwqh6(noseg, notot, isys, 1, a(iconc:), &
                        gm_sol(1, ith), a(imas2:), a(idmps:), intopt, j(isdmp:))
            end do

            ! mass balance
            iaflag = 1
            call dlwq64(a(idisp:), a(idnew:), a(iarea:), a(iflow:), a(ileng:), &
                    a(ivnew:), a(iconc:), a(iboun:), j(ixpnt:), nosys, &
                    notot, noq1, noq2, noq, nddim, &
                    nvdim, j(idpnw:), j(ivpnw:), intopt, a(imas2:), &
                    ilflag, a(idmpq:), ndmpq, j(iqdmp:))
            call dlwq66(a(iderv:), a(ivol:), a(iconc:), notot, noseg)

            call write_output(notot, noseg, nopa, nosfun, itstrt, &
                    c(imnam:), c(isnam:), c(idnam:), j(idump:), nodump, &
                    a(iconc:), a(icons:), a(iparm:), a(ifunc:), a(isfun:), &
                    a(ivol:), nocons, nofun, 1, noutp, &
                    file_name_list, file_unit_list, j(iiout:), j(iiopo:), a(iriob:), &
                    c(iosnm:), c(iouni:), c(iodsc:), c(issnm:), c(isuni:), c(isdsc:), &
                    c(ionam:), nx, ny, j(igrid:), c(iedit:), &
                    nosys, a(iboun:), j(ilp:), a(iderv:), a(imas2:), &
                    a(ismas:), nflux, a(iflxi:), isflag, iaflag, &
                    ibflag, imstrt, imstop, imstep, idstrt, &
                    idstop, idstep, ihstrt, ihstop, ihstep, &
                    imflag, idflag, ihflag, noloc, a(iploc:), &
                    nodef, a(idefa:), itstrt, itstop, ndmpar, &
                    c(idana:), ndmpq, ndmps, j(iqdmp:), j(isdmp:), &
                    j(ipdmp:), a(idmpq:), a(idmps:), a(iflxd:), ntdmpq, &
                    c(icbuf:), noraai, ntraaq, j(ioraa:), j(nqraa:), &
                    j(iqraa:), a(itrra:), c(irnam:), a(istoc:), nogrid, &
                    novar, j(ivarr:), j(ividx:), j(ivtda:), j(ivdag:), &
                    j(iaknd:), j(iapoi:), j(iadm1:), j(iadm2:), j(ivset:), &
                    j(ignos:), j(igseg:), a, nobnd, nobtyp, &
                    c(ibtyp:), j(intyp:), c(icnam:), noq, j(ixpnt:), &
                    intopt, c(ipnam:), c(ifnam:), c(isfna:), j(idmpb:), &
                    nowst, nowtyp, c(iwtyp:), j(iwast:), j(inwtyp:), &
                    a(iwdmp:), iknmkv, isegcol)

            call close_hydro_files(dlwqd%collcoll)
            call close_files(file_unit_list)

            call write_restart_map_file (file_unit_list, file_name_list, a(iconc:), itstrt, c(imnam:), &
                    c(isnam:), notot, noseg)
        end associate
        if (timon) call timstop (ithandl)
    end subroutine integration_scheme_18
end module m_integration_scheme_18
