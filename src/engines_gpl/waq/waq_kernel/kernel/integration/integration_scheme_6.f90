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
module m_integration_scheme_6
    use m_waq_precision
    use m_hsurf
    use m_dlwqtr
    use m_write_output
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes

    implicit none

contains

    !> Upwind advection, direct stationaly method (6)
    !! Stationairy solution. Upwind 1st order.
    !! Fully implicit with a direct method.\n
    !! Matrices become very large in 3D and method unworkable. In 2D
    !! the method can be used. In 1D the method outperforms the
    !! iterative methods.
    subroutine integration_scheme_6(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_dlwq67
        use m_dlwq66
        use m_dlwq65
        use m_dlwq64
        use m_dlwq63
        use m_dlwq62
        use m_dlwq61
        use m_dlwq60
        use m_dlwq41
        use m_dlwq15
        use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
        use m_write_restart_map_file
        use m_delmat
        use m_array_manipulation, only: initialize_real_array
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

        type(waq_data_buffer), target :: buffer              !< System total array space
        integer(kind=int_wp), intent(inout) :: file_unit_list(*) !< Array with logical unit numbers
        character(len=*), intent(in) :: file_name_list(*)   !< Array with file names
        integer(kind=int_wp), intent(in) :: action              !< Span of the run or type of action to perform
        !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data), target :: dlwqd               !< DELWAQ data structure
        type(GridPointerColl) :: gridps              !< Collection of all grid definitions

        ! Local variables
        logical imflag, idflag, ihflag
        logical ldummy, lstrec, lrewin

        integer(kind=int_wp) :: itime
        integer(kind=int_wp) :: itimel
        integer(kind=int_wp) :: iaflag
        integer(kind=int_wp) :: ibflag
        integer(kind=int_wp) :: isys
        integer(kind=int_wp) :: icsys
        integer(kind=int_wp) :: nsys
        integer(kind=int_wp) :: inwtyp
        integer(kind=int_wp) :: i
        integer(kind=int_wp) :: nosss
        integer(kind=int_wp) :: noqtt

        integer(kind=int_wp) :: ithandl

        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
            !
            ! Distinguishing the actions is superfluous:
            ! there is only one step
            !
            if (action == action_initialisation .or. &
                action == action_finalisation) then
                return
            end if

            ! some initialisation
            ithandl = 0
            if (timon) call timstrt("integration_scheme_6", ithandl)

            ITIMEL = ITSTRT
            ITIME = ITSTRT + IDT
            IBFLAG = 0
            if (mod(INTOPT, 16) >= 8) IBFLAG = 1
            call initialize_real_array(A(IMAS2:), NOTOT*5)
            LDUMMY = .false.
            LSTREC = .false.
            nosss = noseg + nseg2
            NOQTT = NOQ + NOQ4
            inwtyp = intyp + nobnd

            ! Determine the volumes and areas that ran dry,
            ! They cannot have explicit processes during this time step
            call hsurf(noseg, nopa, c(ipnam:), a(iparm:), nosfun, &
                       c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(noseg, nosss, nolay, a(ivol:), &
                        noq1 + noq2, a(iarea:), nocons, c(icnam:), a(icons:), surface, &
                        j(iknmr:), iknmkv)

            ! make closure error correction
            if (idt == 0) then

                call initialize_real_array(a(ivol2:), noseg)
            else if (j(inrh2 + 1) >= 0 .and. ivflag == 0) then
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                            j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                            j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                            ldummy, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                            lstrec, lrewin, a(ivoll:), dlwqd)
                call dlwq65(a(ivol2:), a(ivol:), idt, noseg)
            else
                call initialize_real_array(a(ivol2:), noseg)
                write (file_unit_list(19), 1000)
            end if

            ! loop over the systems
            nsys = 1
            iaflag = 1
            do isys = 1, nosys
                if (isys == nosys) nsys = 1 + notot - nosys

                ! do the user transport processes
                icsys = isys
                call dlwqtr(notot, nosys, noseg, noq, noq1, &
                            noq2, noq3, nopa, nosfun, nodisp, &
                            novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                            a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                            a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), icsys, &
                            idt, c(isnam:), nocons, nofun, c(icnam:), &
                            c(ipnam:), c(ifnam:), c(isfna:), ldummy, ilflag)

                ! do the user water quality processes
                call dlwq60(a(iderv:), a(iconc:), notot, noseg, itfact, &
                            a(imas2:), isys, nsys, a(idmps:), intopt, &
                            j(isdmp:))

                ! add the waste loads
                call dlwq15(nosys, notot, noseg, noq, nowst, &
                            nowtyp, ndmps, intopt, 1, itime, &
                            iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                            a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                            j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                            iknmkv, nopa, c(ipnam:), a(iparm:), nosfun, &
                            c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                            a(iwdmp:), isys, nsys)

                ! fill the matrix
                call dlwq61(a(iconc:), a(iderv:), a(ivol2:), a(itimr:), noseg, &
                            notot, isys, nsys, jtrack)
                call dlwq62(a(idisp:), a(idiff:), a(iarea:), a(iflow:), a(ileng:), &
                            a(ivelo:), a(iboun:), j(ixpnt:), notot, isys, &
                            nsys, noq1, noq2, noq, nodisp, &
                            novelo, j(idpnt:), j(ivpnt:), a(iderv:), a(itimr:), &
                            jtrack, intopt, ilflag)
                call dlwq67(a(itimr:), noseg, jtrack)

                ! invert the matrix and store the results
                call delmat(noseg, jtrack, jtrack, nsys, a(itimr:), &
                            a(iderv:), 0)
                call dlwq63(a(iconc:), a(iderv:), a(imas2:), noseg, notot, &
                            isys, nsys, a(idmps:), intopt, j(isdmp:))
            end do

            ! mass balance
            iaflag = 1
            call dlwq64(a(idisp:), a(idiff:), a(iarea:), a(iflow:), a(ileng:), &
                        a(ivelo:), a(iconc:), a(iboun:), j(ixpnt:), nosys, &
                        notot, noq1, noq2, noq, nodisp, &
                        novelo, j(idpnt:), j(ivpnt:), intopt, a(imas2:), &
                        ilflag, a(idmpq:), ndmpq, j(iqdmp:))
            call dlwq66(a(iderv:), a(ivol:), a(iconc:), notot, noseg)

            ! Call OUTPUT system
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

            ! close files, except monitor file
            call close_hydro_files(dlwqd%collcoll)
            call close_files(file_unit_list)

            ! write restart file
            call write_restart_map_file(file_unit_list, file_name_list, a(iconc:), itstrt, c(imnam:), &
                    c(isnam:), notot, noseg)

            1000 FORMAT ('No closure error corrections !')

        end associate
        if (timon) call timstop(ithandl)
    end subroutine integration_scheme_6

end module m_integration_scheme_6
