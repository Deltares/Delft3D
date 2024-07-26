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
module m_integration_scheme_24
    use m_waq_precision
    use m_zercum
    use m_setset
    use m_integrate_areas_fluxes
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
    use time_dependent_variables, only: initialize_time_dependent_variables

    implicit none

contains

    !>  FCT horizontal, central implicit vertical, with adaptive timestep (24)
    !!  Performs time dependent integration. Flux Corrected Transport
    !!  (Boris and Book) horizontally, central implicit vertically.
    !!  The timestep is locally adjusted if the stability for a segment requires this.
    !!  Method has the option to treat additional velocities, like
    !!  settling of suspended matter, upwind to avoid wiggles.
    subroutine integration_scheme_24(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_write_output
        use m_dlwqf8
        use m_dlwqce
        use m_dlwqb3
        use m_dlwq41
        use m_dlwq19
        use m_dlwq17
        use m_dlwq15a
        use m_dlwq14
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
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics
        use m_real_array_indices          ! Pointers in real array workspace
        use m_integer_array_indices          ! Pointers in integer array workspace
        use m_character_array_indices          ! Pointers in character array workspace
        use m_dlwqdata_save_restore

        type(waq_data_buffer), target :: buffer                  !< System total array space
        integer(kind=int_wp), intent(inout) :: file_unit_list(*) !< Array with logical unit numbers
        character(len=*), intent(in) :: file_name_list(*)        !< Array with file names
        integer(kind=int_wp), intent(in) :: action               !< Span of the run or type of action to perform
                                                                 !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data), target :: dlwqd                       !< DELWAQ data structure
        type(GridPointerColl) :: gridps                          !< Collection of all grid definitions

        ! Local variables
        logical imflag, idflag, ihflag
        logical lrewin, ldumm2
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
        integer(kind=int_wp) :: i

        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt("integration_scheme_24", ithandl)
                goto 20
            end if

            if (ACTION == ACTION_INITIALISATION .or. &
                ACTION == ACTION_FULLCOMPUTATION) then

                ! some initialisation
                ithandl = 0
                ITIME = ITSTRT
                NSTEP = (ITSTOP - ITSTRT)/IDT
                IFFLAG = 0
                IAFLAG = 0
                IBFLAG = 0
                if (mod(INTOPT, 16) >= 8) IBFLAG = 1
                LDUMMY = .false.
                if (num_dispersion_arrays_new == 0) then
                    NDDIM = num_dispersion_arrays
                else
                    NDDIM = num_dispersion_arrays_new
                end if
                if (num_velocity_arrays_new == 0) then
                    NVDIM = num_velocity_arrays
                else
                    NVDIM = num_velocity_arrays_new
                end if
                LSTREC = ICFLAG == 1
                FORESTER = btest(INTOPT, 6)
                NOWARN = 0

                ! initialize second volume array with the first one
                nosss = num_cells + num_cells_bottom
                call copy_real_array_elements(A(IVOL:), A(IVOL2:), nosss)
            end if

            ! Save/restore the local persistent variables,
            ! if the computation is split up in steps
            ! Note: the handle to the timer (ithandl) needs to be
            ! properly initialised and restored
            if (ACTION == ACTION_INITIALISATION) then
                if (timon) call timstrt("integration_scheme_24", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop(ithandl)
                return
            end if

            if (ACTION == ACTION_SINGLESTEP) then
                call dlwqdata_restore(dlwqd)
            end if

            ! adaptations for layered bottom 08-03-2007  lp
            nosss = num_cells + num_cells_bottom
            NOQTT = num_exchanges + num_exchanges_bottom_dir
            inwtyp = intyp + num_boundary_conditions

            ! set alternating set of pointers
            noqt = num_exchanges_u_dir + num_exchanges_v_dir
            lnoq = noqtt - noqt
            ldisp = idisp + 2
            ldiff = idnew + nddim*noqt
            larea = iarea + noqt
            lflow = iflow + noqt
            lleng = ileng + noqt*2
            lvelo = ivnew + nvdim*noqt
            lxpnt = ixpnt + noqt*4
            lqdmp = iqdmp + noqt

            if (timon) call timstrt("integration_scheme_24", ithandl)

            !======================= simulation loop ============================
            10 continue

            ! Determine the volumes and areas that ran dry at start of time step
            call hsurf(nosss, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                       c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(num_cells, nosss, num_layers, a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, &
                        a(iarea:), num_constants, c(icnam:), a(icons:), surface, &
                        j(iknmr:), iknmkv)

            ! user transport processes
            call dlwqtr(num_substances_total, num_cells, num_exchanges, num_exchanges_u_dir, &
                num_exchanges_v_dir, num_exchanges_z_dir, num_spatial_parameters, &
                j(ixpnt:), a(ivol:), &
                a(ileng:), a(iparm:), &
                c(ipnam:), ilflag)

            ! Temporary ? set the variables grid-setting for the DELWAQ variables
            call setset(file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                        num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                        num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                        nopred, num_vars, num_grids, j(ivset:))

            ! Return conc and take-over from previous step or initial condition,
            ! and do particle tracking of this step (will be back-coupled next call)
            call delpar01(itime, num_cells, num_layers, num_exchanges, num_substances_transported, &
                          num_substances_total, a(ivol:), surface, a(iflow:), c(isnam:), &
                          num_spatial_time_fuctions, c(isfna:), a(isfun:), a(imass:), a(iconc:), &
                          iaflag, intopt, num_monitoring_cells, j(isdmp:), a(idmps:), &
                          a(imas2:))

            call proces(num_substances_total, nosss, a(iconc:), a(ivol:), itime, &
                        idt, a(iderv:), ndmpar, num_processes_activated, num_fluxes, &
                        j(iipms:), j(insva:), j(iimod:), j(iiflu:), j(iipss:), &
                        a(iflux:), a(iflxd:), a(istoc:), ibflag, bloom_status_ind, &
                        bloom_ind, a(imass:), num_substances_transported, &
                        itfact, a(imas2:), iaflag, intopt, a(iflxi:), &
                        j(ixpnt:), p_iknmkv, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                        num_exchanges_bottom_dir, num_dispersion_arrays_new, j(idpnw:), a(idnew:), num_dispersion_arrays, &
                        j(idpnt:), a(idiff:), num_dispersion_arrays_extra, a(idspx:), a(idsto:), &
                        num_velocity_arrays_new, j(ivpnw:), a(ivnew:), num_velocity_arrays, j(ivpnt:), &
                        a(ivelo:), num_velocity_arrays_extra, a(ivelx:), a(ivsto:), a(idmps:), &
                        j(isdmp:), j(ipdmp:), ntdmpq, a(idefa:), j(ipndt:), &
                        j(ipgrd:), j(ipvar:), j(iptyp:), j(ivarr:), j(ividx:), &
                        j(ivtda:), j(ivdag:), j(ivtag:), j(ivagg:), j(iapoi:), &
                        j(iaknd:), j(iadm1:), j(iadm2:), j(ivset:), j(ignos:), &
                        j(igseg:), num_vars, a, num_grids, num_monitoring_cells, &
                        c(iprna:), intsrt, &
                        j(iprvpt:), j(iprdon:), num_input_ref, j(ipror:), num_defaults, &
                        surface, file_unit_list(19))

            ! set new boundaries
            if (itime >= 0) then
                ! first: adjust boundaries by OpenDA
                if (dlwqd%inopenda) then
                    do ibnd = 1, num_boundary_conditions
                        do isys = 1, num_substances_transported
                            call get_openda_buffer(isys, ibnd, 1, 1, &
                                                   A(ibset:+(ibnd - 1)*num_substances_transported + isys - 1))
                        end do
                    end do
                end if
                call thatcher_harleman_bc(a(ibset:), a(ibsav:), j(ibpnt:), num_boundary_conditions, num_substances_transported, &
                                          num_substances_total, idt, a(iconc:), a(iflow:), a(iboun:))
            end if

            call write_output(num_substances_total, nosss, num_spatial_parameters, num_spatial_time_fuctions, ITIME, &
                              C(IMNAM:), C(ISNAM:), C(IDNAM:), J(IDUMP:), num_monitoring_points, &
                              A(ICONC:), A(ICONS:), A(IPARM:), A(IFUNC:), A(ISFUN:), &
                              A(IVOL:), num_constants, num_time_functions, IDT, num_output_files, &
                              file_name_list, file_unit_list, J(IIOUT:), J(IIOPO:), A(IRIOB:), &
                              C(IOSNM:), C(IOUNI:), C(IODSC:), C(ISSNM:), C(ISUNI:), C(ISDSC:), &
                              C(IONAM:), num_cells_u_dir, num_cells_v_dir, J(IGRID:), C(IEDIT:), &
                              num_substances_transported, A(IBOUN:), J(ILP:), A(IMASS:), A(IMAS2:), &
                              A(ISMAS:), num_fluxes, A(IFLXI:), ISFLAG, IAFLAG, &
                              IBFLAG, IMSTRT, IMSTOP, IMSTEP, IDSTRT, &
                              IDSTOP, IDSTEP, IHSTRT, IHSTOP, IHSTEP, &
                              IMFLAG, IDFLAG, IHFLAG, num_local_vars, A(IPLOC:), &
                              num_defaults, A(IDEFA:), ITSTRT, ITSTOP, NDMPAR, &
                              C(IDANA:), NDMPQ, num_monitoring_cells, J(IQDMP:), J(ISDMP:), &
                              J(IPDMP:), A(IDMPQ:), A(IDMPS:), A(IFLXD:), NTDMPQ, &
                              C(ICBUF:), num_transects, num_transect_exchanges, J(IORAA:), J(NQRAA:), &
                              J(IQRAA:), A(ITRRA:), C(IRNAM:), A(ISTOC:), num_grids, &
                              num_vars, J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), &
                              J(IAKND:), J(IAPOI:), J(IADM1:), J(IADM2:), J(IVSET:), &
                              J(IGNOS:), J(IGSEG:), A, num_boundary_conditions, num_boundary_types, &
                              C(IBTYP:), J(INTYP:), C(ICNAM:), noqtt, J(IXPNT:), &
                              INTOPT, C(IPNAM:), C(IFNAM:), C(ISFNA:), J(IDMPB:), &
                              num_waste_loads, num_waste_load_types, C(IWTYP:), J(IWAST:), J(INWTYP:), &
                              A(IWDMP:), iknmkv, isegcol)

            ! zero cummulative array's
            if (imflag .or. (ihflag .and. num_transects > 0)) then
                call set_cumulative_arrays_zero(num_substances_total, num_substances_transported, num_fluxes, ndmpar, ndmpq, &
                                                num_monitoring_cells, a(ismas:), a(iflxi:), a(imas2:), &
                                                a(idmpq:), a(idmps:), num_transects, imflag, ihflag, &
                                                a(itrra:), ibflag, num_waste_loads, a(iwdmp:))
            end if

            ! simulation done ?
            if (itime < 0) goto 9999
            if (itime >= itstop) goto 20

            ! add processes
            call scale_processes_derivs_and_update_balances(a(iderv:), num_substances_total, nosss, itfact, a(imas2:), &
                                                            idt, iaflag, a(idmps:), intopt, j(isdmp:))

            ! get new volumes
            itimel = itime
            itime = itime + idt
            select case (ivflag)
            case (1) ! computation of volumes for computed volumes only
                call copy_real_array_elements(a(ivol:), a(ivol2:), num_cells)
                call dlwqb3(a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), num_substances_total, &
                            num_exchanges, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                            a(imas2:), idt, iaflag, num_substances_transported, a(idmpq:), &
                            ndmpq, j(iqdmp:))
                updatr = .true.
            case (2) ! the fraudulent computation option
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                            j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivoll:), &
                            j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                            updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                            lstrec, lrewin, a(ivol2:), dlwqd)
                call dlwqf8(num_cells, num_exchanges, j(ixpnt:), idt, iknmkv, &
                            a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               !     read new volumes from files
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                            j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivol2:), &
                            j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                            updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                            lstrec, lrewin, a(ivoll:), dlwqd)
            end select

            ! update the info on dry volumes with the new volumes
            call identify_wet_cells(num_cells, nosss, a(ivol2:), num_layers, num_constants, &
                        c(icnam:), a(icons:), surface, j(iknmr:), iknmkv)

            ! add the waste loads
            call dlwq15a(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
                         num_waste_load_types, num_monitoring_cells, intopt, idt, itime, &
                         iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                         a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                         j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                         wdrawal, iknmkv, num_spatial_parameters, c(ipnam:), a(iparm:), &
                         num_spatial_time_fuctions, c(isfna:), a(isfun:), j(isdmp:), a(idmps:), &
                         a(imas2:), a(iwdmp:), 1, num_substances_total)

            ! self adjusting time step size method
            call dlwq19(file_unit_list(19), num_substances_transported, num_substances_total, num_substances_part, num_cells, &
                        nosss, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges, &
                        num_exchanges_bottom_dir, nddim, nvdim, a(idisp:), a(idnew:), &
                        a(ivnew:), a(ivol:), a(ivol2:), a(iarea:), a(iflow:), &
                        surface, a(ileng:), j(ixpnt:), j(idpnw:), j(ivpnw:), &
                        a(imass:), a(iconc:), dconc2, a(iboun:), idt, &
                        ibas, ibaf, dwork, volint, iords, &
                        iordf, a(iderv:), wdrawal, iaflag, a(imas2:), &
                        ndmpq, num_monitoring_cells, num_waste_loads, j(iqdmp:), a(idmpq:), &
                        j(isdmp:), a(idmps:), j(iwast:), a(iwdmp:), intopt, &
                        ilflag, arhs, adiag, acodia, bcodia, &
                        nvert, ivert, num_constants, c(icnam:), a(icons:))

            ! new time values, volumes excluded
            call initialize_time_dependent_variables(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                                                     j(inrha:), j(inrh2:), j(inrft:), idt, a(ivol:), &
                                                     a(idiff:), a(iarea:), a(iflow:), a(ivelo:), a(ileng:), &
                                                     a(iwste:), a(ibset:), a(icons:), a(iparm:), a(ifunc:), &
                                                     a(isfun:), j(ibulk:), file_name_list, c(ilunt:), ftype, &
                                                     intsrt, isflag, ifflag, ivflag, ilflag, &
                                                     ldumm2, j(iktim:), j(iknmr:), j(inisp:), a(inrsp:), &
                                                     j(intyp:), j(iwork:), .false., ldummy, rdummy, &
                                                     .false., gridps, dlwqd)

            ! calculate closure error
            if (lrewin .and. lstrec) then
                call dlwqce(a(imass:), a(ivoll:), a(ivol2:), num_substances_transported, num_substances_total, &
                            num_cells, file_unit_list(19))
                call copy_real_array_elements(a(ivoll:), a(ivol:), num_cells)
            else
                ! replace old by new volumes
                call copy_real_array_elements(a(ivol2:), a(ivol:), num_cells)
            end if

            ! integrate the fluxes at dump segments fill ASMASS with mass
            if (ibflag > 0) then
                call integrate_fluxes_for_dump_areas(num_fluxes, ndmpar, idt, itfact, a(iflxd:), &
                                                     a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            end if

            ! end of loop
            if (ACTION == ACTION_FULLCOMPUTATION) goto 10

            20 continue

            if (ACTION == ACTION_FINALISATION .or. &
                ACTION == ACTION_FULLCOMPUTATION) then

                call close_hydro_files(dlwqd%collcoll)
                call close_files(file_unit_list)

                call write_restart_map_file(file_unit_list, file_name_list, A(ICONC:), ITIME, C(IMNAM:), &
                                            C(ISNAM:), num_substances_total, NOSSS)
            end if

        end associate
        9999    if (timon) call timstop(ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime
    end subroutine integration_scheme_24
end module m_integration_scheme_24
