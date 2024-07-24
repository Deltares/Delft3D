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
    use m_integrate_areas_fluxes
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use time_dependent_variables, only: initialize_time_dependent_variables
    use m_write_output
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells

    implicit none

contains


    !> Flux Corrected Transport (FCT) according to Boris and Book (5)
    !! 3D flux corrected transport according to Boris and Book.
    !! Explicit in time. Very fast and accurate solution. Time
    !! step size is limited by a stability criterion.
    subroutine integration_scheme_5(buffer, file_unit_list, file_name_list, &
            action, dlwqd, gridps)

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
        use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
        use m_write_restart_map_file
        use m_delpar01
        use m_array_manipulation, only : copy_real_array_elements
        use data_processing, only : close_files
        use m_grid_utils_external
        use timers
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use variable_declaration          ! module with the more recently added arrays
        use m_actions
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics
        use m_real_array_indices          ! Pointers in real array workspace
        use m_integer_array_indices          ! Pointers in integer array workspace
        use m_character_array_indices          ! Pointers in character array workspace
        use m_dlwqdata_save_restore

        implicit none

        type(waq_data_buffer), target         :: buffer              !< System total array space
        integer(kind = int_wp), intent(inout) :: file_unit_list  (*) !< Array with logical unit numbers
        character(len=*),       intent(in)    :: file_name_list(*)   !< Array with file names
        integer(kind = int_wp), intent(in)    :: action              !< Span of the run or type of action to perform
                                                                     !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data),      target        :: dlwqd               !< DELWAQ data structure
        type(GridPointerColl)                 :: gridps              !< Collection of all grid definitions


        ! Local variables
        logical :: imflag                 !< True if monitoring took place, set in write_output, used in zercum
        logical :: ihflag                 !< True if history    took place, set in write_output, used in zercum
        logical :: idflag                 !< True if dump       took place, set in write_output, not used
        logical :: lrewin                 !< True if rewind     took place, set in dlwq41, used for closure error corr.
        logical :: ldumm2                 !< Dummy logical, parameter in initialize_time_dependent_variables
        real(kind = real_wp) :: rdummy(1) !< Dummy real in initialize_time_dependent_variables
        integer(kind = int_wp) :: nstep   !< Number of time steps (does not work if idt is time varying !)
        integer(kind = int_wp) :: ierr    !< Error variable
        integer(kind = int_wp) :: ibnd    !< Loop counter boundaries (loop should be in a called subroutine !)
        integer(kind = int_wp) :: isys    !< Loop counter substances (loop should be in a called subroutine !)
        
        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt("integration_scheme_5", ithandl)
                goto 20
            end if

            IF (ACTION == ACTION_INITIALISATION  .OR. &
                    ACTION == ACTION_FULLCOMPUTATION) THEN

                ! some initialisation
                ithandl = 0
                itime = itstrt
                nstep = (itstop - itstrt) / idt
                ifflag = 0
                iaflag = 0
                ibflag = 0

                ! Dummy variables - used in DLWQD
                itimel = itime
                lleng = 0
                ioptzb = 0
                nopred = 6
                NOWARN = 0
                tol = 0.0D0
                forester = .false.
                updatr = .false.
                noqt = num_exchanges + num_exchanges_bottom_dir

                if (btest(intopt, 3)) ibflag = 1
                ldummy = .false.
                if (num_dispersion_arrays_new == 0) then
                    nddim = num_dispersion_arrays
                else
                    nddim = num_dispersion_arrays_new
                end if
                if (num_velocity_arrays_new == 0) then
                    nvdim = num_velocity_arrays
                else
                    nvdim = num_velocity_arrays_new
                end if
                lstrec = icflag == 1
                nosss = num_cells + num_cells_bottom
                noqtt = num_exchanges + num_exchanges_bottom_dir
                inwtyp = intyp + num_boundary_conditions
                ! initialize second volume array with the first one
                call copy_real_array_elements(A(IVOL:), A(IVOL2:), nosss)
            end if

            !
            !     Save/restore the local persistent variables,
            !     if the computation is split up in steps
            !
            !     Note: the handle to the timer (ithandl) needs to be
            !     properly initialised and restored
            !
            IF (ACTION == ACTION_INITIALISATION) THEN
                if (timon) call timstrt("integration_scheme_5", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop(ithandl)
                RETURN
            end if

            IF (ACTION == ACTION_SINGLESTEP) THEN
                call dlwqdata_restore(dlwqd)
            end if

            if (timon) call timstrt("integration_scheme_5", ithandl)

            !======================= simulation loop ============================

            10 continue

            ! Determine the volumes and areas that ran dry at start of time step
            call hsurf(num_cells, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
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

            !jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

            call setset(file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                    num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                    num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                    nopred, num_vars, num_grids, j(ivset:))

            !        return conc and take-over from previous step or initial condition,
            !        and do particle tracking of this step (will be back-coupled next call)
            call delpar01(itime, num_cells, num_layers, num_exchanges, num_substances_transported, &
                    num_substances_total, a(ivol:), surface, a(iflow:), c(isnam:), &
                    num_spatial_time_fuctions, c(isfna:), a(isfun:), a(imass:), a(iconc:), &
                    iaflag, intopt, num_monitoring_cells, j(isdmp:), a(idmps:), &
                    a(imas2:))

            ! call PROCES subsystem
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
                if (dlwqd%inopenda) then
                    do ibnd = 1, num_boundary_conditions
                        do isys = 1, num_substances_transported
                            call get_openda_buffer(isys, ibnd, 1, 1, &
                                    A(ibset:+(ibnd - 1) * num_substances_transported + isys - 1))
                        end do
                    end do
                end if
                call thatcher_harleman_bc(a(ibset:), a(ibsav:), j(ibpnt:), num_boundary_conditions, num_substances_transported, &
                        num_substances_total, idt, a(iconc:), a(iflow:), a(iboun:))
            endif

            ! Call OUTPUT system
            call write_output(num_substances_total, nosss, num_spatial_parameters, num_spatial_time_fuctions, itime, &
                    c(imnam:), c(isnam:), c(idnam:), j(idump:), num_monitoring_points, &
                    a(iconc:), a(icons:), a(iparm:), a(ifunc:), a(isfun:), &
                    a(ivol:), num_constants, num_time_functions, idt, num_output_files, &
                    file_name_list, file_unit_list, j(iiout:), j(iiopo:), a(iriob:), &
                    c(iosnm:), c(iouni:), c(iodsc:), c(issnm:), c(isuni:), c(isdsc:), &
                    c(ionam:), num_cells_u_dir, num_cells_v_dir, j(igrid:), c(iedit:), &
                    num_substances_transported, a(iboun:), j(ilp:), a(imass:), a(imas2:), &
                    a(ismas:), num_fluxes, a(iflxi:), isflag, iaflag, &
                    ibflag, imstrt, imstop, imstep, idstrt, &
                    idstop, idstep, ihstrt, ihstop, ihstep, &
                    imflag, idflag, ihflag, num_local_vars, a(iploc:), &
                    num_defaults, a(idefa:), itstrt, itstop, ndmpar, &
                    c(idana:), ndmpq, num_monitoring_cells, j(iqdmp:), j(isdmp:), &
                    j(ipdmp:), a(idmpq:), a(idmps:), a(iflxd:), ntdmpq, &
                    c(icbuf:), num_transects, num_transect_exchanges, j(ioraa:), j(nqraa:), &
                    j(iqraa:), a(itrra:), c(irnam:), a(istoc:), num_grids, &
                    num_vars, j(ivarr:), j(ividx:), j(ivtda:), j(ivdag:), &
                    j(iaknd:), j(iapoi:), j(iadm1:), j(iadm2:), j(ivset:), &
                    j(ignos:), j(igseg:), a, num_boundary_conditions, num_boundary_types, &
                    c(ibtyp:), j(intyp:), c(icnam:), noqtt, j(ixpnt:), &
                    intopt, c(ipnam:), c(ifnam:), c(isfna:), j(idmpb:), &
                    num_waste_loads, num_waste_load_types, c(iwtyp:), j(iwast:), j(inwtyp:), &
                    a(iwdmp:), iknmkv, isegcol)

            ! zero cummulative array's
            if (imflag .or. (ihflag .and. num_transects > 0)) then
                call set_cumulative_arrays_zero (num_substances_total, num_substances_transported, num_fluxes, ndmpar, ndmpq, &
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
            case (1)                 ! computation of volumes for computed volumes only
                call copy_real_array_elements(a(ivol:), a(ivol2:), num_cells)
                call dlwqb3(a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), num_substances_total, &
                        num_exchanges, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                        a(imas2:), idt, iaflag, num_substances_transported, a(idmpq:), &
                        ndmpq, j(iqdmp:))
                updatr = .true.
            case (2)                 ! the fraudulent computation option
                call dlwq41(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivoll:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivol2:), dlwqd)
                if (lrewin) call copy_real_array_elements (a(ivol2:), a(ivoll:), num_cells)
                call dlwqf8(num_cells, num_exchanges, j(ixpnt:), idt, iknmkv, &
                        a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               ! read new volumes from files
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
            call dlwq15(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
                    num_waste_load_types, num_monitoring_cells, intopt, idt, itime, &
                    iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                    a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                    j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                    iknmkv, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                    c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                    a(iwdmp:), 1, num_substances_total)

            ! do the transport itself
            call first_step_fct(num_substances_transported, num_substances_total, nosss, noqtt, nvdim, &
                    a(ivnew:), a(iarea:), a(iflow:), j(ixpnt:), j(ivpnw:), &
                    a(iconc:), a(iboun:), idt, a(iderv:), iaflag, &
                    a(imas2:))

            ! set the first guess in array CONC2 == ITIMR
            call update_concs_explicit_time_step (num_substances_transported, num_substances_total, num_substances_part, nosss, a(ivol2:), &
                    surface, a(imass:), a(itimr:), a(iderv:), idt, &
                    ivflag, file_unit_list(19))

            ! perform the flux correction on conc2 == a(itimr:)
            call dlwq51(num_substances_transported, num_substances_total, nosss, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, noqtt, nddim, nvdim, a(idisp:), &
                    a(idnew:), a(ivnew:), a(ivol2:), a(iarea:), a(iflow:), &
                    a(ileng:), j(ixpnt:), iknmkv, j(idpnw:), j(ivpnw:), &
                    a(iconc:), a(itimr:), a(iboun:), intopt, ilflag, &
                    idt, iaflag, a(imas2:), ndmpq, j(iqdmp:), &
                    a(idmpq:))
            call dlwq52(num_substances_transported, num_substances_total, nosss, a(ivol2:), a(imass:), &
                    a(itimr:), a(iconc:))

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

            if (ACTION == ACTION_FINALISATION    .or. &
                    ACTION == ACTION_FULLCOMPUTATION) then
                ! close files, except monitor file

                call close_hydro_files(dlwqd%collcoll)
                call close_files(file_unit_list)

                ! write restart file
                call write_restart_map_file(file_unit_list, file_name_list, a(iconc:), itime, c(imnam:), &
                        c(isnam:), num_substances_total, nosss)
            end if

        end associate
        9999 if (timon) call timstop (ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime
    end subroutine integration_scheme_5
end module m_integration_scheme_5
