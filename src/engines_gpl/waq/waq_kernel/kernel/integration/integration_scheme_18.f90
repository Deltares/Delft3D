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
        use m_waq_memory_dimensions          ! System characteristics
        use m_timer_variables          ! Timer characteristics
        use m_real_array_indices          ! Pointers in real array workspace
        use m_integer_array_indices          ! Pointers in integer array workspace
        use m_character_array_indices          ! Pointers in character array workspace


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

            call initialize_gmres(file_unit_list(19), num_constants, c(icnam:), a(icons:), ioptpc, &
                    iter, tol, iscale, litrep, num_cells, &
                    num_exchanges_z_dir, num_exchanges, num_fast_solver_vectors, fast_solver_arr_size, &
                    num_layers, intsrt, intopt)

            itime = itstrt + idt
            ifflag = 0
            iaflag = 0
            ibflag = 0
            if (mod(intopt, 16) >= 8) ibflag = 1
            if (num_dispersion_arrays_new == 0) then
                nddim = num_dispersion_arrays
            else
                nddim = num_dispersion_arrays_new
            endif
            if (num_velocity_arrays_new == 0) then
                nvdim = num_velocity_arrays
            else
                nvdim = num_velocity_arrays_new
            endif
            lstrec = icflag == 1
            nosss = num_cells + num_cells_bottom
            noqtt = num_exchanges + num_exchanges_bottom_dir
            inwtyp = intyp + num_boundary_conditions

            ! Initialize pointer matrices for fast solvers
            call dlwqf1(num_cells, num_boundary_conditions, num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, &
                    fast_solver_arr_size, j(ixpnt:), j(iwrk:), j(imat:), rowpnt, &
                    fmat, tmat)

            iexseg = 1     !  There is nothing to mask. This array is meant for method 21

            !======================= simulation loop ============================
            ! make closure error correction
            if (j(inrh2 + 1) >= 0 .and. ivflag == 0 .and. &
                    idt       > 0 .and. lstrec) then
                call dlwq41(file_unit_list, itstrt + idt, itstrt, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivol2:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        update, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
                call dlwq65(a(ivol2:), a(ivol:), idt, num_cells)
            else
                call initialize_real_array(a(ivol2:), num_cells)
            endif

            ! Determine the volumes and areas that ran dry,
            ! They cannot have explicit processes during this time step
            call hsurf(num_cells, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(num_cells, nosss, num_layers, a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, &
                    a(iarea:), num_constants, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            ! user transport processes
            call dlwqtr(num_substances_total, num_substances_transported, nosss, num_exchanges, num_exchanges_u_dir, &
                    num_exchanges_v_dir, num_exchanges_z_dir, num_spatial_parameters, num_spatial_time_fuctions, num_dispersion_arrays, &
                    num_velocity_arrays, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam:), num_constants, num_time_functions, c(icnam:), &
                    c(ipnam:), c(ifnam:), c(isfna:), ldummy, ilflag)

            ! Temporary ? set the variables grid-setting for the DELWAQ variables
            call setset(file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                    num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                    num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                    nopred, num_vars, num_grids, j(ivset:))

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

            ! loop over the systems
            ith = 1
            iaflag = 1
            do isys = 1, num_substances_transported
                ! do the user water quality processes
                icsys = isys
                call dlwq60(a(iderv:), a(iconc:), num_substances_total, num_cells, itfact, &
                        a(imas2:), isys, 1, a(idmps:), intopt, &
                        j(isdmp:))

                ! add the waste loads
                call dlwq15(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
                        num_waste_load_types, num_monitoring_cells, intopt, 1, itime, &
                        iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                        a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                        j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                        iknmkv, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                        c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                        a(iwdmp:), isys, 1)

                ! fill the diagonal of the matrix, with conc-array and closure error
                call dlwqh1(num_cells, num_substances_total, num_boundary_conditions, isys, gm_diag(1, ith), &
                        a(ivol2:), a(iconc:))

                ! build rest of matrix like in option 16

                call dlwqg3(num_cells, num_boundary_conditions, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, &
                        j(ixpnt:), nddim, nvdim, j(idpnw:), j(ivpnw:), &
                        a(iarea:), a(iflow:), a(ileng:), a(idisp:), a(idnew:), &
                        a(ivnew:), isys, intopt, ilflag, fast_solver_arr_size, &
                        gm_amat(1, ith), j(imat:), rowpnt, gm_diag(1, ith), gm_diac(1:, ith), &
                        iscale, fmat, tmat, iknmkv)

                ! initial guess : take rhs / diagonal
                call dlwqh3(num_cells, num_substances_transported, num_substances_total, num_boundary_conditions, isys, &
                        a(iderv:), a(iboun:), gm_rhs(1, ith), gm_diac(1:, ith), gm_sol (1, ith))

                ! solve linear system of equations
                ! note that RHS is in A(IDERV:) for steady state otpions
                call sgmres(num_cells + num_boundary_conditions, gm_rhs (1, ith), gm_sol (1, ith), num_fast_solver_vectors, gm_work(1, ith), &
                        num_cells + num_boundary_conditions, gm_hess(1, ith), num_fast_solver_vectors + 1, iter, tol, &
                        fast_solver_arr_size, gm_amat(1, ith), j(imat:), gm_diag(1, ith), rowpnt, &
                        num_layers, ioptpc, num_boundary_conditions, gm_trid(1, ith), iexseg (:, ith), &
                        file_unit_list(19), litrep)

                ! copy solution for this substance into concentration array, note that the array for
                ! segment dumps is not filled yet
                call dlwqh6(num_cells, num_substances_total, isys, 1, a(iconc:), &
                        gm_sol(1, ith), a(imas2:), a(idmps:), intopt, j(isdmp:))
            end do

            ! mass balance
            iaflag = 1
            call dlwq64(a(idisp:), a(idnew:), a(iarea:), a(iflow:), a(ileng:), &
                    a(ivnew:), a(iconc:), a(iboun:), j(ixpnt:), num_substances_transported, &
                    num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, nddim, &
                    nvdim, j(idpnw:), j(ivpnw:), intopt, a(imas2:), &
                    ilflag, a(idmpq:), ndmpq, j(iqdmp:))
            call dlwq66(a(iderv:), a(ivol:), a(iconc:), num_substances_total, num_cells)

            call write_output(num_substances_total, num_cells, num_spatial_parameters, num_spatial_time_fuctions, itstrt, &
                    c(imnam:), c(isnam:), c(idnam:), j(idump:), num_monitoring_points, &
                    a(iconc:), a(icons:), a(iparm:), a(ifunc:), a(isfun:), &
                    a(ivol:), num_constants, num_time_functions, 1, num_output_files, &
                    file_name_list, file_unit_list, j(iiout:), j(iiopo:), a(iriob:), &
                    c(iosnm:), c(iouni:), c(iodsc:), c(issnm:), c(isuni:), c(isdsc:), &
                    c(ionam:), num_cells_u_dir, num_cells_v_dir, j(igrid:), c(iedit:), &
                    num_substances_transported, a(iboun:), j(ilp:), a(iderv:), a(imas2:), &
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
                    c(ibtyp:), j(intyp:), c(icnam:), num_exchanges, j(ixpnt:), &
                    intopt, c(ipnam:), c(ifnam:), c(isfna:), j(idmpb:), &
                    num_waste_loads, num_waste_load_types, c(iwtyp:), j(iwast:), j(inwtyp:), &
                    a(iwdmp:), iknmkv, isegcol)

            call close_hydro_files(dlwqd%collcoll)
            call close_files(file_unit_list)

            call write_restart_map_file (file_unit_list, file_name_list, a(iconc:), itstrt, c(imnam:), &
                    c(isnam:), num_substances_total, num_cells)
        end associate
        if (timon) call timstop (ithandl)
    end subroutine integration_scheme_18
end module m_integration_scheme_18
