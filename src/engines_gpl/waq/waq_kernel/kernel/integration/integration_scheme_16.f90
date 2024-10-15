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
module m_integration_scheme_16
    use m_waq_precision
    use timers
    use delwaq2_data
    use m_grid_utils_external
    use m_waq_openda_exchange_items, only: get_openda_buffer
    use variable_declaration             ! Global memory with allocatable GMRES arrays
    use m_actions
    use m_waq_memory_dimensions          ! System characteristics
    use m_timer_variables                ! Timer characteristics
    use m_real_array_indices             ! Pointers in real array workspace
    use m_integer_array_indices          ! Pointers in integer array workspace
    use m_character_array_indices        ! Pointers in character array workspace
    use m_dlwqdata_save_restore
    use m_write_output, only: write_output
    use m_initialize_variables, only: initialize_variables
    use m_process_calculation, only: calculate_processes
    use m_time_dependent_variables, only: update_time_dependent_external_forcing
    use m_set_horizontal_surface_area, only: set_horizontal_surface_area
    use m_set_vertical_dispersion_length, only: set_vertical_dispersion_length
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
    use data_processing, only: close_files

    use m_zero_cumulative_arrays, only: set_cumulative_arrays_zero
    use m_integrate_areas_fluxes, only: integrate_fluxes_for_dump_areas


    implicit none

contains


    !> Upwind horizontally, central vertically, GMRES solver (16)
    !! Performs time dependen integration. Upwind horizontally,
    !! central vertically, implicit in time. Uses the GMRES
    !! iterative solver with Krilov sub-spaces.\n
    !! Forester filter is optional to ensure monotoneous behaviour
    !! in the vertical.
    subroutine scheme_16_time_implicit_hz_upwind_vl_central(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_gmres_utils, only: fill_matrix_hz_backward_diff_vl_central_diff
        use m_calculate_new_volumes, only: calculate_new_volumes
        use m_scheme_16_utils, only: tranfer_solution_from_rhs_into_conc
        use m_gmres_utils, only: fill_rhs_for_gmres_solver
        use m_scheme_16_utils, only: Initialize_diagonal_for_gmres
        use m_initialize_pointer_matrices_fast_solver, only: initialize_pointer_matrices_fast_solver
        use m_vertical_forester_filter, only: vertical_forester_filter
        use m_closure_error_correction, only: calculate_closure_error_correction
        use m_concentration_calculations, only: calculate_concentrations_from_mass
        use m_mass_balance_calculation, only: calculate_mass_balance_implicit_schemes
        use m_mass_calculation, only: calculate_mass_for_transported_substances
        use m_make_new_volumes, only: make_new_volumes
        use m_time_dependent_variables
        use m_thatcher_harleman_bc, only: thatcher_harleman_bc
        use m_add_waste_loads, only: add_waste_loads
        use m_scale_derivatives_steady_state, only: scale_processes_derivs_and_update_balances
        use m_write_restart_map_file, only: write_restart_map_file
        use m_delpar01, only: delpar01
        use m_array_manipulation, only: copy_real_array_elements

        use m_zlayer, only: zflows
        use m_sgmres, only: sgmres, initialize_gmres
        use omp_lib

        type(waq_data_buffer), target :: buffer              !< System total array space
        integer(kind = int_wp), intent(inout) :: file_unit_list  (*) !< Array with logical unit numbers
        character(len = *), intent(in) :: file_name_list(*)   !< Array with file names
        integer(kind = int_wp), intent(in) :: action              !< Span of the run or type of action to perform
        !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data), target :: dlwqd               !< DELWAQ data structure
        type(GridPointerColl) :: gridps              !< Collection of all grid definitions

        real(kind = real_wp) :: rdummy(1)
        logical :: imflag, idflag, ihflag
        logical :: update, lrewin
        logical :: timon_old
        integer(kind = int_wp) :: substance_i
        integer(kind = int_wp) :: nstep

        integer(kind = int_wp), save :: ithand1 = 0 ! Make this one "global"
        integer(kind = int_wp) :: noth
        integer(kind = int_wp) :: ith

        integer(kind = int_wp) :: ibnd

        !
        ! Variables specific to this method: leave them SAVEd
        !
        integer(kind = int_wp), save :: ioptpc
        integer(kind = int_wp), save :: iter
        integer(kind = int_wp), save :: iscale

        integer(kind = int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
            !
            !     SPECIAL REMARKS    : MASS-ARRAY IS USED FOR RHS VECTOR!!
            !
            !     This option is a mix of option 1 (discretization of transport
            !     in space) and option 6 (matrix inversion to perform implicit
            !     integration in time.
            !     The processes part is integrated EXPLICITLY, in order to allow
            !     for any complexity of the processes.
            !     Strictly speaking, a loop over substances should be added
            !     To anticipate this, the method uses an
            !     extra VOLUME-array (IVOL2), and uses the AMASS-array (IMASS)
            !     for the rhs-matrix, instead of the DERIV-array as in method 6.
            !     (JvG, May 8 1992)
            !
            !     This option implements:
            !     1) Euler backward time integration
            !     2) upwind differences for advection
            !     3) central differences for diffusion
            !     The resulting systems of equations are solved by an iterative
            !     solution method (GMRES).
            !     With such an iterative method, systems with multiple rhs cannot be solved
            !     (simultaneously). So we loop over the substances and solve each system
            !     individually. So RHS can be reduced to an REAL array of size num_cells+num_boundary_conditions.
            !
            !     possible improvements:
            !
            !     - Use FGMRES instead of GMRES for solving system of equations.
            !       This makes it possible to keep search directions which have already
            !       been computed in previous FGMRES calls and hence find the solution
            !       of new systems at lower costs!
            !
            !     - Tune the preconditioner to speed up the iteration process.
            !       Only Gaus-Seidel, and SSOR preconditioning has been implemented yet.
            !
            !     - Integrate processes in an implicit way as well. Enables users to
            !       potentially take larger time steps (better stability properties)
            !       or even compute steady states in "one time step" (the latter subject
            !       to constraint that calculate_processes formulation is time independent).
            !       Implicit time integration of processes requires the Inexact Newton
            !       solution method described in:
            !
            !       "DELWAQ FASTSOLVER II"
            !       Newton-Krylov methods for solving linear and non-linear equations
            !       report T1596, January 1996, Deltares

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt("scheme_16_time_implicit_hz_upwind_vl_central", ithandl)
                goto 50
            endif

            IF (ACTION == ACTION_INITIALISATION  .OR. &
                    ACTION == ACTION_FULLCOMPUTATION) THEN

                !        some initialisation
                !        IOPTPC = preconditioner switch [0 = none, 1 = GS (L), 2 = GS (U),
                !        3 = SSOR], ITER = maximum number of iterations [ > 0],
                !        TOL = relative tolerance [10^-3, 10^-10], ISCALE = row scaling
                !        of system of equations [0 = no, 1 =yes], KLAT = number of
                !        layers in preconditioner [1,num_layers_grid]
                call initialize_gmres(file_unit_list(19), num_constants, c(icnam:), a(icons:), ioptpc, &
                        iter, tol, iscale, litrep, num_cells, num_exchanges_z_dir, num_exchanges, num_fast_solver_vectors, fast_solver_arr_size, &
                        num_layers, intsrt, intopt)

                ithandl = 0
                itime = itstrt
                nstep = (itstop - itstrt) / idt
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
                noqt = num_exchanges_u_dir + num_exchanges_v_dir
                lleng = ileng + noqt * 2
                forester = btest(intopt, 6)
                nowarn = 0

                ! initialize second volume array with the first one

                call copy_real_array_elements   (a(ivol:), a(ivol2:), nosss)
            ENDIF

            ! Save/restore the local persistent variables,
            ! if the computation is split up in steps
            ! Note: the handle to the timer (ithandl) needs to be
            ! properly initialised and restored
            IF (ACTION == ACTION_INITIALISATION) THEN
                if (timon) call timstrt("scheme_16_time_implicit_hz_upwind_vl_central", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop(ithandl)
                RETURN
            ENDIF

            IF (ACTION == ACTION_SINGLESTEP) THEN
                call dlwqdata_restore(dlwqd)
            ENDIF

            if (timon) call timstrt("scheme_16_time_implicit_hz_upwind_vl_central", ithandl)

            iexseg = 1     !  There is nothing to mask.

            !======================= simulation loop ============================
            10 continue

            ! Determine the volumes and areas that ran dry at start of time step
            call set_horizontal_surface_area(num_cells, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(num_cells, nosss, num_layers, a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, &
                    a(iarea:), num_constants, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            ! user transport processes
            update = updatr
            call set_vertical_dispersion_length(num_substances_total, num_cells, num_exchanges, num_exchanges_u_dir, &
                    num_exchanges_v_dir, num_exchanges_z_dir, num_spatial_parameters, &
                    j(ixpnt:), a(ivol:), &
                    a(ileng:), a(iparm:), &
                    c(ipnam:), ilflag)

            if (update) updatr = .true.

            ! Temporary ? set the variables grid-setting for the DELWAQ variables
            call initialize_variables(file_unit_list(19), num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, &
                    num_substances_transported, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                    num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                    nopred, num_vars, num_grids, j(ivset:))

            ! return conc and take-over from previous step or initial condition,
            ! and do particle tracking of this step (will be back-coupled next call)

            call delpar01(itime, num_cells, num_layers, num_exchanges, num_substances_transported, &
                    num_substances_total, a(ivol:), surface, a(iflow:), c(isnam:), &
                    num_spatial_time_fuctions, c(isfna:), a(isfun:), a(imass:), a(iconc:), &
                    iaflag, intopt, num_monitoring_cells, j(isdmp:), a(idmps:), &
                    a(imas2:))

            ! call calculate_processes subsystem
            call calculate_processes(num_substances_total, nosss, a(iconc:), a(ivol:), itime, &
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
                        do substance_i = 1, num_substances_transported
                            call get_openda_buffer(substance_i, ibnd, 1, 1, &
                                    A(ibset:+(ibnd - 1) * num_substances_transported + substance_i - 1))
                        enddo
                    enddo
                endif
                call thatcher_harleman_bc(a(ibset:), a(ibsav:), j(ibpnt:), num_boundary_conditions, num_substances_transported, &
                        num_substances_total, idt, a(iconc:), a(iflow:), a(iboun:))
            endif

            call write_output(num_substances_total, num_cells, num_spatial_parameters, num_spatial_time_fuctions, ITIME, &
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
                    C(IBTYP:), J(INTYP:), C(ICNAM:), num_exchanges, J(IXPNT:), &
                    INTOPT, C(IPNAM:), C(IFNAM:), C(ISFNA:), J(IDMPB:), &
                    num_waste_loads, num_waste_load_types, C(IWTYP:), J(IWAST:), J(INWTYP:), &
                    A(IWDMP:), iknmkv, isegcol)

            ! zero cumulative arrays
            if (imflag .or. (ihflag .and. num_transects > 0)) then
                call set_cumulative_arrays_zero(num_substances_total, num_substances_transported, num_fluxes, ndmpar, ndmpq, &
                        num_monitoring_cells, a(ismas:), a(iflxi:), a(imas2:), &
                        a(idmpq:), a(idmps:), num_transects, imflag, ihflag, &
                        a(itrra:), ibflag, num_waste_loads, a(iwdmp:))
            endif

            ! simulation done ?
            if (itime < 0) goto 9999
            if (itime >= itstop) goto 50

            ! restore conc-array from mass array
            call calculate_concentrations_from_mass(num_substances_transported, num_substances_total, num_substances_part, num_cells, a(ivol:), &
                    surface, a(imass:), a(iconc:))

            ! add processes
            call scale_processes_derivs_and_update_balances(a(iderv:), num_substances_total, num_cells, itfact, a(imas2:), &
                    idt, iaflag, a(idmps:), intopt, j(isdmp:))

            !        get new volumes

            itimel = itime
            itime = itime + idt
            select case (ivflag)
            case (1) !     computation of volumes for computed volumes only
                call copy_real_array_elements(a(ivol:), a(ivol2:), num_cells)
                call make_new_volumes(a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), num_substances_total, &
                        num_exchanges, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                        a(imas2:), idt, iaflag, num_substances_transported, a(idmpq:), &
                        ndmpq, j(iqdmp:))
                updatr = .true.
            case (2) !     the fraudulent computation option
                call update_volumes_and_time_step(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivoll:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivol2:), dlwqd)
                call calculate_new_volumes(num_cells, num_exchanges, j(ixpnt:), idt, iknmkv, &
                        a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               !     read new volumes from files
                call update_volumes_and_time_step(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), num_cells, a(ivol2:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
            end select

            ! Update the info on dry volumes with the new volumes
            call identify_wet_cells(num_cells, nosss, a(ivol2:), num_layers, num_constants, &
                    c(icnam:), a(icons:), surface, j(iknmr:), iknmkv)
            ! Compute new from-topointer on the basis of non-zeroflows
            call zflows(num_exchanges, noqt, num_layers, num_constants, c(icnam:), &
                    a(iflow:), j(ixpnt:))
            ! Initialize pointer matices for fast solvers
            call initialize_pointer_matrices_fast_solver(num_cells, num_boundary_conditions, num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, &
                    fast_solver_arr_size, j(ixpnt:), j(iwrk:), j(imat:), rowpnt, &
                    fmat, tmat)

            ! add the waste loads
            call add_waste_loads(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
                    num_waste_load_types, num_monitoring_cells, intopt, idt, itime, &
                    iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                    a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                    j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                    iknmkv, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                    c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                    a(iwdmp:), 1, num_substances_total)

            ! Here we implement a loop that inverts the same matrix
            ! for series of subsequent substances having the same
            ! additional VELO and DISPER array. (JvG, April 24, 1993).
            ! In solving equations with multiple rhs, the *FULL* fast (or should
            ! I say slow?) solver algorithm needs to be applied to each rhs vector
            ! So solve_linear_equations_lu_decomposition may outperform FS when we deal with a large number of
            ! substances with the same additional velocity and dispersion field
            ! In future we need a smart switch between solve_linear_equations_lu_decomposition and FS at this place
            ! For now always do FS!
            if (timon) call timstrt("ADE solver", ithand1)
            timon_old = timon
            noth = OMP_GET_MAX_THREADS()
            if (noth > 1) timon = .false.

            !$OMP PARALLEL
            !$OMP DO PRIVATE(ith) SCHEDULE(DYNAMIC)
            ! start of loop over substances

            do substance_i = 1, num_substances_transported
                ! number of threads for parallel processing
                ith = OMP_GET_THREAD_NUM() + 1

                ! initialize diagonal
                call Initialize_diagonal_for_gmres(num_cells, num_boundary_conditions, idt, a(ivol2:), gm_diag(1, ith))

                ! do the transport itself, fill matrix, scale diagonal
                call fill_matrix_hz_backward_diff_vl_central_diff(num_cells, num_boundary_conditions, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, &
                        j(ixpnt:), nddim, nvdim, j(idpnw:), j(ivpnw:), &
                        a(iarea:), a(iflow:), a(ileng:), a(idisp:), a(idnew:), &
                        a(ivnew:), substance_i, intopt, ilflag, fast_solver_arr_size, &
                        gm_amat(1, ith), j(imat:), rowpnt, gm_diag(1, ith), gm_diac(1:, ith), &
                        iscale, fmat, tmat, iknmkv)

                ! compute RHS (substance after substance)
                call fill_rhs_for_gmres_solver(num_cells, num_boundary_conditions, num_substances_transported, num_substances_total, substance_i, &
                        idt, a(iconc:), a(iderv:), a(ivol:), a(iboun:), &
                        gm_rhs(1, ith), gm_diac(1:, ith), gm_sol(1, ith))

                ! solve linear system of equations
                call sgmres(num_cells + num_boundary_conditions, gm_rhs (1, ith), gm_sol (1, ith), num_fast_solver_vectors, gm_work(1, ith), &
                        num_cells + num_boundary_conditions, gm_hess(1, ith), num_fast_solver_vectors + 1, iter, tol, &
                        fast_solver_arr_size, gm_amat(1, ith), j(imat:), gm_diag(1, ith), rowpnt, &
                        num_layers, ioptpc, num_boundary_conditions, gm_trid(1, ith), iexseg (:, ith), &
                        file_unit_list(19), litrep)

                ! copy solution for this substance into concentration array
                call tranfer_solution_from_rhs_into_conc(num_cells, num_substances_total, substance_i, 1, gm_sol(1, ith), &
                        a(iconc:), iknmkv)

                ! end loop over the substances

            end do

            !$OMP ENDDO
            !$OMP ENDPARALLEL
            if (noth > 1) timon = timon_old

            if (timon) call timstop(ithand1)

            ! mass balance of transport
            call calculate_mass_balance_implicit_schemes(a(idisp:), a(idnew:), a(iarea:), a(iflow:), a(ileng:), &
                    a(ivnew:), a(iconc:), a(iboun:), j(ixpnt:), num_substances_transported, &
                    num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, nddim, &
                    nvdim, j(idpnw:), j(ivpnw:), intopt, a(imas2:), &
                    ilflag, a(idmpq:), ndmpq, idt, j(iqdmp:))

            ! update mass array, explicit step for passive substances
            call calculate_mass_for_transported_substances(num_substances_transported, num_substances_total, num_substances_part, num_cells, a(ivol2:), &
                    surface, a(imass:), a(iconc:), a(iderv:), idt)

            ! Forester filter on the vertical
            if (forester) then
                call vertical_forester_filter(file_unit_list(19), num_substances_transported, num_substances_total, num_cells, num_exchanges_z_dir, &
                        num_layers_grid, a(iconc:), a(lleng:), nowarn)
            endif

            ! calculate closure error
            if (lrewin .and. lstrec) then
                call calculate_closure_error_correction(a(imass:), a(ivoll:), a(ivol2:), num_substances_transported, num_substances_total, &
                        num_cells, file_unit_list(19))
                call copy_real_array_elements(a(ivoll:), a(ivol:), num_cells)    !  replace old by new volumes
            else
                call copy_real_array_elements(a(ivol2:), a(ivol:), num_cells)    !  replace old by new volumes
            endif

            ! integrate the fluxes at dump segments fill asmass with mass
            if (ibflag > 0) then
                call integrate_fluxes_for_dump_areas(num_fluxes, ndmpar, idt, itfact, a(iflxd:), &
                        a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            endif

            ! new time values, volumes excluded
            call update_time_dependent_external_forcing(file_unit_list, itime, itimel, a(iharm:), a(ifarr:), &
                    j(inrha:), j(inrh2:), j(inrft:), idt, a(ivol:), &
                    a(idiff:), a(iarea:), a(iflow:), a(ivelo:), a(ileng:), &
                    a(iwste:), a(ibset:), a(icons:), a(iparm:), a(ifunc:), &
                    a(isfun:), j(ibulk:), file_name_list, c(ilunt:), ftype, &
                    intsrt, isflag, ifflag, ivflag, ilflag, &
                    update, j(iktim:), j(iknmr:), j(inisp:), a(inrsp:), &
                    j(intyp:), j(iwork:), .false., ldummy, rdummy, &
                    .false., gridps, dlwqd)
            if (update) updatr = .true.

            ! end of time loop
            if (action == ACTION_FULLCOMPUTATION) then
                goto 10
            end if

            50 continue
            if (action == ACTION_FINALISATION    .or. &
                    action == ACTION_FULLCOMPUTATION) then

                ! close files, except monitor file
                call close_hydro_files(dlwqd%collcoll)
                call close_files(file_unit_list)

                ! write restart file
                call write_restart_map_file(file_unit_list, file_name_list, a(iconc:), itime, c(imnam:), &
                        c(isnam:), num_substances_total, num_cells)
            endif
        end associate

        9999 if (timon) call timstop (ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime
    end subroutine scheme_16_time_implicit_hz_upwind_vl_central
end module m_integration_scheme_16
