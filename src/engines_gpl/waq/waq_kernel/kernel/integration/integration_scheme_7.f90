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
module m_integration_scheme_7
    use m_waq_precision
    use timers
    use delwaq2_data
    use variable_declaration          ! module with the more recently added arrays
    use m_actions
    use m_waq_memory_dimensions          ! System characteristics
    use m_timer_variables          ! Timer characteristics
    use m_real_array_indices          ! Pointers in real array workspace
    use m_integer_array_indices          ! Pointers in integer array workspace
    use m_character_array_indices          ! Pointers in character array workspace
    use m_grid_utils_external
    use m_waq_openda_exchange_items, only: get_openda_buffer
    use m_set_horizontal_surface_area, only: set_horizontal_surface_area
    use m_set_vertical_dispersion_length, only: set_vertical_dispersion_length
    use m_write_output, only: write_output
    use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes
    use data_processing, only: close_files

    implicit none

contains


    !> Horizontally upwind, vertically central, direct stationary method (7)
    !! Stationary solution. Upwind 1st order horizontally, central
    !! vertically. Fully implicit with a direct method.\n
    !! Matrices become very large in 3D and method unworkable. In 2D
    !! the method can be used. In 1D the method outperforms the
    !! iterative methods.
    subroutine scheme_7_steady_state_hz_upwind_vl_central(buffer, file_unit_list, file_name_list, action, dlwqd, gridps)

        use m_mass_balance_calculation, only: calculate_mass_balance_space_central_difference
        use m_gmres_utils, only: fill_matrix_central_difference_space
        use m_scheme_6_and_7_utils, only: update_diagonal
        use m_mass_calculation, only: calculate_mass_from_concentration
        use m_closure_error_correction, only: calculate_closure_error_correction_steady_state
        use m_concentration_calculations, only: calculate_concentrations_from_derivatives
        use m_solver_utils, only: prepare_matrix_for_solver
        use m_scale_derivatives_steady_state, only: scale_derivatives_steady_state
        use m_time_dependent_variables
        use m_add_waste_loads, only: add_waste_loads
        use m_wet_dry_cells, only: set_dry_cells_to_zero_and_update_volumes, identify_wet_cells
        use m_write_restart_map_file, only: write_restart_map_file
        use m_solver_utils, only: solve_linear_equations_lu_decomposition
        use m_array_manipulation, only: initialize_real_array

        type(waq_data_buffer), target :: buffer              !< System total array space
        integer(kind = int_wp), intent(inout) :: file_unit_list  (*) !< Array with logical unit numbers
        character(len = *), intent(in) :: file_name_list(*)   !< Array with file names
        integer(kind = int_wp), intent(in) :: action              !< Span of the run or type of action to perform
        !< (run_span = {initialise, time_step, finalise, whole_computation})
        type(delwaq_data), target :: dlwqd               !< DELWAQ data structure
        type(GridPointerColl) :: gridps              !< Collection of all grid definitions

        ! Local variables
        logical         imflag, idflag, ihflag
        logical         ldummy, lstrec, lrewin

        integer(kind = int_wp) :: itime
        integer(kind = int_wp) :: itimel
        integer(kind = int_wp) :: iaflag
        integer(kind = int_wp) :: ibflag
        integer(kind = int_wp) :: substance_i
        integer(kind = int_wp) :: icsys
        integer(kind = int_wp) :: nsys
        integer(kind = int_wp) :: inwtyp
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: nosss
        integer(kind = int_wp) :: noqtt

        integer(kind = int_wp) :: ithandl

        integer(kind = int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)
            !
            ! Distinguishing the actions is superfluous:
            ! there is only one step
            !
            IF (ACTION == ACTION_INITIALISATION .OR. &
                    ACTION == ACTION_FINALISATION) THEN
                RETURN
            ENDIF

            ! some initialisation
            ithandl = 0
            if (timon) call timstrt("scheme_7_steady_state_hz_upwind_vl_central", ithandl)

            ITIMEL = ITSTRT
            ITIME = ITSTRT + IDT
            IBFLAG = 0
            IF (MOD(INTOPT, 16) >= 8) IBFLAG = 1
            call initialize_real_array(A(IMAS2:), num_substances_total * 5)
            LDUMMY = .FALSE.
            LSTREC = .FALSE.
            nosss = num_cells + num_cells_bottom
            NOQTT = num_exchanges + num_exchanges_bottom_dir
            inwtyp = intyp + num_boundary_conditions

            ! Determine the volumes and areas that ran dry,
            ! They cannot have explicit processes during this time step

            call set_horizontal_surface_area(num_cells, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call set_dry_cells_to_zero_and_update_volumes(num_cells, nosss, num_layers, a(ivol:), num_exchanges_u_dir + num_exchanges_v_dir, &
                    a(iarea:), num_constants, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            ! makes closure error
            IF (IDT==0) THEN
                call initialize_real_array(A(IVOL2:), num_cells)
            ELSE IF (J(INRH2 + 1)>=0 .AND. IVFLAG==0) THEN
                CALL update_volumes_and_time_step(file_unit_list, ITIME, ITIMEL, A(IHARM:), A(IFARR:), &
                        J(INRHA:), J(INRH2:), J(INRFT:), num_cells, A(IVOL2:), &
                        J(IBULK:), file_name_list, ftype, ISFLAG, IVFLAG, &
                        LDUMMY, J(INISP:), A(INRSP:), J(INTYP:), J(IWORK:), &
                        LSTREC, LREWIN, A(IVOLL:), dlwqd)
                CALL calculate_closure_error_correction_steady_state(A(IVOL2:), A(IVOL:), IDT, num_cells)
            ELSE
                call initialize_real_array(A(IVOL2:), num_cells)
                WRITE(file_unit_list(19), 1000)
            ENDIF

            ! loop over the systems
            NSYS = 1
            IAFLAG = 1
            DO substance_i = 1, num_substances_transported
                IF (substance_i == num_substances_transported) NSYS = 1 + num_substances_total - num_substances_transported

                ! do the user transport processes
                ICSYS = substance_i
                call set_vertical_dispersion_length(num_substances_total, num_cells, num_exchanges, num_exchanges_u_dir, &
                        num_exchanges_v_dir, num_exchanges_z_dir, num_spatial_parameters, &
                        j(ixpnt:), a(ivol:), &
                        a(ileng:), a(iparm:), &
                        c(ipnam:), ilflag)

                ! do the user water quality processes
                CALL scale_derivatives_steady_state(A(IDERV:), A(ICONC:), num_substances_total, num_cells, ITFACT, &
                        A(IMAS2:), substance_i, NSYS, A(IDMPS:), INTOPT, &
                        J(ISDMP:))

                ! add the waste loads
                call add_waste_loads(num_substances_transported, num_substances_total, num_cells, num_exchanges, num_waste_loads, &
                        num_waste_load_types, num_monitoring_cells, intopt, 1, itime, &
                        iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                        a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                        j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                        iknmkv, num_spatial_parameters, c(ipnam:), a(iparm:), num_spatial_time_fuctions, &
                        c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                        a(iwdmp:), substance_i, nsys)

                ! fill the matrix
                CALL prepare_matrix_for_solver(A(ICONC:), A(IDERV:), A(IVOL2:), A(ITIMR:), num_cells, &
                        num_substances_total, substance_i, NSYS, num_codiagonals)
                CALL fill_matrix_central_difference_space(A(IDISP:), A(IDIFF:), A(IAREA:), A(IFLOW:), A(ILENG:), &
                        A(IVELO:), A(IBOUN:), J(IXPNT:), num_substances_total, substance_i, &
                        NSYS, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
                        num_velocity_arrays, J(IDPNT:), J(IVPNT:), A(IDERV:), A(ITIMR:), &
                        num_codiagonals, INTOPT, ILFLAG)
                CALL update_diagonal(A(ITIMR:), num_cells, num_codiagonals)

                ! invert the matrix and store the results
                CALL solve_linear_equations_lu_decomposition(num_cells, num_codiagonals, num_codiagonals, NSYS, A(ITIMR:), &
                        A(IDERV:), 0)
                CALL calculate_concentrations_from_derivatives(A(ICONC:), A(IDERV:), A(IMAS2:), num_cells, num_substances_total, &
                        substance_i, NSYS, A(IDMPS:), INTOPT, J(ISDMP:))
            end do

            ! mass balance
            IAFLAG = 1
            CALL calculate_mass_balance_space_central_difference(A(IDISP:), A(IDIFF:), A(IAREA:), A(IFLOW:), A(ILENG:), &
                    A(IVELO:), A(ICONC:), A(IBOUN:), J(IXPNT:), num_substances_transported, &
                    num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
                    num_velocity_arrays, J(IDPNT:), J(IVPNT:), INTOPT, A(IMAS2:), &
                    ILFLAG, A(IDMPQ:), NDMPQ, J(IQDMP:))
            CALL calculate_mass_from_concentration(A(IDERV:), A(IVOL:), A(ICONC:), num_substances_total, num_cells)

            ! Call OUTPUT system
            CALL write_output(num_substances_total, num_cells, num_spatial_parameters, num_spatial_time_fuctions, ITSTRT, &
                    C(IMNAM:), C(ISNAM:), C(IDNAM:), J(IDUMP:), num_monitoring_points, &
                    A(ICONC:), A(ICONS:), A(IPARM:), A(IFUNC:), A(ISFUN:), &
                    A(IVOL:), num_constants, num_time_functions, 1, num_output_files, &
                    file_name_list, file_unit_list, J(IIOUT:), J(IIOPO:), A(IRIOB:), &
                    C(IOSNM:), C(IOUNI:), C(IODSC:), C(ISSNM:), C(ISUNI:), C(ISDSC:), &
                    C(IONAM:), num_cells_u_dir, num_cells_v_dir, J(IGRID:), C(IEDIT:), &
                    num_substances_transported, A(IBOUN:), J(ILP:), A(IDERV:), A(IMAS2:), &
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

            ! close files, except monitor file
            call close_hydro_files(dlwqd%collcoll)
            call close_files(file_unit_list)

            ! write restart file
            CALL write_restart_map_file (file_unit_list, file_name_list, A(ICONC:), ITSTRT, C(IMNAM:), &
                    C(ISNAM:), num_substances_total, num_cells)

            ! output formats
            1000 FORMAT ('No closure error corrections !')
            !
        end associate
        if (timon) call timstop (ithandl)
        RETURN
    end subroutine scheme_7_steady_state_hz_upwind_vl_central
end module m_integration_scheme_7
