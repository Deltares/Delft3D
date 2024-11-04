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
module m_gmres_utils
    use m_waq_precision
    use timers

    implicit none

    private
    public :: fill_matrix_hz_upwind_diff_vl_upwind_diff, fill_matrix_hz_backward_diff_vl_central_diff, &
            fill_matrix_hz_backward_diff_vl_backward_diff, fill_matrix_set_diagonal_steady_state, &
            fill_matrix_for_theta_algorithm, fill_rhs_initial_guess_theta, &
            fill_rhs_for_gmres_solver

contains

    !> Fills matrix to be used by the GMRES fast solver, once per substance
    !! Matrix is filled:
    !! - horizontally according to upwind differences in space
    !! - vertically   according to upwind differences in space
    !! It is assumed that any logic on drying and flooding is in the
    !! precomputed flowtot and disptot arrays./n
    !! The routine is very efficient because of the precomputed fmat
    !! and tmat arrays for the from and to locations in the matrix.
    subroutine fill_matrix_hz_upwind_diff_vl_upwind_diff(idt, num_cells, volnew, num_boundary_conditions, &
            num_exchanges, ipoint, flowtot, disptot, diag, iscale, &
            diagcc, fast_solver_arr_size, amat, idiag, fmat, &
            tmat)

        integer(kind = int_wp), intent(in) :: idt                  !< Time step
        integer(kind = int_wp), intent(in) :: num_cells                !< Number of cells or computational volumes
        real(kind = real_wp), intent(in) :: volnew(num_cells)          !< Volumes of cells
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: num_exchanges                  !< Total number fluxes in the water phase
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)       !< From, to, from-1, to+1 volume numbers per flux
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)           !< Flows plus additional velocities (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)           !< Dispersion plus additional dipersion (dim: num_exchanges)
        real(kind = dp), intent(inout) :: diag(num_cells + num_boundary_conditions)        !< Diagonal of the matrix
        integer(kind = int_wp), intent(in) :: iscale               !< = 1 row scaling with the diagonal
        real(kind = dp), intent(inout) :: diagcc(num_cells + num_boundary_conditions)      !< Copy of (unscaled) diagonal of the matrix
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                !< Dimension of off-diagonal matrix amat
        real(kind = dp), intent(out) :: amat(fast_solver_arr_size)                !< Matrix with off-diagonal entries
        integer(kind = int_wp), intent(in) :: idiag(0:num_cells + num_boundary_conditions) !< Position of the diagonals in amat
        integer(kind = int_wp), intent(in) :: fmat(num_exchanges)            !< Location from(iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat(num_exchanges)            !< Location to  (iq) in matrix

        ! Local variables
        integer(kind = int_wp) :: cell_i  !< Index of current cell
        integer(kind = int_wp) :: iq    !< Index current edge
        integer(kind = int_wp) :: jq    !< Index current edge 2
        integer(kind = int_wp) :: ito   !< Index to volume
        integer(kind = int_wp) :: ifrom !< Index from volume

        real(kind = real_wp) :: q1 !< flow 1
        real(kind = real_wp) :: q2 !< flow 2

        real(kind = dp) :: dt !< time step in double precision

        ! WAQ timers

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt("fill_matrix_hz_upwind_diff_vl_upwind_diff", ithandl)

        ! set the diagonal
        dt = idt
        do cell_i = 1, num_cells
            diag(cell_i) = volnew(cell_i) / dt
        end do
        do cell_i = num_cells + 1, num_cells + num_boundary_conditions
            diag(cell_i) = 1.0
        end do

        ! reset the entire matrix
        amat = 0.0d0
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            if (flowtot(iq) > 0.0) then
                q1 = flowtot(iq)
                q2 = 0.0
            else
                q1 = 0.0
                q2 = flowtot(iq)
            end if

            if (ifrom > 0) then
                diag(ifrom) = diag(ifrom) + q1 + disptot(iq)
                amat(fmat(iq)) = amat(fmat(iq)) + q2 - disptot(iq)
            end if
            if (ito > 0) then
                diag(ito) = diag(ito) - q2 + disptot(iq)
                amat(tmat(iq)) = amat(tmat(iq)) - q1 - disptot(iq)
            end if
        end do

        ! finally scale the matrix to avoid possible round-off errors in GMRES
        ! this scaling may need some adaption for future domain decomposition b.c.
        if (iscale == 1) then
            do iq = 1, num_cells + num_boundary_conditions
                ifrom = idiag(iq - 1) + 1
                ito = idiag(iq)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(iq)) < 1.0d-100) diag(iq) = 1.0

                do jq = ifrom, ito
                    amat(jq) = amat(jq) / diag(iq)
                end do

                ! copy of diag for later scaling purposes in fill_rhs_for_gmres_solver
                diagcc(iq) = diag(iq)
                diag(iq) = 1.0d00
            end do
        else
            do iq = 1, num_cells + num_boundary_conditions
                diagcc(iq) = 1.0d00
            end do
        end if
        if (timon) call timstop(ithandl)
    end subroutine fill_matrix_hz_upwind_diff_vl_upwind_diff


    !> Fills in the off-diagonal values of the matrix for GMRES fast solver.
    !! Horizontally: according to backward differences in space
    !! Vertically:   according to central  differences in space
    !! The additional velocities in the vertical (like the settling
    !! velocities of suspended sediments) are maintained BACKWARD !!
    subroutine fill_matrix_hz_backward_diff_vl_central_diff(num_cells, num_boundary_conditions, num_exchanges_u_dir, &
            num_exchanges_v_dir, num_exchanges, ipoint, num_dispersion_arrays, num_velocity_arrays, idpnt, ivpnt, &
            area, flow, aleng, disp, disper, &
            velo, substance_i, integration_id, ilflag, fast_solver_arr_size, &
            amat, imat, idiag, diag, diagcc, &
            iscale, fmat, tmat, iknmrk)

        integer(kind = int_wp), intent(in) :: num_cells                  !< Number of cells or computational volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                  !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                   !< Number of fluxes first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                   !< Number of fluxes second direction
        integer(kind = int_wp), intent(in) :: num_exchanges                    !< Total number fluxes in the water phase
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)         !< From, to, from-1, to+1 volume numbers per flux
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                 !< Number of additional dispersion arrays
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                 !< Number of additional velocity   arrays
        integer(kind = int_wp), intent(in) :: idpnt(*)               !< Dispersion array to be applied per substance
        integer(kind = int_wp), intent(in) :: ivpnt(*)               !< Velocity array to be applied per substance
        real(kind = real_wp), intent(in) :: area(num_exchanges)              !< Crosssectional surface areas of the fluxes
        real(kind = real_wp), intent(in) :: flow(num_exchanges)              !< Fluxes
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges)          !< From and to distances to the surface area
        real(kind = real_wp), intent(in) :: disp(3)                !< Default dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges)    !< Additional dispersion arrays
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)      !< Additional velocity arrays
        integer(kind = int_wp), intent(in) :: substance_i                   !< Substances number to be used for this matrix
        integer(kind = int_wp), intent(in) :: integration_id         !< = 0 or 2 DISP at zero flow
        !< = 1 or 3 no DISP at zero flow
        !< = 0 or 1 DISP over boundary
        !< = 2 or 3 no DISP over boundary
        integer(kind = int_wp), intent(in) :: ilflag                 !< If 0 then only 3 length values
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                  !< Dimension of off-diagonal matrix amat
        real(kind = dp), intent(out) :: amat(fast_solver_arr_size)            !< Matrix with off-diagonal entries
        integer(kind = int_wp), intent(in) :: imat(fast_solver_arr_size)            !< Indeces of the off-diagonals in amat
        integer(kind = int_wp), intent(in) :: idiag(0:num_cells + num_boundary_conditions) !< Position of the diagonals in amat
        real(kind = dp), intent(inout) :: diag(num_cells + num_boundary_conditions)    !< Diagonal of the matrix
        real(kind = dp), intent(inout) :: diagcc(num_cells + num_boundary_conditions)  !< Copy of (unscaled) diagonal of the matrix
        integer(kind = int_wp), intent(in) :: iscale                 !< = 0 no row scaling of diagonal
        !< = 1    row scaling of diagonal
        integer(kind = int_wp), intent(in) :: fmat(num_exchanges)              !< Location from(iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat(num_exchanges)              !< Location to  (iq) in matrix
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)          !< Feature array

        ! Local variables
        logical :: zerof  !< NO dispersion at zero flow?
        logical :: zerob  !< NO dispersion accross open boundaries?
        logical :: loword !< Apply lower order scheme at open boundaries?
        logical :: lscale !< APPLY row scaling of the diagonal?
        logical :: length !< Array of lengths is provided?
        integer(kind = int_wp) :: iadd   !< Extra offset for horizontal off-diagonals in the case of 3D
        integer(kind = int_wp) :: ifrom  !< From volume number
        integer(kind = int_wp) :: ito    !< To   volume number
        integer(kind = int_wp) :: ifr2   !< From row number
        integer(kind = int_wp) :: ito2   !< To   row number
        integer(kind = int_wp) :: ip     !< Index in amat of the 'from' volume for this flux
        integer(kind = int_wp) :: jp     !< Index in amat of the 'to'   volume for this flux
        real(kind = real_wp) :: a      !< Auxiliary variable for exchange surface area in m2
        real(kind = real_wp) :: q      !< Auxiliary variable for the flux in m3/s
        real(kind = real_wp) :: e      !< Auxiliary variable for diffusive flux in m3/s
        real(kind = real_wp) :: dl     !< Auxiliary variable for the diffusive multiplier area/leng in m
        integer(kind = int_wp) :: idp    !< Auxiliary variables for idpnt(substance_i)
        integer(kind = int_wp) :: ivp    !< Auxiliary variables for ivpnt(substance_i)
        real(kind = real_wp) :: q1     !< Auxiliary variable
        real(kind = real_wp) :: q2     !< Auxiliary variable
        real(kind = real_wp) :: qvel   !< Auxiliary variable to dintinguish normal and additional vertical velocity
        real(kind = real_wp) :: f1     !< Auxiliary variable for (weighed) central differences
        real(kind = real_wp) :: f2     !< Auxiliary variable for (weighed) central differences
        integer(kind = int_wp) :: iq     !< Loop counter
        integer(kind = int_wp) :: jq     !< Loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt("fill_matrix_hz_backward_diff_vl_central_diff", ithandl)

        ! set the logicals for dispersion and scaling and other fixed items
        zerof = btest(integration_id, 0)
        zerob = btest(integration_id, 1)
        loword = btest(integration_id, 2)
        lscale = iscale == 1
        length = ilflag == 1
        idp = idpnt(substance_i)
        ivp = ivpnt(substance_i)

        ! reset the entire matrix
        amat = 0.0d0

        do iq = 1, num_exchanges
            ! pointer administration check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            end if
            if (ito > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if

            ! initialisations
            a = area(iq)
            q = flow(iq)
            if (abs(q) < 10.0e-25 .and. btest(integration_id, 0)) cycle   ! thin dam option, no dispersion at zero flow
            if (iq <= num_exchanges_u_dir) then
                e = disp(1)
                if (length) then
                    if (aleng(1, iq) + aleng(2, iq) > 1.0e-25) then
                        dl = a / (aleng(1, iq) + aleng(2, iq))
                    else
                        dl = 0.0
                    end if
                else
                    dl = a / aleng(1, 1)         ! first element of the array
                end if
            else if (iq <= num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp(2)
                if (length) then
                    if (aleng(1, iq) + aleng(2, iq) > 1.0e-25) then
                        dl = a / (aleng(1, iq) + aleng(2, iq))
                    else
                        dl = 0.0
                    end if
                else
                    dl = a / aleng(2, 1)         ! second element of the array
                end if
            else
                e = disp(3)
                if (length) then
                    if (aleng(1, iq) + aleng(2, iq) > 1.0e-25) then
                        dl = a / (aleng(1, iq) + aleng(2, iq))
                        f1 = aleng(2, iq) / (aleng(1, iq) + aleng(2, iq))
                        f2 = aleng(1, iq) / (aleng(1, iq) + aleng(2, iq))
                    else
                        dl = 0.0
                        f1 = 0.5
                        f2 = 0.5
                    end if
                else
                    dl = a / aleng(1, 2)         ! third element of the array
                    f1 = 0.5
                    f2 = 0.5
                end if
            end if
            e = e * dl

            ! add additional dispersions and fluxes
            if (idp > 0) e = e + disper(idp, iq) * dl
            if (ivp > 0) then
                qvel = velo(ivp, iq) * a
            else
                qvel = 0.0
            end if

            ! Option zero disp over the boundaries (also for additonal dispersions)
            if (zerob .and. (ifrom < 0 .or. ito < 0)) e = 0.0

            if (iq <= num_exchanges_u_dir + num_exchanges_v_dir) then

                ! for the first two directions apply  first order Upwind
                q = q + qvel
                if (q > 0.0) then
                    q1 = q
                    q2 = 0.0
                else
                    q1 = 0.0
                    q2 = q
                end if

                ! for the third direction apply central discretization
            else
                ! apply upwind at boundaries (these are vertical boundaries !) for loword option
                if (loword .and. (ifrom < 0 .or. ito < 0)) then
                    q = q + qvel
                    if (q > 0.0) then
                        q1 = q
                        q2 = 0.0
                    else
                        q1 = 0.0
                        q2 = q
                    end if
                else
                    q1 = q * f1
                    q2 = q * f2
                    if (qvel > 0.0) then
                        q1 = q1 + qvel
                    else
                        q2 = q2 + qvel
                    end if
                end if
            end if

            ! fill the matrix
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) then
                    if (q + qvel > 0.0) then
                        q1 = q + qvel
                        q2 = 0.0
                    else
                        q1 = 0.0
                        q2 = q + qvel
                    end if
                end if
                diag(ifrom) = diag(ifrom) + q1 + e
                amat(fmat(iq)) = amat(fmat(iq)) + q2 - e
            end if
            if (ito > 0) then
                if (.not. btest(iknmrk(ito), 0)) then
                    if (q + qvel > 0.0) then
                        q1 = q + qvel
                        q2 = 0.0
                    else
                        q1 = 0.0
                        q2 = q + qvel
                    end if
                end if
                diag(ito) = diag(ito) - q2 + e
                amat(tmat(iq)) = amat(tmat(iq)) - q1 - e
            end if

            ! end of the loop over exchanges
        end do

        ! finally scale the matrix to avoid possible round-off errors in GMRES
        ! this scaling may need some adaption for future domain decomposition b.c.
        if (lscale) then
            do iq = 1, num_cells + num_boundary_conditions
                if (iq > 1) then
                    ifrom = idiag(iq - 1) + 1
                else
                    ifrom = 1
                end if
                ito = idiag(iq)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(iq)) < 1.0d-100) diag(iq) = 1.0

                do jq = ifrom, ito
                    amat(jq) = amat(jq) / diag(iq)
                end do

                ! copy of diag for later scaling purposes in fill_rhs_for_gmres_solver
                diagcc(iq) = diag(iq)
                diag(iq) = 1.0d00
            end do
        else
            do iq = 1, num_cells + num_boundary_conditions
                diagcc(iq) = 1.0d00
            end do
        end if

        if (timon) call timstop(ithandl)
    end subroutine fill_matrix_hz_backward_diff_vl_central_diff

    !> Fills off-diagonal values of the matrix for GMRES fast solver
    !! horizontally and vertically according to backward differences in space
    subroutine fill_matrix_hz_backward_diff_vl_backward_diff(num_cells, num_boundary_conditions, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, &
            ipoint, num_dispersion_arrays, num_velocity_arrays, idpnt, ivpnt, &
            area, flow, disp, disper, velo, &
            substance_i, fast_solver_arr_size, amat, imat, idiag, &
            diag, diagcc, iscale, fmat, tmat, &
            mixlen, iknmrk)

        integer(kind = int_wp), intent(in) :: num_cells                  !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                  !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                   !< Number of fluxes first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                   !< Number of fluxes second direction
        integer(kind = int_wp), intent(in) :: num_exchanges                    !< Total number fluxes in the water phase
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)         !< From, to, from-1, to+1 volume numbers per flux
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                 !< Number of additional dispersion arrays
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                 !< Number of additional velocity   arrays
        integer(kind = int_wp), intent(in) :: idpnt(*)               !< Dispersion array to be applied per substance
        integer(kind = int_wp), intent(in) :: ivpnt(*)               !< Velocity   array to be applied per substance
        real(kind = real_wp), intent(in) :: area(num_exchanges)              !< Crosssectional surface areas of the fluxes
        real(kind = real_wp), intent(in) :: flow(num_exchanges)              !< Fluxes
        real(kind = real_wp), intent(in) :: disp(3)                !< Default dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges)    !< Additional dispersion arrays
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)      !< Additional velocity arrays
        integer(kind = int_wp), intent(in) :: substance_i                   !< Substances number to be used for this matrix
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                  !< Dimension of off-diagonal matrix amat
        real(kind = dp), intent(out) :: amat(fast_solver_arr_size)            !< Matrix with off-diagonal entries
        integer(kind = int_wp), intent(in) :: imat(fast_solver_arr_size)            !< Pointers of the off-diagonals in amat
        integer(kind = int_wp), intent(in) :: idiag(0:num_cells + num_boundary_conditions) !< Position of the diagonals in amat
        real(kind = dp), intent(inout) :: diag(num_cells + num_boundary_conditions)    !< Diagonal of the matrix
        real(kind = dp), intent(inout) :: diagcc(num_cells + num_boundary_conditions)  !< Copy of (unscaled) diagonal of the matrix
        integer(kind = int_wp), intent(in) :: iscale                 !< = 0 no row scaling of diagonal
        !< = 1    row scaling of diagonal
        integer(kind = int_wp), intent(in) :: fmat(num_exchanges)              !< Location from(iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat(num_exchanges)              !< Location to  (iq) in matrix
        real(kind = real_wp), intent(in) :: mixlen(num_exchanges)            !< Area/length for diffusion
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)          !< Feature array, bit zero indicates wet or not

        ! Local variables
        logical lscale                !<  APPLY row scaling of the diagonal?
        integer(kind = int_wp) :: ifrom !<  From cell index
        integer(kind = int_wp) :: ito   !<  To cell index
        real(kind = real_wp) :: a     !<  Auxiliary variable for exchange surface area in m2
        real(kind = real_wp) :: q     !<  Auxiliary variable for the flux in m3/s
        real(kind = real_wp) :: e     !<  Auxiliary variable for diffusive flux in m3/s
        integer(kind = int_wp) :: idp   !<  Auxiliary variable for idpnt(substance_i)
        integer(kind = int_wp) :: ivp   !<  Auxiliary variable for ivpnt(substance_i)
        real(kind = real_wp) :: q1    !<  Auxiliary variable
        real(kind = real_wp) :: q2    !<  Auxiliary variable
        integer(kind = int_wp) :: iq    !<  Loop counter
        integer(kind = int_wp) :: jq    !<  Loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt("fill_matrix_hz_backward_diff_vl_backward_diff", ithandl)

        ! set the logicals for dispersion and scaling and other fixed items
        lscale = iscale == 1
        idp = idpnt(substance_i)
        ivp = ivpnt(substance_i)

        ! reset the entire matrix
        amat = 0.0d0

        do iq = 1, num_exchanges
            ! pointer administration check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            end if
            if (ito > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if

            ! initialisations
            a = area(iq)
            q = flow(iq)
            if (a < 1.0e-25) a = 1.0
            if (iq <= num_exchanges_u_dir) then
                e = disp(1)
            else if (iq <= num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp(2)
            else
                e = disp(3)
            end if
            e = e * mixlen(iq)

            ! add additional dispersions and fluxes
            if (idp > 0) e = e + disper(idp, iq) * mixlen(iq)
            if (ivp > 0) q = q + velo(ivp, iq) * a

            ! the backward differencing in space
            if (q > 0.0) then
                q1 = q
                q2 = 0.0
            else
                q1 = 0.0
                q2 = q
            end if

            ! fill the matrix
            if (ifrom > 0) then
                diag(ifrom) = diag(ifrom) + q1 + e
                amat(fmat(iq)) = amat(fmat(iq)) + q2 - e
            end if
            if (ito > 0) then
                diag(ito) = diag(ito) - q2 + e
                amat(tmat(iq)) = amat(tmat(iq)) - q1 - e
            end if

            ! end of the loop over exchanges

        end do

        ! finally scale the matrix to avoid possible round-off errors in GMRES
        ! this scaling may need some adaption for future domain decomposition b.c.
        if (lscale) then
            do iq = 1, num_cells + num_boundary_conditions
                ifrom = idiag(iq - 1) + 1
                ito = idiag(iq)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(iq)) < 1.0d-100) diag(iq) = 1.0

                do jq = ifrom, ito
                    amat(jq) = amat(jq) / diag(iq)
                end do

                ! copy of diag for later scaling purposes in fill_rhs_for_gmres_solver
                diagcc(iq) = diag(iq)
                diag(iq) = 1.0d00
            end do
        else
            do iq = 1, num_cells + num_boundary_conditions
                diagcc(iq) = 1.0d00
            end do
        end if

        if (timon) call timstop(ithandl)
    end subroutine fill_matrix_hz_backward_diff_vl_backward_diff

    !> Sets the diagonal for the steady state option,
    !! updates first order term on the diagonal
    !! and compresses DERIV for use in SGMRES
    subroutine fill_matrix_set_diagonal_steady_state(num_cells, num_substances_total, num_boundary_conditions, substance_i, diag, &
            delvol, conc)

        integer(kind = int_wp), intent(in) :: num_cells               !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_total               !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_boundary_conditions            !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: substance_i                        !< This substance number
        real(kind = dp), intent(inout) :: diag(num_cells + num_boundary_conditions) !< Diagonal vector (1st order term)
        real(kind = real_wp), intent(in) :: delvol(num_cells)       !< Closure error correction
        real(kind = real_wp), intent(in) :: conc(num_substances_total, num_cells)  !< First order term

        ! Local variables
        integer(kind = int_wp) :: cell_i ! loop counter for computational volumes
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("fill_matrix_set_diagonal_steady_state", ithandl)

        ! set the right hand side and
        ! set the diagonal for steady state
        ! first order decay in conc
        do cell_i = 1, num_cells
            diag(cell_i) = -conc(substance_i, cell_i) + delvol(cell_i)
        end do
        do cell_i = num_cells + 1, num_cells + num_boundary_conditions
            diag(cell_i) = 1.0
        end do
        if (timon) call timstop(ithandl)
    end subroutine fill_matrix_set_diagonal_steady_state


    !> Fills in the matrix for self adjusting theta algorithm
    subroutine fill_matrix_for_theta_algorithm(idt, num_cells, volnew, num_boundary_conditions, num_exchanges, &
            ipoint, flowtot, disptot, theta, diag, &
            iscale, diagcc, fast_solver_arr_size, mat, rowpnt, &
            fmat, tmat, iexseg)

        integer(kind = int_wp), intent(in) :: idt                         !< Time step
        integer(kind = int_wp), intent(in) :: num_cells                   !< Number of cells
        real(kind = real_wp), intent(in) :: volnew(num_cells)             !< Segment volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions     !< Number of boundary cells
        integer(kind = int_wp), intent(in) :: num_exchanges               !< Number of exchanges
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)    !< Exchange pointers
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)    !< Total flows (including additional velocities)
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)  !< Total dispersion (including additional dipersion)
        real(kind = real_wp), intent(in) :: theta(num_exchanges)    !< Variable theta coefficients
        real(kind = dp), intent(out) :: diag(num_cells + num_boundary_conditions)  !< Diagonal matrix (scaled or not) elements
        integer(kind = int_wp), intent(in) :: iscale                  !< 0: no diagonal scaling
        !< 1: diagonal scaling
        real(kind = dp), intent(out) :: diagcc(num_cells + num_boundary_conditions)   !< Copy of the unscaled diagonal, needed to scale the rhs later
        integer(kind = int_wp), intent(in) :: fast_solver_arr_size                   !< Number of non-zero off-diagonal matrix elements
        real(kind = dp), intent(out) :: mat(fast_solver_arr_size)              !< Non-zero off-diagonal matrix (scaled or not) elements (elsewhere: amat)
        integer(kind = int_wp), intent(in) :: rowpnt(0:num_cells + num_boundary_conditions) !< Row pointer, contains row lengths of mat (elsewhere: itrac)
        integer(kind = int_wp), intent(in) :: fmat(num_exchanges)               !< Index from (iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat(num_exchanges)               !< Index to (iq) in matrix
        integer(kind = int_wp), intent(in) :: iexseg(num_cells + num_boundary_conditions)   !< Zero if explicit

        ! Local variables
        integer(kind = int_wp) :: cell_i  !< Index of current cell
        integer(kind = int_wp) :: iq    !< Index of current edge
        integer(kind = int_wp) :: ito   !< Index from cell
        integer(kind = int_wp) :: ifrom !< Index to cell
        real(kind = real_wp) :: q1    !< Flow 1
        real(kind = real_wp) :: q2    !< Flow 2

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("fill_matrix_for_theta_algorithm", ithandl)

        ! set the diagonal
        do cell_i = 1, num_cells
            diag(cell_i) = volnew(cell_i) / real(idt)
        end do
        do cell_i = num_cells + 1, num_cells + num_boundary_conditions
            diag(cell_i) = 1.0
        end do

        ! reset the offdiagonal entries
        do iq = 1, fast_solver_arr_size
            mat(iq) = 0.0
        end do

        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            if (flowtot(iq) > 0.0) then
                q1 = (flowtot(iq) + disptot(iq)) * theta(iq)
                q2 = (0.0 - disptot(iq)) * theta(iq)
            else
                q1 = (0.0 + disptot(iq)) * theta(iq)
                q2 = (flowtot(iq) - disptot(iq)) * theta(iq)
            end if

            if (ifrom > 0) then
                diag(ifrom) = diag(ifrom) + q1
                mat(fmat(iq)) = mat(fmat(iq)) + q2
            end if
            if (ito > 0) then
                diag(ito) = diag(ito) - q2
                mat(tmat(iq)) = mat(tmat(iq)) - q1
            end if
        end do

        ! finally scale the matrix to avoid possible round-off errors in gmres
        ! this scaling may need some adaption for future domain decomposition b.c.
        if (iscale == 1) then
            do cell_i = 1, num_cells + num_boundary_conditions
                ifrom = rowpnt(cell_i - 1) + 1
                ito = rowpnt(cell_i)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(cell_i)) < 1.0e-35) diag(cell_i) = 1.0

                do iq = ifrom, ito
                    mat(iq) = mat(iq) / diag(cell_i)
                end do

                ! copy of diag for later scaling purposes in fill_rhs_for_gmres_solver
                diagcc(cell_i) = diag(cell_i)
                diag(cell_i) = 1.0
            end do
        else
            do cell_i = 1, num_cells + num_boundary_conditions
                diagcc(cell_i) = 1.0
            end do
        end if
        if (timon) call timstop(ithandl)
    end subroutine fill_matrix_for_theta_algorithm


    !> Fills in the rhs and the initial guess for the adjusting theta algorithm
    subroutine fill_rhs_initial_guess_theta(idt, substance_i, num_substances_transported, num_substances_total, num_cells, &
            conc, deriv, volold, num_boundary_conditions, bound, &
            num_exchanges, ipoint, flowtot, disptot, theta, &
            diag, iscale, rhs, sol)

        integer(kind = int_wp), intent(in) :: idt                 !< Time step
        integer(kind = int_wp), intent(in) :: substance_i                !< Current active substance
        integer(kind = int_wp), intent(in) :: num_substances_transported               !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total               !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells               !< Number of cells or segments
        real(kind = real_wp), intent(in) :: conc(num_substances_total, num_cells)  !< Concentrations
        real(kind = real_wp), intent(in) :: deriv(num_substances_total, num_cells) !< Processes and discharges (divided by the time step idt)
        real(kind = real_wp), intent(in) :: volold(num_cells)       !< Segment volumes at the previous time
        integer(kind = int_wp), intent(in) :: num_boundary_conditions               !< Number of boundary segments
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions) !< Boundary concentrions
        integer(kind = int_wp), intent(in) :: num_exchanges                 !< Number of exchanges
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)      !< Exchange pointers (dim: 4 x num_exchanges)
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)        !< Flows plus additional velos. (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)        !< Dispersion plus additional dipers. (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: theta(num_exchanges)          !< Variable theta coefficients
        real(kind = dp), intent(in) :: diag(num_cells + num_boundary_conditions) !< Diagonal of the matrix (lhs)
        integer(kind = int_wp), intent(in) :: iscale              !< 0: no diagonal scaling
        !< 1: diagonal scaling
        real(kind = dp), intent(out) :: rhs(num_cells + num_boundary_conditions)  !< Right hand side
        real(kind = dp), intent(out) :: sol(num_cells + num_boundary_conditions)  !< Initial guess

        ! Local variables
        integer(kind = int_wp) :: ifrom  !< Index cell from
        integer(kind = int_wp) :: ito    !< Index cell to
        real(kind = real_wp) :: ci     !< Concentration from
        real(kind = real_wp) :: cj     !< Concentration to
        real(kind = real_wp) :: fluxij !< Flux from cell i to cell j
        integer(kind = int_wp) :: cell_i   !< Index current cell
        integer(kind = int_wp) :: ibnd   !< Index current boundary cell
        integer(kind = int_wp) :: iq     !< Index current edge

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("fill_rhs_initial_guess_theta", ithandl)

        ! volumes, processes, and discharges
        do cell_i = 1, num_cells
            rhs(cell_i) = volold(cell_i) * conc(substance_i, cell_i) / real(idt) + deriv(substance_i, cell_i)
        end do

        do ibnd = 1, num_boundary_conditions
            rhs(num_cells + ibnd) = bound(substance_i, ibnd)
        end do

        ! flow and diffusion
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            ! compute from- and to concentrations
            if (ifrom > 0) then
                ci = conc(substance_i, ifrom)
            else
                ci = bound(substance_i, -ifrom)
            end if
            if (ito > 0) then
                cj = conc(substance_i, ito)
            else
                cj = bound(substance_i, -ito)
            end if

            ! compute flux from i to j
            if (flowtot(iq) > 0) then         ! flow from i to j
                fluxij = flowtot(iq) * ci - disptot(iq) * (cj - ci)
            else                                   ! flow from j to i
                fluxij = flowtot(iq) * cj - disptot(iq) * (cj - ci)
            end if

            ! add flux to both neighbours
            if (ifrom > 0) rhs(ifrom) = rhs(ifrom) - (1 - theta(iq)) * fluxij
            if (ito > 0) rhs(ito) = rhs(ito) + (1 - theta(iq)) * fluxij
        end do

        ! scale rhs (diagonal scaling to improve convergence of gmres)
        if (iscale == 1) then
            do cell_i = 1, num_cells + num_boundary_conditions
                rhs(cell_i) = rhs(cell_i) / diag(cell_i)
            end do
        end if

        ! zero initial guess, try previous concentration for water volumes
        ! ( alternatively take zero vector ). Zero initial guess for boundaries.
        sol = 0.0
        do cell_i = 1, num_cells
            sol(cell_i) = conc(substance_i, cell_i) + 0.01
        end do

        if (timon) call timstop(ithandl)
    end subroutine fill_rhs_initial_guess_theta

    !> Define right hand side of the matrix equation
    subroutine fill_rhs_for_gmres_solver(num_cells, num_boundary_conditions, num_substances_transported, &
            num_substances_total, substance_i, idt, conc, deriv, volold, bound, rhs, diag, sol)

        integer(kind = int_wp), intent(in) :: num_cells               !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions           !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: num_substances_transported        !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total              !< Total number of substances
        integer(kind = int_wp), intent(in) :: substance_i                !< This substance
        integer(kind = int_wp), intent(in) :: idt                 !< Timestep
        real(kind = real_wp), intent(in) :: conc(num_substances_total, num_cells)  !< All concentrations
        real(kind = real_wp), intent(in) :: deriv(num_substances_total, num_cells) !< All derivatives (loads, processes)
        real(kind = real_wp), intent(in) :: volold(num_cells)       !< Volumes at beginning of time step
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions) !< Open boundary concentrations
        real(kind = dp), intent(out) :: rhs(num_cells + num_boundary_conditions)  !< Right hand side of the equation
        real(kind = dp), intent(in) :: diag(num_cells + num_boundary_conditions) !< Value of the diagonal
        real(kind = dp), intent(out) :: sol(num_cells + num_boundary_conditions)  !< Initial guess

        ! Local variables
        real(kind = dp) :: ddt          !< 1.0 / time step in double precision
        integer(kind = int_wp) :: cell_i  !< Loop variable

        ! The WAQ-timer
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt("fill_rhs_for_gmres_solver", ithandl)

        ! set the right hand side, normal part
        ddt = 1.0d00 / idt

        do cell_i = 1, num_cells
            rhs(cell_i) = deriv(substance_i, cell_i) + volold(cell_i) * conc(substance_i, cell_i) * ddt
        end do

        ! set the right hand side, open boundary part
        do cell_i = 1, num_boundary_conditions
            rhs(num_cells + cell_i) = bound(substance_i, cell_i)
        end do

        ! row scaling
        do cell_i = 1, num_cells + num_boundary_conditions
            rhs(cell_i) = rhs(cell_i) / diag(cell_i)
        end do

        ! zero initial guess, try previous concentration for water volumes
        ! ( alternatively take zero vector ). Zero initial guess for boundaries.
        sol = 0.0
        do cell_i = 1, num_cells
            sol(cell_i) = conc(substance_i, cell_i) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine fill_rhs_for_gmres_solver

end module m_gmres_utils
