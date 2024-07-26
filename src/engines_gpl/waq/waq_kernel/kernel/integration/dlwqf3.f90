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
module m_dlwqf3
    use m_waq_precision

    implicit none

contains

    !> Fills matrix to be used by the GMRES fast solver, once per substance
    !! Matrix is filled:
    !! - horizontally according to upwind differences in space
    !! - vertically   according to upwind differences in space
    !! It is assumed that any logic on drying and flooding is in the
    !! precomputed flowtot and disptot arrays./n
    !! The routine is very efficient because of the precomputed fmat
    !! and tmat arrays for the from and to locations in the matrix.
    subroutine dlwqf3(idt, num_cells, volnew, num_boundary_conditions, num_exchanges, &
                      ipoint, flowtot, disptot, diag, iscale, &
                      diagcc, fast_solver_arr_size, amat, idiag, fmat, &
                      tmat)

        use timers
        implicit none

        integer(kind=int_wp), intent(in) :: idt                  !< Time step
        integer(kind=int_wp), intent(in) :: num_cells                !< Number of cells or computational volumes
        real(kind=real_wp), intent(in) :: volnew(num_cells)          !< Volumes of cells
        integer(kind=int_wp), intent(in) :: num_boundary_conditions                !< Number of open boundaries
        integer(kind=int_wp), intent(in) :: num_exchanges                  !< Total number fluxes in the water phase
        integer(kind=int_wp), intent(in) :: ipoint(4, num_exchanges)       !< From, to, from-1, to+1 volume numbers per flux
        real(kind=real_wp), intent(in) :: flowtot(num_exchanges)           !< Flows plus additional velocities (dim: num_exchanges)
        real(kind=real_wp), intent(in) :: disptot(num_exchanges)           !< Dispersion plus additional dipersion (dim: num_exchanges)
        real(kind=dp), intent(inout) :: diag(num_cells+num_boundary_conditions)        !< Diagonal of the matrix
        integer(kind=int_wp), intent(in) :: iscale               !< = 1 row scaling with the diagonal
        real(kind=dp), intent(inout) :: diagcc(num_cells+num_boundary_conditions)      !< Copy of (unscaled) diagonal of the matrix
        integer(kind=int_wp), intent(in) :: fast_solver_arr_size                !< Dimension of off-diagonal matrix amat
        real(kind=dp), intent(out) :: amat(fast_solver_arr_size)                !< Matrix with off-diagonal entries
        integer(kind=int_wp), intent(in) :: idiag(0:num_cells+num_boundary_conditions) !< Position of the diagonals in amat
        integer(kind=int_wp), intent(in) :: fmat(num_exchanges)            !< Location from(iq) in matrix
        integer(kind=int_wp), intent(in) :: tmat(num_exchanges)            !< Location to  (iq) in matrix

        ! Local variables
        integer(kind=int_wp) :: iseg  !< Index of current cell
        integer(kind=int_wp) :: iq    !< Index current edge
        integer(kind=int_wp) :: jq    !< Index current edge 2
        integer(kind=int_wp) :: ito   !< Index to volume
        integer(kind=int_wp) :: ifrom !< Index from volume

        real(kind=real_wp) :: q1 !< flow 1
        real(kind=real_wp) :: q2 !< flow 2

        real(kind=dp) :: dt !< time step in double precision

        ! WAQ timers

        integer(kind=int_wp) :: ithandl = 0
        if (timon) call timstrt("dlwqf3", ithandl)

        ! set the diagonal
        dt = idt
        do iseg = 1, num_cells
            diag(iseg) = volnew(iseg)/dt
        end do
        do iseg = num_cells + 1, num_cells + num_boundary_conditions
            diag(iseg) = 1.0
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
                    amat(jq) = amat(jq)/diag(iq)
                end do

                ! copy of diag for later scaling purposes in DLWQF4
                diagcc(iq) = diag(iq)
                diag(iq) = 1.0d00
            end do
        else
            do iq = 1, num_cells + num_boundary_conditions
                diagcc(iq) = 1.0d00
            end do
        end if
        if (timon) call timstop(ithandl)
    end subroutine dlwqf3
end module m_dlwqf3
