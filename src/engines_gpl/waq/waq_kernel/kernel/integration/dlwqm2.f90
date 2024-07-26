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
module m_dlwqm2
    use m_waq_precision

    implicit none

contains

    !> Fills in the matrix for self adjusting theta algorithm
    subroutine dlwqm2(idt, num_cells, volnew, num_boundary_conditions, num_exchanges, &
                      ipoint, flowtot, disptot, theta, diag, &
                      iscale, diagcc, fast_solver_arr_size, mat, rowpnt, &
                      fmat, tmat, iexseg)

        use timers
        implicit none

        integer(kind=int_wp), intent(in   ) :: idt                     !< Time step
        integer(kind=int_wp), intent(in   ) :: num_cells                   !< Number of cells
        real(kind=real_wp),   intent(in   ) :: volnew(num_cells)           !< Segment volumes
        integer(kind=int_wp), intent(in   ) :: num_boundary_conditions                   !< Number of boundary cells
        integer(kind=int_wp), intent(in   ) :: num_exchanges                     !< Number of exchanges
        integer(kind=int_wp), intent(in   ) :: ipoint(4, num_exchanges)          !< Exchange pointers
        real(kind=real_wp),   intent(in   ) :: flowtot(num_exchanges)            !< Total flows (including additional velocities)
        real(kind=real_wp),   intent(in   ) :: disptot(num_exchanges)            !< Total dispersion (including additional dipersion)
        real(kind=real_wp),   intent(in   ) :: theta(num_exchanges)              !< Variable theta coefficients
        real(kind=dp),        intent(  out) :: diag(num_cells + num_boundary_conditions)     !< Diagonal matrix (scaled or not) elements
        integer(kind=int_wp), intent(in   ) :: iscale                  !< 0: no diagonal scaling
                                                                       !< 1: diagonal scaling
        real(kind=dp),        intent(  out) :: diagcc(num_cells + num_boundary_conditions)   !< Copy of the unscaled diagonal, needed to scale the rhs later
        integer(kind=int_wp), intent(in   ) :: fast_solver_arr_size                   !< Number of non-zero off-diagonal matrix elements
        real(kind=dp),        intent(  out) :: mat(fast_solver_arr_size)              !< Non-zero off-diagonal matrix (scaled or not) elements (elsewhere: amat)
        integer(kind=int_wp), intent(in   ) :: rowpnt(0:num_cells + num_boundary_conditions) !< Row pointer, contains row lengths of mat (elsewhere: itrac)
        integer(kind=int_wp), intent(in   ) :: fmat(num_exchanges)               !< Index from (iq) in matrix
        integer(kind=int_wp), intent(in   ) :: tmat(num_exchanges)               !< Index to (iq) in matrix
        integer(kind=int_wp), intent(in   ) :: iexseg(num_cells + num_boundary_conditions)   !< Zero if explicit

        ! Local variables
        integer(kind=int_wp) :: iseg  !< Index of current cell
        integer(kind=int_wp) :: iq    !< Index of current edge
        integer(kind=int_wp) :: ito   !< Index from cell
        integer(kind=int_wp) :: ifrom !< Index to cell
        real(kind=real_wp)   :: q1    !< Flow 1
        real(kind=real_wp)   :: q2    !< Flow 2

        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqm2", ithandl)

        ! set the diagonal
        do iseg = 1, num_cells
            diag(iseg) = volnew(iseg)/real(idt)
        end do
        do iseg = num_cells + 1, num_cells + num_boundary_conditions
            diag(iseg) = 1.0
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
                q1 = (flowtot(iq) + disptot(iq))*theta(iq)
                q2 = (0.0 - disptot(iq))*theta(iq)
            else
                q1 = (0.0 + disptot(iq))*theta(iq)
                q2 = (flowtot(iq) - disptot(iq))*theta(iq)
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
            do iseg = 1, num_cells + num_boundary_conditions
                ifrom = rowpnt(iseg - 1) + 1
                ito = rowpnt(iseg)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(iseg)) < 1.0e-35) diag(iseg) = 1.0

                do iq = ifrom, ito
                    mat(iq) = mat(iq)/diag(iseg)
                end do

                ! copy of diag for later scaling purposes in dlwqf4
                diagcc(iseg) = diag(iseg)
                diag(iseg) = 1.0
            end do
        else
            do iseg = 1, num_cells + num_boundary_conditions
                diagcc(iseg) = 1.0
            end do
        end if
        if (timon) call timstop(ithandl)
    end subroutine dlwqm2
end module m_dlwqm2
