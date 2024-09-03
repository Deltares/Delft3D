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
module m_scheme_16_utils
    use m_waq_precision
    use timers

    implicit none

    private
    public :: initialize_diagonal_for_gmres, tranfer_solution_from_rhs_into_conc

contains


    !> Initialize diagonal for fast solvers
    subroutine initialize_diagonal_for_gmres(num_cells, num_boundary_conditions, idt, volnew, trace)

        integer(kind = int_wp), intent(in) :: num_cells !< Number of cells or computational volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: idt   !< Time step size

        real(kind = real_wp), intent(in   ) :: volnew(num_cells)        !< Volumes end of time step
        real(kind = dp),      intent(  out) :: trace(num_cells + num_boundary_conditions) !< Diagonal vector

        ! Local variables
        real(kind = dp) :: dt           !< Time step in double precision
        integer(kind = int_wp) :: cell_i  !< Loop variable

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("Initialize_diagonal_for_gmres", ithandl)

        ! set the diagonal
        dt = idt
        do cell_i = 1, num_cells
            trace(cell_i) = volnew(cell_i) / dt
        end do
        do cell_i = num_cells + 1, num_cells + num_boundary_conditions
            trace(cell_i) = 1.0
        end do
        if (timon) call timstop (ithandl)
    end subroutine Initialize_diagonal_for_gmres

    !>  Tranfers solution from RHS into CONC, and makes RHS equal to zero
    subroutine tranfer_solution_from_rhs_into_conc(num_cells, num_substances_total, substance_i, nsys, rhs, conc, iknmrk)

        integer(kind = int_wp), intent(in) :: num_cells              !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_total              !< Total number of substances
        integer(kind = int_wp), intent(in) :: substance_i               !< First substance to update
        integer(kind = int_wp), intent(in) :: nsys               !< Total number of substances to update
        real(kind = dp), intent(inout) :: rhs(nsys, num_cells)   !< RHS matrix for the nsys substances
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells) !< Target array for update
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)      !< feature array, bit zero indicates wet or not

        ! Local variables
        integer(kind = int_wp) :: cell_i, j       !< Loop variables
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("tranfer_solution_from_rhs_into_conc", ithandl)

        do cell_i = 1, num_cells
            do j = 1, nsys
                conc(substance_i + j - 1, cell_i) = rhs(j, cell_i)
                rhs(j, cell_i) = 0.0d00
            end do
        end do
        if (timon) call timstop(ithandl)
    end subroutine tranfer_solution_from_rhs_into_conc
end module m_scheme_16_utils
