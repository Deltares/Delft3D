!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_scheme_17_and_18_utils
    use m_waq_precision
    use timers

    implicit none

    private
    public :: copy_derivatives_boundaries_to_rhs_gmres, copy_steady_state_solution_to_concentration_array

contains

    !> Move terms for boundaries and derivatives to right hand side
    subroutine copy_derivatives_boundaries_to_rhs_gmres(num_cells, num_substances_transported, num_substances_total, &
            num_boundary_conditions, substance_i, deriv, bound, rhs, diag, sol)

        integer(kind = int_wp), intent(in) :: num_cells                          !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_transported         !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total               !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_boundary_conditions            !< Number of boundaries
        integer(kind = int_wp), intent(in) :: substance_i                        !< This substance
        real(kind = real_wp), intent(in) :: deriv(num_substances_total, num_cells) !< Derivatives
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions) !< Open boundary values
        real(kind = dp), intent(inout) :: rhs(num_cells + num_boundary_conditions)  !< Right hand side of the equation
        real(kind = dp), intent(in) :: diag(num_cells + num_boundary_conditions) !< diagonal for scaling
        real(kind = dp), intent(inout) :: sol(num_cells + num_boundary_conditions)  !< initial guess for solution

        !     Local variables
        integer(kind = int_wp) :: cell_i       !< loop counter

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("copy_derivatives_boundaries_to_rhs_gmres ", ithandl)
        ! initialize the rhs and apply row scaling
        do cell_i = 1, num_cells
            rhs(cell_i) = deriv(substance_i, cell_i) / diag(cell_i)
        end do
        do cell_i = 1, num_boundary_conditions
            rhs(cell_i + num_cells) = bound(substance_i, cell_i)
        end do
        ! zero initial guess, try rhs plus small value
        sol = 0.0
        do cell_i = 1, num_cells
            sol(cell_i) = rhs(cell_i) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine copy_derivatives_boundaries_to_rhs_gmres

    !> Places the steady state solution in the concentration array
    subroutine copy_steady_state_solution_to_concentration_array(num_cells, num_substances_total, substance_i, nsys, conc, &
            sol, amass2, dmps, intopt, isdmp)

        integer(kind = int_wp), intent(in) :: num_cells              !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_total              !< Total number of substances
        integer(kind = int_wp), intent(in) :: substance_i               !< First substance to update
        integer(kind = int_wp), intent(in) :: nsys               !< Total number of substances to update
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells) !< Target array for update
        real(kind = dp), intent(inout) :: sol(nsys, num_cells)   !< Solution matrix for the nsys substances
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)   !< Mass accumulation array
        real(kind = real_wp), intent(inout) :: dmps(num_substances_total, *)     !< Dumped segment fluxes if intopt > 7
        integer(kind = int_wp), intent(in) :: intopt             !< Integration sub options
        integer(kind = int_wp), intent(in) :: isdmp(num_cells)       !< Pointer dumped segments

        ! Local variables
        integer(kind = int_wp) :: cell_i, i, ip   ! loop variables

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt("copy_steady_state_solution_to_concentration_array", ithandl)

        ! Place result in concentration array
        if (.not. btest(intopt, 3)) then
            do cell_i = 1, num_cells
                do i = substance_i, substance_i + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, cell_i) * sol(i - substance_i + 1, cell_i)
                    conc(i, cell_i) = sol(i - substance_i + 1, cell_i)
                    sol(i - substance_i + 1, cell_i) = 0.0d00
                end do
            end do
        else
            do cell_i = 1, num_cells
                ip = isdmp(cell_i)
                do i = substance_i, substance_i + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, cell_i) * sol(i - substance_i + 1, cell_i)
                    if (ip > 0) then
                        dmps(i, ip) = dmps(i, ip) + conc(i, cell_i) * sol(i - substance_i + 1, cell_i)
                    end if
                    conc(i, cell_i) = sol(i - substance_i + 1, cell_i)
                    sol(i - substance_i + 1, cell_i) = 0.0d00
                end do
            end do
        end if
        if (timon) call timstop(ithandl)
    end subroutine copy_steady_state_solution_to_concentration_array
end module m_scheme_17_and_18_utils
