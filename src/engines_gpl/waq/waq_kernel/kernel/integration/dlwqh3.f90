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
module m_dlwqh3
    use m_waq_precision

    implicit none

contains

    !> Move terms for boundaries and derivatives to right hand side
    subroutine dlwqh3(num_cells, num_substances_transported, num_substances_total, num_boundary_conditions, isys, &
                      deriv, bound, rhs, diag, sol)
        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: num_cells               !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: num_substances_transported               !< Number of transported substances
        integer(kind=int_wp), intent(in   ) :: num_substances_total               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: num_boundary_conditions               !< Number of boundaries
        integer(kind=int_wp), intent(in   ) :: isys                !< This substance
        real(kind=real_wp),   intent(in   ) :: deriv(num_substances_total, num_cells) !< Derivatives
        real(kind=real_wp),   intent(in   ) :: bound(num_substances_transported, num_boundary_conditions) !< Open boundary values
        real(kind=dp),        intent(inout) :: rhs(num_cells + num_boundary_conditions)  !< Right hand side of the equation
        real(kind=dp),        intent(in   ) :: diag(num_cells + num_boundary_conditions) !< diagonal for scaling
        real(kind=dp),        intent(inout) :: sol(num_cells + num_boundary_conditions)  !< initial guess for solution

        !     Local variables
        integer(kind=int_wp) :: iseg       !< loop counter

        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqh3", ithandl)
        ! initialize the rhs and apply row scaling
        do iseg = 1, num_cells
            rhs(iseg) = deriv(isys, iseg)/diag(iseg)
        end do
        do iseg = 1, num_boundary_conditions
            rhs(iseg + num_cells) = bound(isys, iseg)
        end do
        ! zero initial guess, try rhs plus small value
        sol = 0.0
        do iseg = 1, num_cells
            sol(iseg) = rhs(iseg) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqh3
end module m_dlwqh3
