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
module m_dlwqf4
    use m_waq_precision

    implicit none

contains

    !> Define right hand side of the matrix equation
    subroutine dlwqf4(num_cells, num_boundary_conditions, num_substances_transported, num_substances_total, isys, &
            &                    idt, conc, deriv, volold, bound, &
            &                                      rhs, diag, sol)

        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: num_cells               !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: num_boundary_conditions               !< Number of open boundaries
        integer(kind=int_wp), intent(in   ) :: num_substances_transported               !< Number of transported substances
        integer(kind=int_wp), intent(in   ) :: num_substances_total               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: isys                !< This substance
        integer(kind=int_wp), intent(in   ) :: idt                 !< Timestep
        real(kind=real_wp),   intent(in   ) :: conc(num_substances_total, num_cells)  !< All concentrations
        real(kind=real_wp),   intent(in   ) :: deriv(num_substances_total, num_cells) !< All derivatives (loads, processes)
        real(kind=real_wp),   intent(in   ) :: volold(num_cells)       !< Volumes at beginning of time step
        real(kind=real_wp),   intent(in   ) :: bound(num_substances_transported, num_boundary_conditions) !< Open boundary concentrations
        real(kind=dp),        intent(  out) :: rhs(num_cells + num_boundary_conditions)  !< Right hand side of the equation
        real(kind=dp),        intent(in   ) :: diag(num_cells + num_boundary_conditions) !< Value of the diagonal
        real(kind=dp),        intent(  out) :: sol(num_cells + num_boundary_conditions)  !< Initial guess

        ! Local variables
        real(kind=dp) :: ddt          !< 1.0 / time step in double precision
        integer(kind=int_wp) :: iseg  !< Loop variable

        ! The WAQ-timer
        integer(kind=int_wp) :: ithandl = 0
        if (timon) call timstrt("dlwqf4", ithandl)

        ! set the right hand side, normal part
        ddt = 1.0d00/idt

        do iseg = 1, num_cells
            rhs(iseg) = deriv(isys, iseg) + volold(iseg)*conc(isys, iseg)*ddt
        end do

        ! set the right hand side, open boundary part
        do iseg = 1, num_boundary_conditions
            rhs(num_cells + iseg) = bound(isys, iseg)
        end do

        ! row scaling
        do iseg = 1, num_cells + num_boundary_conditions
            rhs(iseg) = rhs(iseg)/diag(iseg)
        end do

        ! zero initial guess, try previous concentration for water volumes
        ! ( alternatively take zero vector ). Zero initial guess for boundaries.
        sol = 0.0
        do iseg = 1, num_cells
            sol(iseg) = conc(isys, iseg) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqf4
end module m_dlwqf4
