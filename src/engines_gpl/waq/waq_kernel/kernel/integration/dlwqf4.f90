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
    subroutine dlwqf4(noseg, nobnd, nosys, notot, isys, &
            &                    idt, conc, deriv, volold, bound, &
            &                                      rhs, diag, sol)

        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: noseg               !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: nobnd               !< Number of open boundaries
        integer(kind=int_wp), intent(in   ) :: nosys               !< Number of transported substances
        integer(kind=int_wp), intent(in   ) :: notot               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: isys                !< This substance
        integer(kind=int_wp), intent(in   ) :: idt                 !< Timestep
        real(kind=real_wp),   intent(in   ) :: conc(notot, noseg)  !< All concentrations
        real(kind=real_wp),   intent(in   ) :: deriv(notot, noseg) !< All derivatives (loads, processes)
        real(kind=real_wp),   intent(in   ) :: volold(noseg)       !< Volumes at beginning of time step
        real(kind=real_wp),   intent(in   ) :: bound(nosys, nobnd) !< Open boundary concentrations
        real(kind=dp),        intent(  out) :: rhs(noseg + nobnd)  !< Right hand side of the equation
        real(kind=dp),        intent(in   ) :: diag(noseg + nobnd) !< Value of the diagonal
        real(kind=dp),        intent(  out) :: sol(noseg + nobnd)  !< Initial guess

        ! Local variables
        real(kind=dp) :: ddt          !< 1.0 / time step in double precision
        integer(kind=int_wp) :: iseg  !< Loop variable

        ! The WAQ-timer
        integer(kind=int_wp) :: ithandl = 0
        if (timon) call timstrt("dlwqf4", ithandl)

        ! set the right hand side, normal part
        ddt = 1.0d00/idt

        do iseg = 1, noseg
            rhs(iseg) = deriv(isys, iseg) + volold(iseg)*conc(isys, iseg)*ddt
        end do

        ! set the right hand side, open boundary part
        do iseg = 1, nobnd
            rhs(noseg + iseg) = bound(isys, iseg)
        end do

        ! row scaling
        do iseg = 1, noseg + nobnd
            rhs(iseg) = rhs(iseg)/diag(iseg)
        end do

        ! zero initial guess, try previous concentration for water volumes
        ! ( alternatively take zero vector ). Zero initial guess for boundaries.
        sol = 0.0
        do iseg = 1, noseg
            sol(iseg) = conc(isys, iseg) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqf4
end module m_dlwqf4
