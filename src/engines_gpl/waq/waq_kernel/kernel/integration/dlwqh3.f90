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
    subroutine dlwqh3(noseg, nosys, notot, nobnd, isys, &
                      deriv, bound, rhs, diag, sol)
        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: noseg               !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: nosys               !< Number of transported substances
        integer(kind=int_wp), intent(in   ) :: notot               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: nobnd               !< Number of boundaries
        integer(kind=int_wp), intent(in   ) :: isys                !< This substance
        real(kind=real_wp),   intent(in   ) :: deriv(notot, noseg) !< Derivatives
        real(kind=real_wp),   intent(in   ) :: bound(nosys, nobnd) !< Open boundary values
        real(kind=dp),        intent(inout) :: rhs(noseg + nobnd)  !< Right hand side of the equation
        real(kind=dp),        intent(in   ) :: diag(noseg + nobnd) !< diagonal for scaling
        real(kind=dp),        intent(inout) :: sol(noseg + nobnd)  !< initial guess for solution

        !     Local variables
        integer(kind=int_wp) :: iseg       !< loop counter

        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqh3", ithandl)
        ! initialize the rhs and apply row scaling
        do iseg = 1, noseg
            rhs(iseg) = deriv(isys, iseg)/diag(iseg)
        end do
        do iseg = 1, nobnd
            rhs(iseg + noseg) = bound(isys, iseg)
        end do
        ! zero initial guess, try rhs plus small value
        sol = 0.0
        do iseg = 1, noseg
            sol(iseg) = rhs(iseg) + 0.01
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqh3
end module m_dlwqh3
