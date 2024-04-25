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


    subroutine dlwqh3 (noseg, nosys, notot, nobnd, isys, &
            deriv, bound, rhs, diag, sol)

        !     Deltares Software Center

        !     Created    : February 1997 by RJVos (like dlwqf4 from scheme 15)

        !     Function   : put boundaries and derivatives in right hand side

        !     Modified   : July 2008, Leo Postma  : WAQ performance timers
        !                  June 2010, Leo Postma  : double precission version

        !     File I/O   : none

        !     Subroutines: none

        use timers                         ! WAQ performance timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name                  Description

        integer(kind = int_wp), intent(in) :: noseg               ! Number of computational volumes
        integer(kind = int_wp), intent(in) :: nosys               ! Number of transported substances
        integer(kind = int_wp), intent(in) :: notot               ! Total number of substances
        integer(kind = int_wp), intent(in) :: nobnd               ! Number of boundaries
        integer(kind = int_wp), intent(in) :: isys                ! This substance
        real(kind = real_wp), intent(in) :: deriv(notot, noseg)  ! Derivatives
        real(kind = real_wp), intent(in) :: bound(nosys, nobnd)  ! Open boundary values
        real(kind = dp), intent(inout) :: rhs  (noseg + nobnd)  ! Right hand side of the equation
        real(kind = dp), intent(in) :: diag (noseg + nobnd)  ! diagonal for scaling
        real(kind = dp), intent(out) :: sol  (noseg + nobnd)  ! initial guess for solution

        !     Local variables

        integer(kind = int_wp) :: iseg       ! loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqh3", ithandl)

        !        initialize the rhs and apply row scaling

        do iseg = 1, noseg
            rhs(iseg) = deriv(isys, iseg) / diag(iseg)
        enddo
        do iseg = 1, nobnd
            rhs(iseg + noseg) = bound(isys, iseg)
        enddo

        !        zero initial guess, try rhs plus small value

        sol = 0.0
        do iseg = 1, noseg
            sol(iseg) = rhs(iseg) + 0.01
        enddo

        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_dlwqh3
