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
module m_dlwqh6
    use m_waq_precision

    implicit none

contains


    subroutine dlwqh6 (noseg, notot, isys, nsys, conc, &
            &                    sol, amass2, dmps, intopt, isdmp)

        !     Deltares Software Centre

        !>\File Puts the steady state solution in the concentration array

        !     Created   : June  1988 by Leo Postma as dlwq63.f

        !     Modified  : March 2011, Leo Postma  : for iterative GMRES solver

        use timers                          ! WAQ performance timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name                    Description

        integer(kind = int_wp), intent(in) :: noseg                 !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: notot                 !< Total number of substances
        integer(kind = int_wp), intent(in) :: isys                  !< First substance to update
        integer(kind = int_wp), intent(in) :: nsys                  !< Total number of substances to update
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg)   !< Target array for update
        real(kind = dp), intent(inout) :: sol   (nsys, noseg)   !< Solution matrix for the nsys substances
        real(kind = real_wp), intent(inout) :: amass2(notot, 5)   !< Mass accumulation array
        real(kind = real_wp), intent(inout) :: dmps  (notot, *)   !< Dumped segment fluxes if intopt > 7
        integer(kind = int_wp), intent(in) :: intopt                !< Integration sub options
        integer(kind = int_wp), intent(in) :: isdmp (noseg)         !< Pointer dumped segments

        !     Local declarations

        integer(kind = int_wp) :: iseg, i, ip   ! loop variables

        !     The WAQ-timer

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqh6", ithandl)

        !         put result in concentration array

        if (.not. btest(intopt, 3)) then
            do iseg = 1, noseg
                do i = isys, isys + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, iseg) * sol(i - isys + 1, iseg)
                    conc  (i, iseg) = sol(i - isys + 1, iseg)
                    sol   (i - isys + 1, iseg) = 0.0d00
                enddo
            enddo
        else
            do iseg = 1, noseg
                ip = isdmp(iseg)
                do i = isys, isys + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, iseg) * sol(i - isys + 1, iseg)
                    if (ip > 0) then
                        dmps(i, ip) = dmps(i, ip) + conc(i, iseg) * sol(i - isys + 1, iseg)
                    endif
                    conc  (i, iseg) = sol(i - isys + 1, iseg)
                    sol   (i - isys + 1, iseg) = 0.0d00
                enddo
            enddo
        endif

        if (timon) call timstop (ithandl)

        return
    end

end module m_dlwqh6
