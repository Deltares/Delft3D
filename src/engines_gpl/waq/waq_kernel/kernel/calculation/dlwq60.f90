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
module m_dlwq60
    use m_waq_precision

    implicit none

contains

    !> Scales derivatives after the user quality processes, for steady-state computation.
    subroutine dlwq60(deriv, conc, notot, noseg, itfact, &
                      amass2, isys, nsys, dmps, intopt, &
                      isdmp)

        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: notot               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: noseg               !< Number of computational volumes
        real(kind=real_wp),   intent(inout) :: deriv(notot, noseg) !< Derivatives to be scaled
        real(kind=real_wp),   intent(inout) :: conc(notot, noseg)  !< Concentrations per substance per volume
        integer(kind=int_wp), intent(in   ) :: itfact              !< Ratio delta-t process to delta-t transport
        real(kind=real_wp),   intent(inout) :: amass2(notot, 5)    !< Mass balance array
        integer(kind=int_wp), intent(in   ) :: isys                !< Index current substance
        integer(kind=int_wp), intent(in   ) :: nsys                !< Number of substances
        real(kind=real_wp),   intent(inout) :: dmps(notot, *)      !< Dumped fluxes is intopt > 7
        integer(kind=int_wp), intent(in   ) :: intopt              !< Integration suboptions
        integer(kind=int_wp), intent(in   ) :: isdmp(noseg)        !< Pointer dumped segments

        ! Local variables
        integer(kind=int_wp) :: iseg, i, ip
        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwq60", ithandl)

        do iseg = 1, noseg
            conc(isys, iseg) = conc(isys, iseg)/itfact
            do i = isys, isys + nsys - 1
                deriv(i, iseg) = deriv(i, iseg)/itfact
                amass2(i, 2) = amass2(i, 2) + deriv(i, iseg)
            end do
        end do

        if (mod(intopt, 16) >= 8) then
            do iseg = 1, noseg
                ip = isdmp(iseg)
                if (ip > 0) then
                    do i = isys, isys + nsys - 1
                        dmps(i, ip) = dmps(i, ip) + deriv(i, iseg)
                    end do
                end if
            end do
        end if

        if (timon) call timstop(ithandl)
    end subroutine dlwq60
end module m_dlwq60
