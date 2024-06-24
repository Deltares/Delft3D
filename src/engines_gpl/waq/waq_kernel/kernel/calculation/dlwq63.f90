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
module m_dlwq63
    use m_waq_precision

    implicit none

contains

    !> Calculates concentrations from derivatives, and zeroes these derivatives
    subroutine dlwq63(conc, deriv, amass2, noseg, notot, &
            isys, nsys, dmps, intopt, isdmp)
        use timers

        real(kind=real_wp), intent(inout) :: conc(notot, *)   !< First order term
        real(kind=real_wp), intent(inout) :: deriv(*)         !< Right hand side matrix
        real(kind=real_wp), intent(inout) :: amass2(notot, *) !< Mass accumulation array
        real(kind=real_wp), intent(inout) :: dmps(*)          !< Dumped segment fluxes if intopt>7

        integer(kind = int_wp), intent(in   ) :: noseg    !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: notot    !< Total number of systems
        integer(kind = int_wp), intent(in   ) :: isys     !< Index of considered system
        integer(kind = int_wp), intent(in   ) :: nsys     !< Number of systems to take
        integer(kind = int_wp), intent(in   ) :: intopt   !< Integration suboptions
        integer(kind = int_wp), intent(in   ) :: isdmp(*) !< Indeces dumped segments

        ! Local variables
        integer(kind = int_wp) :: i, ip, j, ntot, iset, iseg
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq63", ithandl)

        ! Calculate concentrations
        iset = 1
        if (mod(intopt, 16) < 8) then
            do iseg = 1, noseg
                do i = isys, isys + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc (i, iseg) * deriv(iset)
                    conc  (i, iseg) = deriv (iset)
                    iset = iset + 1
                end do
            end do
        else
            do iseg = 1, noseg
                ip = isdmp(iseg)
                j = (ip - 1) * notot
                do i = isys, isys + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, iseg) * deriv(iset)
                    if (ip > 0) then
                        dmps(j + i) = dmps(j + i) + conc(i, iseg) * deriv(iset)
                    endif
                    conc  (i, iseg) = deriv (iset)
                    iset = iset + 1
                end do
            end do
        endif
        ! Zero the derivative
        ntot = notot * noseg
        do i = 1, ntot
            deriv(i) = 0.0
        end do

        if (timon) call timstop (ithandl)
    end subroutine dlwq63
end module m_dlwq63
