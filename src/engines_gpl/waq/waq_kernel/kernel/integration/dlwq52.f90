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
module m_dlwq52
    use m_waq_precision

    implicit none

contains
    !> Calculates masses and concentrations after the flux correction step
    subroutine dlwq52(nosys, notot, noseg, volume, amass, &
            conc2, conc)

        use timers

        implicit none

        integer(kind = int_wp), intent(in   ) :: nosys                !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: notot                !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: noseg                !< Number of computational volumes
        real(kind = real_wp),   intent(inout) :: volume(noseg)        !< Volumes of the segments
        real(kind = real_wp),   intent(inout) :: amass (notot, noseg) !< Masses per substance per volume
        real(kind = real_wp),   intent(in   ) :: conc2 (notot, noseg) !< Concentrations per substance per volume
        real(kind = real_wp),   intent(  out) :: conc  (notot, noseg) !< Concentrations per substance per volume

        ! Local variables
        integer(kind = int_wp) :: isys          !< Loop counter substances
        integer(kind = int_wp) :: iseg          !< Loop counter computational volumes
        real(kind = real_wp) :: vol             !< Auxiliary variable for this volume
        integer(kind = int_wp), save :: ithandl !< Timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq52", ithandl)

        ! Loop along the number of computational elements
        do iseg = 1, noseg
            vol = volume(iseg)
            do isys = 1, nosys
                conc (isys, iseg) = conc2(isys, iseg)
                amass(isys, iseg) = conc2(isys, iseg) * vol
            end do
            do isys = nosys + 1, notot
                conc (isys, iseg) = conc2(isys, iseg)
            end do
        enddo
        if (timon) call timstop (ithandl)
    end subroutine dlwq52
end module m_dlwq52
