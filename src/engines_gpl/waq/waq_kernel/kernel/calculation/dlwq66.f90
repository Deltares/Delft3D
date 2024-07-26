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
module m_dlwq66
    use m_waq_precision

    implicit none

contains


    !> Calculates masses form concentrations and volumes
    subroutine dlwq66(amass, volume, conc, num_substances_total, num_cells)

        use timers

        real(kind = real_wp), intent(inout) :: amass(num_substances_total, *) !< Closure error correction (num_substances_total x num_cells)
        real(kind = real_wp), intent(in   ) :: volume(*)       !< Volume  (num_cells)
        real(kind = real_wp), intent(in   ) :: conc(num_substances_total, *)  !< Concentrations  (num_substances_total x num_cells)

        integer(kind = int_wp), intent(in) :: num_substances_total !< Number of systems
        integer(kind = int_wp), intent(in) :: num_cells !< Number of cells or segments

        ! Local variables
        integer(kind = int_wp) :: isys, iseg
        integer(kind = int_wp) :: ithandl = 0
        real(kind = real_wp) :: v1

        if (timon) call timstrt ("dlwq66", ithandl)

        ! loop over the number of segments and systems
        DO ISEG = 1, num_cells
            V1 = VOLUME(ISEG)
            DO ISYS = 1, num_substances_total
                AMASS(ISYS, ISEG) = CONC(ISYS, ISEG) * V1
            end do
        end do
        !
        if (timon) call timstop (ithandl)
    end subroutine dlwq66
end module m_dlwq66
