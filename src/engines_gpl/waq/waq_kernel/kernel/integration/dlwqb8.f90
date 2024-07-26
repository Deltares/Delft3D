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
module m_dlwqb8
    use m_waq_precision

    implicit none

contains


    !> Restores concentration array after mass has changed by process routines
    subroutine dlwqb8(num_substances_transported, num_substances_total, num_substances_part, num_cells, volume, &
            surface, amass, conc)

        use timers

        implicit none

        integer(kind = int_wp), intent(in) :: num_substances_transported                 !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_substances_part                !< Number of particle substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(num_cells)        !< Volumes of the segments
        real(kind = real_wp), intent(in) :: surface(num_cells)          !< Horizontal surface area
        real(kind = real_wp), intent(inout) :: amass (num_substances_total, num_cells) !< Masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells) !< Concentrations per substance per volume

        ! Local variables
        real(kind = real_wp) :: surf !< Horizontal surface area of the cell
        real(kind = real_wp) :: vol  !< Auxiliary variable for this volume

        integer(kind = int_wp) :: isys !< Loop index substances
        integer(kind = int_wp) :: iseg !< Loop index computational volumes

        integer(kind = int_wp), save :: ithandl !< Timer handle
        data       ithandl  /0/

        if (timon) call timstrt ("dlwq18", ithandl)

        ! loop along the number of computational volumes for the concentrations
        do iseg = 1, num_cells
            ! check for positivity
            vol = volume(iseg)
            surf = surface(iseg)
            if (abs(vol) < 1.0e-25) vol = 1.0

            ! transported substances first
            do isys = 1, num_substances_transported
                conc (isys, iseg) = amass(isys, iseg) / vol
            end do

            ! then the passive substances
            do isys = num_substances_transported + 1, num_substances_total - num_substances_part
                conc(isys, iseg) = amass(isys, iseg) / surf
            end do
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwqb8
end module m_dlwqb8
