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
module m_concentration_calculations
    use m_waq_precision
    use timers

    implicit none

    private
    public :: calculate_concentrations_from_mass

contains

    !> Restores concentration array after mass has changed by process routines
    subroutine calculate_concentrations_from_mass(num_substances_transported, num_substances_total, &
            num_substances_part, num_cells, volume, &
            surface, amass, conc)

        integer(kind = int_wp), intent(in) :: num_substances_transported          !< Number of transported substances
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

        integer(kind = int_wp) :: substance_i !< Loop index substances
        integer(kind = int_wp) :: cell_i !< Loop index computational volumes

        integer(kind = int_wp), save :: ithandl !< Timer handle
        data       ithandl  /0/

        if (timon) call timstrt ("calculate_concentrations_from_mass", ithandl)

        ! loop along the number of computational volumes for the concentrations
        do cell_i = 1, num_cells
            ! check for positivity
            vol = volume(cell_i)
            surf = surface(cell_i)
            if (abs(vol) < 1.0e-25) vol = 1.0

            ! transported substances first
            do substance_i = 1, num_substances_transported
                conc (substance_i, cell_i) = amass(substance_i, cell_i) / vol
            end do

            ! then the passive substances
            do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part
                conc(substance_i, cell_i) = amass(substance_i, cell_i) / surf
            end do
        end do
        if (timon) call timstop (ithandl)
    end subroutine calculate_concentrations_from_mass
end module m_concentration_calculations
