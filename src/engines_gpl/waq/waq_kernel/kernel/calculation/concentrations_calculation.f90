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
    public :: calculate_concentrations_from_derivatives, calculate_concentrations_from_mass

contains

    !> Calculates concentrations from derivatives, and zeroes these derivatives
    subroutine calculate_concentrations_from_derivatives(conc, deriv, amass2, num_cells, num_substances_total, &
            substance_i, nsys, dmps, intopt, isdmp)

        real(kind = real_wp), intent(inout) :: conc(num_substances_total, *)   !< First order term
        real(kind = real_wp), intent(inout) :: deriv(*)         !< Right hand side matrix
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, *) !< Mass accumulation array
        real(kind = real_wp), intent(inout) :: dmps(*)          !< Dumped segment fluxes if intopt>7

        integer(kind = int_wp), intent(in) :: num_cells    !< Number of cells or segments
        integer(kind = int_wp), intent(in) :: num_substances_total    !< Total number of systems
        integer(kind = int_wp), intent(in) :: substance_i     !< Index of considered system
        integer(kind = int_wp), intent(in) :: nsys     !< Number of systems to take
        integer(kind = int_wp), intent(in) :: intopt   !< Integration suboptions
        integer(kind = int_wp), intent(in) :: isdmp(*) !< Indeces dumped segments

        ! Local variables
        integer(kind = int_wp) :: i, ip, j, ntot, iset, cell_i
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("calculate_concentrations_from_derivatives", ithandl)

        ! Calculate concentrations
        iset = 1
        if (mod(intopt, 16) < 8) then
            do cell_i = 1, num_cells
                do i = substance_i, substance_i + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc (i, cell_i) * deriv(iset)
                    conc  (i, cell_i) = deriv (iset)
                    iset = iset + 1
                end do
            end do
        else
            do cell_i = 1, num_cells
                ip = isdmp(cell_i)
                j = (ip - 1) * num_substances_total
                do i = substance_i, substance_i + nsys - 1
                    amass2(i, 2) = amass2(i, 2) + conc(i, cell_i) * deriv(iset)
                    if (ip > 0) then
                        dmps(j + i) = dmps(j + i) + conc(i, cell_i) * deriv(iset)
                    endif
                    conc  (i, cell_i) = deriv (iset)
                    iset = iset + 1
                end do
            end do
        endif
        ! Zero the derivative
        ntot = num_substances_total * num_cells
        do i = 1, ntot
            deriv(i) = 0.0
        end do

        if (timon) call timstop (ithandl)
    end subroutine calculate_concentrations_from_derivatives

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
