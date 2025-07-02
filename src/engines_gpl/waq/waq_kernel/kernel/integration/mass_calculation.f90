!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_mass_calculation
    use m_waq_precision
    use timers

    implicit none
    private
    public :: calculate_mass_from_concentration_end_time_step, calculate_mass_from_concentration, &
        calculate_mass_for_transported_substances, calculate_masses_from_implicitly_calc_concentrations

contains

    !> Calculates masses form concentrations and volumes
    subroutine calculate_mass_from_concentration(amass, volume, conc, num_substances_total, num_cells)

        real(kind = real_wp), intent(inout) :: amass(num_substances_total, *) !< Closure error correction (num_substances_total x num_cells)
        real(kind = real_wp), intent(in) :: volume(*)       !< Volume  (num_cells)
        real(kind = real_wp), intent(in) :: conc(num_substances_total, *)  !< Concentrations(num_substances_total x num_cells)

        integer(kind = int_wp), intent(in) :: num_substances_total !< Number of systems
        integer(kind = int_wp), intent(in) :: num_cells !< Number of cells or segments

        ! Local variables
        integer(kind = int_wp) :: substance_i, cell_i
        integer(kind = int_wp) :: ithandl = 0
        real(kind = real_wp) :: v1

        if (timon) call timstrt ("calculate_mass_from_concentration", ithandl)

        ! loop over the number of segments and systems
        do cell_i = 1, num_cells
            v1 = volume(cell_i)
            do substance_i = 1, num_substances_total
                amass(substance_i, cell_i) = conc(substance_i, cell_i) * v1
            end do
        end do

        if (timon) call timstop (ithandl)
    end subroutine calculate_mass_from_concentration


    !> Calculates masses and concentrations after the flux correction step
    subroutine calculate_mass_from_concentration_end_time_step(num_substances_transported, num_substances_total, num_cells, volume, amass, &
            conc2, conc)

        integer(kind = int_wp), intent(in) :: num_substances_transported          !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                !< Number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(num_cells)        !< Volumes of the segments
        real(kind = real_wp), intent(inout) :: amass (num_substances_total, num_cells) !< Masses per substance per volume
        real(kind = real_wp), intent(in) :: conc2 (num_substances_total, num_cells) !< Concentrations per substance per volume
        real(kind = real_wp), intent(out) :: conc  (num_substances_total, num_cells) !< Concentrations per substance per volume

        ! Local variables
        integer(kind = int_wp) :: substance_i          !< Loop counter substances
        integer(kind = int_wp) :: cell_i          !< Loop counter computational volumes
        real(kind = real_wp) :: vol             !< Auxiliary variable for this volume
        integer(kind = int_wp), save :: ithandl !< Timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("calculate_mass_from_concentration_end_time_step", ithandl)

        ! Loop along the number of computational elements
        do cell_i = 1, num_cells
            vol = volume(cell_i)
            do substance_i = 1, num_substances_transported
                conc (substance_i, cell_i) = conc2(substance_i, cell_i)
                amass(substance_i, cell_i) = conc2(substance_i, cell_i) * vol
            end do

            do substance_i = num_substances_transported + 1, num_substances_total
                conc (substance_i, cell_i) = conc2(substance_i, cell_i)
            end do
        enddo
        if (timon) call timstop (ithandl)
    end subroutine calculate_mass_from_concentration_end_time_step


    !> Computes masses from conc for transported substances, sets explicit step for non transported ones
    !! and resets derivatives to zero.
    subroutine calculate_mass_for_transported_substances(num_substances_transported, num_substances_total, num_substances_part, num_cells, volume, &
            surface, amass, conc, deriv, idt)

        integer(kind = int_wp), intent(in) :: num_substances_transported         !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total               !< total number of substances
        integer(kind = int_wp), intent(in) :: num_substances_part                !< number of particle substances
        integer(kind = int_wp), intent(in) :: num_cells                          !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(num_cells)                 !< volumes of the segments
        real(kind = real_wp), intent(in) :: surface(num_cells)                   !< horizontal surface area
        real(kind = real_wp), intent(inout) :: amass(num_substances_total, num_cells) !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells)  !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv(num_substances_total, num_cells) !< derivatives per substance per volume
        integer(kind = int_wp), intent(in) :: idt                 !< integration time step size

        ! Local variables
        integer(kind = int_wp) :: substance_i          !< Loop counter substances
        integer(kind = int_wp) :: cell_i          !< Loop counter computational volumes
        real(kind = real_wp) :: surf            !< The horizontal surface area of the cell
        real(kind = real_wp) :: vol             !< Auxiliary variable for this volume
        integer(kind = int_wp), save :: ithandl !< Timer handle
        data ithandl/0/

        if (timon) call timstrt("calculate_mass_for_transported_substances", ithandl)

        ! loop accross the number of computational volumes for the concentrations
        do cell_i = 1, num_cells
            vol = volume(cell_i)
            surf = surface(cell_i)
            ! transported substances first
            do substance_i = 1, num_substances_transported
                amass(substance_i, cell_i) = conc(substance_i, cell_i) * vol
                deriv(substance_i, cell_i) = 0.0
            end do
            ! then the passive substances
            do substance_i = num_substances_transported + 1, num_substances_total - num_substances_part
                amass(substance_i, cell_i) = amass(substance_i, cell_i) + idt * deriv(substance_i, cell_i)
                conc(substance_i, cell_i) = amass(substance_i, cell_i) / surf
                deriv(substance_i, cell_i) = 0.0
            end do
        end do
        if (timon) call timstop(ithandl)
    end subroutine calculate_mass_for_transported_substances

    !> Calculates the masses from implicitly obtained concentrations
    subroutine calculate_masses_from_implicitly_calc_concentrations(num_substances_transported, &
            num_substances_total, num_cells, volume, amass, conc, deriv)

        integer(kind = int_wp), intent(in) :: num_substances_transported      !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total            !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                       !< Number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(num_cells)               !< Volumes of the segments
        real(kind = real_wp), intent(inout) :: amass (num_substances_total, num_cells) !< Masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells) !< Concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv (num_substances_total, num_cells) !< Derivatives per substance per volume

        ! Local variables
        integer(kind = int_wp) :: substance_i          !< Loopcounter substances
        integer(kind = int_wp) :: cell_i          !< Loopcounter computational volumes
        real(kind = real_wp) :: vol             !< Helpvariable for this volume
        integer(kind = int_wp), save :: ithandl !< Timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("calculate_masses_from_implicitly_calc_concentrations", ithandl)

        ! loop accross the number of computational elements
        do cell_i = 1, num_cells
            vol = 1.0
            if (abs(volume(cell_i)) > 1.0e-25) vol = volume(cell_i)
            do substance_i = 1, num_substances_transported
                conc (substance_i, cell_i) = conc(substance_i, cell_i) / deriv(substance_i, cell_i)
                amass(substance_i, cell_i) = conc(substance_i, cell_i) * vol
                deriv(substance_i, cell_i) = 0.0
            end do
        end do

        if (timon) call timstop (ithandl)
    end subroutine calculate_masses_from_implicitly_calc_concentrations
end module m_mass_calculation
