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
module m_properties
    use m_waq_precision

    implicit none

contains


    subroutine add_biofilm(diameter, density, biofilm_thk, biofilm_density)
        implicit none

        real(kind = real_wp), intent(inout) :: diameter, density
        real(kind = real_wp), intent(in) :: biofilm_thk, biofilm_density

        real(kind = real_wp) :: mass

        real(kind = real_wp), parameter :: pi = 3.1415926

        mass = 1.0 / 6.0 * pi * diameter ** 3 * density + &
                1.0 / 6.0 * pi * ((diameter + 2.0 * biofilm_thk)**3 - diameter ** 3) * biofilm_density

        diameter = diameter + 2.0 * biofilm_thk
        density = 6.0 * mass / pi / diameter ** 3
    end subroutine add_biofilm

    subroutine combine_particles(diameter1, diameter2, density1, density2, combined_diameter, combined_density, new_shape_factor)
        implicit none

        real(kind = real_wp), intent(in) :: diameter1, diameter2, density1, density2
        real(kind = real_wp), intent(out) :: combined_diameter, combined_density, new_shape_factor

        real(kind = real_wp), parameter :: pi = 3.1415926
        real(kind = real_wp) :: mass, volume

        mass = density1 * 1.0 / 6.0 * pi * diameter1 ** 3 + &
                density2 * 1.0 / 6.0 * pi * diameter2 ** 3
        volume = 1.0 / 6.0 * pi * diameter1 ** 3 + &
                1.0 / 6.0 * pi * diameter2 ** 3

        combined_diameter = (6.0 * volume / pi) ** (1.0 / 3.0)
        combined_density = mass / volume
        new_shape_factor = sqrt(max(diameter1, diameter2) / combined_diameter)
    end subroutine combine_particles

    subroutine calculate_sedim(diameter, density, shape_factor, settling, crit_stress)
        implicit none

        real(kind = real_wp), intent(in) :: diameter, density, shape_factor
        real(kind = real_wp), intent(out) :: settling, crit_stress

        real(kind = real_wp) :: dstar, crit_coeff

        real(kind = real_wp), parameter :: perday = 86400.0 ! Convert from s to day
        real(kind = real_wp), parameter :: denswater = 1000.0
        real(kind = real_wp), parameter :: gravacc = 9.81 ! m/s^2
        real(kind = real_wp), parameter :: kinematic_viscosity = 1.0e-3 ! m^2/s

        if (density > denswater) then
            dstar = (diameter * 1.0e-6) * ((density / denswater - 1) * gravacc / kinematic_viscosity ** 2) ** 0.333333

            settling = 2.5 * dstar / 1000.

            settling = perday * settling

            if (dstar < 1.5) then
                crit_coeff = 0.126 * dstar ** (-0.44)
            elseif (dstar < 10.0) then
                crit_coeff = 0.131 * dstar ** (-0.55)
            elseif (dstar < 20.0) then
                crit_coeff = 0.0685 * dstar ** (-0.27)
            elseif (dstar < 40.0) then
                crit_coeff = 0.0173 * dstar ** (0.19)
            elseif (dstar < 150.0) then
                crit_coeff = 0.0115 * dstar ** (0.30)
            else
                crit_coeff = 0.052
            endif

            crit_stress = crit_coeff * (density - denswater) * gravacc * diameter * 1.0e-6
        else
            settling = 0.0
            crit_stress = 1.0e20
        endif
    end subroutine calculate_sedim

end module m_properties
