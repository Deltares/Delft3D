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
module m_prinma
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *        SUBROUTINE TO PRINT MAXIMUM SOLUTION TO DEBUG FILE         *
    !  *********************************************************************

    subroutine prinma(x, bio2, total, ni, nin, int)

        use bloom_data_dim
        use bloom_data_io
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: x(mx), biopos, bio2, total
        integer(kind = int_wp) :: int, j, k, k2, l1, l2, ni, nin

        !  Print maximum solution on unit outdbg
        write (outdbg, 10)
        10 format (12X, '******* MAXIMUM SOLUTION *******')
        write(outdbg, 20)
        20 format (2X, 'Species', 34X, 'Types', /, 26X, '1', 13X, '2', 13X, '3', 13X, '4')
        do j = 1, nuecog
            l1 = it2(j, 1)
            l2 = it2(j, 2)
            write (outdbg, 50) grname(j), (x(k + nurows), k = l1, l2)
            50    format (2x, a8, 11x, 4(f11.4, 3x))
        end do
        biopos = bio2
        if (biopos < 0.0) biopos = 0.0
        write (outdbg, 70) biopos
        70 format (2X, 'Total biomass', 6X, F11.4, 3X, 'g/m3')
        write (outdbg, 90) total
        90 format (2X, 'Chlorophyll', 8X, F11.4, 3X, 'mg/m3', /)

        !  Print nutrient concentrations
        write (outdbg, 100)
        100 format (2X, 'Nutrient', 14X, 'Total', 9X, 'Slacks')
        write (outdbg, 110) (cstra(k), concen(k), x(k), k = 1, nunuco)
        110 format (6(2X, A8, 11X, F11.4, 3X, F11.4, /))
        write(outdbg, 120) ni, nin, int
        120 format ('  Number of intervals:', I3, 2X, 'Infeasible:', I3, 2X, 'Maximum interval:', I3, //)
        return
    end

end module m_prinma
