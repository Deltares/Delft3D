!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
module math_utils
    use m_waq_precision

    implicit none

    private
    public :: greatest_common_divisor

contains


    subroutine greatest_common_divisor(num_elements, numbers, gcd_result)

        ! calculates the greatest common divisor (gcd) (largest common denominator) for a set of numbers.
        integer(kind = int_wp) :: num_elements, gcd_result, i, inner_index
        integer(kind = int_wp) :: numbers(num_elements)
        integer(kind = int_wp) :: min_number
        logical :: divisor_found

        min_number = numbers(1)
        do i = 2, num_elements
            min_number = MIN(numbers(i), min_number)
        enddo

        do i = min_number, 1, -1
            divisor_found = .TRUE.
            do inner_index = 1, num_elements
                if (MOD(numbers(inner_index), i) /= 0) then
                    divisor_found = .FALSE.
                    exit
                end if
            end do
            if (divisor_found) then
                gcd_result = i
                exit
            end if
        end do

    end subroutine greatest_common_divisor
end module math_utils
