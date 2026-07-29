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

module m_extract_waq_attribute

    implicit none

contains
    subroutine extract_waq_attribute(position_digit, attribute, value_digit)
        !< Extracts the value of the digit in "attribute" located at "position_digit" (from right to left).
        !< For example: if attribute = 1234, and position_digit = 2, then value_digit = 3.
        !< The second digit from the right in attribute is number 3.
        !< The indices in attribute are used, for example, to indicate if a cell is at the bottom, at the surface or
        !! in the middle.
        !! Also if a cell is active or not.
        integer, intent(in) :: position_digit !< position (from right to left) of the digit to be extracted
        integer, intent(in) :: attribute      !< attribute containing multiple digits (each with a different meaning)
        integer, intent(out) :: value_digit    !< digit located at the desired position in attribute
        if (position_digit < 0 .OR. position_digit > 9) then
            value_digit = -999.
        else
            value_digit = mod(attribute / 10**(position_digit - 1), 10)
        end if
    end subroutine extract_waq_attribute
end module m_extract_waq_attribute
