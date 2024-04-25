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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the following functions:
!!    - protist_phagotrophy_functions
!!

module protist_phagotrophy_functions

    use protist_math_functions
    use protist_cell_functions
    IMPLICIT NONE
contains

    real function lightInhibition(PFD, relFeeding) result(lightInh)
        real, intent(in) :: PFD
        real, intent(in) :: relFeeding  ! rel feeding in night : day
        real, parameter :: k = 10.0          ! growth rate to form curve
        real, parameter :: b = 1.0           ! displacement along the x-axis

        lightInh = sigmoidLogistic(relFeeding, k, b, PFD) + (1.0 - relFeeding)
    end function lightInhibition

end module protist_phagotrophy_functions

