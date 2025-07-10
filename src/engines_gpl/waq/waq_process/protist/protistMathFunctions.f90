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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the following functions:
!!    - protist_math_functions
!!              contains all the mathematical functions needed to run the
!!              other modules e.g. normalize, sigmoidLogistic
!!

module protist_math_functions

    use protist_types

    IMPLICIT NONE
contains

    ! normalize between two values
    real function normalize(x, lowerVal, upperVal) result(normX)
        real, intent(in) :: x, lowerVal, upperVal
        normX = (x - lowerVal) / (upperVal - lowerVal)
    end function normalize

    ! Gompertz sigmoidal function
    real function sigmoidGompertz(L, b, k, x) result(y)
        real, intent(in) :: L  ! upper asymptote
        real, intent(in) :: b  ! displacement along the x-axis
        real, intent(in) :: k  ! growth rate of curve
        real, intent(in) :: x
        y = L * exp(-b * exp(-k * x))
    end function sigmoidGompertz

    ! monod function
    real function monod(resource, halfSat) result(y)
        real, intent(in) :: resource, halfSat
        y = resource / (resource + halfSat)
    end function monod

    ! Logistic sigmoidal function
    real function sigmoidLogistic(L, k, b, x) result(y)
        real, intent(in) :: L                  ! upper asymptote
        real, intent(in) :: k                  ! growth rate curve
        real, intent(in) :: b                  ! displacement along the x-axis
        real, intent(in) :: x
        real, parameter :: maxexp = log(huge(1.0)) - 1.0
        y = L / (1.0 + exp(min(maxexp, -k * (x - b))))
    end function sigmoidLogistic

end module protist_math_functions
