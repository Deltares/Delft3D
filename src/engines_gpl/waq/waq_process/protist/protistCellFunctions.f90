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
!!    - protist_cell_functions
!!              contains all the functions needed to determine the status
!!              and maintanence of the cell, e.g. cell quota, nutrient status,
!!              respiration, growth and mortality
!!


module protist_cell_functions

    use protist_math_functions
    use protist_constants
    IMPLICIT NONE
contains

    ! calculate the internal nutrient quotas
    ! Units: gNut gC-1
    real function quota(protNut, protC) result(NutC)
        real, intent(in) :: protNut, protC
        NutC = protNut / protC
    end function quota

    ! calculate Q10 rate, i.e. rate at current temperature using the Q10 approach
    ! Units: gC gC-1 d-1
    real function Q10rate(referenceRate, Q10, Temp, referenceTemp) result(rate)
        real, intent(in) :: referenceRate, Q10, Temp, referenceTemp
        rate = referenceRate * Q10**((Temp - referenceTemp) / 10.0)
    end function Q10rate

    ! basal respiration rate
    ! Units: gC gC-1 d-1
    real function basal_respiration(maxUmT, CR) result(BR)
        real, intent(in) :: maxUmT
        real, intent(in) :: CR      ! catabolic respiration quotient    (-)
        BR = maxUmT * CR
    end function basal_respiration

    ! calculates voiding of nutrients if the maximum/minimum is reached
    ! Units: gNut m-3 d-1
    real function voiding(protNut, protC, maxNutC) result(NutRegen)
        real, intent(in) :: protNut, protC, maxNutC
        NutRegen = max(0.0, protNut - protC * maxNutC)
    end function voiding

    ! Nitrogen to Carbon nutrient status of protist
    ! Units: gN gC-1
    real function statusNC(NutC, NutCmin, NutCopt) result(NutStat)
        real, intent(in) :: NutC, NutCmin, NutCopt
        real :: nNutC, NutStat_lowBound
        nNutC = normalize(NutC, NutCmin, NutCopt)
        ! makes lower Bound
        NutStat_lowBound = max(0.0, nNutC)
        ! makes upper Bound
        NutStat = min(1.0, NutStat_lowBound)
    end function statusNC

    ! Phosphate to Carbon nutrient status of protist
    ! Units: gP gC-1
    real function statusPC(NutC, NutCmin, NutCmax) result(NutStat)
        real, intent(in) :: NutC, NutCmin, NutCmax
        real, parameter :: L = 1.0   ! upper asymptote
        real, parameter :: b = 6.0   ! displacement along the x-axis
        real, parameter :: k = 10.0  ! growth rate to form the curve
        real normX
        normX = normalize(NutC, NutCmin, NutCmax)
        NutStat = sigmoidGompertz(L, b, k, normX)
    end function statusPC


    ! Silica to Carbon nutrient status of protist
    ! Units: gSi gC-1
    real function statusSC(NutCopt, NutCmin, resource, halfSat) result(NutStat)
        real, intent(in) :: NutCopt, NutCmin, resource, halfSat
        NutStat = min(((resource / (resource + halfSat)) * (NutCopt / NutCmin)), 1.0)
    end function statusSC

    ! Units: gC gC-1 d-1
    real function totalRespiration(redco, upNO3, upNH4, assC, assN, propLostIngC, BR) result(totR)
        real, intent(in) :: upNO3, upNH4    ! uptake of NH4 and NO3
        real, intent(in) :: assC, assN      ! assimilation of prey C
        real, intent(in) :: propLostIngC    ! SDA GIVE EXPLANATION
        real, intent(in) :: BR              ! basal respiration
        real, intent(in) :: redco           ! C respired to support nitrate reduction for NH4        (gC gN-1)
        real, parameter :: anaResp = 1.5       ! anabolic respiration cost in terms of C                (gC gN-1 d-1)
        totR = (redco * upNO3) + anaResp * (upNH4 + upNO3 + assN * propLostIngC) + (assC * propLostIngC) + BR
    end function totalRespiration

    ! calculate instantaneous C-specific growth rate
    ! Units: gC gC-1 d-1
    real function CgrowthRate(Cfix, assC, totR) result(Cu)
        real, intent(in) :: Cfix, assC, totR
        Cu = Cfix + assC - totR
    end function CgrowthRate

    ! calculate mortality
    ! Units: gC gC-1 d-1
    real function mortality(mortQ10, frac) result(mortFrac)
        real, intent(in) :: mortQ10
        real, intent(in) :: frac   ! fraction towards detritus/autolysis
        mortFrac = mortQ10 * frac
    end function mortality

    ! calculates motility
    ! Citation: Flynn, K. J. and Mitra, A. (2016). Why Plankton Modelers Should Reconsider Using Rectangular Hyperbolic
    ! (Michaelis-Menten, Monod) Descriptions of Predator-Prey Interactions. Frontiers in Marine Science
    ! valid for organisms in the size range of 1.2 - 1900 um
    ! Units: m/s
    real function motility(radiusProtist) result(velProt)
        real, intent(in) :: radiusProtist                   ! radius of the protist cell  in um
        real ESD                             ! equivalend sperical diameter
        real, parameter :: a = 38.542        ! constant
        real, parameter :: k = 0.5424        ! exponent
        ! Unit: um
        ESD = (radiusProtist * 2)
        ! Units: m/s
        velProt = C_velProt * (a * (ESD)**k) + small
    end function motility

end module protist_cell_functions

