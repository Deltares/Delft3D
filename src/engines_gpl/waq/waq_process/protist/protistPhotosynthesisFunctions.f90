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
!!    - protist_photosynthesis_functions
!!              contains all the functions needed to calculated photosynthesis
!!              and chlorophyll synthesis


module protist_photosynthesis_functions

    use protist_math_functions
    use protist_constants
    IMPLICIT NONE
contains

    ! photosynthesis function, but it does not have a status related function included that would lower the playeau (f[1:4))
    ! Units: gC gC-1 d-1
    real function plateauPS(maxUmT, maxPSreq, relPS, NCopt, redco, NPSiCu, BR, PSDOC) result(plateau)
        real, intent(in) :: maxUmT, NCopt
        real, intent(in) :: maxPSreq                          ! fraction of C through photosynthesis (diatom = 1; MX < 1)
        real, intent(in) :: NPSiCu                            ! limiting internal nutrient value
        real, intent(in) :: PSDOC                             ! proportion of photosynthesis leaked as C
        real, intent(in) :: BR                                ! basal respiration
        real, intent(in) :: redco                             ! C respired to support nitrate reduction for NH4        (gC gN-1)
        real, intent(in) :: relPS                             ! relative PSmax:Umax on phototrophy                     (-)
        real, parameter :: anaResp = 1.5       ! anabolic respiration cost in terms of C                (gC gN-1 d-1)

        plateau = ((1.0 + PSDOC) * maxUmT * maxPSreq * relPS * (1.0 + NCopt * (redco + anaResp)) * NPSiCu) + BR + 1e-6

    end function plateauPS

    ! gross PS
    ! Units: gC gC-1 d-1
    real function grossPS(ChlC, PFD, exat, atten, plateau, alpha) result(grossPhotoRate)
        real, intent(in) :: ChlC
        real, intent(in) :: plateau                           ! see function plateau above
        real, intent(in) :: PFD                               ! Photon flux density
        real, intent(in) :: exat                              ! extinction
        real, intent(in) :: atten                             ! attenuation
        real, intent(in) :: alpha                             ! alpha for photosynthesis in protist in PE curve        ()
        real :: intermediateVal

        ! (-)   intermediate in depth-integrated photosynthesis calculation according to the Smith equation
        intermediateVal = (alpha * ChlC * PFD * numSecPerDay) / plateau
        ! gC gC-1 d-1   gross photosynthesis rate
        grossPhotoRate = plateau * (log(intermediateVal + sqrt(1.0 + intermediateVal**2)) - log(intermediateVal * exat &
                & + sqrt(1.0 + (intermediateVal * exat)**2))) / (atten + tiny(atten))
    end function grossPS

    ! net PS
    ! Units: gC gC-1 d-1
    real function netPS(grossPhotoRate, PSDOC) result(netPhotoRate)
        real, intent(in) :: grossPhotoRate    ! see function grossPS
        real, intent(in) :: PSDOC             ! proportion of photosynthesis leaked as C

        !gC gC-1 d-1   net photosynthesis rate retained for physiology
        netPhotoRate = grossPhotoRate * (1.0 - PSDOC)
    end function netPS

    ! Chl synthesis
    ! it also used a sigmoidal curve instead of a hyperbolic curve as a safetly net.... thus it needs to normalize ChlC between 0 and ChlCm
    ! Units: gChl gC-1 d-1
    real function synthesisChl(ChlC, ChlCmin, ChlCmax, maxUmT, maxPSreq, NPSiCu, netPhotoRate, plateau) result(synChl)
        real, intent(in) :: ChlC, ChlCmin, ChlCmax, maxUmT
        real, intent(in) :: maxPSreq                        ! maximum requirement to cover C through photosynthesis (diatom = 1; MX < 1)
        real, intent(in) :: NPSiCu                          ! limiting internal nutrient value
        real, intent(in) :: netPhotoRate                    ! see function netPS above (Cfixation)
        real, intent(in) :: plateau                         ! see function plateau above (max photosynthetic rate possible)
        real, parameter :: Mphoto = 0.5      ! 2.0 scalar for controlling photoacclimation rate           (-)
        real, parameter :: L = 0.95          ! upper asymptote
        real, parameter :: k = -24           ! growth rate to form curve
        real, parameter :: b = 0.7! 0.85          ! displacement along the x-axis
        real :: nChlC

        nChlC = normalize(ChlC, ChlCmin, ChlCmax)
        ! Chlorophyll synthesis and degradation
        synChl = ChlCmax * maxUmT * maxPSreq * NPSiCu * Mphoto * (1.0 - netPhotoRate / plateau) * sigmoidLogistic(L, k, b, nChlC)

    end function synthesisChl

    ! Chl degradation
    ! it also used a sigmoidal curve instead of a hyperbolic curve as a safetly net.... thus it needs to normalize ChlC between 0 and ChlCm
    ! Units: gChl gC-1 d-1
    real function degradeChl(ChlC, ChlCmax, maxUmT, NPSiCu) result(degChl)
        real, intent(in) :: ChlC, maxUmT, ChlCmax
        real, intent(in) :: NPSiCu                          ! limiting internal nutrient value

        ! Chlorophyll degradation
        degChl = (min(ChlC, ChlCmax) * maxUmT * (1.0 - NPSiCu))
    end function degradeChl

    ! uptake of protist chlorophyll by NCM
    ! Units: (-)
    real function upChl(ChlC, ChlCmax) result(APchl)
        real, intent(in) :: ChlC, ChlCmax
        real :: nChlC
        real, parameter :: L = 1.0           ! upper asymptote
        real, parameter :: k = -80           ! growth rate to form curve
        real, parameter :: b = 0.93          ! displacement along the x-axis

        nChlC = normalize(ChlC, 0.0, ChlCmax)
        ! Chlorophyll uptake
        APchl = sigmoidLogistic(L, k, b, nChlC)
    end function upChl

end module protist_photosynthesis_functions

