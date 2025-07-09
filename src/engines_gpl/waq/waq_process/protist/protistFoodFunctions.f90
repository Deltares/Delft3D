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
!!    - protist_food_functions


module protist_food_functions

    use m_waq_precision
    use protist_types
    use protist_cell_functions
    use protist_constants
    IMPLICIT NONE
contains

    !! FOOD QUANTITY -------------------------------------------------------------------------------
    subroutine protistFoodQuantity(prot_array, r, wTurb, Ccell, optCR, mot, sumCP, ingNC, ingPC)
        type(protist_array), intent(inout) :: prot_array
        real(kind = real_wp), intent(in) :: r, wTurb, Ccell, optCR, mot
        real(kind = real_wp), intent(out) :: sumCP, ingNC, ingPC

        ! encounter Rate
        ! Units: prey predator-1 d-1
        prot_array%smallerVel = min(prot_array%motPrey, mot)
        prot_array%largerVel = max(prot_array%motPrey, mot)
        prot_array%encPrey = numSecPerDay * PI_8 * (prot_array%rPrey / 1E6 + r / 1E6)**2 * &
                & prot_array%nrPrey * ((prot_array%smallerVel**2 + 3 * prot_array%largerVel**2 + 4 * wTurb**2) * &
                & ((prot_array%largerVel**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)

        ! potential C-specific capture of prey
        ! Units: gC gC-1 d-1
        prot_array%capturedPrey = prot_array%encPrey * prot_array%PR * optCR * prot_array%CcellPrey / Ccell

        ! sum of all potential C-specific prey captures
        ! Units: gC gC-1 d-1 (1.0E-20: protection against division by 0 in following line)
        sumCP = sum(prot_array%capturedPrey) + 1.0E-20

        ! proportion iPrey of total prey
        ! Units: (-)
        prot_array%propPrey = prot_array%capturedPrey / sumCP

        ! total captured prey Nut:C
        ! Units: gNut gC-1 d-1
        prot_array%ingNC = prot_array%propPrey * prot_array%preyN / prot_array%preyC
        prot_array%ingPC = prot_array%propPrey * prot_array%preyP / prot_array%preyC
        ingNC = sum(prot_array%ingNC)
        ingPC = sum(prot_array%ingPC)

    end subroutine protistFoodQuantity


    !! FOOD QUALITY -------------------------------------------------------------------------------
    subroutine protistFoodQuality(ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo, ppNC, ppPC, stoichP, opAE)
        real(kind = real_wp), intent(in) :: ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo
        real(kind = real_wp), intent(out) :: ppNC, ppPC, stoichP, opAE

        ! quota of captured prey in relation to predator
        ! Units: (-)
        ppNC = ingNC / NCopt
        ppPC = ingPC / PCopt
        ! determine limiting nutrient in prey or set to 1 if preNut > predNut
        ! Units: (-)
        stoichP = min(ppNC, ppPC, 1.0)

        !! assimilation efficiency for prey
        !! Units: (-)
        opAE = (AEo + (AEm - AEo) * stoichP / (stoichP + kAE) * (1.0 + kAE)) * stoichP + 1.0E-20

    end subroutine protistFoodQuality


    ! INGESTION  -------------------------------------------------------------------------------
    subroutine protistIngestion(maxIng, sumCP, ingNC, ingPC, KI, ingSat, ingC, ingN, ingP)
        real(kind = real_wp), intent(in) :: maxIng, sumCP, ingNC, ingPC
        real(kind = real_wp), intent(out) :: KI, ingSat, ingC, ingN, ingP

        ! half saturation constant for satiation feedback (see paper by Flynn and Mitra 2016)
        ! Units: gC gC-1 d-1
        KI = (maxIng / 4.0)

        ! ingestion with satiation feedback included
        ! Units: gC gC-1 d-1
        ingSat = maxIng * sumCP / (sumCP + KI)

        ! ingestion of C
        ! Units: gC gC-1 d-1
        ingC = min(ingSat, sumCP)
        ingN = ingC * ingNC
        ingP = ingC * ingPC

    end subroutine protistIngestion


    ! MORTALITY -------------------------------------------------------------------------------
    subroutine protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut, mrtFrDet)
        real(kind = real_wp), intent(in) :: protC, MrtRT, Q10, Temp, RT, FrAut, FrDet
        real(kind = real_wp), intent(out) :: mrt, mrtFrAut, mrtFrDet

        ! Calculate mortality  ---------------------------------------
        ! Units: gC gC-1 d-1
        if (protC >= 1.0E-5) then
            mrt = Q10rate(MrtRT, Q10, Temp, RT)
        else
            mrt = 0.0
        end if
        mrtFrAut = mortality(mrt, FrAut)
        mrtFrDet = mortality(mrt, FrDet)

    end subroutine protistMortality

end module protist_food_functions
