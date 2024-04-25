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
module m_protistncm
    use m_waq_precision

    implicit none

contains



    ! 6 char name for process mathc with second line of PDF
    subroutine PRONCM     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !
        !*******************************************************************************
        !
        use m_evaluate_waq_attribute
        use protist_math_functions
        use protist_cell_functions
        use protist_types
        use protist_phagotrophy_functions
        use protist_photosynthesis_functions
        use protist_food_functions
        use protist_constants
        use ieee_arithmetic

        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: pmsa(*)      ! I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)    ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(*)    ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg        ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        !     support variables
        integer(kind = int_wp), parameter :: nrIndInp = 5    !   nr of species independent input items
        integer(kind = int_wp), parameter :: nrSpecInp = 32  !   nr of inputs per species
        integer(kind = int_wp), parameter :: nrSpecOut = 35  !   nr of outputs per species
        integer(kind = int_wp), parameter :: nrSpecFlux = 23 !   nr of fluxes per species
        integer(kind = int_wp), parameter :: nrPreyInp = 8   !   nr of inputs per prey
        integer(kind = int_wp) :: nrInputItems     !   nr of input items need for output PMSA
        integer(kind = int_wp) :: nrOutputItems    !   nr of output items need for output PMSA
        integer(kind = int_wp) :: ipointLength     !   total length of the PMSA input and output pointer array
        integer(kind = int_wp), allocatable :: ipnt(:)          !   Local work array for the pointering

        integer(kind = int_wp) :: iseg          ! Local loop counter for computational element loop
        integer(kind = int_wp) :: ioq
        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk1        ! first segment attribute

        integer(kind = int_wp) :: iSpec         ! local species number counter
        integer(kind = int_wp) :: iPrey         ! local prey number counter
        integer(kind = int_wp) :: spInc         ! local species PMSA/FL number increment
        integer(kind = int_wp) :: prInc         ! local pray FL number increment

        ! INPUT PARAMETERS
        integer(kind = int_wp) :: nrSpec        ! total nr species implemented in process (from proc_def)
        integer(kind = int_wp) :: nrPrey        ! total nr prey implemented in process (from proc_def)
        real(kind = real_wp) :: relPhag                             ! feeding night:day
        real(kind = real_wp) :: UmRT, Q10, RT, CR                   ! growth and respiration rate calculation
        real(kind = real_wp) :: NCm, PCm                            ! maximum NC and PC quotas
        real(kind = real_wp) :: NCo, PCo                            ! minimum NC and PC quotas
        real(kind = real_wp) :: NCopt, PCopt                        ! optimal NC and PC quotas
        real(kind = real_wp) :: ChlCm, degChl
        real(kind = real_wp) :: CcellProt, rProt                    ! parameters for protozooplankton cell
        real(kind = real_wp) :: optCR                               ! parameters for encounter
        real(kind = real_wp) :: kAE, AEm, AEo                       ! parameters for assimilation efficiency
        real(kind = real_wp) :: SDA                                 ! specific dynamic action
        real(kind = real_wp) :: MrtRT, FrAut, FrDet                 ! reference mortality and fractions
        real(kind = real_wp) :: redco, PSDOC, maxPSreq, relPS       ! photosynthesis related input
        real(kind = real_wp) :: alpha                               ! inital slope

        ! INPUT STATE VARIABLES
        real(kind = real_wp) :: protC, protN, protP, protChl           ! protist state variables
        real(kind = real_wp) :: Temp                                   ! physical abiotic variables

        real(kind = real_wp) :: PFD, atten, exat                       ! available light and extinction

        ! AUXILIARIES
        real(kind = real_wp) :: lightInh        ! inhibtion of feeding in dark
        real(kind = real_wp) :: NC, PC, ChlC    ! nutrient quotas
        real(kind = real_wp) :: UmT, BR         ! growth and repsiration rates
        real(kind = real_wp) :: NCu, PCu, NPCu  ! nutrient limitations
        real(kind = real_wp) :: mot             ! motility
        ! food quantity
        real(kind = real_wp) :: sumCP        ! total captured prey
        real(kind = real_wp) :: ingNC, ingPC ! total ingested N and P
        real(kind = real_wp) :: preyFlag     ! sum of preyFlag (can be 0 = both low, 1 = 1 ok, 2 = both ok)
        ! food quality
        real(kind = real_wp) :: stoichP, ppNC, ppPC                    ! stoichiometry comparison
        real(kind = real_wp) :: opAE                                   ! assimilation efficiency
        real(kind = real_wp) :: maxIng, ingSat, ingC, ingN, ingP, KI   ! ingestion
        real(kind = real_wp) :: assC, assN, assP                       ! assimilation
        ! ingestion and assimilation
        real(kind = real_wp) :: totR, Cu, NPP                          ! respiration, C-growth and nett primary production
        real(kind = real_wp) :: mrt, mrtFrAut, mrtFrDet                ! mortality to detritus and autolysis
        ! photosynthesis
        real(kind = real_wp) :: PSqm, PS, Cfix

        ! other parameters
        real(kind = real_wp), parameter :: wTurb = 0.0 ! this needs to be an input from model eventually!!!!

        ! Fluxes
        real(kind = real_wp) :: dCeat, dNeat, dPeat                         ! assimilation fluxes
        real(kind = real_wp) :: dCresp                                      ! respiration flux
        real(kind = real_wp) :: dCfix, dChldeg, dChlout                     ! photosynthesis and Chl degradation fluxes
        real(kind = real_wp) :: dDOCleak, dDOCvoid                          ! DOC leakage
        real(kind = real_wp) :: dPOCout, dPONout, dPOPout                   ! voiding organic fluxes
        real(kind = real_wp) :: dNH4out, dPout                              ! voding inorganic fluxes
        real(kind = real_wp) :: dAutC, dAutN, dAutP, dAutChl                ! autolysis fluxes
        real(kind = real_wp) :: dDetC, dDetN, dDetP, dDetChl                ! detritus fluxes
        real(kind = real_wp) :: dChlup                                      ! uptake of prey chlorophyll


        ! Protist arrays
        type(protist_array) :: prot_array                 ! type containing all protist specific arrays

        !
        !*******************************************************************************
        !
        ! segment and species independent items
        nrSpec = nint(PMSA(ipoint(1)))   !   total nr species implemented in process                (-)
        nrPrey = nint(PMSA(ipoint(2)))   !   nr of prey species implemented                         (-)

        !   nrInputs  = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec) = 5 + 2 * 32 + 6 * (8 + 2) = 129
        !   nrOutputs = nrSpec * nrSpecOut = 2 * 35 = 70
        !   ipointLength = nrInputs + nrOutputs = 197
        !   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * (23 + 6 * 5) = 106

        ! length of the PMSA input pointer array.
        nrInputItems = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec)
        nrOutputItems = nrSpec * nrSpecOut
        ipointLength = nrInputItems + nrOutputItems

        allocate (ipnt(ipointLength))
        ipnt(1:ipointLength) = ipoint(1:ipointLength)
        iflux = 0

        ! allocation of prey input array
        call allocate_prot_array(prot_array, nrPrey)


        ! segment loop
        segmentLoop : do iseg = 1, noseg
            call evaluate_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                Temp = PMSA(ipnt(3))  !    temperature                                            (C)
                PFD = PMSA(ipnt(4))  !    from rad to photon flux density                        (umol photon m-2)
                atten = PMSA(ipnt(5))  !    attenuation of light by water + plankton Chl           (-)
                exat = EXP(-atten)       !    -ve exponent of attenuation                            (-)

                ! species loop
                speciesLoop : do iSpec = 1, nrSpec

                    spInc = nrSpecInp * iSpec

                    ! species dependent items
                    ! (number of species independent items + location of input item in vector + species loop)
                    spInc = nrIndInp + (iSpec - 1) * nrSpecInp

                    protC = PMSA(ipnt(spInc + 1))  ! C-biomass                                              (gC m-3)

                    if (protC <= threshCmass) then
                        cycle speciesLoop
                    end if

                    protChl = PMSA(ipnt(spInc + 2))  ! Chl-biomass                                            (gChl m-3)
                    protN = PMSA(ipnt(spInc + 3))  ! N-biomass                                              (gN m-3)
                    protP = PMSA(ipnt(spInc + 4))  ! P-biomass                                              (gP m-3)
                    AEm = PMSA(ipnt(spInc + 5))  ! maximum assimilation efficiency (AE)                   (-)
                    AEo = PMSA(ipnt(spInc + 6))  ! minimum AE                                             (-)
                    alpha = PMSA(ipnt(spInc + 7))  ! alpha for photosynthesis in protist                    (Figure this out!)
                    CcellProt = PMSA(ipnt(spInc + 8))  ! C content of protist cell                              (pgC cell-1)
                    ChlCm = PMSA(ipnt(spInc + 9))  ! maximum cellular Chl:C ratio                           (gChl gC-1)
                    CR = PMSA(ipnt(spInc + 10))  ! catabolic respiration quotient                         (-)
                    degChl = PMSA(ipnt(spInc + 11))  ! Chl degradation see Ghyoot 2017                        (d-1)
                    FrAut = PMSA(ipnt(spInc + 12))  ! fraction of mortality to autolysis                     (-)
                    FrDet = PMSA(ipnt(spInc + 13))  ! fraction of mortality to detritus                      (-)
                    kAE = PMSA(ipnt(spInc + 14))  ! Control of AE in response to prey quality              (-)
                    MrtRT = PMSA(ipnt(spInc + 15))  ! mortality at reference temperature                     (-)
                    maxPSreq = PMSA(ipnt(spInc + 16))  ! maximum C to come from PS                              (-)
                    NCm = PMSA(ipnt(spInc + 17))  ! N:C that totally represses NH4 transport               (gN gC-1)
                    NCo = PMSA(ipnt(spInc + 18))  ! minimum N-quota                                        (gN gC-1)
                    NCopt = PMSA(ipnt(spInc + 19))  ! N:C for growth under optimal conditions                (gN gC-1)
                    optCR = PMSA(ipnt(spInc + 20))  ! proportion of prey captured by starved Zoo             (-)
                    PCm = PMSA(ipnt(spInc + 21))  ! PC maximum quota                                       (gP gC-1)
                    PCo = PMSA(ipnt(spInc + 22))  ! PC minimum quota                                       (gP gC-1)
                    PCopt = PMSA(ipnt(spInc + 23))  ! PC optimum quota                                       (gP gC-1)
                    PSDOC = PMSA(ipnt(spInc + 24))  ! proportion of current PS being leaked as DOC           (-)
                    Q10 = PMSA(ipnt(spInc + 25))  ! Q10 for UmRT                                           (-)
                    rProt = PMSA(ipnt(spInc + 26))  ! radius of nutrient repleted protist cell               (um)
                    redco = PMSA(ipnt(spInc + 27))  ! C respired to support nitrate reduction for NH4        (gC gN-1)
                    relPhag = PMSA(ipnt(spInc + 28))  ! rel. phagotrophy in dark : in light                    (-)
                    relPS = PMSA(ipnt(spInc + 29))  ! relative PSmax:Umax on phototrophy                     (-)
                    RT = PMSA(ipnt(spInc + 30))  ! reference temperature for UmRT                         (deg C)
                    SDA = PMSA(ipnt(spInc + 31))  ! specific dynamic action                                (-)
                    UmRT = PMSA(ipnt(spInc + 32))  ! maximum growth rate at reference T                     (d-1)


                    ! Calculate the nutrient quota of the cell-------------------------------------------------------------------------------
                    ! Units: gNut gC-1
                    NC = quota(protN, protC)
                    PC = quota(protP, protC)
                    ChlC = quota(protChl, protC)

                    ! Calculate maximum growth and respiration -------------------------------------------------------------------------------
                    ! Units: gC gC-1 d-1
                    UmT = Q10rate(UmRT, Q10, Temp, RT)
                    BR = basal_respiration(UmT, CR)

                    ! Calculate nutrient status within cell compared to ideal status (nutrient status = 1) ---------------------------------------
                    ! Determine minimum of N-P-Si limitation; Liebig-style limitation of growth (NPCu)
                    ! Units: (-)
                    NCu = statusNC(NC, NCo, NCopt)
                    PCu = statusPC(PC, PCo, PCopt)
                    NPCu = min(NCu, PCu)

                    ! swimming speed -------------------------------------------------------------------------------
                    ! Units: m s-1
                    mot = motility(rProt)

                    call initialize_prot_array(prot_array, nrPrey, PMSA, ipnt, nrIndInp, nrSpec, nrSpecInp, iSpec, nrPreyInp)


                    ! for output (-)
                    preyFlag = sum(prot_array%preyFlag)

                    !! FOOD QUANTITY -------------------------------------------------------------------------------
                    ! reduction of phagotrophy during night
                    ! Units: (-)
                    relPhag = 1.0 - relPhag
                    lightInh = lightInhibition(PFD, relPhag)

                    ! cell abundance of prey per m3
                    ! Units: nr cells m-3 (1e12: transform between g (preyC) and pg (CcontentPrey))
                    prot_array%nrPrey = lightInh * prot_array%preyFlag * 1e12 * prot_array%preyC / prot_array%CcellPrey
                    call protistFoodQuantity(prot_array, rProt, wTurb, CcellProt, optCR, mot, sumCP, ingNC, ingPC)



                    !! FOOD QUALITY -------------------------------------------------------------------------------
                    call protistFoodQuality(ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo, ppNC, ppPC, stoichP, opAE)



                    ! INGESTION  -------------------------------------------------------------------------------
                    ! maximum ingestion
                    ! Units: gC gC-1 d-1
                    maxIng = ((UmT + BR) / (1.0 - SDA)) / opAE
                    call protistIngestion(maxIng, sumCP, ingNC, ingPC, KI, ingSat, ingC, ingN, ingP)



                    ! ASSIMILATION -------------------------------------------------------------------------------
                    ! assimilation of ingested prey
                    ! Units: gC gC-1 d-1 / gNut gC-1 d-1
                    assC = ingC * opAE
                    assN = assC * NCopt
                    assP = assC * PCopt

                    ! Calculate photosynthesis related equation ---------------------------------------
                    ! Units: gC gC-1 d-1
                    ! LS: I do not like the variable maxPSreq. Not measureable and pretty "strong" influence
                    ! at the moment they keep all the chl from prey.... is this correct... ???
                    ! only keep if C is low???
                    ! NPCu set to 1 following Flynn and Mitra 2009 logic
                    ! can only use ingested Chl in next timestep
                    PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
                    PS = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
                    Cfix = netPS(PS, PSDOC)

                    ! Calculate nett primary production per m3 ---------------------------------------
                    ! Units: gC m-3 d-1
                    ! NCM can't do nutrient uptake like Diat, Green, CM so there is no respiration directly associated with PP
                    NPP = Cfix * protC

                    ! Calculate respiration   ---------------------------------------
                    ! Units: gC gC-1 d-1
                    ! protzoo cannot recover loss N
                    if (protC >= 1.0E-5) then
                        totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)
                    else
                        totR = 0.0
                    end if
                    !totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)
                    Cu = CgrowthRate(Cfix, assC, totR)

                    ! Calculate mortality  ---------------------------------------
                    call protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut, mrtFrDet)






                    ! Output -------------------------------------------------------------------

                    ! (input items + position of specific output item in vector + species loop * total number of output)
                    spInc = nrInputItems + (iSpec - 1) * nrSpecOut

                    PMSA(ipnt(spInc + 1)) = NC
                    PMSA(ipnt(spInc + 2)) = PC
                    PMSA(ipnt(spInc + 3)) = ChlC
                    PMSA(ipnt(spInc + 4)) = UmT
                    PMSA(ipnt(spInc + 5)) = BR
                    PMSA(ipnt(spInc + 6)) = NCu
                    PMSA(ipnt(spInc + 7)) = PCu
                    PMSA(ipnt(spInc + 8)) = NPCu
                    PMSA(ipnt(spInc + 9)) = mot
                    PMSA(ipnt(spInc + 10)) = sumCP
                    PMSA(ipnt(spInc + 11)) = ingNC
                    PMSA(ipnt(spInc + 12)) = ingPC
                    PMSA(ipnt(spInc + 13)) = ppNC
                    PMSA(ipnt(spInc + 14)) = ppPC
                    PMSA(ipnt(spInc + 15)) = stoichP
                    PMSA(ipnt(spInc + 16)) = opAE
                    PMSA(ipnt(spInc + 17)) = maxIng
                    PMSA(ipnt(spInc + 18)) = ingSat
                    PMSA(ipnt(spInc + 19)) = ingC
                    PMSA(ipnt(spInc + 20)) = assC
                    PMSA(ipnt(spInc + 21)) = ingN
                    PMSA(ipnt(spInc + 22)) = ingP
                    PMSA(ipnt(spInc + 23)) = assN
                    PMSA(ipnt(spInc + 24)) = assP
                    PMSA(ipnt(spInc + 25)) = PSqm
                    PMSA(ipnt(spInc + 26)) = PS
                    PMSA(ipnt(spInc + 27)) = Cfix
                    PMSA(ipnt(spInc + 28)) = NPP
                    PMSA(ipnt(spInc + 29)) = totR
                    PMSA(ipnt(spInc + 30)) = Cu
                    PMSA(ipnt(spInc + 31)) = mrt
                    PMSA(ipnt(spInc + 32)) = mrtFrAut
                    PMSA(ipnt(spInc + 33)) = mrtFrDet
                    PMSA(ipnt(spInc + 34)) = preyFlag
                    PMSA(ipnt(spInc + 35)) = lightInh

                    ! FLUXES -------------------------------------------------------------------
                    ! Protist gains------------------------------------------------------------
                    ! Protist growth through assimilation -----------------------------------------------------
                    ! gX m-3 d-1 assimilation of X from prey
                    dCeat = protC * assC
                    dNeat = protC * assN
                    dPeat = protC * assP

                    ! gC m-3 d-1   total contribution to biomass growth from C-fixation
                    dCfix = protC * Cfix

                    ! Protist losses-----------------------------------------------------------
                    ! gChl m-3 d-1 Chl synthesis or degradation
                    dChldeg = protChl * degChl

                    ! gChl m-3 d-1 voiding of protist Chl if interanl maximum is reached
                    dChlout = voiding(protChl, protC, ChlCm)

                    ! gC m-3 d-1   total respiration rate
                    dCresp = protC * totR

                    ! gC m-3 d-1   release of DOC
                    dDOCleak = protC * (PS - Cfix)

                    ! gC m-3 d-1   voiding of C as DOC if NC falls below NCo
                    if (NC < NCo) then
                        dDOCvoid = protC - protN / NCo
                    else
                        dDOCvoid = 0.0
                    end if

                    ! gX m-3 d-1  rate of voiding of X as particulates
                    dPOCout = protC * (ingC - assC)
                    dPONout = protC * (ingN - assN)
                    dPOPout = protC * (ingP - assP)

                    ! gNut m-3 d-1 voiding of nutrient P and N if interanl maximum is reached
                    dNH4out = voiding(protN, protC, NCopt)
                    dPout = voiding(protP, protC, PCopt)

                    ! gNut m-3 d-1 mortality
                    dAutC = protC **2 * mrtFrAut
                    dDetC = protC **2 * mrtFrDet
                    dAutN = protN **2 * mrtFrAut
                    dDetN = protN **2 * mrtFrDet
                    dAutP = protP **2 * mrtFrAut
                    dDetP = protP **2 * mrtFrDet
                    dAutChl = protChl **2 * mrtFrAut
                    dDetChl = protChl **2 * mrtFrDet



                    ! Prey losses through pred ing. ----------------------------------------------------
                    ! ingestion of nut of iPrey through iPred gNut m-3 d-1
                    prot_array%dPreyC = protC * (ingC * prot_array%propPrey)
                    prot_array%dPreyChl = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)
                    prot_array%dPreyN = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC)
                    prot_array%dPreyP = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC)
                    prot_array%dPreySi = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)

                    ! Chlorophyll uptake ----------------------------------------------------
                    ! acquistion of prey chlorphyll gChl m-3 d-1
                    dChlup = sum(prot_array%dPreyChl) * upChl(ChlC, ChlCm)

                    ! (1 + SpeciesLoop * (nr of fluxes per individual species + total prey fluxes) + total number of fluxes
                    spInc = iFlux + (iSpec - 1) * (nrSpecFlux + nrPrey * nrLossFluxes)

                    fl (spInc + 1) = dCeat
                    fl (spInc + 2) = dNeat
                    fl (spInc + 3) = dPeat
                    fl (spInc + 4) = dCfix
                    fl (spInc + 5) = dChldeg
                    fl (spInc + 6) = dChlout
                    fl (spInc + 7) = dCresp
                    fl (spInc + 8) = dDOCleak
                    fl (spInc + 9) = dDOCvoid
                    fl (spInc + 10) = dPOCout
                    fl (spInc + 11) = dPONout
                    fl (spInc + 12) = dPOPout
                    fl (spInc + 13) = dNH4out
                    fl (spInc + 14) = dPout
                    fl (spInc + 15) = dAutC
                    fl (spInc + 16) = dDetC
                    fl (spInc + 17) = dAutN
                    fl (spInc + 18) = dDetN
                    fl (spInc + 19) = dAutP
                    fl (spInc + 20) = dDetP
                    fl (spInc + 21) = dAutChl
                    fl (spInc + 22) = dDetChl
                    fl (spInc + 23) = dChlup

                    ! loop over prey ingestion fluxes
                    do iPrey = 1, nrPrey
                        ! (nr prey independent fluxes + prey Flux # + loop) + (move on to next predator) + total number of fluxes
                        prInc = spInc + nrSpecFlux + (iPrey - 1) * nrLossFluxes

                        fl (prInc + 1) = prot_array%dPreyC(iPrey)
                        fl (prInc + 2) = prot_array%dPreyChl(iPrey)
                        fl (prInc + 3) = prot_array%dPreyN(iPrey)
                        fl (prInc + 4) = prot_array%dPreyP(iPrey)
                        fl (prInc + 5) = prot_array%dPreySi(iPrey)
                    end do

                    if (ieee_is_nan(protC)) write (*, *) 'ERROR: in ProtistNCM, NaN in protC in segment:', iseg
                    if (ieee_is_nan(Cfix))  write (*, *) 'ERROR: in ProtistNCM, NaN in Cfix in segment:', iseg
                    if (ieee_is_nan(totR))  write (*, *) 'ERROR: in ProtistNCM, NaN in totR in segment:', iseg
                    if (ieee_is_nan(mrt))   write (*, *) 'ERROR: in ProtistNCM, NaN in mrt in segment:', iseg
                    if (ieee_is_nan(NC))    write (*, *) 'ERROR: in ProtistNCM, NaN in NC in segment:', iseg
                    if (ieee_is_nan(PC))    write (*, *) 'ERROR: in ProtistNCM, NaN in PC in segment:', iseg
                    if (ieee_is_nan(ChlC))  write (*, *) 'ERROR: in ProtistNCM, NaN in ChlC in segment:', iseg
                    if (ieee_is_nan(ingC))  write (*, *) 'ERROR: in ProtistNCM, NaN in ingC in segment:', iseg

                enddo speciesLoop ! end loop over species

            endif ! end if check for dry cell

            !allocate pointers
            iflux = iflux + noflux
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

        enddo segmentLoop! end loop over segments


        ! deallocation of prey input array
        call deallocate_prot_array(prot_array)
        deallocate (ipnt)
        return
    end
    ! end subroutine

end module m_protistncm
