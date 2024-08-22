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
module m_protistzoo
    use m_waq_precision

    implicit none

contains



    ! 6 char name for process mathc with second line of PDF
    subroutine PROZOO     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !
        !*******************************************************************************
        !
        use m_extract_waq_attribute
        use protist_math_functions
        use protist_cell_functions
        use protist_phagotrophy_functions
        use protist_types
        use protist_food_functions
        use protist_constants
        use m_protistlog
        use ieee_arithmetic

        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)      ! I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)    ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(*)    ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells        ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir         ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        !     support variables
        integer(kind = int_wp), parameter :: nrIndInp = 3     !   nr of species independent input items
        integer(kind = int_wp), parameter :: nrSpecInp = 23   !   nr of inputs per species
        integer(kind = int_wp), parameter :: nrSpecOut = 29   !   nr of outputs per species
        integer(kind = int_wp), parameter :: nrSpecFlux = 15  !   nr of fluxes per species
        integer(kind = int_wp), parameter :: nrPreyInp = 8    !   nr of inputs per prey
        integer(kind = int_wp) :: nrInputItems     !   nr of input items need for output process_space_real
        integer(kind = int_wp) :: nrOutputItems    !   nr of output items need for output process_space_real
        integer(kind = int_wp) :: ipointLength     !   total length of the process_space_real input and output pointer array
        integer(kind = int_wp), allocatable :: ipnt(:)          !   Local work array for the pointering

        integer(kind = int_wp) :: iseg          ! Local loop counter for computational element loop
        integer(kind = int_wp) :: ioq
        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk1        ! first segment attribute

        integer(kind = int_wp) :: iSpec         ! local species number counter
        integer(kind = int_wp) :: iPrey         ! local prey number counter
        integer(kind = int_wp) :: spInc         ! local species process_space_real/FL number increment
        integer(kind = int_wp) :: prInc         ! local pray FL number increment

        ! INPUT PARAMETERS
        integer(kind = int_wp) :: nrSpec        ! total nr species implemented in process (from proc_def)
        integer(kind = int_wp) :: nrPrey        ! total nr prey implemented in process (from proc_def)
        real(kind = real_wp) :: UmRT, Q10, RT, CR                   ! growth and respiration rate calculation
        real(kind = real_wp) :: NCm, PCm                            ! maximum NC and PC quotas
        real(kind = real_wp) :: NCo, PCo                            ! minimum NC and PC quotas
        real(kind = real_wp) :: NCopt, PCopt                        ! optimal NC and PC quotas
        real(kind = real_wp) :: CcellZoo, rZoo                      ! parameters for protozooplankton cell
        real(kind = real_wp) :: optCR                               ! parameters for encounter
        real(kind = real_wp) :: kAE, AEm, AEo                       ! parameters for assimilation efficiency
        real(kind = real_wp) :: SDA                                 ! specific dynamic action
        real(kind = real_wp) :: MrtRT, FrAut, FrDet                 ! reference mortality and fractions

        ! INPUT STATE VARIABLES
        real(kind = real_wp) :: protC, protN, protP                    ! protist state variables
        real(kind = real_wp) :: Temp                                   ! physical abiotic variables

        ! AUXILIARIES
        real(kind = real_wp) :: NC, PC          ! nutrient quotas
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
        real(kind = real_wp) :: totR, Cu                               ! respiration and C-growth
        real(kind = real_wp) :: mrt, mrtFrAut, mrtFrDet                ! mortality to detritus and autolysis

        ! other parameters
        real(kind = real_wp), parameter :: wTurb = 0.0                  ! necessary for empirical relations
        ! can, if desired, be modified to be an input from the hydromechanics

        ! Fluxes
        real(kind = real_wp) :: dCeat, dNeat, dPeat                         ! assimilation fluxes
        real(kind = real_wp) :: dCresp                                      ! respiration flux
        real(kind = real_wp) :: dPOCout, dPONout, dPOPout                   ! voiding organic fluxes
        real(kind = real_wp) :: dNH4out, dPout                              ! voding inorganic fluxes
        real(kind = real_wp) :: dAutC, dAutN, dAutP                         ! autolysis fluxes
        real(kind = real_wp) :: dDetC, dDetN, dDetP                         ! detritus fluxes


        ! Protist arrays
        type(protist_array) :: prot_array                 ! type containing all protist specific arrays

        !
        !*******************************************************************************
        !
        ! segment and species independent items
        nrSpec = nint(process_space_real(ipoint(1)))   !   total nr species implemented in process                (-)
        nrPrey = nint(process_space_real(ipoint(2)))   !   nr of prey species implemented                         (-)

        !   nrInputs  = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec) = 3 + 2 * 23 + 8 * (8 + 2) = 129
        !   nrOutputs = nrSpec * nrSpecOut = 2 * 29 = 58
        !   ipointLength = nrInputs + nrOutputs = 118
        !   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * (15 + 8 * 5) = 110

        ! length of the process_space_real input pointer array.
        nrInputItems = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec)
        nrOutputItems = nrSpec * nrSpecOut
        ipointLength = nrInputItems + nrOutputItems

        allocate (ipnt(ipointLength))
        ipnt(1:ipointLength) = ipoint(1:ipointLength)
        iflux = 0

        ! allocation of prey input array
        call allocate_prot_array(prot_array, nrPrey)


        ! segment loop
        segmentLoop : do iseg = 1, num_cells
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                Temp = process_space_real(ipnt(3))   !   temperature                                            (C)

                ! species loop
                speciesLoop : do iSpec = 1, nrSpec

                    spInc = nrIndInp + (iSpec - 1) * nrSpecInp

                    ! species dependent items
                    ! (number of species independent items + location of input item in vector + species loop)
                    protC = process_space_real(ipnt(spInc + 1))   !      C-biomass                                              (gC m-3)

                    if (protC <= threshCmass) then
                        cycle speciesLoop
                    end if

                    protN = process_space_real(ipnt(spInc + 2))   !      N-biomass                                              (gN m-3)
                    protP = process_space_real(ipnt(spInc + 3))   !      P-biomass                                              (gP m-3)
                    AEm = process_space_real(ipnt(spInc + 4))   !      maximum assimilation efficiency (AE)                   (-)
                    AEo = process_space_real(ipnt(spInc + 5))   !      minimum AE                                             (-)
                    CcellZoo = process_space_real(ipnt(spInc + 6))   !      C content of protist cell                              (pgC cell-1)
                    CR = process_space_real(ipnt(spInc + 7))   !      catabolic respiration quotient                         (-)
                    FrAut = process_space_real(ipnt(spInc + 8))   !      fraction of mortality to autolysis                     (-)
                    FrDet = process_space_real(ipnt(spInc + 9))   !      fraction of mortality to detritus                      (-)
                    kAE = process_space_real(ipnt(spInc + 10))   !      Control of AE in response to prey quality              (-)
                    MrtRT = process_space_real(ipnt(spInc + 11))   !      mortality at reference temperature                     (-)
                    NCm = process_space_real(ipnt(spInc + 12))   !      N:C that totally represses NH4 transport               (gN gC-1)
                    NCo = process_space_real(ipnt(spInc + 13))   !      minimum N-quota                                        (gN gC-1)
                    NCopt = process_space_real(ipnt(spInc + 14))   !      N:C for growth under optimal conditions                (gN gC-1)
                    optCR = process_space_real(ipnt(spInc + 15))   !      proportion of prey captured by starved Zoo             (-)
                    PCm = process_space_real(ipnt(spInc + 16))   !      PC maximum quota                                       (gP gC-1)
                    PCo = process_space_real(ipnt(spInc + 17))   !      PC minimum quota                                       (gP gC-1)
                    PCopt = process_space_real(ipnt(spInc + 18))   !      PC optimum quota                                       (gP gC-1)
                    Q10 = process_space_real(ipnt(spInc + 19))   !      Q10 for UmRT                                           (-)
                    RT = process_space_real(ipnt(spInc + 20))   !      reference temperature for UmRT                         (deg C)
                    rZoo = process_space_real(ipnt(spInc + 21))   !      radius of nutrient repleted protist cell               (um)
                    SDA = process_space_real(ipnt(spInc + 22))   !      specific dynamic action                                (-)
                    UmRT = process_space_real(ipnt(spInc + 23))   !      maximum growth rate using NH4-N at reference T         (d-1)


                    ! Calculate the nutrient quota of the cell-------------------------------------------------------------------------------
                    ! Units: gNut gC-1
                    NC = quota(protN, protC)
                    PC = quota(protP, protC)

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
                    mot = motility(rZoo)

                    call initialize_prot_array(prot_array, nrPrey, process_space_real, ipnt, nrIndInp, nrSpec, nrSpecInp, iSpec, nrPreyInp)


                    ! for output (-)
                    preyFlag = sum(prot_array%preyFlag)


                    !! FOOD QUANTITY -------------------------------------------------------------------------------
                    ! cell abundance of prey per m3
                    ! Units: nr cells m-3 (1e12: transform between g (preyC) and pg (CcontentPrey))
                    prot_array%nrPrey = prot_array%preyFlag * 1e12 * prot_array%preyC / prot_array%CcellPrey

                    call protistFoodQuantity(prot_array, rZoo, wTurb, CcellZoo, optCR, mot, sumCP, ingNC, ingPC)



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

                    ! Calculate respiration   ---------------------------------------
                    ! Units: gC gC-1 d-1
                    ! protzoo cannot recover loss N
                    if (protC >= 1.0E-5) then
                        totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)
                    else
                        totR = 0.0
                    end if
                    !totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)
                    Cu = CgrowthRate(0.0, assC, totR)

                    ! Calculate mortality  ---------------------------------------
                    call protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut, mrtFrDet)



                    ! Output -------------------------------------------------------------------

                    ! (input items + position of specific output item in vector + species loop * total number of output)
                    spInc = nrInputItems + (iSpec - 1) * nrSpecOut

                    process_space_real(ipnt(spInc + 1)) = NC
                    process_space_real(ipnt(spInc + 2)) = PC
                    process_space_real(ipnt(spInc + 3)) = UmT
                    process_space_real(ipnt(spInc + 4)) = BR
                    process_space_real(ipnt(spInc + 5)) = NCu
                    process_space_real(ipnt(spInc + 6)) = PCu
                    process_space_real(ipnt(spInc + 7)) = NPCu
                    process_space_real(ipnt(spInc + 8)) = mot
                    process_space_real(ipnt(spInc + 9)) = sumCP
                    process_space_real(ipnt(spInc + 10)) = ingNC
                    process_space_real(ipnt(spInc + 11)) = ingPC
                    process_space_real(ipnt(spInc + 12)) = ppNC
                    process_space_real(ipnt(spInc + 13)) = ppPC
                    process_space_real(ipnt(spInc + 14)) = stoichP
                    process_space_real(ipnt(spInc + 15)) = opAE
                    process_space_real(ipnt(spInc + 16)) = maxIng
                    process_space_real(ipnt(spInc + 17)) = ingSat
                    process_space_real(ipnt(spInc + 18)) = ingC
                    process_space_real(ipnt(spInc + 19)) = assC
                    process_space_real(ipnt(spInc + 20)) = ingN
                    process_space_real(ipnt(spInc + 21)) = ingP
                    process_space_real(ipnt(spInc + 22)) = assN
                    process_space_real(ipnt(spInc + 23)) = assP
                    process_space_real(ipnt(spInc + 24)) = totR
                    process_space_real(ipnt(spInc + 25)) = Cu
                    process_space_real(ipnt(spInc + 26)) = mrt
                    process_space_real(ipnt(spInc + 27)) = mrtFrAut
                    process_space_real(ipnt(spInc + 28)) = mrtFrDet
                    process_space_real(ipnt(spInc + 29)) = preyFlag

                    ! FLUXES -------------------------------------------------------------------
                    ! Protist gains------------------------------------------------------------
                    ! Protist growth through assimilation -----------------------------------------------------
                    ! gX m-3 d-1 assimilation of X from prey
                    dCeat = protC * assC
                    dNeat = protC * assN
                    dPeat = protC * assP

                    ! Protist losses-----------------------------------------------------------
                    ! gC m-3 d-1   total respiration rate
                    dCresp = protC * totR

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


                    ! (1 + SpeciesLoop * (nr of fluxes per individual species + total prey fluxes) + total number of fluxes
                    spInc = iFlux + (iSpec - 1) * (nrSpecFlux + nrPrey * nrLossFluxes)

                    fl (spInc + 1) = dCeat
                    fl (spInc + 2) = dNeat
                    fl (spInc + 3) = dPeat
                    fl (spInc + 4) = dCresp
                    fl (spInc + 5) = dPOCout
                    fl (spInc + 6) = dPONout
                    fl (spInc + 7) = dPOPout
                    fl (spInc + 8) = dNH4out
                    fl (spInc + 9) = dPout
                    fl (spInc + 10) = dAutC
                    fl (spInc + 11) = dDetC
                    fl (spInc + 12) = dAutN
                    fl (spInc + 13) = dDetN
                    fl (spInc + 14) = dAutP
                    fl (spInc + 15) = dDetP

                    if (.not. ieee_is_finite(protC)) call write_warning('ERROR: in ProtistZoo, NaN/Inf in protC in segment:', iseg)
                    if (.not. ieee_is_finite(ingC))  call write_warning('ERROR: in ProtistZoo, NaN/Inf in ingC in segment:', iseg)
                    if (.not. ieee_is_finite(assC))  call write_warning('ERROR: in ProtistZoo, NaN/Inf in assC in segment:', iseg)
                    if (.not. ieee_is_finite(totR))  call write_warning('ERROR: in ProtistZoo, NaN/Inf in totR in segment:', iseg)
                    if (.not. ieee_is_finite(mrt))   call write_warning('ERROR: in ProtistZoo, NaN/Inf in mrt in segment:', iseg)

                    ! Prey losses through pred ing. ----------------------------------------------------

                    ! ingestion of nut of iPrey through iPred gNut m-3 d-1
                    prot_array%dPreyC = protC * (ingC * prot_array%propPrey)
                    prot_array%dPreyChl = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)
                    prot_array%dPreyN = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC)
                    prot_array%dPreyP = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC)
                    prot_array%dPreySi = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)

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

                enddo speciesLoop ! end loop over species

            endif ! end if check for dry cell

            !allocate pointers
            iflux = iflux + noflux
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

        enddo segmentLoop ! end loop over segments


        ! deallocation of prey input array
        call deallocate_prot_array(prot_array)
        deallocate (ipnt)
        return
    end
    ! end subroutine


end module m_protistzoo
