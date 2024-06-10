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
module m_protistgreen
    use m_waq_precision

    implicit none

contains



    ! 6 char name for process mathc with second line of PDF
    subroutine PROGRE     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !
        !*******************************************************************************
        !
        use m_extract_waq_attribute
        use protist_math_functions
        use protist_cell_functions
        use protist_uptake_functions
        use protist_photosynthesis_functions
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
        integer(kind = int_wp), parameter :: nrIndInp = 7     !   nr of species independent input items
        integer(kind = int_wp), parameter :: nrSpecInp = 33   !   nr of inputs per species
        integer(kind = int_wp), parameter :: nrSpecOut = 23   !   nr of outputs per species
        integer(kind = int_wp), parameter :: nrSpecFlux = 19  !   nr of fluxes per species
        integer(kind = int_wp) :: nrInputItems     !   nr of input items need for output PMSA
        integer(kind = int_wp) :: nrOutputItems    !   nr of output items need for output PMSA
        integer(kind = int_wp) :: ipointLength     !   total length of the PMSA input and output pointer array
        integer(kind = int_wp), allocatable :: ipnt(:)          !   Local work array for the pointering

        integer(kind = int_wp) :: iseg          ! Local loop counter for computational element loop
        integer(kind = int_wp) :: ioq
        integer(kind = int_wp) :: iflux
        integer(kind = int_wp) :: ikmrk1        ! first segment attribute

        integer(kind = int_wp) :: iSpec         ! local species number counter
        integer(kind = int_wp) :: spInc         ! local species PMSA/FL number increment

        !input parameters
        integer(kind = int_wp) :: nrSpec       ! total nr species implemented in process (from proc_def)
        real(kind = real_wp) :: UmRT, Q10, RT, CR                           ! growth and respiration rate calculation
        real(kind = real_wp) :: NCm, NO3Cm, PCm, ChlCm                      ! maximum NC, PC, ChlC quotas
        real(kind = real_wp) :: NCo, PCo, ChlCo                             ! minimum NC and PC quotas
        real(kind = real_wp) :: NCopt, NO3Copt, PCopt                       ! optimal NC and PC quotas
        real(kind = real_wp) :: KtP, KtNH4, KtNO3                           ! half saturation constants
        real(kind = real_wp) :: PCoNCopt, PCoNCm                            ! P status influence on optimum NC
        real(kind = real_wp) :: ReUmNH4, ReUmNO3, redco, PSDOC, relPS       ! relative growth rates with specific nutrients
        real(kind = real_wp) :: MrtRT, FrAut, FrDet                         ! reference mortality and fractions
        real(kind = real_wp) :: rProt                                       ! radius of cell
        real(kind = real_wp) :: alpha                                       ! inital slope

        ! input state variables
        real(kind = real_wp) :: protC, protChl, protN, protP                ! protist state variables
        real(kind = real_wp) :: PO4, NH4, NO3                               ! nutrient state variables
        real(kind = real_wp) :: Temp                                        ! physical abiotic variables
        real(kind = real_wp) :: PFD, atten, exat                            ! available light and extinction


        ! auxiliaries
        real(kind = real_wp) :: NC, PC, ChlC                                ! cell nutrient quotas
        real(kind = real_wp) :: UmT, BR                                     ! growth and repsiration rates
        real(kind = real_wp) :: NCu, PCu, NPCu                              ! nutrient status within the cell
        real(kind = real_wp) :: mot                                         ! motility
        real(kind = real_wp) :: upP, upNH4, upNO3                           ! nutrient uptake
        real(kind = real_wp) :: PSqm, Cfix, synChl, degChl                  ! plateau and Cifx through photosynthesis
        real(kind = real_wp) :: maxPSreq, PS                                ! req for C to come from PS (==1 for diatoms)
        real(kind = real_wp) :: totR, Cu, NPP                               ! respiration, C-growth and nett primary production
        real(kind = real_wp) :: mrt, mrtFrAut, mrtFrDet                     ! mortality to detritus and autolysis


        ! Fluxes
        real(kind = real_wp) :: dNH4up, dNO3up, dPup                        ! uptake fluxes
        real(kind = real_wp) :: dCfix                                       ! photosynthesis flux
        real(kind = real_wp) :: dChlsyn, dChldeg                            ! Chl synthesis  and degradation flux
        real(kind = real_wp) :: dCresp                                      ! respiration flux
        real(kind = real_wp) :: dDOCleak                                    ! C leak through photosynthesis
        real(kind = real_wp) :: dDOCvoid, dNH4out, dPout                    ! voiding fluxes
        real(kind = real_wp) :: dAutC, dAutN, dAutP, dAutChl                ! autolysis fluxes
        real(kind = real_wp) :: dDetC, dDetN, dDetP, dDetChl                ! voiding fluxes

        !
        !*******************************************************************************
        !

        ! segment and species independent items
        nrSpec = nint(PMSA(ipoint(1)))   !   total nr species implemented in process                (-)

        !   nrInputItems  = nrIndInp + nrSpec * nrSpecInp = 7 + 2 * 33 = 73
        !   nrOutputItems = nrSpec * nrSpecOut = 2 * 23 = 46
        !   ipointLength = nrInputItems + nrOutputItems = 117
        !   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * 19 = 38

        ! length of the PMSA input pointer array.
        nrInputItems = nrIndInp + nrSpec * nrSpecInp
        nrOutputItems = nrSpec * nrSpecOut
        ipointLength = nrInputItems + nrOutputItems

        allocate (ipnt(ipointLength))
        ipnt(1:ipointLength) = ipoint(1:ipointLength)
        iflux = 0

        ! segment loop
        segmentLoop : do iseg = 1, noseg
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                ! species independent items
                PO4 = PMSA(ipnt(2))  !    initial external DIP                                   (gP m-3)
                NH4 = PMSA(ipnt(3))  !    initial external NH4                                   (gN m-3)
                NO3 = PMSA(ipnt(4))  !    initial external NO3                                   (gN m-3)
                Temp = PMSA(ipnt(5))  !    ambient water temperature                              (oC)
                PFD = PMSA(ipnt(6))  !    from rad to photon flux density                        (umol photon m-2)
                atten = PMSA(ipnt(7))  !    attenuation of light by water + plankton Chl           (-)
                exat = EXP(-atten)        !    -ve exponent of attenuation                            (-)

                ! species loop
                speciesLoop : do iSpec = 1, nrSpec

                    spInc = nrIndInp + (iSpec - 1) * nrSpecInp

                    ! species dependent items
                    ! (number of species independent items + location of input item in vector + species loop)
                    protC = PMSA(ipnt(spInc + 1))   !      C-biomass                                              (gC m-3)

                    ! skip if biomass is below threshold
                    if (protC <= threshCmass) then
                        cycle speciesLoop
                    end if

                    protChl = PMSA(ipnt(spInc + 2))   !      Chl-biomass                                            (gChl m-3)
                    protN = PMSA(ipnt(spInc + 3))   !      N-biomass                                              (gN m-3)
                    protP = PMSA(ipnt(spInc + 4))   !      P-biomass                                              (gP m-3)
                    alpha = PMSA(ipnt(spInc + 5))   !      alpha for photosynthesis in protist                    (Figure this out!)
                    ChlCm = PMSA(ipnt(spInc + 6))   !      maximum cellular Chl:C ratio                           (gChl gC-1)
                    ChlCo = PMSA(ipnt(spInc + 7))   !      minimum cellular Chl:C ratio                           (gChl gC-1)
                    CR = PMSA(ipnt(spInc + 8))   !      catabolic respiration quotient                         (-)
                    FrAut = PMSA(ipnt(spInc + 9))   !      fraction of mortality to autolysis                     (-)
                    FrDet = PMSA(ipnt(spInc + 10))   !      fraction of mortality to detritus                      (-)
                    KtNH4 = PMSA(ipnt(spInc + 11))   !      Kt for NH4 transport                                   (gN m-3)
                    KtNO3 = PMSA(ipnt(spInc + 12))   !      Kt for NO3 transport                                   (gN m-3)
                    KtP = PMSA(ipnt(spInc + 13))   !      Kt for DIP transport                                   (gP m-3)
                    MrtRT = PMSA(ipnt(spInc + 14))   !      mortality at reference temperature                     (-)
                    NCm = PMSA(ipnt(spInc + 15))   !      N:C that totally represses NH4 transport               (gN gC-1)
                    NCo = PMSA(ipnt(spInc + 16))   !      minimum N-quota                                        (gN gC-1)
                    NCopt = PMSA(ipnt(spInc + 17))   !      N:C for growth under optimal conditions                (gN gC-1)
                    NO3Cm = PMSA(ipnt(spInc + 18))   !      N:C that totally represses NO3 transport               (gN gC-1)
                    NO3Copt = PMSA(ipnt(spInc + 19))   !      N:C for growth on NO3 under optimal conditions         (gN gC-1)
                    PCm = PMSA(ipnt(spInc + 20))   !      PC maximum quota                                       (gP gC-1)
                    PCo = PMSA(ipnt(spInc + 21))   !      PC minimum quota                                       (gP gC-1)
                    PCoNCm = PMSA(ipnt(spInc + 22))   !      maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
                    PCoNCopt = PMSA(ipnt(spInc + 23))   !      optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
                    PCopt = PMSA(ipnt(spInc + 24))   !      PC optimum quota                                       (gP gC-1)
                    PSDOC = PMSA(ipnt(spInc + 25))   !      proportion of current PS being leaked as DOC           (-)
                    Q10 = PMSA(ipnt(spInc + 26))   !      Q10 for UmRT                                           (-)
                    redco = PMSA(ipnt(spInc + 27))   !      C respired to support nitrate reduction for NH4        (gC gN-1)
                    relPS = PMSA(ipnt(spInc + 28))   !      relative PSmax:Umax on phototrophy                     (-)
                    ReUmNH4 = PMSA(ipnt(spInc + 29))   !      max. growth rate supported by NH4-N:Umax               (-)
                    ReUmNO3 = PMSA(ipnt(spInc + 30))   !      max. growth rate supported by NO3-N:Umax               (-)
                    RT = PMSA(ipnt(spInc + 31))   !      reference temperature for UmRT                         (deg C)
                    rProt = PMSA(ipnt(spInc + 32))   !      radius of nutrient repleted protist cell               (um)
                    UmRT = PMSA(ipnt(spInc + 33))   !      maximum growth rate at reference T                     (d-1)


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
                    mot = motility(rProt) ! 1.00E-12 !
                    !mot = (1.0 - NPCu) * motility(rProt) + 1.00E-12

                    ! Calculate uptake for the nutrients ---------------------------------------
                    ! Units: gNut gC-1 d-1
                    upP = uptakeP(PC, PCo, PCopt, PCm, UmT, PO4, KtP)
                    upNH4 = uptakeNH4(PCoNCopt, PCoNCm, PCu, NCu, NC, NCo, NCopt, NCm, UmT, ReUmNH4, NH4, KtNH4)
                    upNO3 = uptakeNO3(PCoNCm, PCu, NCu, NC, NCo, NO3Copt, NO3Cm, UmT, ReUmNO3, NO3, KtNO3)

                    ! Calculate photosynthesis related equation ---------------------------------------
                    ! Units: gC gC-1 d-1
                    maxPSreq = 1.0 ! need to cover all C through photosynthesis
                    PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
                    PS = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
                    Cfix = netPS(PS, PSDOC)

                    ! Calculate chlorophyll synthesis and degradation ---------------------------------------
                    ! Units: gChl gC-1 d-1
                    synChl = synthesisChl(ChlC, ChlCo, ChlCm, UmT, maxPSreq, NPCu, Cfix, PSqm)
                    degChl = degradeChl(ChlC, ChlCm, UmT, NPCu)

                    ! Calculate respiration and C-growth  ---------------------------------------
                    ! Units: gC gC-1 d-1
                    ! 0.0 because it cannot assimilate prey
                    if (protC >= 1.0E-5) then
                        totR = totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, BR)
                    else
                        totR = 0.0
                    end if
                    !totR = totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, BR)
                    Cu = Cfix - totR

                    ! Calculate nett primary production per m3 ---------------------------------------
                    ! Units: gC m-3 d-1
                    NPP = Cu * protC

                    ! Calculate mortality  ---------------------------------------
                    ! Units: gC gC-1 d-1
                    if (protC >= 1.0E-5) then
                        mrt = Q10rate(MrtRT, Q10, Temp, RT)
                    else
                        mrt = 0.0
                    end if
                    !mrt = Q10rate(MrtRT, Q10, Temp, RT)
                    mrtFrAut = mortality(mrt, FrAut)
                    mrtFrDet = mortality(mrt, FrDet)

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
                    PMSA(ipnt(spInc + 10)) = upP
                    PMSA(ipnt(spInc + 11)) = upNH4
                    PMSA(ipnt(spInc + 12)) = upNO3
                    PMSA(ipnt(spInc + 13)) = PSqm
                    PMSA(ipnt(spInc + 14)) = PS
                    PMSA(ipnt(spInc + 15)) = Cfix
                    PMSA(ipnt(spInc + 16)) = NPP
                    PMSA(ipnt(spInc + 17)) = synChl
                    PMSA(ipnt(spInc + 18)) = degChl
                    PMSA(ipnt(spInc + 19)) = totR
                    PMSA(ipnt(spInc + 20)) = Cu
                    PMSA(ipnt(spInc + 21)) = mrt
                    PMSA(ipnt(spInc + 22)) = mrtFrAut
                    PMSA(ipnt(spInc + 23)) = mrtFrDet


                    ! FLUXES -------------------------------------------------------------------
                    ! Protist gains------------------------------------------------------------
                    ! gNut m-3 d-1   uptake of nutrients into algal biomass
                    dNH4up = protC * upNH4
                    dNO3up = protC * upNO3
                    dPup = protC * upP

                    ! gC m-3 d-1   total contribution to biomass growth from C-fixation
                    dCfix = protC * Cfix

                    ! gChl m-3 d-1 Chl synthesis or degradation
                    dChlsyn = protC * synChl
                    dChldeg = protC * degChl

                    ! Protist losses-----------------------------------------------------------
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

                    ! gNut m-3 d-1 voiding of nutrient P and N if interanl maximum is reached
                    dNH4out = voiding(protN, protC, NCm)
                    dPout = voiding(protP, protC, PCm)

                    ! gNut m-3 d-1 mortality
                    dAutC = protC * mrtFrAut
                    dDetC = protC * mrtFrDet
                    dAutN = protN * mrtFrAut
                    dDetN = protN * mrtFrDet
                    dAutP = protP * mrtFrAut
                    dDetP = protP * mrtFrDet
                    dAutChl = protChl * mrtFrAut
                    dDetChl = protChl * mrtFrDet

                    ! (1 + SpeciesLoop * (nr of fluxes per individual species) + total number of fluxes)
                    spInc = iflux + (iSpec - 1) * nrSpecFlux

                    fl (spInc + 1) = dNH4up
                    fl (spInc + 2) = dNO3up
                    fl (spInc + 3) = dPup
                    fl (spInc + 4) = dCfix
                    fl (spInc + 5) = dChlsyn
                    fl (spInc + 6) = dChldeg
                    fl (spInc + 7) = dCresp
                    fl (spInc + 8) = dDOCleak
                    fl (spInc + 9) = dDOCvoid
                    fl (spInc + 10) = dNH4out
                    fl (spInc + 11) = dPout
                    fl (spInc + 12) = dAutC
                    fl (spInc + 13) = dDetC
                    fl (spInc + 14) = dAutN
                    fl (spInc + 15) = dDetN
                    fl (spInc + 16) = dAutP
                    fl (spInc + 17) = dDetP
                    fl (spInc + 18) = dAutChl
                    fl (spInc + 19) = dDetChl

                    if (ieee_is_nan(protC)) write (*, *) 'ERROR: in Protist Green NaN in protC in segment:', iseg
                    if (ieee_is_nan(Cfix))  write (*, *) 'ERROR: in Protist Green NaN in Cfix in segment:', iseg
                    if (ieee_is_nan(totR))  write (*, *) 'ERROR: in Protist Green NaN in totR in segment:', iseg
                    if (ieee_is_nan(mrt))   write (*, *) 'ERROR: in Protist Green NaN in mrt in segment:', iseg
                    if (ieee_is_nan(NC))    write (*, *) 'ERROR: in Protist Green NaN in NC in segment:', iseg
                    if (ieee_is_nan(PC))    write (*, *) 'ERROR: in Protist Green NaN in PC in segment:', iseg
                    if (ieee_is_nan(ChlC))  write (*, *) 'ERROR: in Protist Green NaN in ChlC in segment:', iseg

                enddo speciesLoop ! end loop over species

            endif ! end if check for dry cell

            !allocate pointers
            iflux = iflux + noflux
            ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

        enddo segmentLoop ! end loop over segments
        deallocate (ipnt)
        return
    end
    ! end subroutine

end module m_protistgreen
