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

module m_calpowfl
    use m_waq_precision

    implicit none

contains

    subroutine calpowfl ( process_space_real, fl, ipoint, increm, num_cells, &
                          noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                          num_exchanges_z_dir, num_exchanges_bottom_dir )
        use m_extract_waq_attribute

        !>\file
        !>       Compute the porewater fluxes due to consolidation and evaporation
        !        (part 3 of the 2-layer bed model)
        
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE POREWATER FLUXES DUE TO CONSOLIDATION AND EVAPORATION
        !        BASED ON THE EXCESS PORE PRESSURE IN BED LAYERS
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! WATS1     R*4 1 I  water content of sediment layer S1         [g/m2]
        ! WATS2     R*4 1 I  water content of sediment layer S2         [g/m2]
        ! PHI_IM1S1 R*4 1 I  volumetric fraction IM1 in layer S1        [m3/m3]
        ! PHI_IM2S1 R*4 1 I  volumetric fraction IM2 in layer S1        [m3/m3]
        ! PHI_IM3S1 R*4 1 I  volumetric fraction IM3 in layer S1        [m3/m3]
        ! PHI_IM1S2 R*4 1 I  volumetric fraction IM1 in layer S2        [m3/m3]
        ! PHI_IM2S2 R*4 1 I  volumetric fraction IM2 in layer S2        [m3/m3]
        ! PHI_IM3S2 R*4 1 I  volumetric fraction IM3 in layer S2        [m3/m3]
        ! ZTL       R*4 1 I  material thickness layer S1                [m]
        ! ZBL       R*4 1 I  material thickness layer S2                [m]
        ! RHOIM1    R*4 1 I  specific density of IM1                    [gDM/m3]
        ! RHOIM2    R*4 1 I  specific density of IM2                    [gDM/m3]
        ! RHOIM3    R*4 1 I  specific density of IM3                    [gDM/m3]
        ! RHOW      R*4 1 I  density of water                           [kg/m3]
        ! HTL       R*4 1 I  actual thickness layer S1                  [m]
        ! HBL       R*4 1 I  actual thickness layer S2                  [m]
        ! DEPTH     R*4 1 I  depth of segment                           [m]
        ! MINDEPTH  R*4 1 I  minimum waterdepth for sedimentation/resuspension [m]
        ! DELT      R*4 1 I  timestep for processes                     [d]
        ! FTL0      R*4 1 I  user defined additional porewater flux in S1 [m/s]
        ! FBL0      R*4 1 I  user defined additional porewater flux in S2 [m/s]
        ! POWFL_S1  R*4 1 O  porewater flux in layer S1                 [g/m2/day]
        ! POWFL_S2  R*4 1 O  porewater flux in layer S2                 [g/m2/day]
        ! PETL      R*4 1 O  excess pore pressure in layer S1           [Pa]
        ! PEBL      R*4 1 O  excess pore pressure in layer S2           [Pa]
        
        !     Logical Units : -
        
        !     Modules called : -
        
        !     Name     Type   Library
        
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(28), INCREM(28), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, IPNT(28)
!
        IPNT = IPOINT
!
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                
                    ! Water concent in S1 and S2
                    WATS1     = process_space_real(IPNT(1 ))
                    WATS2     = process_space_real(IPNT(2 ))
                    !        Substance 1
                    PHI_IM1S1 = process_space_real(IPNT(3 ))
                    PHI_IM1S2 = process_space_real(IPNT(6 ))
                    !        Substance 2
                    PHI_IM2S1 = process_space_real(IPNT(4 ))
                    PHI_IM2S2 = process_space_real(IPNT(7 ))
                    !        Substance 3
                    PHI_IM3S1 = process_space_real(IPNT(5 ))
                    PHI_IM3S2 = process_space_real(IPNT(8 ))
                
                    ZTL      = process_space_real(IPNT(9 ))
                    ZBL      = process_space_real(IPNT(10))
                    RHOIM1   = process_space_real(IPNT(11)) / 1.0E+03 ! (kg/m3)
                    RHOIM2   = process_space_real(IPNT(12)) / 1.0E+03 ! (kg/m3)
                    RHOIM3   = process_space_real(IPNT(13)) / 1.0E+03 ! (kg/m3)
                    RHOW     = process_space_real(IPNT(14))
                    HTL      = process_space_real(IPNT(15))
                    HBL      = process_space_real(IPNT(16))
                    DEPTH    = process_space_real(IPNT(17))
                    MINDEP   = process_space_real(IPNT(18))
                    DELT     = process_space_real(IPNT(19)) ! (day)
                    KK       = process_space_real(IPNT(20)) ! Permeability parameter (m/s)
                    KS       = process_space_real(IPNT(21)) ! Effective stress parameter (Pa)
                    DF       = process_space_real(IPNT(22)) ! Fractal dimension (-)
                    ISBEDS1  = NINT(process_space_real(IPNT(23)))
                    ISBEDS2  = NINT(process_space_real(IPNT(24)))
                
                    !***********************************************************************
                    !**** Calculations of pore water flux in sediment-water mixed layer
                    !***********************************************************************
                
                    ! Define the constants
                    P0 = 0.0
                    G  = 9.8
                    RHOS = 2600.0    ! sediment density (kg/m3)
                
                    NF = 2.0 / (3.0 - DF)
                
                    CMAX = 1400.0    ! maximum dry bed concentration (kg/m3)
                    CGEL = 150.0     ! gel concentration (kg/m3)
                
                    FEVA = 1.16E-07  ! MAXIMAL EVAPORATION FLUX (M/S)
                
                    ! Iniialize the outputs
                    PETL = 0.0
                    PEBL = 0.0
                    FTL  = 0.0
                    FBL  = 0.0
                
                    Z = ZTL + ZBL    ! Total material height
                    HBLMIN = RHOS * ZBL / CMAX
                    HBLMAX = RHOS * ZBL / CGEL
                    HTLMIN = RHOS * ZTL / CMAX
                    HTLMAX = RHOS * ZTL / CGEL
                    
                    ! Calculate solid fraction and water fraction
                    PHIS_TL = PHI_IM1S1 + PHI_IM2S1 + PHI_IM3S1
                    PHIS_BL = PHI_IM1S2 + PHI_IM2S2 + PHI_IM3S2
                    PHIW_TL = 1.0 - PHIS_TL
                    PHIW_BL = 1.0 - PHIS_BL
                    
                    PSUC = (-KS) * (CMAX / RHOS)**NF   ! PARALLEL CONSOLIDATION AND EVAPORATION TRUE/FALSE
                    
                    ! Porewater flux FTL is only computed when S1 is not empty 
                    IF (ISBEDS1 == 1) THEN
                
                        KTL   = KK * PHIS_TL**(-NF)
                        SEFTL = KS * PHIS_TL**NF
                        STTL  = ZTL * (RHOS - RHOW) * G  ! TOTAL EXCESS STRESS AT TOP LAYER (PA)
                        PETL  = STTL - SEFTL
                        
                        ! COMPUTE TOP LAYER WATER FLUX FTL (Both FTL and FBL should be non-negative)
                        FTL = MAX(0.0, MIN(PHIS_TL * (RHOS / RHOW - 1.0), 2.0 * (PETL - P0) / RHOW / G / HTL) * KTL / PHIW_TL) ! m/s
                        
                        ! FIRST TERM ABOVE IS MAXIMUM WATER FLUX IF EFFECTIVE STRESS = 0 (ACCORDING TO DARCY) SO FLUX LIMITED TO THIS NUMBER
                        IF (DEPTH .LT. MINDEP) THEN
                            IF (FTL < FEVA) THEN
                                P0 = PSUC
                                FTL = MIN(FEVA, MAX(0.0, 2.0 * ((PETL - P0) / HTL) * KTL / (RHOW * G * PHIW_TL)))
                            ENDIF
                            ! WATER FLUX IN M/S, FACTOR 2 AS PRESSURE GRADIENT OVER 0.5H
                        ENDIF
                
                    ENDIF
                
                    ! Porewater flux FBL is only computed when S2 is not empty 
                    IF (ISBEDS2 == 1) THEN
                        
                        KBL   = KK * PHIS_BL**(-NF)
                        SEFBL = KS * PHIS_BL**NF
                        STBL  = Z * (RHOS - RHOW) * G  ! TOTAL EXCESS STRESS AT BASE LAYER (PA)
                        PEBL  = STBL - SEFBL
                
                        ! COMPUTE BOTTOM LAYER WATER FLUX TO TOP LAYER FBL (Both FTL and FBL should be non-negative)
                        FBL = MAX(0.0, MIN(PHIS_BL * (RHOS / RHOW - 1.0), 2.0 * (PEBL - MAX(0.0, PETL) / RHOW / G / HBL)) * KBL / PHIW_BL) ! m/s
                        
                        IF (DEPTH .LT. MINDEP) THEN
                            IF (FBL < FEVA) THEN
                                FBL = MIN(FTL, MAX(0.0, 2.0 * ((PEBL - PETL) / (HBL + HTL)) * KBL / (RHOW * G * PHIW_BL)))
                            ENDIF
                        ENDIF
                        
                    ENDIF
                
                    ! WATER FLUX LIMITED TO 50% OF WATER AVAILABLE IN A LAYER PER TIME STEP (TO AVOID INSTABILITY FOR TOO LARGE TIME STEP)
                    IF (ISBEDS2 == 1) THEN
                        FBL = MAX(0.0, MIN(FBL, 0.5 * (HBL - (ZBL * RHOS / CMAX)) / (DELT * 86400.0)))
                    ENDIF
                
                    IF (ISBEDS1 == 1) THEN
                        IF (DEPTH .LT. MINDEP) THEN
                            FTL = MAX(0.0, MIN(FTL, FBL + 0.5 * (HTL - (ZTL * RHOS / CMAX)) / (DELT * 86400.0)))
                        ELSE
                            ! LINES BELOW TO AVOID SIGN CHANGE IN PE DUE TO EXCESSIVE DRAINAGE
                            ! THE MAXIMUM WATER FLOW SHOULD BE SUCH THAT PE BECOMES 0 (IF WET)
                            CP0 = RHOS * (ZTL * (RHOS - RHOW) * G / KS)**(1.0 / NF)
                            FTL = MAX(0.0, MIN(FTL, FBL + (HTL - (ZTL * RHOS / CP0)) / (DELT * 86400.0)))
                        ENDIF
                    ENDIF
                
                    ! Limit the porewater flux to the available water content
                    POWFL_S2 = MIN(MAX(0.0, HBL - HBLMIN) * PHIW_BL * RHOW * 1000.0 / DELT, FBL * RHOW * 1000.0 * 86400.0) ! g/m2/day
                    ! POWFL_S2 leaves S2 and enters S1
                    POWFL_S1 = MIN(MAX(0.0, HTL - HTLMIN) * PHIW_TL * RHOW * 1000.0 / DELT + POWFL_S2, FTL * RHOW * 1000.0 * 86400.0) ! g/m2/day
                
                    process_space_real(IPNT(25)) = POWFL_S1 ! g/m2/day
                    process_space_real(IPNT(26)) = POWFL_S2 ! g/m2/day
                    process_space_real(IPNT(27)) = PETL     ! Pa
                    process_space_real(IPNT(28)) = PEBL     ! Pa
                
                ENDIF
            ENDIF
!
            IFLUX = IFLUX + NOFLUX
            IPNT  = IPNT  + INCREM

        END DO
!
        RETURN
!
    END

end module m_calpowfl