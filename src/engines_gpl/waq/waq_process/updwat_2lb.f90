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

module m_updwat_2lb
    use m_waq_precision

    implicit none

contains

    subroutine updwat_2lb (process_space_real, fl, ipoint, increm, num_cells, &
                          noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                          num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Compute water fluxes due to resuspension, deposition and burial
        !        (part 8 of the 2-layer bed model)
        
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE CHANGES IN WATER MASS IN BED LAYERS DUE TO
        !        EROSION, DEPOSITION AND BURIAL PROCESSES
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! DMS1       R*4 1 I  total amount of dry matter in layer S1           [gDM/m2]
        ! DMS2       R*4 1 I  total amount of dry matter in layer S2           [gDM/m2]
        ! WATS1      R*4 1 I  water content of sediment layer S1               [gDM/m2]
        ! WATS2      R*4 1 I  water content of sediment layer S2               [gDM/m2]
        ! FSEDIM1    R*4 1 I  sedimentation flux IM1 towards S1                [g/m2/d]
        ! FSEDIM2    R*4 1 I  sedimentation flux IM2 towards S1                [g/m2/d]
        ! FSEDIM3    R*4 1 I  sedimentation flux IM3 towards S1                [g/m2/d]
        ! FRESS1IM1  R*4 1 I  resuspension flux IM1 from layer S1              [g/m2/d]
        ! FRESS2IM1  R*4 1 I  resuspension flux IM1 from layer S2              [g/m2/d]
        ! FRESS1IM2  R*4 1 I  resuspension flux IM2 from layer S1              [g/m2/d]
        ! FRESS2IM2  R*4 1 I  resuspension flux IM2 from layer S2              [g/m2/d]
        ! FRESS1IM3  R*4 1 I  resuspension flux IM3 from layer S1              [g/m2/d]
        ! FRESS2IM3  R*4 1 I  resuspension flux IM3 from layer S2              [g/m2/d]
        ! FBURS1DM   R*4 1 I  total burial flux DM from layer S1               [gDM/m2/d]
        ! FBURS2DM   R*4 1 I  total burial flux DM from layer S2               [gDM/m2/d]
        ! RHOIM1     R*4 1 I  bulk density IM1                                 [gDM/m3]
        ! RHOIM2     R*4 1 I  bulk density IM2                                 [gDM/m3]
        ! RHOIM3     R*4 1 I  bulk density IM3                                 [gDM/m3]
        ! RHOW       R*4 1 I  density of water                                 [kg/m3]
        ! FPOWS1     R*4 1 I  porewater flux in layer S1                       [g/m2/day]
        ! FPOWS2     R*4 1 I  porewater flux in layer S2                       [g/m2/day]
        ! DELT       R*4 1 I  timestep for processes                           [d]
        ! DEPTH      R*4 1 I  depth of segment                                 [m]
        ! MINDEPTH   R*4 1 I  minimum waterdepth for sedimentation/resuspension[m]
        ! FWATS1_SED R*4 1 O  water flux due to sedimentation in S1            [g/m2/day]
        ! FWATS1_RES R*4 1 O  water flux leaving S1 due to erosion             [g/m2/day]
        ! FWATS2_BUR R*4 1 O  water flux due to burial in S2                   [g/m2/day]
        ! FWATS2_RES R*4 1 O  water flux leaving S2 due to erosion             [g/m2/day]
        ! FWATS1_IN  R*4 1 O  total water flux towards S1                      [g/m2/day]
        ! FWATS1_OUT R*4 1 O  total water flux leaving S1                      [g/m2/day]
        ! FWATS2_IN  R*4 1 O  total water flux towards S2                      [g/m2/day]
        ! FWATS2_OUT R*4 1 O  total water flux leaving S2                      [g/m2/day]

        !     Logical Units : -
        
        !     Modules called : -
        
        !     Name     Type   Library
        
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(39), INCREM(39), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, IPNT(39)
        REAL(kind = real_wp) :: IM1S1, IM2S1, IM3S1, IM1S2, IM2S2, IM3S2
!
        IPNT = IPOINT
!
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                
                    ! Total dry matter in S1 and S2
                    IM1S1     = MAX(0.0, process_space_real(IPNT(1 )))
                    IM2S1     = MAX(0.0, process_space_real(IPNT(2 )))
                    IM3S1     = MAX(0.0, process_space_real(IPNT(3 )))
                    IM1S2     = MAX(0.0, process_space_real(IPNT(4 )))
                    IM2S2     = MAX(0.0, process_space_real(IPNT(5 )))
                    IM3S2     = MAX(0.0, process_space_real(IPNT(6 )))
                    ! water content in S1 and S2
                    WATS1     = MAX(0.0, process_space_real(IPNT(7 )))
                    WATS2     = MAX(0.0, process_space_real(IPNT(8 )))
                    ! Sedimentation flux towards S1 ! (gDM/m2/day)
                    fSedIM1   = process_space_real(IPNT(9 ))
                    fSedIM2   = process_space_real(IPNT(10))
                    fSedIM3   = process_space_real(IPNT(11))
                    ! Resuspension flux from S1 ! (gDM/m2/day)
                    fResS1IM1 = process_space_real(IPNT(12))
                    fResS1IM2 = process_space_real(IPNT(13))
                    fResS1IM3 = process_space_real(IPNT(14))
                    ! Resuspension flux from S2 ! (gDM/m2/day)
                    fResS2IM1 = process_space_real(IPNT(15))
                    fResS2IM2 = process_space_real(IPNT(16))
                    fResS2IM3 = process_space_real(IPNT(17))
                    ! Burial flux from S1 to S2, from S2 to deeper (sink) ! (gDM/m2/day)
                    fBurS1IM1 = process_space_real(IPNT(18))
                    fBurS1IM2 = process_space_real(IPNT(19))
                    fBurS1IM3 = process_space_real(IPNT(20))
                
                    RHOIM1    = process_space_real(IPNT(21)) ! (gDM/m3)
                    RHOIM2    = process_space_real(IPNT(22)) ! (gDM/m3)
                    RHOIM3    = process_space_real(IPNT(23)) ! (gDM/m3)
                    RHOW      = process_space_real(IPNT(24)) ! (kg/m3)
                    DELT      = process_space_real(IPNT(25)) ! (day)
                    DEPTH     = process_space_real(IPNT(26))
                    MINDEP    = process_space_real(IPNT(27))
                    PORS1     = process_space_real(IPNT(28))
                    PORS2     = process_space_real(IPNT(29))
                    fPOWS1    = process_space_real(IPNT(30))
                    fPOWS2    = process_space_real(IPNT(31))
                    ISBEDS1   = NINT(process_space_real(IPNT(32)))
                    ISBEDS2   = NINT(process_space_real(IPNT(33)))
                
                    !***********************************************************************
                    !**** Calculations of tau_crit for erosion and erosion parameter
                    !***********************************************************************
                
                    CGEL = 150.0 * 1000.0  ! gel concentration (gDM/m3)
                    CMAX = 1400.0 * 1000.0 ! maximum dry bed concentration (gDM/m3)
                    RHOS = 2600.0 * 1000.0 ! sediment dry density (gDM/m3)
                
                    PORS0  = 1.0 - CGEL / RHOS ! Porosity of fluff layer
                    PORMAX = 1.0 - CMAX / RHOS ! Porosity of consolidated layer
                
                    ! Volume of sediment in bed
                    SOMVS1 = IM1S1 / RHOIM1 + IM2S1 / RHOIM2 + IM3S1 / RHOIM3
                    SOMVS2 = IM1S2 / RHOIM1 + IM2S2 / RHOIM2 + IM3S2 / RHOIM3
                    
                    ! Total deposition, resuspension
                    fvSedS1 = fSedIM1   / RHOIM1 + fSedIM2   / RHOIM2 + fSedIM3   / RHOIM3 ! unit - m/day
                    fvResS1 = fResS1IM1 / RHOIM1 + fResS1IM2 / RHOIM2 + fResS1IM3 / RHOIM3 ! unit - m/day
                    fvResS2 = fResS2IM1 / RHOIM1 + fResS2IM2 / RHOIM2 + fResS2IM3 / RHOIM3 ! unit - m/day
                    fvBurS1 = fBurS1IM1 / RHOIM1 + fBurS1IM2 / RHOIM2 + fBurS1IM3 / RHOIM3 ! unit - m/day
                
                    fWATS1_Sed = 0.0 ! add mass to WATS1
                    fWATS1_Bur = 0.0 ! add mass to WATS2, remove the same amount from WATS1
                    fWATS1_Res = 0.0 ! remove mass from WATS1
                    fWATS2_Res = 0.0 ! remove mass from WATS2
                    
                    ! compute the water flux due to resuspension from S1
                    IF (ISBEDS1 == 1) THEN
                        fWATS1_Res = fvResS1 / (1.0 - PORS1) * PORS1 * RHOW * 1000.0
                    ENDIF                    
                    ! compute the water flux due to resuspension from S2
                    IF (ISBEDS2 == 1) THEN
                        fWATS2_Res = fvResS2 / (1.0 - PORS2) * PORS2 * RHOW * 1000.0
                    ENDIF
                    ! compute the water flux due to sedimentation towards S1
                    IF (PORS0 > 0.0 .AND. PORS0 < 1.0) THEN
                        fWATS1_Sed = fvSedS1 / (1.0 - PORS0) * PORS0 * RHOW * 1000.0
                    ENDIF
                    ! compute the water flux due to burial from S1 towards S2
                    IF (ISBEDS1 == 1) THEN
                        fWATS1_Bur = fvBurS1 / (1.0 - PORS1) * PORS1 * RHOW * 1000.0
                    ENDIF
                
                    ! Compute the max and min of water mass
                    WATS1_MAX = SOMVS1 / (1.0 - PORS0) * PORS0 * RHOW * 1000.0
                    WATS1_MIN = SOMVS1 / (1.0 - PORMAX) * PORMAX * RHOW * 1000.0
                    WATS2_MAX = SOMVS2 / (1.0 - PORS0) * PORS0 * RHOW * 1000.0
                    WATS2_MIN = SOMVS2 / (1.0 - PORMAX) * PORMAX * RHOW * 1000.0
                
                    ! control the water mass balance in layer S1
                    fWATS1_In  = fWATS1_Sed + fPOWS2
                    fWATS1_Out = fWATS1_Res + fWATS1_Bur + fPOWS1
                    
                    IF (fWATS1_In > fWATS1_Out) THEN ! positive is influx
                        fWATS1 = MIN(fWATS1_In - fWATS1_Out, MAX(0.0, WATS1_MAX - WATS1) / DELT)
                    ELSE
                        fWATS1 = MAX(fWATS1_In - fWATS1_Out, MIN(0.0, WATS1_MIN - WATS1) / DELT)
                    ENDIF
                
                    ! control the water mass balance in layer S2
                    fWATS2_In  = fWATS1_Bur
                    fWATS2_Out = fWATS2_Res + fPOWS2
                    
                    IF (fWATS2_In > fWATS2_Out) THEN ! positive is influx
                        fWATS2 = MIN(fWATS2_In - fWATS2_Out, MAX(0.0, WATS2_MAX - WATS2) / DELT)
                    ELSE
                        fWATS2 = MAX(fWATS2_In - fWATS2_Out, MIN(0.0, WATS2_MIN - WATS2) / DELT)
                    ENDIF
                    
                    ! when sediment is gone, water should also be gone
                    DMS1 = IM1S1 + IM2S1 + IM3S1
                    DMS2 = IM1S2 + IM2S2 + IM3S2
                    fSedS1 = fSedIM1 + fSedIM2 + fSedIM3
                    fResS1 = fResS1IM1 + fResS1IM2 + fResS1IM3
                    fResS2 = fResS2IM1 + fResS2IM2 + fResS2IM3
                    fBurS1 = fBurS1IM1 + fBurS1IM2 + fBurS1IM3
                    
                    IF (DMS1 + (fSedS1 - fResS1 - fBurS1) * DELT <= 1.E-16) THEN
                        fWATS1 = - WATS1 / DELT ! negative is outflux
                    ENDIF
                    
                    IF (DMS2 + (fBurS1 - fResS2) * DELT <= 1.E-16) THEN
                        fWATS2 = - WATS2 / DELT ! negative is outflux
                    ENDIF
                    
                    ! convert to fluxes
                    process_space_real(IPNT(34)) = fWATS1_Sed ! g/m2/day
                    process_space_real(IPNT(35)) = fWATS1_Res ! g/m2/day
                    process_space_real(IPNT(36)) = fWATS1_Bur ! g/m2/day
                    process_space_real(IPNT(37)) = fWATS2_Res ! g/m2/day
                
                    process_space_real(IPNT(38)) = fWATS1 ! g/m2/day
                    process_space_real(IPNT(39)) = fWATS2 ! g/m2/day
                
                    FL(1 + IFLUX) = fWATS1 / DEPTH ! g/m3/day
                    FL(2 + IFLUX) = fWATS2 / DEPTH ! g/m3/day
                
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

end module m_updwat_2lb