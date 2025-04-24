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

module m_caltaucm
    use m_waq_precision

    implicit none

contains

    subroutine caltaucm ( process_space_real, fl, ipoint, increm, num_cells, &
                          noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                          num_exchanges_z_dir, num_exchanges_bottom_dir )
        use m_extract_waq_attribute

        !>\file
        !>       Compute critical shear stress for erosion and erosion parameter 
        !>       in sediment layers (part 2 of the 2-layer bed model)
        
        !
        !     Description of the module :
        !        CALCULATES THE TIME AND SPACE DEPENDENT CRITICAL SHEAR STRESS FOR
        !        EROSION AND EROSION PARAMETER BASED ON BED PROPERTIES
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! PHI_IM1S1 R*4 1 I  volumetric fraction IM1 in layer S1        [m3/m3]
        ! PHI_IM2S1 R*4 1 I  volumetric fraction IM2 in layer S1        [m3/m3]
        ! PHI_IM3S1 R*4 1 I  volumetric fraction IM3 in layer S1        [m3/m3]
        ! PHI_IM1S2 R*4 1 I  volumetric fraction IM1 in layer S2        [m3/m3]
        ! PHI_IM2S2 R*4 1 I  volumetric fraction IM2 in layer S2        [m3/m3]
        ! PHI_IM3S2 R*4 1 I  volumetric fraction IM3 in layer S2        [m3/m3]
        ! RHOIM1    R*4 1 I  specific density of IM1                    [gDM/m3]
        ! RHOIM2    R*4 1 I  specific density of IM2                    [gDM/m3]
        ! RHOIM3    R*4 1 I  specific density of IM3                    [gDM/m3]
        ! RHOWAT    R*4 1 I  density of water                           [kg/m3]
        ! HTL       R*4 1 I  actual thickness layer S1                  [m]
        ! HBL       R*4 1 I  actual thickness layer S2                  [m]
        ! ZTL       R*4 1 I  material thickness layer S1                [m]
        ! ZBL       R*4 1 I  material thickness layer S2                [m]
        ! GRAV      R*4 1 I  Gravitational acceleration                 [m/s2]
        ! MGEL      R*4 1 I  Erosion parameter at gel concentration     [gDM/m2/day]
        ! TCTL      R*4 1 O  critical shear stress for erosion in layer S1 [m/s2]
        ! MTL       R*4 1 O  erosion parameter of layer S1              [gDM/m2/day]
        ! TCBL      R*4 1 O  critical shear stress for erosion in layer S2 [m/s2]
        ! MBL       R*4 1 O  erosion parameter of layer S2              [gDM/m2/day]

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
                
                    !        Substance 1
                    PHI_IM1S1 = process_space_real(IPNT(1 ))
                    PHI_IM1S2 = process_space_real(IPNT(4 ))
                    !        Substance 2
                    PHI_IM2S1 = process_space_real(IPNT(2 ))
                    PHI_IM2S2 = process_space_real(IPNT(5 ))
                    !        Substance 3
                    PHI_IM3S1 = process_space_real(IPNT(3 ))
                    PHI_IM3S2 = process_space_real(IPNT(6 ))
                
                    RHOIM1   = process_space_real(IPNT(7 )) / 1.0E+03 ! (kg/m3)
                    RHOIM2   = process_space_real(IPNT(8 )) / 1.0E+03 ! (kg/m3)
                    RHOIM3   = process_space_real(IPNT(9 )) / 1.0E+03 ! (kg/m3)
                    RHOW     = process_space_real(IPNT(10))
                    HTL      = process_space_real(IPNT(11))
                    HBL      = process_space_real(IPNT(12))
                    ZTL      = process_space_real(IPNT(13))
                    ZBL      = process_space_real(IPNT(14))
                    GRAV     = process_space_real(IPNT(15))
                    MGEL     = process_space_real(IPNT(16)) / 1.0E+03 / 86400.0 ! EROSION PARAMETER OF GEL (KG/M2/S)
                    TCGEL    = process_space_real(IPNT(17)) ! CRITICAL SHEAR STRESS FOR EROSION OF GEL (PA)
                    TCCSL    = process_space_real(IPNT(18)) ! CRITICAL SHEAR STRESS FOR EROSION AT CRITICAL STATE	(PA)
                    KK       = process_space_real(IPNT(19)) ! Permeability parameter (m/s)
                    KS       = process_space_real(IPNT(20)) ! Effective stress parameter (Pa)
                    DF       = process_space_real(IPNT(21)) ! Fractal dimension (-)
                    D50F     = process_space_real(IPNT(22)) ! MEDIAN FLOC DIAMETER (M)
                    ISBEDS1  = NINT(process_space_real(IPNT(23)))
                    ISBEDS2  = NINT(process_space_real(IPNT(24)))
                
                    !***********************************************************************
                    !**** Calculations of tau_crit for erosion and erosion parameter
                    !***********************************************************************
                
                    CMAX = 1400.0    ! maximum dry bed concentration (kg/m3)
                    CGEL = 150.0     ! GEL CONCENTRATION (KG/M3)
                    NF = 2.0 / (3.0 - DF)
                    CCSL = 800.0     ! CRITICAL STATE CONCENTRATION (KG/M3)
                    PM = 1.0         ! mud fraction in seabed (-)
                    
                    PHIS_TL = PHI_IM1S1 + PHI_IM2S1 + PHI_IM3S1
                    PHIS_BL = PHI_IM1S2 + PHI_IM2S2 + PHI_IM3S2
                    
                    TCTL = TCGEL
                    MTL  = MGEL
                    
                    TCBL = TCGEL
                    MBL  = MGEL
                    
                    IF (ISBEDS1 == 1) THEN 
                        ! DEAL WITH THE TOP LAYER S1
                        PHIW_TL = 1.0 - PHIS_TL
                        RHOS_TL = (PHI_IM1S1*RHOIM1 + PHI_IM2S1*RHOIM2 + PHI_IM3S1*RHOIM3) / PHIS_TL
                        PHICSTL = CCSL / RHOS_TL
                        SEFCSTL = KS * PHICSTL**NF
                        CVCSTL  = NF * KS * KK / (RHOW * GRAV * (1.0-PHICSTL))
                        MCSTL   = MIN(MGEL, CVCSTL * PHICSTL * RHOS_TL * TCCSL / (10.0 * D50F * SEFCSTL))
                        PHISTL  = MIN(CMAX / RHOS_TL, MAX(CGEL / RHOS_TL, ZTL / HTL))
                        CTL     = MAX(CGEL, MIN(CMAX, ZTL * RHOS_TL / HTL))
                        KTL     = KK * PHISTL**(-NF)
                        SEFTL   = KS * PHISTL**NF
                        CVTL    = NF * KS * KK / (RHOW * GRAV * (1.0-PHISTL))
                        TCTL    = MIN(TCCSL, TCGEL + (TCCSL-TCGEL) * ((CTL-CGEL) / (CCSL-CGEL))**NF)
                        
                        IF (PHISTL > (CCSL / RHOS_TL)) THEN
                            MTL = MIN(MCSTL, CVTL * PHISTL * RHOS_TL * TCTL / (10.0 * D50F * SEFTL))
                        ELSE
                            MTL = MIN(MGEL, MGEL + (MCSTL-MGEL) * ((CTL-CGEL) / (CCSL-CGEL))**NF)
                        ENDIF
                    ENDIF
                    
                    IF (ISBEDS2 == 1) THEN 
                        ! DEAL WITH THE BOTTOM LAYER S2
                        PHIW_BL = 1.0 - PHIS_BL
                        RHOS_BL = (PHI_IM1S2*RHOIM1 + PHI_IM2S2*RHOIM2 + PHI_IM3S2*RHOIM3) / PHIS_BL
                        PHICSBL = CCSL / RHOS_BL
                        SEFCSBL = KS * PHICSBL**NF
                        CVCSBL = NF * KS * KK / (RHOW * GRAV * (1.0-PHICSBL))
                        MCSBL  = MIN(MGEL, CVCSBL * PHICSBL * RHOS_BL * TCCSL / (10.0 * D50F * SEFCSBL))
                        PHISBL = MIN(CMAX / RHOS_BL, MAX(CGEL / RHOS_BL, ZBL / HBL))
                        CBL    = MAX(CGEL, MIN(CMAX, ZBL * RHOS_BL / HBL))
                        KBL    = KK * PHISBL**(-NF)
                        SEFBL  = KS * PHISBL**NF
                        CVBL   = NF * KS * KK / (RHOW * GRAV * (1.0-PHISBL))
                        TCBL   = MIN(TCCSL, TCGEL + (TCCSL-TCGEL) * ((CBL-CGEL) / (CCSL-CGEL))**NF)
                        
                        IF (PHISBL > (CCSL/RHOS_BL)) THEN
                            MBL = MIN(MCSBL, PM * CVBL * PHISBL * RHOS_BL * TCBL / (10.0 * D50F * SEFBL))
                        ELSE
                            MBL = MIN(MGEL, MGEL + (MCSBL-MGEL) * ((CBL-CGEL) / (CCSL-CGEL))**NF)
                        ENDIF
                    ENDIF
                
                    process_space_real(IPNT(25)) = TCTL
                    process_space_real(IPNT(26)) = MTL * 1.0E+03 * 86400.0 ! from (KG/M2/S) to (G/M2/DAY)
                    process_space_real(IPNT(27)) = TCBL
                    process_space_real(IPNT(28)) = MBL * 1.0E+03 * 86400.0 ! from (KG/M2/S) to (G/M2/DAY)
                
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

end module m_caltaucm