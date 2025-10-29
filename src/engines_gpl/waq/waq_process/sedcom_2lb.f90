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
module m_sedcom_2lb
    use m_waq_precision

    implicit none

contains


    subroutine sedcom_2lb (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Composition, thickness, total dry mass and density in sediment layers
        !        (part 1 of the 2-layer bed model)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE ACTUAL THICKNESS OF THE LAYER BY SUMMING
        !        THE SUBSTANCES THAT CONTRIBUTE TO SEDIMENT VOLUME: IM1, IM2, IM3
        !        SECONDLY IT CALCULATES THE DRY-MATTER FRACTIONS, POROSITY, AND
        !        VOLUMETRIC CONCENTRATION FOR THESE SUBSTANCES
        !        THIRDLY IT CALCULATES THE TOTAL AMOUNT OF DRY MASS IN THE LAYER
        !        AND THE OVERALL DENSITY OF THE THE LAYER
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! MASS1   R*4 1 I  amount IM1 mass in layer                     [gDM/m2]
        ! MASS2   R*4 1 I  amount IM2 mass in layer                     [gDM/m2]
        ! MASS3   R*4 1 I  amount IM3 mass in layer                     [gDM/m2]
        ! RHO1    R*4 1 I  specific density of IM1                      [gDM/m3]
        ! RHO2    R*4 1 I  specific density of IM2                      [gDM/m3]
        ! RHO3    R*4 1 I  specific density of IM3                      [gDM/m3]
        ! WATM    R*4 1 I  mass of water in layer                         [g/m2]
        ! RHOWAT  R*4 1 I  density of water                              [kg/m3]
        ! SURF    R*4 1 I  horizontal surface area of a DELWAQ segment      [m2]
        ! SOMDM   R*4 1 O  sum of dry matter in layer                   [gDM/m2]
        ! DENS    R*4 1 O  overall bulk density layer                     [g/m3]
        ! THICK   R*4 1 O  actual thickness layer                            [m]
        ! SOMVOL  R*4 1 O  material thickness layer                          [m]
        ! WATVOL  R*4 1 O  water thickness layer                             [m]
        ! FRAC1   R*4 1 O  fraction IM1 in layer                       [gDM/gDM]
        ! FRAC2   R*4 1 O  fraction IM2 in layer                       [gDM/gDM]
        ! FRAC3   R*4 1 O  fraction IM3 in layer                       [gDM/gDM]
        ! PHIIM1  R*4 1 O  volumetric fraction IM1 in layer              [m3/m3]
        ! PHIIM2  R*4 1 O  volumetric fraction IM2 in layer              [m3/m3]
        ! PHIIM3  R*4 1 O  volumetric fraction IM3 in layer              [m3/m3]
        ! POR     R*4 1 O  porosity of sediment layer                        [-]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        LOGICAL  NO1OPT, NO2OPT, NO3OPT

        !        Substance 1 IM1
        IF (INCREM(1) == 0 .AND. INCREM(4) == 0) THEN
            MASS1 = MAX(0.0, process_space_real(IPOINT(1)))
            RHO1  = process_space_real(IPOINT(4))
            SOMV1 = MASS1 / RHO1
            NO1OPT = .TRUE.
        ELSE
            NO1OPT = .FALSE.
        ENDIF
        !        Substance 2  IM2
        IF (INCREM(2) == 0 .AND. INCREM(5) == 0) THEN
            MASS2 = MAX(0.0, process_space_real(IPOINT(2)))
            RHO2  = process_space_real(IPOINT(5))
            SOMV2 = MASS2 / RHO2
            NO2OPT = .TRUE.
        ELSE
            NO2OPT = .FALSE.
        ENDIF
        !        Substance 3 IM3
        IF (INCREM(3) == 0 .AND. INCREM(6) == 0) THEN
            MASS3 = MAX(0.0, process_space_real(IPOINT(3)))
            RHO3  = process_space_real(IPOINT(6))
            SOMV3 = MASS3 / RHO3
            NO3OPT = .TRUE.
        ELSE
            NO3OPT = .FALSE.
        ENDIF
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)
        IN11 = INCREM(11)
        IN12 = INCREM(12)
        IN13 = INCREM(13)
        IN14 = INCREM(14)
        IN15 = INCREM(15)
        IN16 = INCREM(16)
        IN17 = INCREM(17)
        IN18 = INCREM(18)
        IN19 = INCREM(19)
        IN20 = INCREM(20)
        IN21 = INCREM(21)
        IN22 = INCREM(22)
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        IP20 = IPOINT(20)
        IP21 = IPOINT(21)
        IP22 = IPOINT(22)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                
                    !        Substance 1
                    IF (.NOT. NO1OPT) THEN
                        MASS1 = MAX(0.0, process_space_real(IP1))
                        RHO1  = process_space_real(IP4)
                        SOMV1 = MASS1 / RHO1
                    ENDIF
                    !        Substance 2
                    IF (.NOT. NO2OPT) THEN
                        MASS2 = MAX(0.0, process_space_real(IP2))
                        RHO2  = process_space_real(IP5)
                        SOMV2 = MASS2 / RHO2
                    ENDIF
                    !        Substance 3
                    IF (.NOT. NO3OPT) THEN
                        MASS3 = MAX(0.0, process_space_real(IP3))
                        RHO3  = process_space_real(IP6)
                        SOMV3 = MASS3 / RHO3
                    ENDIF
                    !
                    WATM = MAX(0.0, process_space_real(IP7))
                    RHOW = process_space_real(IP8)
                    SURF = process_space_real(IP9)
                
                    !***********************************************************************
                    !**** Calculations connected to the status of the mixed layer
                    !***********************************************************************
                
                    !    Calculate som dry matter in mixed layer
                    SOMDM  = MASS1 + MASS2 + MASS3
                    !    Total thickness = material thickness + porewater thickness
                    SOMVOL = SOMV1 + SOMV2 + SOMV3
                    WATVOL = WATM / (RHOW * 1000.0)
                    THICK  = SOMVOL + WATVOL 
                
                    IF (SOMDM > 1.E-16 .AND. WATM > 1.E-16) THEN ! hence SOMVOL > 0 and THICK > 0
                        FRAC1  = MASS1 / SOMDM
                        FRAC2  = MASS2 / SOMDM
                        FRAC3  = MASS3 / SOMDM
                
                        POR    = WATVOL / THICK
                        DENS   = (SOMDM + WATM) / THICK ! consider the water content
                
                        PHIIM1 = SOMV1 / THICK ! volumetric fraction IM1
                        PHIIM2 = SOMV2 / THICK ! volumetric fraction IM2
                        PHIIM3 = SOMV3 / THICK ! volumetric fraction IM3
                        
                        ISBED = 1
                
                    ELSEIF (SOMDM > 1.E-16 .AND. WATM <= 1.E-16) THEN ! no water but still has sediment
                        FRAC1  = MASS1 / SOMDM
                        FRAC2  = MASS2 / SOMDM
                        FRAC3  = MASS3 / SOMDM
                
                        POR    = 0.0 ! no porewater, but in theory there should always be porewater
                        DENS   = SOMDM / SOMVOL ! consider the water content
                
                        PHIIM1 = SOMV1 / SOMVOL ! volumetric fraction IM1
                        PHIIM2 = SOMV2 / SOMVOL ! volumetric fraction IM2
                        PHIIM3 = SOMV3 / SOMVOL ! volumetric fraction IM3
                        
                        ISBED  = 2
                
                    ELSEIF (SOMDM <= 1.E-16 .AND. WATM > 1.E-16) THEN ! has water but no sediment
                        FRAC1  = -999.
                        FRAC2  = -999.
                        FRAC3  = -999.
                
                        POR    = 1.0 ! only porewater, but in theory there should always be sediment
                        DENS   = RHOW * 1000.0  ! from kg/m3 to g/m3
                
                        PHIIM1 = 0.0
                        PHIIM2 = 0.0
                        PHIIM3 = 0.0
                        
                        ISBED  = 3
                
                    ELSE ! empty layer, no water no sediment
                        FRAC1  = -999.
                        FRAC2  = -999.
                        FRAC3  = -999.
                
                        POR    = -999.
                        DENS   = -999.
                
                        PHIIM1 = -999.
                        PHIIM2 = -999.
                        PHIIM3 = -999.
                        
                        ISBED  = 0
                        
                    ENDIF
                
                    process_space_real(IP10) = SOMDM
                    process_space_real(IP11) = DENS
                    process_space_real(IP12) = THICK
                    process_space_real(IP13) = SOMVOL
                    process_space_real(IP14) = WATVOL
                    process_space_real(IP15) = FRAC1
                    process_space_real(IP16) = FRAC2
                    process_space_real(IP17) = FRAC3
                    process_space_real(IP18) = PHIIM1
                    process_space_real(IP19) = PHIIM2
                    process_space_real(IP20) = PHIIM3
                    process_space_real(IP21) = POR
                    process_space_real(IP22) = ISBED
                
                ENDIF
            ENDIF

            !
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
            IP20 = IP20 + IN20
            IP21 = IP21 + IN21
            IP22 = IP22 + IN22
            !
        ENDDO
        !
        RETURN
        !
    END

end module m_sedcom_2lb
