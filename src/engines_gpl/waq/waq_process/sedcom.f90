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
module m_sedcom
    use m_waq_precision

    implicit none

contains


    subroutine sedcom (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Composition, thickness, total dry mass and density in sediment layers

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE ACTUAL THICKNESS OF THE LAYER BY SUMMING
        !        THE SUBSTANCES THAT CONTRIBUTE TO SEDIMENT VOLUME: IM, IM2, IM3
        !        DETC, OOC, DIATOMS, GREEN ALGAE and BLUE algae
        !        SECONDLY IT CALCULATES THE DRY-MATTER FRACTIONS FOR THESE
        !        SUBSTANCES
        !        THIRDLY IT CALCULATES THE TOTAL AMOUNT OF DRY MASS IN THE LAYER
        !        AND THE OVERALL DENSITY OF THE THE LAYER
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                            ---
        ! THICK   R*4 1 O  actual thickness of the mixed layer               [m]
        ! DEPTH   R*4 1 I  depth water column                                [m]
        ! DMCFy   R*4 1 I  conversion factor for gX->dry matter substy  [gDM/gX]
        ! FRACy   R*4 1 O  fraction of substance_y in layr              [gX/gDM]
        ! MASSy   R*4 1 I  amount substance y in layer                      [gX/m2]
        ! RHOy    R*4 1 I  bulk density of substance y (per dry matter) [gDM/m3]
        ! SOMDM   R*4 1 I  sum of dry matter in layer                      [gDM/m2]
        ! SOMVOL  R*4 1 I  sum of volume in layer                            [m]
        ! VOLUME  R*4 1 I  volume computed by DELWAQ                         [m]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        LOGICAL  NO1OPT, NO2OPT, NO3OPT, NO4OPT, NO5OPT, NO6OPT, &
                NO7OPT, NO8OPT, NO9OPT

        !        Substance 1 IM1
        IF (INCREM(1) == 0 .AND. INCREM(9) == 0 .AND. &
                INCREM(16) == 0) THEN
            MASS1 = process_space_real(IPOINT(1))
            DMCF1 = process_space_real(IPOINT(9))
            RHO1 = process_space_real(IPOINT(16))
            SOM1 = MASS1 * DMCF1
            SOMV1 = SOM1 / RHO1
            NO1OPT = .TRUE.
        ELSE
            NO1OPT = .FALSE.
        ENDIF
        !        Substance 2  IM2
        IF (INCREM(2) == 0 .AND. INCREM(10) == 0 .AND. &
                INCREM(17) == 0) THEN
            MASS2 = process_space_real(IPOINT(2))
            DMCF2 = process_space_real(IPOINT(10))
            RHO2 = process_space_real(IPOINT(17))
            SOM2 = MASS2 * DMCF2
            SOMV2 = SOM2 / RHO2
            NO2OPT = .TRUE.
        ELSE
            NO2OPT = .FALSE.
        ENDIF
        !        Substance 3 IM3
        IF (INCREM(3) == 0 .AND. INCREM(11) == 0 .AND. &
                INCREM(18) == 0) THEN
            MASS3 = process_space_real(IPOINT(3))
            DMCF3 = process_space_real(IPOINT(11))
            RHO3 = process_space_real(IPOINT(18))
            SOM3 = MASS3 * DMCF3
            SOMV3 = SOM3 / RHO3
            NO3OPT = .TRUE.
        ELSE
            NO3OPT = .FALSE.
        ENDIF
        !        Substance 4 DetC
        IF (INCREM(4) == 0 .AND. INCREM(12) == 0 .AND. &
                INCREM(19) == 0 .AND. INCREM(25) == 0 .AND. &
                INCREM(26) == 0 .AND. INCREM(27) == 0) THEN
            MASS4 = process_space_real(IPOINT(4))
            DMCF4 = process_space_real(IPOINT(12))
            RHO4 = process_space_real(IPOINT(19))
            SOM4 = MASS4 * DMCF4
            SOMV4 = SOM4 / RHO4
            NMASS4 = process_space_real(IPOINT(25))
            PMASS4 = process_space_real(IPOINT(26))
            SMASS4 = process_space_real(IPOINT(27))
            CN4 = -999.
            CP4 = -999.
            CS4 = -999.
            IF (MASS4 >= 1E-10) THEN
                CN4 = NMASS4 / MASS4
                CP4 = PMASS4 / MASS4
                CS4 = SMASS4 / MASS4
            ENDIF
            NO4OPT = .TRUE.
        ELSE
            NO4OPT = .FALSE.
        ENDIF
        !        Substance 5 OOC
        IF (INCREM(5) == 0 .AND. INCREM(13) == 0 .AND. &
                INCREM(20) == 0 .AND. INCREM(28) == 0 .AND. &
                INCREM(29) == 0 .AND. INCREM(30) == 0) THEN

            MASS5 = process_space_real(IPOINT(5))
            DMCF5 = process_space_real(IPOINT(13))
            RHO5 = process_space_real(IPOINT(20))
            SOM5 = MASS5 * DMCF5
            SOMV5 = SOM5 / RHO5
            NMASS5 = process_space_real(IPOINT(28))
            PMASS5 = process_space_real(IPOINT(29))
            SMASS5 = process_space_real(IPOINT(30))
            CN5 = -999.
            CP5 = -999.
            CS5 = -999.
            IF (MASS5 >= 1E-10) THEN
                CN5 = NMASS5 / MASS5
                CP5 = PMASS5 / MASS5
                CS5 = SMASS5 / MASS5
            ENDIF
            NO5OPT = .TRUE.
        ELSE
            NO5OPT = .FALSE.
        ENDIF
        !        Substance 6 Diat
        IF (INCREM(6) == 0 .AND. INCREM(14) == 0 .AND. &
                INCREM(21) == 0) THEN
            MASS6 = process_space_real(IPOINT(6))
            DMCF6 = process_space_real(IPOINT(14))
            RHO6 = process_space_real(IPOINT(21))
            SOM6 = MASS6 * DMCF6
            SOMV6 = SOM6 / RHO6
            NO6OPT = .TRUE.
        ELSE
            NO6OPT = .FALSE.
        ENDIF
        !        Substance 7 Green
        IF (INCREM(7) == 0 .AND. INCREM(15) == 0 .AND. &
                INCREM(22) == 0) THEN
            MASS7 = process_space_real(IPOINT(7))
            DMCF7 = process_space_real(IPOINT(15))
            RHO7 = process_space_real(IPOINT(22))
            SOM7 = MASS7 * DMCF7
            SOMV7 = SOM7 / RHO7
            NO7OPT = .TRUE.
        ELSE
            NO7OPT = .FALSE.
        ENDIF
        !        Substance 8 MFB1
        IF (INCREM(31) == 0 .AND. INCREM(33) == 0 .AND. &
                INCREM(35) == 0) THEN
            MASS8 = process_space_real(IPOINT(31))
            DMCF8 = process_space_real(IPOINT(33))
            RHO8 = process_space_real(IPOINT(35))
            SOM8 = MASS8 * DMCF8
            SOMV8 = SOM8 / RHO8
            NO8OPT = .TRUE.
        ELSE
            NO8OPT = .FALSE.
        ENDIF
        !        Substance 9 MFB2
        IF (INCREM(32) == 0 .AND. INCREM(34) == 0 .AND. &
                INCREM(36) == 0) THEN
            MASS9 = process_space_real(IPOINT(32))
            DMCF9 = process_space_real(IPOINT(34))
            RHO9 = process_space_real(IPOINT(36))
            SOM9 = MASS9 * DMCF9
            SOMV9 = SOM9 / RHO9
            NO9OPT = .TRUE.
        ELSE
            NO9OPT = .FALSE.
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
        IN23 = INCREM(23)
        IN24 = INCREM(24)
        IN25 = INCREM(25)
        IN26 = INCREM(26)
        IN27 = INCREM(27)
        IN28 = INCREM(28)
        IN29 = INCREM(29)
        IN30 = INCREM(30)
        IN31 = INCREM(31)
        IN32 = INCREM(32)
        IN33 = INCREM(33)
        IN34 = INCREM(34)
        IN35 = INCREM(35)
        IN36 = INCREM(36)
        IN37 = INCREM(37)
        IN38 = INCREM(38)
        IN39 = INCREM(39)
        IN40 = INCREM(40)
        IN41 = INCREM(41)
        IN42 = INCREM(42)
        IN43 = INCREM(43)
        IN44 = INCREM(44)
        IN45 = INCREM(45)
        IN46 = INCREM(46)
        IN47 = INCREM(47)
        IN48 = INCREM(48)
        IN49 = INCREM(49)
        IN50 = INCREM(50)
        IN51 = INCREM(51)
        IN52 = INCREM(52)
        IN53 = INCREM(53)
        IN54 = INCREM(54)
        IN55 = INCREM(55)
        IN56 = INCREM(56)
        IN57 = INCREM(57)
        IN58 = INCREM(58)
        IN59 = INCREM(59)
        IN60 = INCREM(60)
        IN61 = INCREM(61)
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
        IP23 = IPOINT(23)
        IP24 = IPOINT(24)
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)
        IP28 = IPOINT(28)
        IP29 = IPOINT(29)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)
        IP33 = IPOINT(33)
        IP34 = IPOINT(34)
        IP35 = IPOINT(35)
        IP36 = IPOINT(36)
        IP37 = IPOINT(37)
        IP38 = IPOINT(38)
        IP39 = IPOINT(39)
        IP40 = IPOINT(40)
        IP41 = IPOINT(41)
        IP42 = IPOINT(42)
        IP43 = IPOINT(43)
        IP44 = IPOINT(44)
        IP45 = IPOINT(45)
        IP46 = IPOINT(46)
        IP47 = IPOINT(47)
        IP48 = IPOINT(48)
        IP49 = IPOINT(49)
        IP50 = IPOINT(50)
        IP51 = IPOINT(51)
        IP52 = IPOINT(52)
        IP53 = IPOINT(53)
        IP54 = IPOINT(54)
        IP55 = IPOINT(55)
        IP56 = IPOINT(56)
        IP57 = IPOINT(57)
        IP58 = IPOINT(58)
        IP59 = IPOINT(59)
        IP60 = IPOINT(60)
        IP61 = IPOINT(61)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN

                    !        Substance 1
                    IF (.NOT. NO1OPT) THEN
                        MASS1 = process_space_real(IP1)
                        DMCF1 = process_space_real(IP9)
                        RHO1 = process_space_real(IP16)
                        SOM1 = MASS1 * DMCF1
                        SOMV1 = SOM1 / RHO1
                    ENDIF
                    !        Substance 2
                    IF (.NOT. NO2OPT) THEN
                        MASS2 = process_space_real(IP2)
                        DMCF2 = process_space_real(IP10)
                        RHO2 = process_space_real(IP17)
                        SOM2 = MASS2 * DMCF2
                        SOMV2 = SOM2 / RHO2
                    ENDIF
                    !        Substance 3
                    IF (.NOT. NO3OPT) THEN
                        MASS3 = process_space_real(IP3)
                        DMCF3 = process_space_real(IP11)
                        RHO3 = process_space_real(IP18)
                        SOM3 = MASS3 * DMCF3
                        SOMV3 = SOM3 / RHO3
                    ENDIF
                    !        Substance 4
                    IF (.NOT. NO4OPT) THEN
                        MASS4 = process_space_real(IP4)
                        DMCF4 = process_space_real(IP12)
                        RHO4 = process_space_real(IP19)
                        SOM4 = MASS4 * DMCF4
                        SOMV4 = SOM4 / RHO4
                        NMASS4 = process_space_real(IP25)
                        PMASS4 = process_space_real(IP26)
                        SMASS4 = process_space_real(IP27)
                        CN4 = 0.0
                        CP4 = 0.0
                        CS4 = 0.0
                        IF (MASS4 >= 1E-10) THEN
                            CN4 = NMASS4 / MASS4
                            CP4 = PMASS4 / MASS4
                            CS4 = SMASS4 / MASS4
                        ENDIF
                    ENDIF
                    !        Substance 5
                    IF (.NOT. NO5OPT) THEN
                        MASS5 = process_space_real(IP5)
                        DMCF5 = process_space_real(IP13)
                        RHO5 = process_space_real(IP20)
                        SOM5 = MASS5 * DMCF5
                        SOMV5 = SOM5 / RHO5
                        NMASS5 = process_space_real(IP28)
                        PMASS5 = process_space_real(IP29)
                        SMASS5 = process_space_real(IP30)
                        CN5 = 0.0
                        CP5 = 0.0
                        CS5 = 0.0
                        IF (MASS5 >= 1E-10) THEN
                            CN5 = NMASS5 / MASS5
                            CP5 = PMASS5 / MASS5
                            CS5 = SMASS5 / MASS5
                        ENDIF
                    ENDIF
                    !        Substance 6
                    IF (.NOT. NO6OPT) THEN
                        MASS6 = process_space_real(IP6)
                        DMCF6 = process_space_real(IP14)
                        RHO6 = process_space_real(IP21)
                        SOM6 = MASS6 * DMCF6
                        SOMV6 = SOM6 / RHO6
                    ENDIF
                    !        Substance 7
                    IF (.NOT. NO7OPT) THEN
                        MASS7 = process_space_real(IP7)
                        DMCF7 = process_space_real(IP15)
                        RHO7 = process_space_real(IP22)
                        SOM7 = MASS7 * DMCF7
                        SOMV7 = SOM7 / RHO7
                    ENDIF
                    !        Substance 8
                    IF (.NOT. NO8OPT) THEN
                        MASS8 = process_space_real(IP31)
                        DMCF8 = process_space_real(IP33)
                        RHO8 = process_space_real(IP35)
                        SOM8 = MASS8 * DMCF8
                        SOMV8 = SOM8 / RHO8
                    ENDIF
                    !        Substance 9
                    IF (.NOT. NO9OPT) THEN
                        MASS9 = process_space_real(IP32)
                        DMCF9 = process_space_real(IP34)
                        RHO9 = process_space_real(IP36)
                        SOM9 = MASS9 * DMCF9
                        SOMV9 = SOM9 / RHO9
                    ENDIF
                    !
                    MASS10 = process_space_real(IP8)
                    POR = process_space_real(IP23)
                    SURF = process_space_real(IP24)

                    !***********************************************************************
                    !**** Calculations connected to the status of the mixed layer
                    !***********************************************************************

                    !    Calculate som dry matter in mixed layer
                    SOMDM = SOM1 + SOM2 + SOM3 + SOM4 + SOM5 + SOM6 + SOM7 &
                            + SOM8 + SOM9
                    TIM = MASS1 + MASS2 + MASS3
                    TPOC = MASS4 + MASS5
                    PHYT = MASS6 + MASS7 + MASS8 + MASS9

                    IF (SOMDM > 1.0E-20) THEN
                        FRAC1 = MASS1 / SOMDM
                        FRAC2 = MASS2 / SOMDM
                        FRAC3 = MASS3 / SOMDM
                        FRAC4 = MASS4 / SOMDM
                        FRAC5 = MASS5 / SOMDM
                        FRAC6 = MASS6 / SOMDM
                        FRAC7 = MASS7 / SOMDM
                        FRAC8 = MASS8 / SOMDM
                        FRAC9 = MASS9 / SOMDM

                        POM = (MASS4 * DMCF4 + &
                                MASS5 * DMCF5 + &
                                MASS6 * DMCF6 + &
                                MASS7 * DMCF7 + &
                                MASS8 * DMCF8 + &
                                MASS9 * DMCF9) / SOMDM

                        SOMVOL = SOMV1 + SOMV2 + SOMV3 + SOMV4 + SOMV5 + SOMV6 + SOMV7 &
                                + SOMV8 + SOMV9
                        THICK = SOMVOL / (1.0 - POR)
                        DENS = SOMDM / SOMVOL
                    ELSE
                        FRAC1 = -999.
                        FRAC2 = -999.
                        FRAC3 = -999.
                        FRAC4 = -999.
                        FRAC5 = -999.
                        FRAC6 = -999.
                        FRAC7 = -999.
                        FRAC8 = -999.
                        FRAC9 = -999.
                        POM = -999.
                        SOMVOL = 0.0
                        THICK = 0.0
                        DENS = -999.
                    ENDIF

                    FRAC10 = 0.0
                    IF (SOMDM > 1.0E-20) FRAC10 = MASS10 / SOMDM

                    process_space_real(IP37) = SOMDM
                    process_space_real(IP38) = TIM
                    process_space_real(IP39) = TPOC
                    process_space_real(IP40) = PHYT
                    process_space_real(IP41) = POM
                    process_space_real(IP42) = DENS
                    process_space_real(IP43) = THICK
                    process_space_real(IP44) = FRAC1
                    process_space_real(IP45) = FRAC2
                    process_space_real(IP46) = FRAC3
                    process_space_real(IP47) = FRAC4
                    process_space_real(IP48) = FRAC5
                    process_space_real(IP49) = FRAC6
                    process_space_real(IP50) = FRAC7
                    process_space_real(IP51) = CN4
                    process_space_real(IP52) = CP4
                    process_space_real(IP53) = CS4
                    process_space_real(IP54) = CN5
                    process_space_real(IP55) = CP5
                    process_space_real(IP56) = CS5
                    process_space_real(IP57) = FRAC10
                    process_space_real(IP58) = FRAC8
                    process_space_real(IP59) = FRAC9
                    process_space_real(IP60) = MASS8 / SURF
                    process_space_real(IP61) = MASS9 / SURF

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
            IP23 = IP23 + IN23
            IP24 = IP24 + IN24
            IP25 = IP25 + IN25
            IP26 = IP26 + IN26
            IP27 = IP27 + IN27
            IP28 = IP28 + IN28
            IP29 = IP29 + IN29
            IP30 = IP30 + IN30
            IP31 = IP31 + IN31
            IP32 = IP32 + IN32
            IP33 = IP33 + IN33
            IP34 = IP34 + IN34
            IP35 = IP35 + IN35
            IP36 = IP36 + IN36
            IP37 = IP37 + IN37
            IP38 = IP38 + IN38
            IP39 = IP39 + IN39
            IP40 = IP40 + IN40
            IP41 = IP41 + IN41
            IP42 = IP42 + IN42
            IP43 = IP43 + IN43
            IP44 = IP44 + IN44
            IP45 = IP45 + IN45
            IP46 = IP46 + IN46
            IP47 = IP47 + IN47
            IP48 = IP48 + IN48
            IP49 = IP49 + IN49
            IP50 = IP50 + IN50
            IP51 = IP51 + IN51
            IP52 = IP52 + IN52
            IP53 = IP53 + IN53
            IP54 = IP54 + IN54
            IP55 = IP55 + IN55
            IP56 = IP56 + IN56
            IP57 = IP57 + IN57
            IP58 = IP58 + IN58
            IP59 = IP59 + IN59
            IP60 = IP60 + IN60
            IP61 = IP61 + IN61
            !
        end do
        !
        RETURN
        !
    END

end module m_sedcom
