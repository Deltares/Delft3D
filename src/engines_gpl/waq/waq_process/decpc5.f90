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
module m_decpc5
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DECPC5 (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_logger_helper, only : write_error_message
        use m_evaluate_waq_attribute


        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Mineralization and conversion of fast decomposing detritus
        !        Carbon, Nitrogen and Phosphorus.
        !        Mineralization for nutrients can be faster than for C.
        !        Hence C:N and C:P in slow detr. can be higher than in fast detr
        !        POC, PON, and POP refer to slow detritus.
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! POC     R*4 1 I     concentratrion of detritus C                [gC/m2]
        ! PON     R*4 1 I     concentratrion of detritus N                [gN/m2]
        ! POP     R*4 1 I     concentratrion of detritus P                [gP/m2]
        ! POS     R*4 1 I     concentratrion of detritus S                [gS/m2]
        ! RC20LO  R*4 1 I     degradation rate at 20 oC, lower value      [1/day]
        ! RC20UP  R*4 1 I     degradation rate at 20 oC, upper value      [1/day]
        ! RC20    R*4 1 I     degradation rate at 20 oC                   [1/day]
        ! TEMP    R*4 1 I     temperature                                    [oC]
        ! TC      R*4 1 I     temperature coefficient                         [-]
        ! TEMPC   R*4 1 -     temperature function                            [-]
        ! ANR     R*4 1 I     nitrogen content of refractory detritus     [gN/gC]
        ! APR     R*4 1 I     phosphorus content of refractory detritus   [gP/gC]
        ! ASR     R*4 1 I     sulfur content of refractory detritus       [gS/gC]
        ! ALN     R*4 1 I     nitrogen content of detritus, lower value   [gN/gC]
        ! ALP     R*4 1 I     phosphorus content of detritus, lower value [gN/gC]
        ! AUN     R*4 1 I     nitrogen content of detritus, upper value   [gN/gC]
        ! AUP     R*4 1 I     phosphorus content of detritus, upper value [gN/gC]
        ! FNUT    R*4 1 -     limiting factor for nutrient availability       [-]
        ! N_FACT  R*4 1 -     accelleration factor for nitrogen               [-]
        ! P_FACT  R*4 1 -     accelleration factor for phosphorus             [-]
        ! S_FACT  R*4 1 -     accelleration factor for sulphur                [-]
        ! OXY     R*4 1 I     concentratrion of dissolved oxygen         [gO2/m3]
        ! NO3     R*4 1 I     concentratrion of nitrate                   [gN/m3]
        ! B_NO3   R*4 1 I     attenuation constant for denitrification        [-]
        ! B_SULF  R*4 1 I     attenuation constant for sulfate reduction      [-]
        ! ELFACT  R*4 1 -     attenuation factor for electron acceptor        [-]
        ! B_DTPR  R*4 1 I     conversion ratio for part. refr. detritus       [-]
        ! B_DTDR  R*4 1 I     conversion ratio for diss. refr. detritus       [-]
        ! DECOC   R*4 1 O     degradation flux for detritus C         [gC/m3/day]
        ! DECON   R*4 1 O     degradation flux for detritus N         [gN/m3/day]
        ! DECOP   R*4 1 O     degradation flux for detritus P         [gP/m3/day]
        ! CNVOC   R*4 1 O     conversion flux for part. detritus C    [gC/m3/day]
        ! CNVON   R*4 1 O     conversion flux for part. detritus N    [gN/m3/day]
        ! CNVOP   R*4 1 O     conversion flux for part. detritus P    [gP/m3/day]
        ! CNVDC   R*4 1 O     conversion flux for diss. detritus C    [gC/m3/day]
        ! CNVDN   R*4 1 O     conversion flux for diss. detritus N    [gN/m3/day]
        ! CNVDP   R*4 1 O     conversion flux for diss. detritus P    [gP/m3/day]
        ! DECOCE  R*4 1 O     degradation flux for detritus C emersed [gC/m3/day]
        ! DEPTH   R*4 1 O     depth of segment                               [m3]
        ! NATTEM  R*4 1 O     air temperature                                [oC]
        ! SWEMRS  I*4 1 O     switch for emersion                             [-]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        REAL(kind = real_wp) :: POC, PON, POP, RC20LO, RC20UP, RC20, &
                TEMP, TC, TEMPC, ANR, APR, ALN, &
                ALP, AUN, AUP, FNUT, N_FACT, P_FACT, &
                OXY, NO3, B_NO3, B_SULF, ELFACT, B_DTPR, &
                B_DTDR, DECOC, DECON, DECOP, CNVPC, CNVPN, &
                CNVPP, CNVDC, CNVDN, CNVDP, POS, DECOS, &
                CNVPS, CNVDS, ASR, S_FACT, DEPTH, NATTEM, &
                DECOCE
        INTEGER(kind = int_wp) :: SWEMRS
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

        IFLUX = 0
        !
        DO ISEG = 1, NOSEG
            !
            !       In all active dry or wet segments
            !
            CALL extract_waq_attribute(3, IKNMRK(ISEG), IKMRK3)

            IF (IKMRK3==1 .OR. IKMRK3==3) THEN
                !
                !          INPUT of subroutine
                !
                POC = MAX(PMSA(IP1), 0.0)
                PON = MAX(PMSA(IP2), 0.0)
                POP = MAX(PMSA(IP3), 0.0)
                POS = MAX(PMSA(IP4), 0.0)
                RC20UP = PMSA(IP5)
                RC20LO = PMSA(IP6)
                TC = PMSA(IP7)
                TEMP = PMSA(IP8)
                ANR = PMSA(IP9)
                APR = PMSA(IP10)
                ASR = PMSA(IP11)
                ALN = PMSA(IP12)
                ALP = PMSA(IP13)
                AUN = PMSA(IP14)
                AUP = PMSA(IP15)
                OXY = PMSA(IP16)
                NO3 = PMSA(IP17)
                B_NO3 = PMSA(IP18)
                B_SULF = PMSA(IP19)
                B_DTPR = PMSA(IP20)
                B_DTDR = PMSA(IP21)
                DEPTH = PMSA(IP22)
                SWEMRS = NINT(PMSA(IP23))
                NATTEM = PMSA(IP24)
                POC = POC / DEPTH
                PON = PON / DEPTH
                POP = POP / DEPTH
                POS = POS / DEPTH
                !
                !          Errors if certain vars =< 0
                !
                IF (ANR < 1E-30) CALL write_error_message ('DECPC5: a_dNpr =< 0')
                IF (APR < 1E-30) CALL write_error_message ('DECPC5: a_dPpr =< 0')
                IF (ASR < 1E-30) CALL write_error_message ('DECPC5: a_dSpr =< 0')
                IF (ALN < 1E-30) CALL write_error_message ('DECPC5: al_dNs =< 0')
                IF (ALP < 1E-30) CALL write_error_message ('DECPC5: al_dPs =< 0')
                IF (AUN < 1E-30) CALL write_error_message ('DECPC5: au_dNs =< 0')
                IF (AUP < 1E-30) CALL write_error_message ('DECPC5: au_dPs =< 0')
                !
                !          Errors if upper limits =< lower limits
                !
                IF (AUN <= ALN) CALL write_error_message ('DECPC5: au_dNs =< al_dNs ')
                IF (AUP <= ALP) CALL write_error_message ('DECPC5: au_dPs =< al_dPs ')
                IF (RC20UP < RC20LO) &
                        CALL write_error_message ('DECPC5: ku_dSdec20 < kl_dSdec20 ')
                !
                !          If  detritus = 0 : set fluxes to zero and skip algorithm
                !
                IF (POC < 1E-10 .OR. PON < 1E-10 .OR. POP < 1E-10) &
                        THEN

                    DECOC = 0.0
                    DECON = 0.0
                    DECOP = 0.0
                    DECOS = 0.0
                    CNVPC = 0.0
                    CNVPN = 0.0
                    CNVPP = 0.0
                    CNVPS = 0.0
                    CNVDC = 0.0
                    CNVDN = 0.0
                    CNVDP = 0.0
                    CNVDS = 0.0
                    RC20 = 0.0
                    TEMPC = 0.0
                    ELFACT = 1.0
                    N_FACT = 0.0
                    P_FACT = 0.0
                    DECOC = 0.0
                    DECOCE = 0.0

                ELSE
                    !
                    !             Calculate degrad. rate at 20oC for current stochiometry
                    !
                    IF ((PON / POC) > AUN .AND. (POP / POC) > AUP) THEN
                        !
                        !                -- both stoch's above upper limit
                        !
                        RC20 = RC20UP

                    ELSE IF ((PON / POC) < ALN .OR. (POP / POC) < ALP) &
                            THEN
                        !
                        !                -- one or both stoch's < lower limit
                        !
                        RC20 = RC20LO

                    ELSE
                        !
                        !                -- both stoch's between upper and lower limit
                        !
                        FNUT = MIN(((PON / POC) - ALN) / (AUN - ALN), &
                                ((POP / POC) - ALP) / (AUP - ALP))
                        RC20 = RC20LO + FNUT * (RC20UP - RC20LO)

                    ENDIF
                    !
                    !             Calculate correction factors
                    !             for temperature
                    !
                    IF (IKMRK3 == 1 .AND. SWEMRS == 1) THEN
                        TEMP = NATTEM
                    ENDIF
                    TEMPC = TC**(TEMP - 20)
                    !
                    !             for electron acceptor (aerobic, denitr., sulfate red.)
                    !
                    IF (IKMRK3 == 1 .AND. SWEMRS == 1) THEN
                        ELFACT = 1.0
                    ELSE
                        IF (OXY > 0.0) THEN
                            ELFACT = 1.0
                        ELSE IF (NO3 > 0.0) THEN
                            ELFACT = B_NO3
                        ELSE
                            ELFACT = B_SULF
                        ENDIF
                    ENDIF
                    !
                    !             for nutrient stripping
                    !
                    N_FACT = 1.0 + ((PON / POC) - ANR) / ANR
                    P_FACT = 1.0 + ((POP / POC) - APR) / APR
                    S_FACT = 1.0 + ((POS / POC) - ASR) / ASR
                    N_FACT = MAX(N_FACT, 0.5)
                    P_FACT = MAX(P_FACT, 0.5)
                    S_FACT = MAX(S_FACT, 0.5)
                    !
                    !             Calculate the fluxes for mineralization and conversion
                    !
                    DECOC = RC20 * TEMPC * ELFACT * POC
                    CNVPC = B_DTPR * DECOC
                    CNVDC = B_DTDR * DECOC

                    DECON = RC20 * TEMPC * ELFACT * N_FACT * PON
                    CNVPN = (1 / N_FACT) * B_DTPR * DECON
                    CNVDN = (1 / N_FACT) * B_DTDR * DECON

                    DECOP = RC20 * TEMPC * ELFACT * P_FACT * POP
                    CNVPP = (1 / P_FACT) * B_DTPR * DECOP
                    CNVDP = (1 / P_FACT) * B_DTDR * DECOP

                    DECOS = RC20 * TEMPC * ELFACT * S_FACT * POS
                    CNVPS = (1 / S_FACT) * B_DTPR * DECOS
                    CNVDS = (1 / S_FACT) * B_DTDR * DECOS

                ENDIF

                IF (IKMRK3 == 1 .AND. SWEMRS == 1) THEN

                    ! emersion

                    DECOCE = DECOC
                    DECOC = 0.0

                ELSE

                    DECOCE = 0.0

                ENDIF

                !
                !          OUTPUT of subroutine
                !
                PMSA(IP25) = RC20 * TEMPC * ELFACT
                PMSA(IP26) = N_FACT
                PMSA(IP27) = P_FACT
                PMSA(IP28) = DECOC
                PMSA(IP29) = DECOCE

                FL(1 + IFLUX) = CNVPC
                FL(2 + IFLUX) = CNVPN
                FL(3 + IFLUX) = CNVPP
                FL(4 + IFLUX) = CNVPS
                FL(5 + IFLUX) = CNVDC
                FL(6 + IFLUX) = CNVDN
                FL(7 + IFLUX) = CNVDP
                FL(8 + IFLUX) = CNVDS
                FL(9 + IFLUX) = DECOC
                FL(10 + IFLUX) = DECON
                FL(11 + IFLUX) = DECOP
                FL(12 + IFLUX) = DECOS
                FL(13 + IFLUX) = DECOCE
                FL(14 + IFLUX) = DECOCE

            ENDIF
            !
            !       Pointers ophogen
            !
            IFLUX = IFLUX + NOFLUX
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

        end do

        RETURN
    END

end module m_decpc5
