module m_decdet
    use m_waq_precision

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2025.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine decdet (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message

        !>\file
        !>       Mineralisation & conversion of detritus POC1,POC2,POC3,POC4,DOC

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Mineralization and conversion of detritus Carbon, Nitrogen,
        !        Phosphorus and Sulphur.
        !        Two options are available, with or without nutrient stripping.
        !        Input mineralization rates can be different for C, N and P.
        !        Input rates for C are also used for S.
        !        Mineralization for nutrients N, P and S can be faster than for
        !        C, also due to nutrient stripping. Hence C:N , C:P and C:S in
        !        other detritus can be higher than in fast detritus.
        !        POC, PON, POP and POS refer not only to fast, medium, slow,
        !        refractory particulate detritus (POC1, POC2, POC3, POC4),
        !        but also to dissolved detritus (DOC).
        !        In case of POC4 and DOC some of the iput items are dummies and
        !        have fixed values that lead to:
        !        a) basic rate is always upper limit and equal for C,N,P,S
        !        b) nutrient availability does not affact the rate
        !        c) nutrient stripping is not applied
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! POC     R*4 1 I     concentration of detritus C                 [gC/m3]
        ! PON     R*4 1 I     concentration of detritus N                 [gN/m3]
        ! POP     R*4 1 I     concentration of detritus P                 [gP/m3]
        ! POS     R*4 1 I     concentration of detritus S                 [gS/m3]
        ! RC20LOC R*4 1 I     degradation rate detC at 20 oC, lower value [1/day]
        ! RC20UPC R*4 1 I     degradation rate detC at 20 oC, upper value [1/day]
        ! RC20LON R*4 1 I     degradation rate detN at 20 oC, lower value [1/day]
        ! RC20UPN R*4 1 I     degradation rate detN at 20 oC, upper value [1/day]
        ! RC20LOP R*4 1 I     degradation rate detP at 20 oC, lower value [1/day]
        ! RC20UPP R*4 1 I     degradation rate detP at 20 oC, upper value [1/day]
        ! RC20C   R*4 1 -     degradation rate detC at 20 oC              [1/day]
        ! RC20N   R*4 1 -     degradation rate detN at 20 oC              [1/day]
        ! RC20P   R*4 1 -     degradation rate detP at 20 oC              [1/day]
        ! RC20S   R*4 1 -     degradation rate detS at 20 oC              [1/day]
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
        ! OXY     R*4 1 I     concentration of dissolved oxygen          [gO2/m3]
        ! NO3     R*4 1 I     concentration of nitrate                    [gN/m3]
        ! B_NO3   R*4 1 I     attenuation constant for denitrification        [-]
        ! B_SULF  R*4 1 I     attenuation constant for sulfate reduction      [-]
        ! ELFACT  R*4 1 -     attenuation factor for electron acceptor        [-]
        ! B_DTP   R*4 1 I     conversion ratio for particulate detritus       [-]
        ! B_DTD   R*4 1 I     conversion ratio for dissolved detritus         [-]
        ! SWOMDEC R*4 1 I     option: 0.0 with nutrient stripping, 1.0 without
        ! ID      R*4 1 I     ident.=1 (POC1, POC2, POC3), indent.=2 (POC4, DOC)
        ! DECOC   R*4 1 O     degradation flux for detritus C         [gC/m3/day]
        ! DECON   R*4 1 O     degradation flux for detritus N         [gN/m3/day]
        ! DECOP   R*4 1 O     degradation flux for detritus P         [gP/m3/day]
        ! DECOS   R*4 1 O     degradation flux for detritus S         [gS/m3/day]
        ! CNVPC   R*4 1 O     conversion flux for part. detritus C    [gC/m3/day]
        ! CNVPN   R*4 1 O     conversion flux for part. detritus N    [gN/m3/day]
        ! CNVPP   R*4 1 O     conversion flux for part. detritus P    [gP/m3/day]
        ! CNVPS   R*4 1 O     conversion flux for part. detritus S    [gS/m3/day]
        ! CNVDC   R*4 1 O     conversion flux for diss. detritus C    [gC/m3/day]
        ! CNVDN   R*4 1 O     conversion flux for diss. detritus N    [gN/m3/day]
        ! CNVDP   R*4 1 O     conversion flux for diss. detritus P    [gP/m3/day]
        ! CNVDS   R*4 1 O     conversion flux for diss. detritus S    [gS/m3/day]
        !
        !     Logical Units : -
        !
        !     Modules called : -
        !
        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)
        !
        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        REAL(kind = real_wp) :: POC, PON, POP, POS, RC20LOC, &
                RC20UPC, RC20LON, RC20UPN, RC20LOP, RC20UPP, &
                RC20C, RC20N, RC20P, RC20S, TEMP, &
                TC, TEMPC, ANR, APR, ASR, &
                ALN, ALP, AUN, AUP, FNUT, &
                N_FACT, P_FACT, S_FACT, OXY, NO3, &
                B_NO3, B_SULF, ELFACT, B_DTP, B_DTD, &
                DECOC, DECON, DECOP, DECOS, CNVPC, &
                CNVPN, CNVPP, CNVPS, CNVDC, CNVDN, &
                CNVDP, CNVDS, SWOMDEC, ID

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
        !
        IFLUX = 0
        !
        DO ISEG = 1, num_cells
            !
            !       In all "active" segments
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !          INPUT of subroutine
                !
                POC = MAX(process_space_real(IP1), 0.0)
                PON = MAX(process_space_real(IP2), 0.0)
                POP = MAX(process_space_real(IP3), 0.0)
                POS = MAX(process_space_real(IP4), 0.0)
                ID = process_space_real(IP5)
                !
                !          all rates are equal for ID=2 (POC4 and DOC)
                !
                RC20UPC = process_space_real(IP6)
                IF (NINT(ID) == 2) THEN
                    RC20LOC = process_space_real(IP6)
                    RC20UPN = process_space_real(IP6)
                    RC20LON = process_space_real(IP6)
                    RC20UPP = process_space_real(IP6)
                    RC20LOP = process_space_real(IP6)
                ELSE
                    RC20LOC = process_space_real(IP7)
                    RC20UPN = process_space_real(IP8)
                    RC20LON = process_space_real(IP9)
                    RC20UPP = process_space_real(IP10)
                    RC20LOP = process_space_real(IP11)
                ENDIF
                !
                TC = process_space_real(IP12)
                TEMP = process_space_real(IP13)
                ANR = process_space_real(IP14)
                APR = process_space_real(IP15)
                ASR = process_space_real(IP16)
                ALN = process_space_real(IP17)
                ALP = process_space_real(IP18)
                AUN = process_space_real(IP19)
                AUP = process_space_real(IP20)
                OXY = process_space_real(IP21)
                NO3 = process_space_real(IP22)
                B_NO3 = process_space_real(IP23)
                B_SULF = process_space_real(IP24)
                B_DTP = process_space_real(IP25)
                B_DTD = process_space_real(IP26)
                SWOMDEC = process_space_real(IP27)
                !
                !          Errors if certain vars =< 0
                !
                IF (ANR < 1E-30) CALL write_error_message ('DECDET: a_dNpr =< 0')
                IF (APR < 1E-30) CALL write_error_message ('DECDET: a_dPpr =< 0')
                IF (ASR < 1E-30) CALL write_error_message ('DECDET: a_dSpr =< 0')
                IF (ALN < 1E-30) &
                        CALL write_error_message ('DECDET: al_dN/F/M/S/ =< 0')
                IF (ALP < 1E-30) &
                        CALL write_error_message ('DECDET: al_dP/F/M/S/ =< 0')
                IF (AUN < 1E-30) &
                        CALL write_error_message ('DECDET: au_dN/F/M/S/ =< 0')
                IF (AUP < 1E-30) &
                        CALL write_error_message ('DECDET: au_dP/F/M/S/ =< 0')
                !
                !          Errors if upper limits =< lower limits
                !
                IF (AUN < ALN) &
                        CALL write_error_message ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ')
                IF (AUP < ALP) &
                        CALL write_error_message ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ')
                !
                IF (RC20UPC < RC20LOC) &
                        CALL write_error_message ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ')
                IF (RC20UPN < RC20LON) &
                        CALL write_error_message ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ')
                IF (RC20UPP < RC20LOP) &
                        CALL write_error_message ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ')
                !
                !          If  detritus = 0 : set fluxes to zero and skip algorithm
                !
                IF (POC < 1E-10) &
                        THEN
                    !
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

                    RC20C = 0.0
                    TEMPC = 1.0
                    ELFACT = 1.0
                    RC20N = 0.0
                    RC20P = 0.0
                    N_FACT = 1.0
                    P_FACT = 1.0
                    S_FACT = 1.0
                    !
                ELSE
                    !
                    !             Calculate degrad. rate at 20oC for current stochiometry
                    !
                    IF ((PON / POC) > AUN .AND. (POP / POC) > AUP) THEN
                        !
                        !                -- both stoch's above upper limit
                        !
                        RC20C = RC20UPC
                        RC20N = RC20UPN
                        RC20P = RC20UPP
                        !
                    ELSE IF ((PON / POC) < ALN .OR. (POP / POC) < ALP) &
                            THEN
                        !
                        !                -- one or both stoch's below lower limit
                        !
                        RC20C = RC20LOC
                        RC20N = RC20LON
                        RC20P = RC20LOP
                        !
                    ELSE
                        !
                        !                -- both stoch's between upper and lower limit
                        !                   or one stoch above ul and one between ul and ll
                        !
                        IF (AUN == ALN .OR. AUP == ALP) THEN
                            FNUT = 0.5
                        ELSE
                            FNUT = MIN(((PON / POC) - ALN) / (AUN - ALN), &
                                    ((POP / POC) - ALP) / (AUP - ALP))
                        ENDIF
                        !
                        RC20C = RC20LOC + FNUT * (RC20UPC - RC20LOC)
                        RC20N = RC20LON + FNUT * (RC20UPN - RC20LON)
                        RC20P = RC20LOP + FNUT * (RC20UPP - RC20LOP)
                        !
                    ENDIF
                    !
                    RC20S = RC20C
                    !
                    !             Calculate correction factors
                    !             for temperature
                    !
                    TEMPC = TC**(TEMP - 20)
                    !
                    !             for dominant electron acceptor
                    !
                    IF (OXY > 0.1) THEN
                        ELFACT = 1.0
                    ELSE IF (NO3 > 0.1) THEN
                        ELFACT = B_NO3
                    ELSE
                        ELFACT = B_SULF
                    ENDIF
                    !
                    !             for nutrient stripping
                    !
                    IF (NINT(SWOMDEC) == 0) THEN
                        N_FACT = 1.0 + ((PON / POC) - ANR) / ANR
                        P_FACT = 1.0 + ((POP / POC) - APR) / APR
                        S_FACT = 1.0 + ((POS / POC) - ASR) / ASR
                        N_FACT = MAX(N_FACT, 0.5)
                        P_FACT = MAX(P_FACT, 0.5)
                        S_FACT = MAX(S_FACT, 0.5)
                        N_FACT = MIN(N_FACT, 5.0)
                        P_FACT = MIN(P_FACT, 5.0)
                        S_FACT = MIN(S_FACT, 5.0)
                    ELSE
                        N_FACT = 1.0
                        P_FACT = 1.0
                        S_FACT = 1.0
                    ENDIF
                    !
                    !             Calculate the fluxes for mineralization and conversion
                    !
                    DECOC = RC20C * TEMPC * ELFACT * POC
                    CNVPC = B_DTP * DECOC
                    CNVDC = B_DTD * DECOC
                    !
                    DECON = RC20N * TEMPC * ELFACT * N_FACT * PON
                    CNVPN = (1 / N_FACT) * B_DTP * DECON
                    CNVDN = (1 / N_FACT) * B_DTD * DECON
                    !
                    DECOP = RC20P * TEMPC * ELFACT * P_FACT * POP
                    CNVPP = (1 / P_FACT) * B_DTP * DECOP
                    CNVDP = (1 / P_FACT) * B_DTD * DECOP
                    !
                    DECOS = RC20S * TEMPC * ELFACT * S_FACT * POS
                    CNVPS = (1 / S_FACT) * B_DTP * DECOS
                    CNVDS = (1 / S_FACT) * B_DTD * DECOS
                    !
                ENDIF
                !
                !          OUTPUT of subroutine
                !
                process_space_real(IP28) = RC20C * TEMPC * ELFACT
                process_space_real(IP29) = RC20N * TEMPC * ELFACT
                process_space_real(IP30) = RC20P * TEMPC * ELFACT
                process_space_real(IP31) = N_FACT
                process_space_real(IP32) = P_FACT
                process_space_real(IP33) = S_FACT
                process_space_real(IP34) = DECOC
                !
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
                !
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
            IP30 = IP30 + IN30
            IP31 = IP31 + IN31
            IP32 = IP32 + IN32
            IP33 = IP33 + IN33
            IP34 = IP34 + IN34
            !
        end do
        !
        RETURN
    END

end module m_decdet
