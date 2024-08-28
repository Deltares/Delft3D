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

MODULE UITZICHT_DATA
    use m_waq_precision

    !***********************************************************************
    !     SPECTRALE GEGEVENS VAN 400 TOT 700 NM MET STAPPEN VAN 5 NM
    !***********************************************************************
    !
    REAL(kind = dp), DIMENSION(61) :: AWATER, BWATER, CHLSPE, PLANCK
    !
    !     SPECIFIEKE ABSORPTIE ALGEN
    !
    DATA CHLSPE / 0.685, 0.781, 0.828, 0.883, 0.913, &
            0.939, 0.973, 1.001, 1.000, 0.971, &
            0.944, 0.928, 0.917, 0.902, 0.870, &
            0.839, 0.798, 0.773, 0.750, 0.717, &
            0.688, 0.645, 0.618, 0.582, 0.528, &
            0.504, 0.474, 0.444, 0.416, 0.384, &
            0.357, 0.321, 0.294, 0.273, 0.276, &
            0.278, 0.281, 0.279, 0.270, 0.252, &
            0.256, 0.262, 0.261, 0.268, 0.281, &
            0.299, 0.316, 0.328, 0.329, 0.337, &
            0.361, 0.397, 0.457, 0.529, 0.556, &
            0.534, 0.485, 0.411, 0.334, 0.270, &
            0.215/
    !
    !     SPECTRALE VERDELING INVALLEND LICHT
    DATA PLANCK / 0.18380, 0.18836, 0.19280, 0.19710, 0.20134, &
            0.20542, 0.20937, 0.21320, 0.21689, 0.22046, &
            0.22389, 0.22720, 0.23037, 0.23342, 0.23630, &
            0.23912, 0.24177, 0.24430, 0.24671, 0.24898, &
            0.25115, 0.25320, 0.25511, 0.25691, 0.25860, &
            0.26018, 0.26166, 0.26300, 0.26427, 0.26540, &
            0.26650, 0.26745, 0.26830, 0.26910, 0.26978, &
            0.27037, 0.27089, 0.27132, 0.27167, 0.27194, &
            0.27214, 0.27226, 0.27232, 0.27231, 0.27223, &
            0.27209, 0.27189, 0.27163, 0.27130, 0.27094, &
            0.27050, 0.27004, 0.26950, 0.26894, 0.26833, &
            0.26767, 0.26697, 0.26620, 0.26545, 0.26464, &
            0.26379/
    !
    !     VERSTROOIING WATER
    !
    DATA BWATER / 0.005290, 0.005025, 0.004776, 0.004543, 0.004323, &
            0.004117, 0.003922, 0.003739, 0.003567, 0.003404, &
            0.003250, 0.003105, 0.002968, 0.002838, 0.002715, &
            0.002599, 0.002488, 0.002384, 0.002285, 0.002191, &
            0.002102, 0.002017, 0.001936, 0.001860, 0.001787, &
            0.001717, 0.001651, 0.001588, 0.001528, 0.001471, &
            0.001416, 0.001364, 0.001314, 0.001267, 0.001221, &
            0.001178, 0.001136, 0.001097, 0.001059, 0.001022, &
            0.000987, 0.000954, 0.000922, 0.000891, 0.000862, &
            0.000833, 0.000806, 0.000780, 0.000755, 0.000731, &
            0.000708, 0.000686, 0.000664, 0.000644, 0.000624, &
            0.000605, 0.000587, 0.000569, 0.000552, 0.000536, &
            0.00052/
    !
    !     ABSORPTIE WATER
    !
    DATA AWATER / 0.00576, 0.00617, 0.00669, 0.00727, 0.00790, &
            0.00854, 0.00918, 0.00980, 0.01039, 0.01093, &
            0.01144, 0.01193, 0.01241, 0.01293, 0.01353, &
            0.01426, 0.01520, 0.01645, 0.01810, 0.02032, &
            0.02380, 0.02818, 0.03294, 0.03768, 0.04212, &
            0.04604, 0.04937, 0.05211, 0.05438, 0.05639, &
            0.05849, 0.06108, 0.06470, 0.06999, 0.07768, &
            0.08886, 0.10519, 0.12723, 0.15488, 0.18723, &
            0.22238, 0.25724, 0.28740, 0.30690, 0.31434, &
            0.31761, 0.31940, 0.32232, 0.32862, 0.33985, &
            0.35650, 0.37763, 0.40052, 0.42033, 0.43334, &
            0.44359, 0.45513, 0.47316, 0.50342, 0.55154, &
            0.62200/
END MODULE UITZICHT_DATA

SUBROUTINE UIT_ZI (DIEP1, DIEP2, ANGLE, C_GL1, C_GL2, &
        C_DET, HELHUM, TAU, CORCHL, CHLORO, &
        DETRIT, GLOEIR, AH_380, SECCHI, D_1, &
        EXTPAR, EXTP_D, DOSECC)
    !>\file
    !>       Transparency due to Chlorophyll, detritus, inorganics and humic accids

    !***********************************************************************
    !     SUBROUTINE UIT_ZI, GEBASEERD OP HET MODEL UITZICHT VAN
    !     H. BUITEVELD, RIZA, POSTBUS 17, 8200 AA LELYSTAD (TEL 03200-70737)
    !
    !     BEREKEND DOORZICHT EN EXTINKTIE OP BASIS VAN CHLOROFYL, DETRITUS
    !     GLOEIREST, ABSORPTIE HUMUSZUREN BIJ 380 NM EN SPECTRA OM DE 5 NM
    !     VAN DE ABSORPTIE EN VERSTROOIING VAN WATER, DE VERDELING DE
    !     INVALLEND LICHT EN DE SPECIFIEKE SPECIFIEKE ABSORPTIE ALGEN
    !
    !     CHANGES IN THE MODULE:
    !     DATE   AUTHOR          DESCRIPTION
    !     ------ --------------- -------------------------------------------
    !     971119 Jan van Beek    Extended Ascii characters verwijderd
    !     970127 Rik Sonneveldt  In subroutine BEP_D beveiliging tegen CHLORO
    !                            < 0 ingebouwd (op verzoek van Maarten Ouboter).
    !     911125 Andre Hendriks  Code beter leesbaar gemaakt, en variabele-
    !                            namen langer dan 6 letters vervangen door korte
    !                            namen.
    !     910926 WOLF MOOIJ      IMPLEMENTATION IN DELWAQ-BLOOM
    !                            BELANGRIJKSTE VERANDERING: INPLAATS VAN
    !                            TOTAAL ZWEVEND STOF WORDT NU DIRECT
    !                            DE DETRITUS CONCENTRATIE INGELEZEN
    !***********************************************************************
    !***********************************************************************
    !     ARGUMENTS:
    !     NAME   TYPE SIZE   I/O DESCRIPTION
    !     ------ ---- ------ --- -------------------------------------------
    !     ANGLE   R*8        IN  FUNCTION, DEFAULT constant = 30 x
    !     AH_380  R*8        IN  PARAMETER: EXTINCTIE HUMUSZUREN (1/m)
    !     CHLORO  R*8        IN  PARAMETER: CHLOROPHYL (mg/m3)
    !     CORCHL  R*8        IN  CONSTANT, DEFAULT = 2.5
    !     C_DET   R*8        IN  CONSTANT, DEFAULT = 0.026
    !     C_GL1   R*8        IN  CONSTANT, DEFAULT = 0.73
    !     C_GL2   R*8        IN  CONSTANT, DEFAULT = 1.0
    !     DETRIT  R*8        IN  PARAMETER: GESUSPENDEERD DETRITUS (gDW/m3)
    !     DIEP1   R*8        IN  CONSTANT, DEFAULT = 1.0 (m)
    !     DIEP2   R*8        IN  CONSTANT, DEFAULT = 1.2 (m)
    !     D_1     R*8        OUT DUMMY PARAMETER D 10% TRANSMIS 560 NM (m)
    !     EXTPAR  R*8        OUT PARAMETER: EXTINCTIE OP 1M (1/m)
    !     EXTP_D  R*8        OUT DUMMY PARAMETER EXTINCTIE OP D_1 (1/m)
    !     GLOEIR  R*8        IN  PARAMETER: ANORGANISCH ZWEVEND STOF (gDW/m3)
    !     HELHUM  R*8        IN  CONSTANT, DEFAULT = 0.014
    !     SECCHI  R*8        OUT PARAMETER: DOORZICHT (m)
    !     TAU     R*8        IN  CONSTANT, DEFAULT = 7.8
    !***********************************************************************
    !***********************************************************************
    !     COMMON VARIABLES:
    !     NAME   TYPE SIZE   DESCRIPTION
    !     ------ ---- ------ -----------------------------------------------
    !     AWATER  R*8 (61)   ABSORPTIE WATER
    !     BWATER  R*8 (61)   VERSTROOING WATER
    !     CHLSPE  R*8 (61)   SPECIFIEKE ABSORPTIE ALGEN
    !     PLANCK  R*8 (61)   VERDELING INVALLEND LICHT
    !***********************************************************************
    !***********************************************************************
    !     LOCAL VARIABLES:
    !     NAME   TYPE SIZE   DESCRIPTION
    !     ------ ---- ------ -----------------------------------------------
    !     A       R*8
    !     A_CHL   R*8
    !     A_DET   R*8
    !     A_HUM   R*8
    !     B       R*8
    !     B_CHL   R*8
    !     B_GL    R*8
    !     C_CHL   R*8
    !     C_GL    R*8
    !     C_MU    R*8
    !     D_2     R*8
    !     EXT_KI  R*8
    !     I_550   I*8
    !     LAMBDA  I*8
    !     SOM_C   R*8
    !     SOM_D1  R*8
    !     SOM_D2  R*8
    !     SOM_H   R*8
    !     S_D1    R*8
    !     S_D2    R*8
    !     TELLER  I*8
    !     ZW_STF  R*8
    !***********************************************************************
    !
    !     include '..\inc\ioblck.inc'
    USE UITZICHT_DATA
    !
    REAL(kind = dp) :: A, A_CHL, A_DET, A_HUM, AH_380, &
            ANGLE, B, B_CHL, B_GL, &
            C_CHL, C_DET, C_GL, C_GL1, C_GL2, &
            C_MU, CHLORO, CORCHL, D_1, D_2, &
            DETRIT, DIEP1, DIEP2, EXT_KI, EXTP_D, &
            EXTPAR, GLOEIR, HELHUM, S_D1, S_D2, &
            SECCHI, SOM_C, SOM_D1, SOM_D2, SOM_H, &
            TAU, ZW_STF
    INTEGER(kind = int_wp) :: I_550, LAMBDA, TELLER
    LOGICAL DOSECC
    !
    IF (CHLORO >= 0.0 .OR. DETRIT >= 0.0 .OR. &
            GLOEIR >= 0.0) THEN
        C_MU = COS (ANGLE * 0.0174533)
        IF (DOSECC) &
                CALL BEP_D  (C_GL1, C_GL2, C_DET, HELHUM, CORCHL, &
                        C_MU, CHLORO, DETRIT, GLOEIR, AH_380, &
                        D_1, D_2)
        I_550 = ((550 - 400) / 5) + 1
        !
        !        CHLOROFYL BUNDELVERZWAKKING
        !
        C_CHL = (0.058 + 0.018 * CHLORO) * CHLSPE (I_550)
        C_CHL = (C_CHL + 0.12 * (CHLORO**0.63)) * CORCHL
        SOM_D1 = 0.0
        SOM_D2 = 0.0
        S_D1 = 0.0
        S_D2 = 0.0
        SOM_C = 0.0
        SOM_H = 0.0
        DO TELLER = 1, 61
            LAMBDA = 400 + (TELLER - 1) * 5
            !
            !           HUMUSZUREN ABOSORPTIE
            !
            A_HUM = AH_380 * EXP (-HELHUM * (LAMBDA - 380.0))
            !
            !           ALGEN ABSOROPTIE EN VERSTROOIING
            !
            IF (CHLORO < 0.000001) THEN
                A_CHL = 0.0
                B_CHL = 0.0
            ELSE
                A_CHL = (0.058 + 0.018 * CHLORO) * CHLSPE (TELLER) * &
                        CORCHL
                B_CHL = C_CHL - A_CHL
            ENDIF
            !
            !           GLOEIREST EN DETRITUS
            !
            ZW_STF = GLOEIR + DETRIT
            C_GL = C_GL1 * ((ZW_STF**C_GL2)) * (400.0 / LAMBDA)
            A_DET = C_DET * DETRIT * (400.0 / LAMBDA)
            B_GL = C_GL - A_DET
            !
            !           TOTAAL ABSORPTIE EN VERSTROOIING BIJ LAMBDA
            !
            A = AWATER (TELLER) + A_HUM + A_DET + A_CHL
            B = BWATER (TELLER) + B_GL + B_CHL
            !
            !           EXTINKTIE BIJ LAMBDA
            !
            EXT_KI = 1 / C_MU * &
                    SQRT (A * A + (0.425 * C_MU - 0.19) * A * B)
            SOM_D1 = SOM_D1 + PLANCK (TELLER) * EXP (-EXT_KI * DIEP1)
            SOM_D2 = SOM_D2 + PLANCK (TELLER) * EXP (-EXT_KI * DIEP2)
            IF (DOSECC) THEN
                S_D1 = S_D1 + PLANCK (TELLER) * EXP (-EXT_KI * D_1)
                S_D2 = S_D2 + PLANCK (TELLER) * EXP (-EXT_KI * D_2)
                SOM_H = SOM_H + PLANCK (TELLER) * EXP (-(A + B) * D_1)
                SOM_C = SOM_C + PLANCK (TELLER) * EXP (-(A + B) * D_2)
            ENDIF
        end do
        EXTPAR = (1.0 / (DIEP1 - DIEP2) * LOG (SOM_D2 / SOM_D1))
        IF (DOSECC) THEN
            EXTP_D = (1.0 / (D_1 - D_2) * LOG (S_D2 / S_D1))
            SOM_C = (1.0 / (D_1 - D_2) * LOG (SOM_C / SOM_H))
            SECCHI = TAU / (EXTP_D + SOM_C)
        ELSE
            SECCHI = -1.0
        ENDIF
    ENDIF
    RETURN
END

SUBROUTINE BEP_D (C_GL1, C_GL2, C_DET, HELHUM, CORCHL, &
        C_MU, CHLORO, DETRIT, GLOEIR, AH_380, &
        D_1, D_2)
    !***********************************************************************
    !     BEPAALD DIEPTE WAAR 10 % VAN HET LICHT OVER IS BIJ 550  NM
    !***********************************************************************
    !***********************************************************************
    !     ARGUMENTS:
    !     NAME   TYPE SIZE   I/O DESCRIPTION
    !     ------ ---- ------ --- -------------------------------------------
    !     AH_380  R*8        IN  PARAMETER: EXTINCTIE HUMUSZUREN (1/M)
    !     CHLORO  R*8        IN  PARAMETER: CHLOROPHYL (fG/L)
    !     CORCHL  R*8        IN  CONSTANT,  DEFAULT  =  2.5
    !     C_DET   R*8        IN  CONSTANT,  DEFAULT  =  0.026
    !     C_GL1   R*8        IN  CONSTANT,  DEFAULT  =  0.73
    !     C_GL2   R*8        IN  CONSTANT,  DEFAULT  =  1.0
    !     C_MU    R*8        IN  CONSTANT,  COSINUS VAN ANGLE (DEFAULT  =  30)
    !     DETRIT  R*8        IN  PARAMETER: GESUSPENDEERD DETRITUS (MG/L)
    !     D_1     R*8        OUT DUMMY PARAMETER D 10% TRANSMIS 560 NM (M)
    !     D_2     R*8        OUT DUMMY PARAMETER D_1 + 0.1 (M)
    !     GLOEIR  R*8        IN  PARAMETER: ANORGANISCH ZWEVEND STOF (MG/L)
    !     HELHUM  R*8        IN  CONSTANT,  DEFAULT  =  0.014
    !***********************************************************************
    !***********************************************************************
    !     COMMON VARIABLES:
    !     NAME   TYPE SIZE   DESCRIPTION
    !     ------ ---- ------ -----------------------------------------------
    !     AWATER  R*8 (61)   ABSORPTIE WATER
    !     BWATER  R*8 (61)   VERSTROOING WATER
    !     CHLSPE  R*8 (61)   SPECIFIEKE ABSORPTIE ALGEN
    !     PLANCK  R*8 (61)   VERDELING INVALLEND LICHT
    !***********************************************************************
    !***********************************************************************
    !     LOCAL VARIABLES:
    !     NAME   TYPE SIZE   DESCRIPTION
    !     ------ ---- ------ -----------------------------------------------
    !     A      R*8
    !     A_CHL  R*8
    !     A_DET  R*8
    !     A_HUM  R*8
    !     B      R*8
    !     B_CHL  R*8
    !     B_GL   R*8
    !     C_CHL  R*8
    !     C_GL   R*8
    !     EXT_KI R*8
    !     I_550  I*8
    !     LAMBDA I*8
    !     TELLER I*8
    !     ZW_STF R*8
    !***********************************************************************

    USE UITZICHT_DATA
    !
    REAL(kind = dp) :: A, A_CHL, A_DET, A_HUM, AH_380, &
            B, B_CHL, B_GL, C_CHL, C_DET, &
            C_GL, C_GL1, C_GL2, C_MU, CHLORO, &
            CORCHL, D_1, D_2, DETRIT, EXT_KI, &
            GLOEIR, HELHUM, ZW_STF
    INTEGER(kind = int_wp) :: I_550, LAMBDA, TELLER
    !
    I_550 = ((550 - 400) / 5) + 1
    !
    !     Beveiliging tegen negatieve waarde CHLORO (RS27jan97 voor Maarten O.)
    !
    CHLORO = MAX(0.0_dp, CHLORO)
    !
    !     CHLOROFYL BUNDEL VERZWAKKING
    !
    C_CHL = (0.058 + 0.018 * CHLORO) * CHLSPE (I_550)
    C_CHL = (C_CHL + 0.12 * (CHLORO**0.63)) * CORCHL
    TELLER = ((560 - 400) / 5) + 1
    LAMBDA = 400 + (TELLER - 1) * 5
    !
    !     ABSORPTIE HUMUSZUREN
    !
    A_HUM = AH_380 * EXP (-HELHUM * (LAMBDA - 380))
    !
    !     CHLOROFYL
    !
    IF (CHLORO < 0.000001) THEN
        A_CHL = 0.0
        B_CHL = 0.0
    ELSE
        A_CHL = (0.058 + 0.018 * CHLORO) * CHLSPE (TELLER) * CORCHL
        B_CHL = C_CHL - A_CHL
    ENDIF
    !
    !     GLOEIREST EN DETRITUS
    !
    ZW_STF = GLOEIR + DETRIT
    C_GL = C_GL1 * (ZW_STF**C_GL2) * (400.0 / LAMBDA)
    A_DET = C_DET * DETRIT * (400.0 / LAMBDA)
    B_GL = C_GL - A_DET
    A = AWATER (TELLER) + A_HUM + A_DET + A_CHL
    B = BWATER (TELLER) + B_GL + B_CHL
    EXT_KI = 1 / C_MU * &
            SQRT ((A * A + (0.425 * C_MU - 0.19) * A * B))
    !
    !     DIEPTE 10 % TRANSMISSIE 560 NM
    !
    D_1 = 2.3 / EXT_KI
    D_2 = D_1 + 0.1
    RETURN
END
