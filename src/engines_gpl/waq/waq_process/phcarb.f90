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
module m_phcarb
    use m_waq_precision

    implicit none

contains


    subroutine phcarb  (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Integrated calculation of pH and CO2 system

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Integrated calculation of pH and CO2 system
        !        Restrictions: ...
        !
        ! Name            T  L I/O   Description                                    Uni
        ! ----           --- -  -   -------------------                             --
        ! AHPLUS         R*4 1 L  activity of H+                                 [mol/kg]
        ! ALK            R*4 1 L  Alkalinity as HCO3-                            [mmolHCO3/kg]
        ! ALKA           R*4 1 I  Alkalinity as HCO3-                            [gHCO3/m3]
        ! TICM           R*4 1 L  total carbonate concentration                  [mmolC/kg]
        ! TIC            R*4 1 I  total carbonate concentration                  [gC/m3]
        ! BT             R*4 1 L  total borate                                   [mmol/kg]
        ! Ca             R*4 1 L  calcium concentration                          [mmol/kg]
        ! MTOMM          R*4 1 L  conversion from mol to mmol                    [mol/mmol]
        ! MMTOM          R*4 1 L  conversion from mmol to mol                    [mmol/mol]
        ! M3TOL          R*4 1 L  conversion from m3 to l                        [m3/l]
        ! MC             R*4 1 L  from gC to mol C (molar weight)                [gC/mol]
        ! MCO2           R*4 1 L  from gCO2 to mol CO2 (molar weight)            [gCO2/mol]
        ! MHCO3          R*4 1 L  fron gHCO3 to mol HCO3 (molar weight)          [gHCO3/mol]
        ! MB             R*4 1 L  from gB to mol B (molar weight)                [gB/mol]
        ! K1             R*4 1 L  dissociation constant CO2-HCO3                 [mol/kg]
        ! K2             R*4 1 L  dissociation constant HCO3-CO3                 [mol/kg]
        ! KB             R*4 1 L  dissociation constant BOH3-BOH4                [mol/kg]
        ! KW             R*4 1 L  dissociation constant H2O                      [mol2/kg2]
        ! Kcal           R*4 1 L  solubility constant calcite                    [mol2/kg2]
        ! Karg           R*4 1 L  solubility constant aragonite                  [mol2/kg2]
        ! K0             R*4 1 L  solubility of CO2 in seawater                  [mol/(kg*atm)]
        ! PH             R*4 1 O  pH                                             [-]
        ! CO2            R*4 1 O  concentration CO2 in water                     [gCO2/m3]
        ! pCO2water      R*4 1 O  partial pressure of CO2 in water               [µ atm]
        ! HCO3           R*4 1 O  concentration of HCO3                          [gC/m3]
        ! CO3	           R*4 1 O  concentration of CO3                           [gC/m3]
        ! Satcal         R*4 1 O  saturation state of calcite                    [-]
        ! Satarg         R*4 1 O  saturation state of aragonite                  [-]
        ! BOH4           R*4 1 O  concentration of borate		             [gB/m3]
        ! S              R*4 1 I  salinity                                       [g/kg]
        ! TEMP           R*4 1 I  temperature                                    [oC]
        ! TEMPK          R*4 1 L  temperature in Kelvin                          [K]
        ! RHOH2O         R*4 1 L  water density                                  [kg/l]
        ! CM3TOM3        R*4 1 L  conversion from cm3 to m3                      [cm3/m3]
        ! ATMTOMICROATM  R*4 1 L  conversion from atm to µ atm                   [atm/µ atm]
        ! ATMTOPA        R*4 1 L  conversion from atm to Pa                      [atm/Pa]
        ! R              R*4 1 L  ideal gas constant                             [m3*Pa/(K*mol)]
        ! MOLKGCO2       R*4 1 L  CO2 concentration in water                     [molCO2/kg]
        ! FCO2           R*4 1 L  fugacity of CO2                                [µ atm]
        ! BV             R*4 1 L  virial coefficient of pure CO2                 [m3/mol]
        ! DELTA          R*4 1 L  virial coefficient of CO2 in air               [m3/mol]
        !-----------------------------------------------------------------------

        !     Logical Units : -

        !     Modules called : -
        !
        use m_logger_helper
        USE MOD_CHEMCONST
        USE MOD_ACBW_PHSOLVERS
        USE PHYSICALCONSTS, ONLY : CtoKelvin

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        DOUBLE PRECISION AHPLUSD, P_VAL

        INTEGER(kind = int_wp) :: ILUMON
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, ISEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14

        LOGICAL, SAVE :: FIRST = .TRUE.
        !
        !     Local declarations, constants in source
        !
        REAL(kind = real_wp), PARAMETER :: MC = 12.0
        REAL(kind = real_wp), PARAMETER :: MCO2 = 44.0
        REAL(kind = real_wp), PARAMETER :: MHCO3 = 61.0
        REAL(kind = real_wp), PARAMETER :: MB = 10.8
        REAL(kind = real_wp), PARAMETER :: MTOMM = 1.0E+3
        REAL(kind = real_wp), PARAMETER :: MMTOM = 1.0E-3
        REAL(kind = real_wp), PARAMETER :: M3TOL = 1.0E+3
        REAL(kind = real_wp), PARAMETER :: CM3TOM3 = 1.0E-6
        REAL(kind = real_wp), PARAMETER :: ATMTOMICROATM = 1.0E+6
        REAL(kind = real_wp), PARAMETER :: ATMTOPA = 101325.0
        REAL(kind = real_wp), PARAMETER :: KELVIN = real(CtoKelvin)
        REAL(kind = real_wp), PARAMETER :: R = 8.314

        REAL(kind = real_wp) :: SAL, TEMP, TIC, ALKA, PH_MIN, PH_MAX
        REAL(kind = real_wp) :: PH_OLD, TEMPK, LNKW, KW
        REAL(kind = real_wp) :: LNK0, K0, LNK1, K1, LNK2, K2, LNKB, KB
        REAL(kind = real_wp) :: LOGCAL, KCAL, LOGARG, KARG
        REAL(kind = real_wp) :: RHOH2O, TICM, ALK, BT, CA
        REAL(kind = real_wp) :: AHPLUS, PH, BV, DELTA, FCO2, MOLKGCO2
        REAL(kind = real_wp) :: CO2, pCO2water, HCO3, CO3, BOH4, SATCAL, SATARG

        integer(kind = int_wp), save :: nr_mes = 0     ! message count negative total carbonate
        integer(kind = int_wp), save :: nrmes2 = 0     ! message count negative salinity
        integer(kind = int_wp), save :: nrmes3 = 0     ! message count high salinity
        integer(kind = int_wp), save :: nrmes4 = 0     ! message count negative alkalinity
        integer(kind = int_wp), save :: nrmes5 = 0     ! message count negative H+
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
        !
        !     Loop over de segmenten
        !
        DO ISEG = 1, num_cells
            !
            !     Eerste kenmerk actief of inactief segment
            !
            !     Alleen actieve en bodem segmenten behandelen
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     Map process_space_real on local variables
                !
                SAL = process_space_real(IP1)
                TIC = process_space_real(IP2)
                ALKA = process_space_real(IP3)
                TEMP = process_space_real(IP4)
                PH_MIN = process_space_real(IP5)
                PH_MAX = process_space_real(IP6)

                !     Try to get the old value, this is a good initial value for our solvers.
                PH_OLD = process_space_real(IP7)
                !     Because the value might not exist, if it is outside the range start neutral with pH = 7.0
                IF (PH_OLD<PH_MIN .OR. PH_OLD>PH_MAX) THEN
                    PH_OLD = 7.0e0
                ENDIF
                !
                !     Error messages

                IF (TIC < 1E-30) THEN
                    CALL get_log_unit_number(ILUMON)
                    IF (NR_MES < 10) THEN
                        NR_MES = NR_MES + 1
                        WRITE (ILUMON, *) 'WARNING :total carbonate <= 0', &
                                ' segment=', ISEG, ' conc=', TIC
                    ENDIF
                    IF (NR_MES == 10) THEN
                        NR_MES = NR_MES + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on total carbonate'
                        WRITE(ILUMON, *) ' No further messages on total carbonate'
                    ENDIF
                    TIC = 1E-30
                ENDIF
                IF (SAL < 1E-30) THEN
                    CALL get_log_unit_number(ILUMON)
                    IF (NRMES2 < 10) THEN
                        NRMES2 = NRMES2 + 1
                        WRITE (ILUMON, *) 'WARNING :salinity <= 0', &
                                ' segment=', ISEG, ' conc=', SAL
                    ENDIF
                    IF (NRMES2 == 10) THEN
                        NRMES2 = NRMES2 + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on salinity'
                        WRITE(ILUMON, *) ' No further messages on salinity'
                    ENDIF
                    SAL = 1E-30
                ENDIF
                IF (SAL > 50.) THEN
                    CALL get_log_unit_number(ILUMON)
                    IF (NRMES4 < 10) THEN
                        NRMES4 = NRMES4 + 1
                        WRITE (ILUMON, *) 'WARNING :salinity => 50.', &
                                ' segment=', ISEG, ' conc=', SAL
                    ENDIF
                    IF (NRMES4 == 10) THEN
                        NRMES4 = NRMES4 + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on salinity'
                        WRITE(ILUMON, *) ' No further messages on salinity'
                    ENDIF
                    SAL = 50.
                ENDIF
                IF (ALKA < 1E-30) THEN
                    CALL get_log_unit_number(ILUMON)
                    IF (NRMES3 < 10) THEN
                        NRMES3 = NRMES3 + 1
                        WRITE (ILUMON, *) 'WARNING: alkalinity <= 0', &
                                ' segment=', ISEG, ' conc=', ALKA
                    ENDIF
                    IF (NRMES3 == 10) THEN
                        NRMES3 = NRMES3 + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on alkalinity'
                        WRITE(ILUMON, *) ' No further messages on alkalinity'
                    ENDIF
                    ALKA = 1E-30
                ENDIF
                IF (TEMP <= -KELVIN) THEN
                    WRITE (ILUMON, *) ' WARNING: Temperature drops below 0 Kelvin', &
                            ' segment=', ISEG, ' Temp set to 15 oC (288.15 K)'
                    TEMP = 15.0E0
                ENDIF
                !
                !---- Process formulation ---------------------------------------
                ! ********************************
                ! Dissociation constants depending on temperature and salinity
                TEMPK = TEMP + KELVIN

                ! Dissociation constant of water. DOE (1994), Zeebe and Wolf-Gladrow (2001). Total pH scale. [mol^2/kg^2 solution]
                LNKW = 148.96502 - 13847.26 / TEMPK - 23.6521 * log(TEMPK) + &
                        (118.67 / TEMPK - 5.977 + 1.0495 * log(TEMPK)) * &
                                SAL**0.5 - 0.01615 * SAL

                KW = exp (LNKW)

                IF (SAL < 5.0e0) THEN
                    ! Dissociation constant of carbonic acid. Roy et al (1993). Total pH scale. [mol/kg H2O] (Molality)
                    ! Roy et al (1993). Salinities below 5 (freshwater). Total pH scale.
                    LNK1 = 290.9097 - 14554.21 / TEMPK - 45.0575 * log(TEMPK) + &
                            (-228.39774 + 9714.36839 / TEMPK + 34.485796 * log(TEMPK)) * &
                                    SAL**0.5 + (54.20871 - 2310.48919 / TEMPK - &
                            8.19515 * log(TEMPK)) * SAL + (-3.969101 + 170.22169 / TEMPK + &
                            0.603627 * log(TEMPK)) * SAL**1.5 - 0.00258768 * SAL**2.0

                    ! --- Unit of K1 and K2 [mol/kg H2O] (Molality). To convert to [mol/kg solution] a fraction is added (Millero 1995)
                    LNK1 = LNK1 + log(1.0E0 - SAL * 0.001005)

                    K1 = exp (LNK1)

                    ! Roy et al (1993). Salinities below 5 (freshwater). Total pH scale.
                    LNK2 = 207.6548 - 11843.79 / TEMPK - 33.6485 * log(TEMPK) + &
                            (-167.69908 + 6551.35253 / TEMPK + 25.928788 * log(TEMPK)) * &
                                    SAL**0.5 + (39.75854 - 1566.13883 / TEMPK - &
                            6.171951 * log(TEMPK)) * SAL + (-2.892532 + 116.270079 / TEMPK &
                            + 0.45788501 * log(TEMPK)) * SAL**1.5 - 0.00613142 * SAL**2.0

                    LNK2 = LNK2 + log(1.0E0 - SAL * 0.001005)

                    K2 = exp (LNK2)

                ELSE

                    ! Roy et al (1993). Salinities 5-45 (seawater). Total pH scale.
                    LNK1 = 2.83655 - 2307.1266 / TEMPK - 1.5529413 * log(TEMPK) + &
                            (-0.20760841 - 4.0484 / TEMPK) * SAL**0.5 + 0.08468345 * SAL - &
                            0.00654208 * SAL**1.5

                    LNK1 = LNK1 + log(1.0E0 - SAL * 0.001005)

                    K1 = exp (LNK1)

                    ! Roy et al (1993). Salinities 5-45 (seawater). Total pH scale.
                    LNK2 = -9.226508 - 3351.6106 / TEMPK - 0.2005743 * log(TEMPK) + &
                            (-0.106901773 - 23.9722 / TEMPK) * SAL**0.5 + 0.1130822 * SAL - &
                            0.00846934 * SAL**1.5

                    LNK2 = LNK2 + log(1.0E0 - SAL * 0.001005)

                    K2 = exp (LNK2)

                END IF

                ! Dissociation constant of boric acid. Dickson (1990). [mol/kg solution]
                ! Dickson (1990). Salinities 5-45 (seawater). Total pH scale.
                LNKB = (-8966.90 - 2890.53 * SAL**0.5 - 77.942 * SAL + &
                        1.728 * SAL**1.5 - 0.0996 * SAL**2) / TEMPK + (148.0248 + &
                        137.1942 * SAL**0.5 + 1.62142 * SAL) + (-24.4344 - &
                        25.085 * SAL**0.5 - 0.2474 * SAL) * log(TEMPK) + &
                        0.053105 * SAL**0.5 * TEMPK

                KB = exp (LNKB)

                ! Solubility constants of calcium carbonate.  Mucci (1983), Millero (1995). [mol^2/kg^2 solution]
                ! Solubility constant of calcite
                LOGCAL = -171.9065 - 0.077993 * TEMPK + 2839.319 / TEMPK + &
                        71.595 * log10(TEMPK) + (-0.77712 + 0.0028426 * TEMPK + &
                        178.34 / TEMPK) * SAL**0.5 - 0.07711 * SAL + 0.0041249 * &
                        SAL**1.5

                Kcal = 10**LOGCAL

                ! Solubility constant of aragonite
                LOGARG = -171.945 - 0.077993 * TEMPK + 2903.293 / TEMPK + &
                        71.595 * log10(TEMPK) + (-0.068393 + 0.0017276 * &
                        TEMPK + 88.135 / TEMPK) * SAL**0.5 - 0.10018 * SAL + &
                        0.0059415 * SAL**1.5

                Karg = 10**LOGARG

                ! Solubility of CO2 in seawater. Weiss (1974). [mol/(kg*atm)].
                LNK0 = -60.2409 + 93.4517 / (TEMPK / 100.0) + 23.3585 * &
                        log(TEMPK / 100.0) + SAL * (0.023517 - 0.023656 * (TEMPK / 100.0) + &
                        0.00447036 * (TEMPK / 100.0)**2)

                K0 = exp (LNK0)

                ! ******************************
                ! Conversion from [g/m3] to [mmol/kg solution]
                ! Density seawater [kg/l]
                RHOH2O = (1000. + 0.7 * SAL / (1.0 - SAL / 1000.) &
                        - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)) / 1000.

                ! Total inorganic carbon [mmol/kg solution]
                TICM = MTOMM * (TIC / M3TOL / MC / RHOH2O)

                ! Alkalinity [mmol/kg solution]
                ALK = MTOMM * (ALKA / M3TOL / MHCO3 / RHOH2O)

                ! Total borate. Millero (1982), Millero (1985) [mmol/kg solution]
                BT = MTOMM * 0.000416 * (SAL / 35.0)

                ! Calcium concentration. Millero (1982) [mmol/kg solution]
                Ca = MTOMM * 0.01028 * (SAL / 35.0)

                !
                ! Solve using solvesaphe from Munhoven (2013)
                !

                ! Set temperature and salinity
                CALL SETUP_API4PHTOT(DBLE(TEMPK), DBLE(SAL), 1.0D0)
                ! First try the fast poly solver
                AHPLUS = real(SOLVE_ACBW_POLYFAST(DBLE(ALK), DBLE(TICM), DBLE(BT)))
                IF (AHPLUS < 0.0d0) THEN
                    ! If not succesfull try the normal poly solver
                    AHPLUS = real(SOLVE_ACBW_POLY(DBLE(ALK), DBLE(TICM), DBLE(BT)))
                    IF (AHPLUS < 0.0d0) THEN
                        ! If still not succesfull use the robust solver
                        AHPLUS = real(SOLVE_ACBW_GENERAL(DBLE(ALK), DBLE(TICM), DBLE(BT)))
                    ENDIF
                ENDIF
                PH = -LOG10(AHPLUS)
                PH = MAX(PH_MIN, PH)
                PH = MIN(PH_MAX, PH)
                AHPLUS = 10.**(-PH)


                ! ---- SPECCARB ----
                ! ---- Calculation of CO2 ----
                CO2 = ((TICM * MMTOM * AHPLUS**2) / (AHPLUS**2 + K1 * AHPLUS + K1 * K2)) &
                        * MCO2 * RHOH2O * M3TOL

                ! ---- Calculation of pCO2 ----
                ! CO2 conversion from gCO2/m3 to molCO2/kg
                MOLKGCO2 = CO2 / (MCO2 * RHOH2O * M3TOL)

                ! Calculation of fugacity of CO2. Dickson (2007). [µ atm]
                FCO2 = (MOLKGCO2 / K0) * ATMTOMICROATM

                ! Calculation of virial coefficient of pure CO2. Weiss (1974). [m3/mol]
                BV = (-1636.75 + 12.0408 * TEMPK - 0.0327957 * TEMPK**2 + 3.16528 * 1.0E-5 * TEMPK**3) &
                        * CM3TOM3

                ! Calculation of virial coefficient of CO2 in air. Weiss (1974). [m3/mol]
                DELTA = (57.7 - 0.118 * TEMPK) * CM3TOM3

                ! Calculation of pCO2 in water. Dickson 2007. [µatm]
                pCO2water = FCO2 / (EXP(ATMTOPA * (BV + 2.0 * (DELTA)) / (R * TEMPK)))

                ! ---- Calculation of HCO3 ----
                HCO3 = ((TICM * MMTOM * K1 * AHPLUS) / (AHPLUS**2 + K1 * AHPLUS + K1 * K2)) &
                        * MC * RHOH2O * M3TOL

                ! ---- Calculation of CO3 ----
                CO3 = ((TICM * MMTOM * K1 * K2) / (AHPLUS**2 + K1 * AHPLUS + K1 * K2)) &
                        * MC * RHOH2O * M3TOL

                ! ---- SPECBOR ----
                ! ---- Calculation of BOH4 ----
                BOH4 = (BT * MMTOM * KB / (AHPLUS + KB)) &
                        * MB * RHOH2O * M3TOL

                ! ---- Saturation state of calcium carbonate ----
                ! ---- Calculation of saturation state of calcite ----
                Satcal = (Ca * MMTOM * CO3 / Kcal) / M3TOL / MC / RHOH2O

                ! ---- Calculation of saturation state of aragonite ----
                Satarg = (Ca * MMTOM * CO3 / Karg) / M3TOL / MC / RHOH2O

                !
                !---- Output --------------------
                !
                process_space_real(IP7) = PH
                process_space_real(IP8) = CO2
                process_space_real(IP9) = pCO2water
                process_space_real(IP10) = HCO3
                process_space_real(IP11) = CO3
                process_space_real(IP12) = Satcal
                process_space_real(IP13) = Satarg
                process_space_real(IP14) = BOH4
                !
            ENDIF
            !
            !---- Pointers ophogen ( altijd buiten de if's op kenmerk) in de segment loop
            !
            10    IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            IP13 = IP13 + INCREM (13)
            IP14 = IP14 + INCREM (14)
            !
        end do
        !
        !
        RETURN
    END

end module m_phcarb
