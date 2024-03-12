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
module m_simph
    use m_waq_precision

    implicit none

contains


    subroutine simph  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Simple calculation of pH

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Simple calculation of pH, TESTVERSIE
        !        Restrictions: ...
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                             --
        ! AHPLUS  R*4 1 L  activity of H+                                [mol/l]
        ! ALKA    R*4 1 I  Alkalinity as HCO3-                          [mol/m3]
        ! CO2     R*4 1 O  concentration CO2 in water                  [gCO2/m3]
        ! CARBTO  R*4 1 I  total carbonate concentration                 [gC/m3]
        ! M3TOL   R*4 1 L  conversion from 1/m3 to 1/l                    [m3/l]
        ! MC      R*4 1 L  from gC/m3 to mol C/m3 (molar weight)         [g/mol]
        ! MCO2    R*4 1 L  from gCO2 to mol CO2 (molar weight)           [g/mol]
        ! MHCO3   R*4 1 L  fron gHCO3 to mol HCO3 (molar weight)         [g/mol]
        ! K1      R*4 1 L  dissociation constant CO2-HCO3                [mol/l]
        ! K2      R*4 1 L  dissociation constant HCO3-CO3                [mol/l]
        ! PH      R*4 1 O  pH                                         [pH units]
        ! SAL     R*4 1 I  salinity                                       [g/kg]
        ! TEMP    R*4 1 I  temperature                                      [oC]
        ! TEMpK   R*4 1 L  temperature in Kelvin                             [K]
        !-----------------------------------------------------------------------

        !     Logical Units : -

        !     Modules called : -
        !


        !     Name     Type   Library
        !     ------   -----  ------------
        use m_monsys
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declarations, constants in source
        !
        PARAMETER (MC = 12.0, MCO2 = 44.0, &
                MHCO3 = 61.0, M3TOL = 1.0E-3, KELVIN = real(CtoKelvin))
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
        !
        !     Loop over de segmenten
        !
        DO ISEG = 1, NOSEG
            !
            !     Eerste kenmerk actief of inactief segment
            !
            !     Alleen actieve en bodem segmenten behandelen
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     Map PMSA on local variables
                !
                SAL = PMSA(IP1)
                CARBTOT = PMSA(IP2)
                ALKA = PMSA(IP3)
                TEMP = PMSA(IP4)
                PH_MIN = PMSA(IP5)
                PH_MAX = PMSA(IP6)
                !
                !     Error messages

                IF (CARBTOT < 1E-30) THEN
                    CALL GETMLU(ILUMON)
                    IF (NR_MES < 10) THEN
                        NR_MES = NR_MES + 1
                        WRITE (ILUMON, *) 'WARNING :total carbonate <= 0', &
                                ' segment=', ISEG, ' conc=', CARBTOT
                    ENDIF
                    IF (NR_MES == 10) THEN
                        NR_MES = NR_MES + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on total carbonate'
                        WRITE(ILUMON, *) ' No further messages on total carbonate'
                    ENDIF
                    CARBTOT = 1E-30
                ENDIF
                IF (SAL < 1E-30) THEN
                    CALL GETMLU(ILUMON)
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
                    CALL GETMLU(ILUMON)
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
                    CALL GETMLU(ILUMON)
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
                    TEMP = 15
                ENDIF
                !
                !---- Procesformuleringen ---------------------------------------
                ! ********************************
                ! Dissociatieconstanten afhankelijk van temperatuur en saliniteit
                TEMPK = TEMP + KELVIN

                ! Roy et al (1993), Millero (1995)
                LNK1 = 290.9097 - 14554.21 / TEMPK - 45.0575 * log(TEMPK) + &
                        (-228.39774 + 9714.36839 / TEMPK + 34.485796 * log(TEMPK)) * &
                                SAL**0.5 + (54.20871 - 2310.48919 / TEMPK - &
                        8.19515 * log(TEMPK)) * SAL + (-3.969101 + 170.22169 / TEMPK + &
                        0.603627 * log(TEMPK)) * SAL**1.5 - 0.00258768 * SAL**2

                ! --- Unit of K1 and K2 [mol/kg H2O]. To convert to [mol/kg solution] a fraction is added (Millero 1995)
                LNK1 = LNK1 + log(1 - SAL * 0.001005)

                K1 = exp (LNK1)

                ! Roy et al (1993), Millero (1995)
                LNK2 = 207.6548 - 11843.79 / TEMPK - 33.6485 * log(TEMPK) + &
                        (-167.69908 + 6551.35253 / TEMPK + 25.928788 * log(TEMPK)) * &
                                SAL**0.5 + (39.75854 - 1566.13883 / TEMPK - &
                        6.171951 * log(TEMPK)) * SAL + (-2.892532 + 116.270079 / TEMPK &
                        + 0.45788501 * log(TEMPK)) * SAL**1.5 - 0.00613142 * SAL**2

                LNK2 = LNK2 + log(1 - SAL * 0.001005)

                K2 = exp (LNK2)

                ! ******************************
                ! Conversion of [g/m3] to [mol/kg solvent]
                ! Density seawater [kg/l]
                RHOH2O = (1000. + 0.7 * SAL / (1 - SAL / 1000.) &
                        - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)) / 1000

                CARBTOT = (CARBTOT * M3TOL) / MC / RHOH2O
                ALKA = (ALKA * M3TOL) / MHCO3 / RHOH2O


                ! Oplossing vierkantsvergelijking
                A = ALKA / K1
                B = ALKA - CARBTOT
                C = K2 * (ALKA - 2 * CARBTOT)
                D = B**2 - 4 * A * C
                IF (D < 0) THEN
                    CALL GETMLU(ILUMON)
                    WRITE (ILUMON, *) 'No solution for pH: discriminant<0'
                    goto 10
                ENDIF

                AHPLUS = (-B + SQRT (D)) / (2 * A)
                IF (AHPLUS <= 0) THEN
                    CALL GETMLU(ILUMON)
                    IF (NRMES5 < 10) THEN
                        NRMES5 = NRMES5 + 1
                        WRITE (ILUMON, *) 'WARNING: H+ negative: ', AHPLUS, ' in segment ', ISEG
                    ENDIF
                    IF (NRMES5 == 10) THEN
                        NRMES5 = NRMES5 + 1
                        WRITE(ILUMON, *) ' 10 WARNINGS on H+'
                        WRITE(ILUMON, *) ' No further messages on H+'
                    ENDIF
                    goto 10
                ENDIF
                PH = - log10 (AHPLUS)
                PH = MAX(PH_MIN, PH)
                PH = MIN(PH_MAX, PH)
                AHPLUS = 10.**(-PH)

                ! --- Berekening CO2 Concentratie ----
                CO2 = (CARBTOT / (1 + K1 / AHPLUS + K1 * K2 / AHPLUS**2)) &
                        * MCO2 * RHOH2O / M3TOL

                !
                !---- Output: voorzover van toepassing --------------------
                !
                PMSA(IP7) = PH
                PMSA(IP8) = CO2
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
            !
        end do
        !
        !
        RETURN
    END

end module m_simph
