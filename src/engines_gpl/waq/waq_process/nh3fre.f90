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
module m_nh3fre
    use m_waq_precision

    implicit none

contains


    subroutine nh3fre (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Calculation conc. unionized ammonia

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                             --
        ! CNH3    R*4 1 L  molar conc NH3      ,,        ,,              [mol/l]
        ! CTNH4   R*4 1 I  total molar conc (gN/m3) NH4                  [gN/m3]
        ! FRNH3   R*4 1 O  fraction NH3 of total NH3 + NH4+              [mol/l]
        ! LKNH3   R*4 1 L  log equilibrium constant                 [log(mol/l)]
        ! LRATIO  R*4 1 L  log{(NH3)/(NH4+)} see eq. (e) below          [log(-)]
        ! M3TOL   R*4 1 L  conversion from 1/m3 to 1/l                    [m3/l]
        ! MNITRO  R*4 1 L  from gN/m3 NH3 or NH4+ to mol/m3             [gN/mol]
        ! KRF1A   R*4 1 I  coefficient a of reprofunction 1 KNH3    [log(mol/l)]
        ! KRF1B   R*4 1 I  coefficient a of reprofunction 1 KNH3 [log(mol/l)/oC]
        ! NH3     R*4 1 O  unionized ammonia                             [gN/m3]
        ! INH3SW  I*4 1 I  switch: option number calculation method KNH3     [-]
        ! PH      R*4 1 I  pH                                         [pH units]
        ! TEMP    R*4 1 I  temperature                                      [oC]
        ! TNH4    R*4 1 I  total concentration (gN/m3) NH4               [gN/m3]
        ! SAL     R*4 1 I  salinity                                       [g/kg]
        !
        !-----------------------------------------------------------------------
        !     Name     Type   Library
        !     ------   -----  ------------
        use m_write_error_message
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        integer(kind = int_wp) :: iseg
        PARAMETER (MNITRO = 14.0, &
                KELVIN = real(CtoKelvin), &
                M3TOL = 1.0E-3)
        !
        !     set pointers
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
        !
        !     Loop over de segmenten
        !
        DO ISEG = 1, NOSEG
            !
            !     Eerste kenmerk actief of inactief segment
            !
            !
            !     Alleen actieve segmenten behandelen ( KENMERK1 = 1 )
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     Map PMSA on local variables
                !
                INH3SW = NINT(PMSA(IP1))
                TNH4 = PMSA(IP2)
                PH = PMSA(IP3)
                TEMP = PMSA(IP4)
                TEMPK = TEMP + KELVIN
                KRF1A = PMSA(IP5)
                KRF1B = PMSA(IP6)
                SAL = MAX(0.0, PMSA(IP7))
                !
                !     Error messages
                !
                IF (TEMP <= -KELVIN) CALL &
                        write_error_message ('TEMP in NH3FREE < 0 KELVIN')
                !
                !---- Procesformuleringen ---------------------------------------
                NH3 = 0.0
                FRNH3 = 0.0

                !     Berekening alleen indien NH4 > 0

                IF (TNH4>1.E-15) THEN
                    CTNH4 = M3TOL * TNH4 / MNITRO
                    !
                    !        OPTION 1 -- log K berekenen
                    !
                    IF (INH3SW==1) THEN
                        LKNH3 = (KRF1A + KRF1B * TEMP)
                        LRATIO = LKNH3 + PH
                        CNH3 = CTNH4 * (10**LRATIO) / (1. + (10**LRATIO))
                        NH3 = CNH3 * MNITRO / M3TOL
                        FRNH3 = CNH3 / CTNH4
                        !
                        !        OPTION 2 ACCORDING TO MILLERO
                        !
                    ELSEIF (INH3SW == 2) THEN
                        LKNH3 = -6285.33 / TEMPK + 0.0001635 * TEMPK - 0.25444 + &
                                (0.46532 - 123.7184 / TEMPK) * SAL**0.5 + &
                                (-0.01992 + 3.17556 / TEMPK) * SAL

                        KNH3 = EXP (LKNH3)

                        ! --- Unit of KNH3 [mol/kg solution], so redefine NH4 in same unit
                        RHOH2O = (1000. + 0.7 * SAL / (1. - SAL / 1000.) &
                                - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)) / 1000

                        NH4 = TNH4 * M3TOL / MNITRO / RHOH2O

                        NH3 = NH4 / (1. + 10**(-PH) / KNH3)
                        NH3 = NH3 * RHOH2O * MNITRO / M3TOL

                        FRNH3 = NH3 / TNH4
                    ELSE
                        CALL write_error_message ('INH3SW in NH3FRE not 1 or 2')
                    ENDIF

                ENDIF

                PMSA(IP8) = NH3
                PMSA(IP9) = FRNH3
                !
            ENDIF
            !
            !---- Pointers ophogen ( altijd buiten de if's op kenmerk) in de segment loop
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            !
        end do
        !
        RETURN
    END

end module m_nh3fre
