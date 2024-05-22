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
module m_satco2
    use m_waq_precision

    implicit none

contains


    subroutine satco2 (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Saturation concentration carbon dioxide

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        COMPUTATION OF CARBON DIOXIDE SATURATION CONCENTRATION
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CL2     R*4 1 I concentration of chloride                        [kg/m3]
        ! CO2SAT  R*4 1 0 saturation concentration                          [g/m3]
        ! PAPCO2  R*4 1 0 partial CO2 pressure                             [g/m3]
        ! SAL     R*4 1 I Salinity                                           [ppt]
        ! SWITCH  I*4 1 I Switch for formulation options                       [-]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        use m_logger, only : terminate_execution, get_log_unit_number
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: SWITCH, LUNREP, ISEG, IP1, &
                IP2, IP3, IP4, IP5, IP6
        REAL(kind = real_wp) :: CL2, TEMP, TEMPA, SAL, PAPCO2, &
                RION, RKCO2, TEMP2, PART1, PART2, &
                CO2SAT, A1, A2, A3, B1, &
                B2, B3
        PARAMETER (A1 = -58.0931, &
                A2 = 90.5069, &
                A3 = 22.2940, &
                B1 = 0.027766, &
                B2 = -0.025888, &
                B3 = 0.0050578)
        !

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        !
        DO ISEG = 1, NOSEG

            CL2 = PMSA(IP1) / 1000.
            TEMP = PMSA(IP2)
            SWITCH = NINT(PMSA(IP3))
            SAL = PMSA(IP4)
            PAPCO2 = PMSA(IP5)

            IF (SWITCH == 1) THEN

                ! === REAERATION CO2 ==================================================
                !
                !     SATURATION CONCENTRATION CO2 = PARTIAL PRESSURE CO2 IN ATMOSPHERE
                !     (ASSUMED MOL/L)              * REACTION CONSTANT
                !
                !     PARTIAL PRESSURE = 10**-3.5 ATM (PAG. 180)
                !
                !     REACTION CONSTANT KCO2 = FUNCTION (ABS.TEMPERATURE,CHLORINITY)
                !     ABS. TEMP  = 273.15 + TEMPD (MODEL TEMP. IN DEGREES CELSIUS)
                !     CHLORINITY = 0.001*CL (CL IS MODEL CONC. CL- IN MG/L)
                !
                !     REF.: AQUATIC CHEMISTRY,  STUMM & MORGAN, WILEY & SONS, 1981
                !
                ! =====================================================================

                TEMPA = TEMP + real(CtoKelvin)
                RION = 0.147E-02 + 0.3592E-01 * CL2 + 0.68E-04 * CL2**2
                RKCO2 = 10.0**(-(- 0.238573E+04 / TEMPA + 0.140184E+02 - &
                        0.152642E-01 * TEMPA + RION * (0.28569 - 0.6167E-05 * TEMPA)))

            ELSEIF (SWITCH == 2) THEN
                !
                !        Weiss volgen Monteiro (CISR)
                !
                TEMP2 = (TEMP + 273.) / 100.
                PART1 = A1 + A2 / TEMP2 + A3 * LOG(TEMP2)
                PART2 = SAL * (B1 + B2 * TEMP2 + B3 * TEMP2 * TEMP2)
                RKCO2 = EXP(PART1 + PART2)
                !
            ELSE
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) 'ERROR in SATCO2'
                WRITE(LUNREP, *) 'Illegal option for CO2 saturation formula'
                WRITE(LUNREP, *) 'Option in input:', SWITCH
                WRITE(*, *) ' ERROR in SATCO2'
                WRITE(*, *) ' Illegal option for CO2 saturation formula'
                WRITE(*, *) ' Option in input:', SWITCH
                CALL terminate_execution(1)
            ENDIF

            !     Output of calculated saturation

            CO2SAT = PAPCO2 * RKCO2 * 1000. * 44.
            PMSA (IP6) = CO2SAT
            !
            !jvb  ENDIF
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            !
        end do
        !
        RETURN
        !
    END

end module m_satco2
