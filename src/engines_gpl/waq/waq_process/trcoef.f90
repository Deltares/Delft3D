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
module m_trcoef
    use m_waq_precision

    implicit none

contains


    subroutine trcoef (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Gas and liquid exchange organic micro pollutants (Lyman and O'Conner)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Calculation of transport coefficients for air-water exhchange
        !        as a function of:
        !        sw=0: wind, veloc and molmass (Chemical estimates, Lyman)
        !        sw=1: veloc, diffusion coefficients (IMPAQT)= O'Conner formulas
        !
        ! Name    T   L I/O   Description                                   Uni
        ! ----    --- -  -    -------------------                            --
        ! SWITCH  R*4 1 I  Switch for calculation method                     [-]
        ! WIND    R*4 1 I  Windspeed                                       [m/s]
        ! VELOC   R*4 1 I  Flow velocity                                   [m/s]
        ! M       R*4 1 I  Molecuulmassa omive                           [g/mol]
        ! LDIF    R*4 1 O  molekular diffusion coefficient waterphase     [m2/d]
        ! GDIF    R*4 1 O  molekular diffusion coefficient gasphase       [m2/d]
        ! KL      R*4 1 O  mass transport coefficient waterphase           [m/d]
        ! KG      R*4 1 O  mass transport coefficient gasphase             [m/d]
        ! VL      R*4 1 LC viscosity waterphase                    [Pa/s=kg/m/s]
        ! VG      R*4 1 LC viscosity gashase                       [Pa/s=kg/m/s]
        ! RHOL    R*4 1 LC density waterphase                            [kg/m3]
        ! RHOG    R*4 1 LC density gasphase                              [kg/m3]
        ! TEMP    R*4 1 I  Temperatuur
        ! DEPTH   R*4 1 I  Diepte
        !-----------------------------------------------------------------------

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        use m_logger_helper
        use m_evaluate_waq_attribute
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        LOGICAL  WOROPT, WNDOPT, TMPOPT
        !
        !     Local declarations, constants in source
        !
        PARAMETER (C1 = 18., &
                C2 = 5.64, &
                C3 = 0.969, &
                C4 = 0.673, &
                C5 = 32., &
                C6 = 0.526, &
                C7 = 0.7, &
                CRIT1 = 1.9, &
                CRIT2 = 5.0, &
                E = 2.718, &
                KELVIN = real(CtoKelvin), &
                VCMIN = 0.001, &
                C11 = 1.293, &
                C12 = 0.00367, &
                C13 = 1000.0, &
                C14 = 0.088, &
                C15 = 1.32, &
                C16 = 0.009, &
                C17 = 1.E-5, &
                C18 = 0.001, &
                C21 = 0.01, &
                C22 = 6.1, &
                C23 = 0.63, &
                C24 = 0.001, &
                C25 = 0.0463, &
                C26 = 0.67, &
                C27 = 1.E-6, &
                C28 = 0.0144, &
                C29 = 0.00341, &
                C30 = 2.2, &
                CRIT3 = 0.3)
        !
        IN2 = INCREM(2)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
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
        !
        EXP1 = EXP (C6 * (CRIT2 - CRIT1))
        IF (IN2 == 0) THEN
            WIND = PMSA(IP2)
            IF(WIND  < 0.0) CALL write_error_message ('WIND       in TRCOEF < 0')
            IF (WIND >= CRIT1) THEN
                IF (WIND < CRIT2) THEN
                    EXP2 = EXP (C6 * (WIND - CRIT1))
                ELSE
                    FAC1 = (1. + (WIND - CRIT2)**C7) * EXP1
                ENDIF
            ENDIF
            !     Calculate wind at watersurface from wind at 10m (m/s)
            FWIND = C21 * WIND * SQRT(C22 + C23 * WIND)
            IF (FWIND < CRIT3) THEN
                FWIN2 = C28 * FWIND**C30
            ELSE
                FWIN2 = C29 * FWIND
            ENDIF
            WNDOPT = .FALSE.
        ELSE
            WNDOPT = .TRUE.
        ENDIF
        !
        IF (IN4 == 0) THEN
            M = PMSA(IP4)
            IF(M     < 1.E-30) CALL write_error_message ('MOLMASS    in TRCOEF = 0')
            WORTL1 = SQRT(C1 / M)
            WORTL5 = SQRT(C5 / M) * C2
            WOROPT = .FALSE.
        ELSE
            WOROPT = .TRUE.
        ENDIF
        !
        IF (IN5 == 0 .AND. IN6 == 0 .AND. IN8 == 0) THEN
            TEMP = PMSA(IP8)
            !--calculation of bulk densitys of water and air:
            RHOG = C11 / (1. + C12 * TEMP)
            RHOL = C13 - C14 * TEMP
            !--calculation of viscosities of water and air:
            VG = (C15 + C16 * TEMP) * C17
            VL = C18
            LDIF = PMSA(IP5)
            GDIF = PMSA(IP6)
            IF(GDIF  < 1.E-30) CALL write_error_message ('GAS-DIFF   in TRCOEF = 0')
            IF(LDIF  < 1.E-30) CALL write_error_message ('WATER-DIFF in TRCOEF = 0')
            !     Calculate Schmidt numbers for water and gas
            SCG = VG / (RHOG * GDIF / 86400.)
            SCL = VL / (RHOL * LDIF / 86400.)
            TMPOPT = .FALSE.
        ELSE
            TMPOPT = .TRUE.
        ENDIF
        !
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==1)) THEN
                    !
                    !     Map PMSA on local variables
                    !
                    ISWTCH = PMSA(IP1) + 0.5
                    !
                    IF (ISWTCH == 0) THEN
                        !
                        VELOC = PMSA(IP3)
                        IF(VELOC < 0.0) CALL write_error_message ('VELOC      in TRCOEF < 0')
                        DEPTH = PMSA(IP7)
                        !
                        IF (WNDOPT) THEN
                            WIND = PMSA(IP2)
                            IF(WIND  < 0.0) CALL write_error_message ('WIND       in TRCOEF < 0')
                            IF (WIND >= CRIT1) THEN
                                IF (WIND < CRIT2) THEN
                                    EXP2 = EXP (C6 * (WIND - CRIT1))
                                ELSE
                                    FAC1 = (1. + (WIND - CRIT2)**C7) * EXP1
                                ENDIF
                            ENDIF
                        ENDIF
                        IF(WIND  < 0.0) CALL write_error_message ('WIND       in TRCOEF < 0')
                        !
                        IF (WOROPT) THEN
                            M = PMSA(IP4)
                            IF(M     < 1.E-30) CALL write_error_message ('MOLMASS    in TRCOEF = 0')
                            WORTL1 = SQRT(C1 / M)
                            WORTL5 = SQRT(C5 / M) * C2
                        ENDIF
                        !
                        !     Ensure that VELOC is >= VCMIN
                        !
                        VELOC = MAX (VELOC, VCMIN)
                        !
                        !     Gasphase exchange coefficient
                        !
                        KG = KELVIN * (WIND + VELOC) * WORTL1
                        !
                        !     Water exchange coefficient
                        !
                        IF (WIND < CRIT1) THEN
                            KL = VELOC**C3 / DEPTH**C4 * WORTL5
                        ELSEIF (WIND < CRIT2) THEN
                            KL = VELOC**C3 / DEPTH**C4 * WORTL5 * EXP2
                        ELSE
                            KL = VELOC**C3 / DEPTH**C4 * WORTL5 * FAC1
                        ENDIF
                        !
                    ENDIF

                    IF (ISWTCH == 1) THEN
                        ! --- Impact formulations (O'connor personal communication?)
                        !
                        IF (WNDOPT) THEN
                            WIND = PMSA(IP2)
                            IF(WIND  < 0.0) CALL write_error_message ('WIND       in TRCOEF < 0')
                            !     Calculate wind at watersurface from wind at 10m (m/s)
                            FWIND = C21 * WIND * SQRT(C22 + C23 * WIND)
                            IF (FWIND < CRIT3) THEN
                                FWIN2 = C28 * FWIND**C30
                            ELSE
                                FWIN2 = C29 * FWIND
                            ENDIF
                        ENDIF
                        !
                        IF (TMPOPT) THEN
                            TEMP = PMSA(IP8)
                            !--calculation of bulk densitys of water and air:
                            RHOG = C11 / (1. + C12 * TEMP)
                            RHOL = C13 - C14 * TEMP
                            !--calculation of viscosities of water and air:
                            VG = (C15 + C16 * TEMP) * C17
                            VL = C18
                            ! --- Impact formulations (O'connor personal communication?)
                            LDIF = PMSA(IP5)
                            GDIF = PMSA(IP6)
                            IF(GDIF  < 1.E-30) CALL write_error_message ('GAS-DIFF   in TRCOEF = 0')
                            IF(LDIF  < 1.E-30) CALL write_error_message ('WATER-DIFF in TRCOEF = 0')
                            !     Calculate Schmidt numbers for water and gas
                            SCG = VG / (RHOG * GDIF / 86400.)
                            SCL = VL / (RHOL * LDIF / 86400.)
                        ENDIF
                        KG = (C24 + C25 * FWIND / SCG**C26) * 86400.
                        KL = (C27 + FWIN2 / SQRT(SCL)) * 86400.
                    ENDIF
                    !
                    !     Output
                    !
                    PMSA(IP9) = KL
                    PMSA(IP10) = KG
                    !
                ENDIF
            ENDIF
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
            IP10 = IP10 + INCREM (10)
            !
        end do
        !
        RETURN
        !
    END

end module m_trcoef
