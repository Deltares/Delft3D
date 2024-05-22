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
module m_calsed
    use m_waq_precision

    implicit none

contains


    subroutine calsed (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger, only : write_error_message

        !>\file
        !>       Sedimentation velocity IMx, DetC OOC, BODC, all algea = f (Temp SS Sal)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SEDIMENTATION VELOCITY BASED ON TEMP, SUSPENDED SOLID CONC AND
        !        SALINITY
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! CRSUSP  R*4 1 I  critical susp solid conc. for flocculation     [gDM/m3]
        ! N       R*4 1 I  coefficient in sedimentation formulation            [-]
        ! SUSP    R*4 1 I  total suspended solid concentration            [gDM/m3]
        ! SEDTC   R*4 1 I  temperature coefficient for sedimentation           [-]
        ! TEMP    R*4 1 I  ambient temperature                             [gradC]
        ! V0SED   R*4 1 I  sedimentaion velocity (no temp, sal, ss influence)[m/d]
        ! SAL     R*4 1 I  salinity                                         [g/kg]
        ! MAXSAL  R*4 1 I  salinity where salinity function is at max       [g/kg]
        ! ENHFAC  R*4 1 I  enhancement factor in salinity functin              [-]
        ! SALFUN  R*4 1 I  salinity function on sedimentation velocity         [-]
        ! FLOFUN  R*4 1 I  flocculation function on sedimentation velocity     [-]
        ! TEMFUN  R*4 1 I  temperature function on sedimentation velocity      [-]
        ! VSED    R*4 1 I  sedimentaion velocity, temp, sal, ss corrected    [m/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL     PMSA  (*), FL    (*)
        INTEGER  IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local
        !
        PARAMETER (PI = 3.14159265)
        INTEGER(kind = int_wp) :: NOQ
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
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                V0SED = PMSA(IP1)
                SUSP = MAX (PMSA(IP2), 0.0)
                CRSUSP = PMSA(IP3)
                N = PMSA(IP4)
                TEMP = PMSA(IP5)
                SEDTC = PMSA(IP6)
                SAL = MAX (PMSA(IP7), 0.0)
                MAXSAL = PMSA(IP8)
                ENHFAC = PMSA(IP9)

                IF (CRSUSP < 1E-20)  CALL write_error_message ('CRSUSP in CALSED zero')

                !*******************************************************************************
                !**** Processes connected to the sedimentation VELOCITY
                !***********************************************************************


                !     Initialisatie
                FLOFUN = 1.0
                SALFUN = 1.0
                TEMFUN = 1.0

                !     Flocculatie functie

                IF (SUSP / CRSUSP >= 1.E-30) THEN
                    FLOFUN = (SUSP / CRSUSP)**N
                ENDIF

                !     Temperatuur functie

                IF (SEDTC /= 1.0) THEN
                    TEMFUN = SEDTC **(TEMP - 20.0)
                ENDIF

                !     Salinity functie

                IF (SAL < MAXSAL) THEN
                    SALFUN = (ENHFAC + 1.) / 2. - ((ENHFAC - 1.) / 2.) * COS(PI * SAL / MAXSAL)
                ELSEIF (MAXSAL >= 0.0) THEN
                    SALFUN = ENHFAC
                ELSE
                    SALFUN = 1.0
                ENDIF

                !     Bereken VSED
                VSED = V0SED * TEMFUN * SALFUN * FLOFUN

                !     Output of calculated sedimentation rate
                PMSA (IP10) = VSED
                PMSA (IP11) = SALFUN
                PMSA (IP12) = FLOFUN
                !
                !     ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
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
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            !
        end do
        !

        NOQ = NOQ1 + NOQ2 + NOQ3

        IP10 = IPOINT(10)
        IN10 = INCREM(10)
        IP13 = IPOINT(13)
        IN13 = INCREM(13)

        DO IQ = 1, NOQ1 + NOQ2

            PMSA(IP13) = 0.0

            IP13 = IP13 + IN13

        end do

        DO IQ = NOQ1 + NOQ2 + 1, NOQ

            IVAN = IEXPNT(1, IQ)
            !
            !        Sedimentation velocity from segment to exchange-area
            !
            IF (IVAN > 0) THEN
                PMSA(IP13) = PMSA(IP10 + (IVAN - 1) * IN10)
            ENDIF

            IP13 = IP13 + IN13

        end do

        RETURN
        !
    END

end module m_calsed
