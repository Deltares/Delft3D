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
module m_calsed
    use m_waq_precision

    implicit none

contains


    subroutine calsed (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message

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

        REAL     process_space_real  (*), FL    (*)
        INTEGER  IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local
        !
        PARAMETER (PI = 3.14159265)
        INTEGER(kind = int_wp) :: num_exchanges
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
        DO ISEG = 1, num_cells
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                V0SED = process_space_real(IP1)
                SUSP = MAX (process_space_real(IP2), 0.0)
                CRSUSP = process_space_real(IP3)
                N = process_space_real(IP4)
                TEMP = process_space_real(IP5)
                SEDTC = process_space_real(IP6)
                SAL = MAX (process_space_real(IP7), 0.0)
                MAXSAL = process_space_real(IP8)
                ENHFAC = process_space_real(IP9)

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
                process_space_real (IP10) = VSED
                process_space_real (IP11) = SALFUN
                process_space_real (IP12) = FLOFUN
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

        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

        IP10 = IPOINT(10)
        IN10 = INCREM(10)
        IP13 = IPOINT(13)
        IN13 = INCREM(13)

        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

            process_space_real(IP13) = 0.0

            IP13 = IP13 + IN13

        end do

        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges

            IVAN = IEXPNT(1, IQ)
            !
            !        Sedimentation velocity from segment to exchange-area
            !
            IF (IVAN > 0) THEN
                process_space_real(IP13) = process_space_real(IP10 + (IVAN - 1) * IN10)
            ENDIF

            IP13 = IP13 + IN13

        end do

        RETURN
        !
    END

end module m_calsed
