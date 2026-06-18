!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

!!        IMPLICIT REAL (A-H, J-Z)
!!        IMPLICIT INTEGER (I)

        REAL     process_space_real  (*), FL    (*)
        INTEGER  IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local
        !
        real, PARAMETER        :: PI = 3.14159265
        INTEGER(kind = int_wp) :: num_exchanges

        integer(kind = int_wp) :: iseg, iq, iflux
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, &
                                  ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, &
                                  in14, in18, ivan
        real                   :: v0sed, susp, crsups, n, temp, sedtc, sal, maxsal, enhfac, &
                                  pom, pom_crit, pom_exp, flofun, salfun, pomfun, temfun, vsed, &
                                  fpom, crsusp
        logical :: usepom

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)   ! Use POM influence
        IP11 = IPOINT(11)   ! Critical POM concentration
        IP12 = IPOINT(12)   ! POM exponent
        IP13 = IPOINT(13)   ! POM from probably the WKCOMP process
        IP14 = IPOINT(14)   ! Settling velocity
        IP15 = IPOINT(15)   ! Salinity factor
        IP16 = IPOINT(16)   ! Flocculation factor
        IP17 = IPOINT(17)   ! Organic material factor

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

                usepom   = ( process_space_real(IP10) == 1.0 )
                pom_crit = process_space_real(IP11)
                pom_exp  = process_space_real(IP12)
                pom      = process_space_real(IP13)

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

                !     Effect of organic matter

                fpom = 1.0
                if ( usepom ) then
                    fpom = 1.0 + ( pom / pom_crit ) ** pom_crit
                endif

                !     Bereken VSED
                VSED = V0SED * TEMFUN * SALFUN * FLOFUN * fpom

                !     Output of calculated sedimentation rate
                process_space_real (IP14) = VSED
                process_space_real (IP15) = SALFUN
                process_space_real (IP16) = FLOFUN
                process_space_real (IP17) = fpom
                !
                !     ENDIF
            ENDIF

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
            IP13 = IP13 + INCREM (13)
            IP14 = IP14 + INCREM (14)
            IP15 = IP15 + INCREM (15)
            IP16 = IP16 + INCREM (16)
            IP17 = IP17 + INCREM (17)

        end do

        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

        IP14 = IPOINT(14)
        IN14 = INCREM(14)
        IP18 = IPOINT(18)
        IN18 = INCREM(18)

        DO IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir

            process_space_real(IP18) = 0.0

            IP18 = IP18 + IN18

        end do

        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges

            IVAN = IEXPNT(1, IQ)
            !
            !        Sedimentation velocity from segment to exchange-area
            !
            IF (IVAN > 0) THEN
                process_space_real(IP18) = process_space_real(IP14 + (IVAN - 1) * IN14)
            ENDIF

            IP18 = IP18 + IN18

        end do

        RETURN

    END

end module m_calsed
