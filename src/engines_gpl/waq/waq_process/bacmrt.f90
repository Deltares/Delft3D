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
module m_bacmrt
    use m_waq_precision

    implicit none

contains


    subroutine bacmrt (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Mortality of bacteria depending on UV-light, salinity and temperature

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        GENERAL ROUTINE FOR THE MORTALITY OF BACTERIA: A FIRST ORDER APPROACH
        !        WITH A USER DEFINED RATE CONSTANT CORRECTED FOR TEMPERATURE AND
        !        SALINITY. THE MORTALITY RATE IS HIGHTENED BY A LIGHT DEPENDANT PART
        !        CONCENTRATION OF BACTERIA EXPRESSED IN SOMETHING/M3
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! BACT    R*4 1 I concentration bacteria                              [gX]
        ! CFRAD   R*4 1 I conversion factor RAD->mortality                [m2/W/d]
        ! CRTEMP  R*4 1 I critical temperature for mortality                  [xC]
        ! MORT    R*4 1 L overall first order mortality rate                 [1/d]
        ! MRTRAD  R*4 1 O part of firt order mortality rate from RAD         [1/d]
        ! DEPTH   R*4 1 I water depth                                          [m]
        ! EXTVL   R*4 1 I extinction of visible light                        [1/m]
        ! FL (1)  R*4 1 O mortality flux                                  [X/m3/d]
        ! RAD     R*4 1 I solar radiation at surface (instantaneous)        [W/m2]
        ! RCMRT   R*4 1 I user defined first order mortality rate            [1/d]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! TEMPF   R*4 1 L temperature function                                 [-]
        ! TCMRT   R*4 1 I temperature coefficient for mortality              [1/d]
        ! VOLUME  R*4 1 L DELWAQ volume                                       [m3]
        ! CL      R*4 1 I chloride concentration                            [g/m3]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        REAL(kind = real_wp) :: BACT, RCMRT, TCMRT, TEMP, CRTEMP, CL, RAD, CFRAD, &
                EXTVL, DEPTH, SPMRTZ, TEMP20, TEMPF, MRTRAD, MORT

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
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                BACT = PMSA(IP1)
                RCMRT = PMSA(IP2)
                TCMRT = PMSA(IP3)
                TEMP = PMSA(IP4)
                CRTEMP = PMSA(IP5)
                CL = PMSA(IP6)
                RAD = PMSA(IP7)
                CFRAD = PMSA(IP8)
                EXTVL = PMSA(IP9)
                DEPTH = PMSA(IP10)
                SPMRTZ = PMSA(IP11)

                !***********************************************************************
                !**** Processes connected to the MORTALITY OF BACTERIA
                !***********************************************************************
                !
                !
                IF (TEMP <= CRTEMP) THEN
                    !
                    !        No mortality at all
                    !
                    FL(1 + IFLUX) = 0.0
                    !
                ELSE
                    !
                    !        Calculation of mortality flux ( M.L-3.t-1)
                    !
                    TEMP20 = TEMP - 20.0
                    TEMPF = TCMRT ** TEMP20

                    !        Calculation of the RAD dependent part of the mortality
                    IF (EXTVL > 0.0) THEN
                        MRTRAD = CFRAD * RAD * (1 - EXP(-EXTVL * DEPTH)) &
                                / (EXTVL * DEPTH)
                    ELSE
                        ! Limit case if extvl zero or negative
                        MRTRAD = CFRAD * RAD
                    ENDIF

                    !        Calculation of the overall mortality
                    MORT = (RCMRT + SPMRTZ * CL) * TEMPF + MRTRAD
                    !
                    FL (1 + IFLUX) = MORT * BACT
                    !
                ENDIF

                PMSA (IP12) = MORT
                PMSA (IP13) = MRTRAD
                !
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
            !
        end do
        !
        RETURN
        !
    END

end module m_bacmrt
