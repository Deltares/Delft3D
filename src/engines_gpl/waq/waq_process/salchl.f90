module m_salchl
    use m_waq_precision

    implicit none

contains

    subroutine salchl (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Converts salinity into chloride or vice versa (Aquatic Chemistry 2nd ed 1981 p567)

        !----- GPL ---------------------------------------------------------------------
        !
        !  Copyright (C)  Stichting Deltares, 2011-2024.
        !
        !  This program is free software: you can redistribute it and/or modify
        !  it under the terms of the GNU General Public License as published by
        !  the Free Software Foundation version 3.
        !
        !  This program is distributed in the hope that it will be useful,
        !  but WITHOUT ANY WARRANTY; without even the implied warranty of
        !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        !  GNU General Public License for more details.
        !
        !  You should have received a copy of the GNU General Public License
        !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
        !
        !  contact: delft3d.support@deltares.nl
        !  Stichting Deltares
        !  P.O. Box 177
        !  2600 MH Delft, The Netherlands
        !
        !  All indications and logos of, and references to, "Delft3D" and "Deltares"
        !  are registered trademarks of Stichting Deltares, and remain the property of
        !  Stichting Deltares. All rights reserved.
        !
        !-------------------------------------------------------------------------------
        !
        !
        !-------------------------------------------------------------------------------
        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                           ----
        ! CL      R*4 1 I/O  chloride concentration                         [g/m3]
        ! SAL     R*4 1 I/O  salinity                                       [g/kg]
        ! SAL0    R*4 1 I    salinity at zero chloride                      [g/kg]
        ! GTCL    R*4 1 I    ratio of salinity and chloride                 [g/g]
        ! TEMP    R*4 1 I    ambient temperature                            [oC]
        ! DENS    R*4 1 -    densioty of water with dissolved salt          [kg/m3]
        ! SWSALCL R*4 1 I    option: 0 SAL simulated, 1 CL simulated
        !
        !   Logical Units : -
        !   Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        REAL(kind = real_wp) :: CL, SAL, SAL0, GTCL, TEMP, DENS, SWSALCL
        integer(kind = int_wp) :: iseg
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
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                SAL = PMSA(IP1)
                CL = PMSA(IP2)
                GTCL = PMSA(IP3)
                TEMP = PMSA(IP4)
                SAL0 = PMSA(IP5)
                SWSALCL = PMSA(IP6)
                !
                !***********************************************************************
                !**** Processes connected to the normalization RIZA method
                !***********************************************************************
                !
                !     factor 0.7 in density correction was derived empirically from RIZA Standard methods
                !     table 210 on p 109 is repoduced within 0.15%
                !     basic relation sal-chlorinity: sal = 0.03 +1.805*chlor/density
                !     density = f(temp and salt concentration)
                !
                IF (NINT(SWSALCL) == 1) THEN
                    DENS = 1000. + 0.7 * CL / 1000 * GTCL &
                            - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)
                    SAL = CL * GTCL / DENS + SAL0
                    !
                ELSE
                    DENS = 1000. + 0.7 * SAL / (1 - SAL / 1000.) &
                            - 0.0061 * (TEMP - 4.0) * (TEMP - 4.0)
                    !
                    IF (SAL <= SAL0) THEN
                        SAL = 0.0
                    ELSE
                        SAL = SAL - SAL0
                    ENDIF
                    !
                    !     g/m3 = (g/kg)*(kg/m3)/(g/g)
                    !
                    CL = SAL * DENS / GTCL
                ENDIF
                !
                PMSA (IP7) = DENS
                PMSA (IP8) = SAL
                PMSA (IP9) = CL
                !
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
            !
        end do
        !
        RETURN
    END

end module m_salchl
