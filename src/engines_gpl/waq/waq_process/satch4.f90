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
module m_satch4
    use m_waq_precision

    implicit none

contains


    subroutine satch4 (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Methane saturation concentration based on atmospheric methane pressure

        !
        !     Description of the module :
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! PCH4    R*4 1 I atmospheric methane pressure                       [atm]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L stand. temperature (20) minus ambient temperature   [oC]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IN1, IN2, IN3
        INTEGER(kind = int_wp) :: ISEG, IFLUX
        !
        REAL(kind = real_wp) :: PCH4, CCH4S
        REAL(kind = real_wp) :: TEMP, TEMP20
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                PCH4 = PMSA(IP1)
                TEMP = PMSA(IP2)
                !
                !           Calculate the saturation concentration
                !
                TEMP20 = 20 - TEMP
                CCH4S = 18.76 * PCH4 * (1.024**TEMP20)
                !
                !           The saturation concentration is output
                !
                PMSA(IP3) = CCH4S
                !
            ENDIF
            !
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            !
        end do
        !
        RETURN
        !
    END

end module m_satch4
