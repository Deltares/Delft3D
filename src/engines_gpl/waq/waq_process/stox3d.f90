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
module m_stox3d
    use m_waq_precision

    implicit none

contains


    subroutine stox3d (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Vertical dispersion (segment -> exchange)

        IMPLICIT NONE
        !
        !     declaration of arguments
        !
        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     i/o from PMSA array
        !
        REAL(kind = real_wp) :: SPARAM                      ! process parameter on segment
        REAL(kind = real_wp) :: FACTOR                      ! scaling factor
        REAL(kind = real_wp) :: QPARAM                      ! process parameter on segment
        !
        !     local declarations
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3               ! index pointers in PMSA array
        INTEGER(kind = int_wp) :: IN1, IN2, IN3               ! increments in PMSA array
        INTEGER(kind = int_wp) :: IQ                          ! loop counter exchanges
        INTEGER(kind = int_wp) :: IFROM                       ! number from-segment
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)

        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)

        !     Exchange-loop over de eerste twee richtingen

        DO IQ = 1, NOQ1 + NOQ2

            !        Uitvoer op exchange niveau gelijk aan nul

            PMSA(IP3) = 0.0

            IP3 = IP3 + IN3

        ENDDO

        !     Exchange-loop over de derde richting

        DO IQ = NOQ1 + NOQ2 + 1, NOQ1 + NOQ2 + NOQ3

            IFROM = IEXPNT(1, IQ)

            IF (IFROM > 0) THEN

                !           Invoer op segment niveau naar uitvoer op exchange niveau

                SPARAM = PMSA(IP1 + (IFROM - 1) * IN1)
                FACTOR = PMSA(IP2 + (IFROM - 1) * IN2)
                QPARAM = SPARAM * FACTOR
            ELSE
                QPARAM = 0.0
            ENDIF

            PMSA(IP3) = QPARAM

            !        Ophogen pointering uitvoer op exchange niveau

            IP3 = IP3 + IN3

        ENDDO

        RETURN
    END

end module m_stox3d
