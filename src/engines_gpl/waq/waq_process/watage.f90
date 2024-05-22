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
module m_watage
    use m_waq_precision

    implicit none

contains


    subroutine watage (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger

        !>\file
        !>       Age of water through the tracer substances

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                             ----
        ! AGE     R*4 1 O average age of the water
        ! CONCWA  R*4 1 I fraction of specific water ( conservative )
        ! CONCTR  R*4 1 I concentration tracer ( 1st order decay )
        ! DECAYR  R*4 1 I decay rate tracer
        ! FDECAY  R*4 1 O flux first order decay on tracer

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IFLUX, ISEG
        REAL(kind = real_wp) :: CONCWA, CONCTR, DECAYR, ARGUM, AGE, FDECAY

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CONCWA = PMSA(IP1)
                CONCTR = PMSA(IP2)
                DECAYR = PMSA(IP3)
                !
                IF (DECAYR < 1E-20) CALL write_error_message ('RCDECTR in WATAGE zero')

                !     Calculate age
                !
                IF (CONCWA <= 1.0E-20) THEN
                    AGE = -999.
                ELSEIF (CONCTR <= 1.0E-20) THEN
                    AGE = -999.
                ELSEIF (CONCTR > CONCWA) THEN
                    AGE = -999.
                ELSE
                    ARGUM = CONCTR / CONCWA
                    IF (ARGUM < 1E-20) THEN
                        AGE = -999.
                    ELSEIF (ABS(ARGUM - 1.0) > 1.0E-3) THEN
                        AGE = - LOG(ARGUM) / DECAYR
                    ELSE
                        AGE = - ((ARGUM - 1.0) - (ARGUM - 1.0)**2 / 2.0) / DECAYR
                    ENDIF
                ENDIF
                !
                !     Calculate decay
                !
                FDECAY = DECAYR * CONCTR
                !
                !     Output
                !
                PMSA(IP4) = AGE
                FL(1 + IFLUX) = FDECAY
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            !
        end do
        !
        RETURN
    END

end module m_watage
