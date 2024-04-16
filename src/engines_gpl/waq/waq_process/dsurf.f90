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
module m_dsurf
    use m_waq_precision

    implicit none

contains


    subroutine dsurf  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_write_error_message

        !>\file
        !>       Dynamic calculation of the horizontal surface area from volume and depth

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                             ----
        ! DEPTH   R*4 1 O depth of the water column                            [m]
        ! SURF    R*4 1 I surface area of segment                             [m2]
        ! VOLUME  R*4 1 I volume of segment                                   [m3]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !

                VOLUME = PMSA(IP1)
                DEPTH = PMSA(IP2)

                IF (DEPTH < 1E-30) CALL write_error_message ('DEPTH in DSURF zero')

                !***********************************************************************
                !**** Calculate SURF
                !***********************************************************************
                !
                SURF = VOLUME / DEPTH
                !
                PMSA(IP3) = SURF
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            !
        end do
        !
        RETURN
        !
    END

end module m_dsurf
