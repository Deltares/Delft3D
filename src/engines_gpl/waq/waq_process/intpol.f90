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
module m_intpol
    use m_waq_precision

    implicit none

contains


    subroutine intpol (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Depth where wave is created or wind fetch from wind direction

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        BLOCK INTERPOLATION
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! Y       R*4 8 I     dependent value pairs
        ! X       R*4 8 I     independent value pairs
        ! VALUE   R*4 1 I     independent value
        ! RESULT  R*4 1 I     resulting dependent value
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: MAXPAR, NUMPAR, i, iseg
        real(kind = real_wp) :: VALUE, RESULT
        PARAMETER (MAXPAR = 8)
        real(kind = real_wp) :: X(MAXPAR)
        real(kind = real_wp) :: Y(MAXPAR)
        integer(kind = int_wp) :: IP(2 * MAXPAR + 2)

        DO I = 1, 2 * MAXPAR + 2
            IP(I) = IPOINT(I)
        end do
        !
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                !     fill and count the number of classes

                VALUE = PMSA(IP(1))
                NUMPAR = 1
                DO I = 1, MAXPAR
                    Y(I) = PMSA(IP(2 * I))
                    X(I) = PMSA(IP(2 * I + 1))
                    IF (X(I)< 0.0) EXIT
                    NUMPAR = I
                ENDDO

                !*******************************************************************************
                !**** RESULT equals the Y corresponding with the interval from the previous
                !****        to the current X (assuming the first interval to be 0 - X(1)
                !***********************************************************************

                I = 0
                30 I = I + 1
                IF ((VALUE<X(I)).OR.(I==NUMPAR)) THEN
                    RESULT = Y(I)
                ELSE
                    GOTO 30
                ENDIF

                PMSA(IP(2 * MAXPAR + 2)) = RESULT

            ENDIF
            !
            DO I = 1, 2 * MAXPAR + 2
                IP(I) = IP(I) + INCREM (I)
            end do
            !
        end do
        !
        RETURN
        !
    END

end module m_intpol
