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
module m_resant
    use m_waq_precision

    implicit none

contains


    subroutine resant (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_evaluate_waq_attribute

        !>\file
        !>       Resuspension of nutrients in organic carbon matrix

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! RFLS1-2 R*4 1 I  sedimention flux organic from S1 or S2        [gX/m2/d]
        ! CNS1-2  R*4 1 I  C-N ratio substance in S1-S2                    [gC/gN]
        ! CPS1-2  R*4 1 I  C-P ratio substance in S1-S2                    [gC/gP]
        ! CSS1-2  R*4 1 I  C-S ratio substance in S1-S2                    [gC/gS]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        integer(kind = int_wp) :: iseg

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL evaluate_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !

                    RFLS1 = PMSA(IP1)
                    CNS1 = PMSA(IP2)
                    CPS1 = PMSA(IP3)
                    CSS1 = PMSA(IP4)
                    DEPTH = PMSA(IP5)
                    IF (DEPTH > 0.0) THEN
                        RFLS1 = RFLS1 / DEPTH
                    ELSE
                        RFLS1 = 0.0
                    ENDIF

                    !*******************************************************************************
                    !**** Processes connected to the SEDIMENTAION and RESUSENSION
                    !***********************************************************************

                    !     RESUSPENSION

                    !
                    FL(1 + IFLUX) = RFLS1 * CNS1

                    FL(2 + IFLUX) = RFLS1 * CPS1

                    FL(3 + IFLUX) = RFLS1 * CSS1

                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            !
        end do
        !

        RETURN
        !
    END

end module m_resant
