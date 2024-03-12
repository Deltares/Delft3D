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
module m_phcomp
    use m_waq_precision

    implicit none

contains


    subroutine phcomp (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Composition of phytoplankton by summing algae fractions - Dynamo - GEM

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: ITEL, ISEG
        INTEGER(kind = int_wp) :: NTYPE, ITYPE
        REAL(kind = real_wp) :: PHYT, ALGN, ALGP, ALGSI, ALGDM, CHLFA, BIOMAS, &
                NCRAT, PCRAT, SICRAT, DMCF, CATOCL

        NTYPE = PMSA(IPOINT(1))
        !
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                PHYT = 0.0
                ALGN = 0.0
                ALGP = 0.0
                ALGSI = 0.0
                ALGDM = 0.0
                CHLFA = 0.0

                DO ITYPE = 1, NTYPE

                    ITEL = 1 + ITYPE
                    BIOMAS = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))
                    ITEL = 1 + ITYPE + NTYPE
                    NCRAT = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))
                    ITEL = 1 + ITYPE + NTYPE * 2
                    PCRAT = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))
                    ITEL = 1 + ITYPE + NTYPE * 3
                    SICRAT = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))
                    ITEL = 1 + ITYPE + NTYPE * 4
                    DMCF = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))
                    ITEL = 1 + ITYPE + NTYPE * 5
                    CATOCL = PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL))

                    !***********************************************************************
                    !**** Calculations connected to the status of the algae
                    !***********************************************************************

                    !             Total Carbon in algae

                    PHYT = PHYT + BIOMAS

                    !             Total nitrogen

                    ALGN = ALGN + BIOMAS * NCRAT

                    !             Total phosphorus

                    ALGP = ALGP + BIOMAS * PCRAT

                    !             Total silica

                    ALGSI = ALGSI + BIOMAS * SICRAT

                    !             Total dry matter

                    ALGDM = ALGDM + BIOMAS * DMCF

                    !             Chlorophyll

                    CHLFA = CHLFA + BIOMAS * CATOCL

                end do

                ITEL = 1 + 6 * NTYPE + 1
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = PHYT
                ITEL = 1 + 6 * NTYPE + 2
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = ALGN
                ITEL = 1 + 6 * NTYPE + 3
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = ALGP
                ITEL = 1 + 6 * NTYPE + 4
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = ALGSI
                ITEL = 1 + 6 * NTYPE + 5
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = ALGDM
                ITEL = 1 + 6 * NTYPE + 6
                PMSA (IPOINT(ITEL) + (ISEG - 1) * INCREM(ITEL)) = CHLFA

            ENDIF
            !
        end do
        !
        RETURN
        !
    END

end module m_phcomp
