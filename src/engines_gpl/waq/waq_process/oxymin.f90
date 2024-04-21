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
module m_oxymin
    use m_waq_precision

    implicit none

contains


    subroutine oxymin (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Potential daily mimimum dissolved oxygen concentration

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATES THE POTENTIAL MINIMUM OXYGEN CONCENTRATION
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! OXY     R*4 1 I  oxygen concentration                          [gO2/m3]
        ! GREEN   R*4 1 I  concentration green algae                      [gC/m3]
        ! PGREEN  R*4 1 I  production green algae                           [1/d]
        ! RGREEN  R*4 1 I  respiration green algae                          [1/d]
        ! DIAT    R*4 1 I  concentration diatoms                          [gC/m3]
        ! PDIAT   R*4 1 I  production diatoms                               [1/d]
        ! RDIAT   R*4 1 I  respiration diatoms                              [1/d]
        ! CMINDO  R*4 1 O  minimum oxygen concentration                  [gO2/m3]
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
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !

                OXY = PMSA(IP1)
                GREEN = PMSA(IP2)
                PGREEN = PMSA(IP3)
                RGREEN = PMSA(IP4)
                DIAT = PMSA(IP5)
                PDIAT = PMSA(IP6)
                RDIAT = PMSA(IP7)
                DL = PMSA(IP8)


                !     CALCULATE MINIMUM OXYGEN FROM PRODUCTION AND RESPIRATION
                !     CORRECT FOR DAY - NIGHT SEQUENCE
                !     original tentative estimate:
                !     CMINDO = OXY - 2.67 * (PROD - DL*RESP)
                !     replaced with the minimum of two estimates, one assuming
                !     occurrence or non-occurrence of compensation by reaeration
                !
                !     first calculate total gross production and respiration
                !
                PROD = (PGREEN + RGREEN) * GREEN + (PDIAT + RDIAT) * DIAT
                RESP = RGREEN * GREEN + RDIAT * DIAT
                !
                !     estimate without compensation
                !
                CMINDO1 = OXY - 0.5 * 2.67 * RESP * (1 - DL)
                !
                !     estimate with compensation on a daily average basis
                !
                CMINDO2 = OXY - 0.5 * 2.67 * PROD * (1 - DL)
                !
                !     take the minimal value of the two estimates
                !
                CMINDO = MIN (CMINDO1, CMINDO2)

                PMSA(IP9) = CMINDO

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
            !
        end do
        !
        RETURN
        !
    END

end module m_oxymin
