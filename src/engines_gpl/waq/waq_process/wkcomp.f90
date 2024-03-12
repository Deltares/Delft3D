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
module m_wkcomp
    use m_waq_precision

    implicit none

contains


    subroutine wkcomp (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Computes sum parameters from fractions (GEM)

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library

        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(80), INCREM(80), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        REAL(kind = real_wp) :: NO3, NH4, PO4, Si, IM1, IM2, IM3, Phyt, AlgN, AlgP, AlgSi, AlgDM, &
                POCnoa, POMnoa, PONnoa, POPnoa, POSnoa, DOC, DON, DOP, &
                DOS, AAP, VIVP, APATP, DmIM1, DmIM2, DmIM3
        REAL(kind = real_wp) :: TIM, POC, TOC, PON, TON, DIN, TotN, Kjel, POP, TOP, PIP, TotP, &
                FrAAP, FrVAP, TotSi, TOSnoa
        REAL(kind = real_wp) :: POC1, PON1, POP1, POS1, DmPOC1, CN1, CP1, CS1, &
                POC2, PON2, POP2, POS2, DmPOC2, CN2, CP2, CS2, &
                POC3, PON3, POP3, POS3, DmPOC3, CN3, CP3, CS3, &
                POC4, PON4, POP4, POS4, DmPOC4, CN4, CP4, CS4
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        INTEGER(kind = int_wp) :: IP(80)
        !
        IP = IPOINT
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            NO3 = PMSA(IP(1))
            NH4 = PMSA(IP(2))
            PO4 = PMSA(IP(3))
            Si = PMSA(IP(4))
            IM1 = PMSA(IP(5))
            IM2 = PMSA(IP(6))
            IM3 = PMSA(IP(7))
            Phyt = PMSA(IP(8))
            AlgN = PMSA(IP(9))
            AlgP = PMSA(IP(10))
            AlgSi = PMSA(IP(11))
            AlgDM = PMSA(IP(12))
            POC1 = PMSA(IP(13))
            POC2 = PMSA(IP(14))
            POC3 = PMSA(IP(15))
            POC4 = PMSA(IP(16))
            PON1 = PMSA(IP(17))
            DOC = PMSA(IP(18))
            DON = PMSA(IP(19))
            DOP = PMSA(IP(20))
            DOS = PMSA(IP(21))
            AAP = PMSA(IP(22))
            VIVP = PMSA(IP(23))
            APATP = PMSA(IP(24))
            !
            DmIM1 = PMSA(IP(25))
            DmIM2 = PMSA(IP(26))
            DmIM3 = PMSA(IP(27))
            !
            PON2 = PMSA(IP(28))
            PON3 = PMSA(IP(29))
            PON4 = PMSA(IP(30))
            POP1 = PMSA(IP(31))
            POP2 = PMSA(IP(32))
            POP3 = PMSA(IP(33))
            POP4 = PMSA(IP(34))
            POS1 = PMSA(IP(35))
            POS2 = PMSA(IP(36))
            POS3 = PMSA(IP(37))
            POS4 = PMSA(IP(38))
            !
            POSnoa = PMSA(IP(39))
            !
            DmPOC1 = PMSA(IP(40))
            DmPOC2 = PMSA(IP(41))
            DmPOC3 = PMSA(IP(42))
            DmPOC4 = PMSA(IP(43))
            !
            TIM = DmIM1 * IM1 + DmIM2 * IM2 + DmIM3 * IM3
            !
            POCnoa = POC1 + POC2 + POC3 + POC4
            POMnoa = POC1 * DmPOC1 + POC2 * DmPOC2 &
                    + POC3 * DmPOC3 + POC4 * DmPOC4
            PONnoa = PON1 + PON2 + PON3 + PON4
            POPnoa = POP1 + POP2 + POP3 + POP4
            TOSnoa = POS1 + POS2 + POS3 + POS4 + DOS
            !
            POC = Phyt + POCnoa
            TOC = POC + DOC
            !
            PON = AlgN + PONnoa
            TON = PON + DON
            DIN = NH4 + NO3
            TotN = TON + DIN
            Kjel = TON + NH4
            !
            POP = AlgP + POPnoa
            TOP = POP + DOP
            PIP = AAP + VIVP + APATP
            TotP = TOP + PO4 + PIP
            !
            IF (TIM > 0.0) THEN
                FrAAP = AAP / TIM
                FrVAP = (VIVP + APATP) / TIM
            ELSE
                FrAAP = 0.0
                FrVAP = 0.0
            ENDIF
            !
            TotSi = AlgSi + POSnoa + Si
            !
            CN1 = 0.0
            CN2 = 0.0
            CN3 = 0.0
            CN4 = 0.0
            CP1 = 0.0
            CP2 = 0.0
            CP3 = 0.0
            CP4 = 0.0
            CS1 = 0.0
            CS2 = 0.0
            CS3 = 0.0
            CS4 = 0.0
            !
            CN1 = RATIO(POC1, PON1)
            CN2 = RATIO(POC2, PON2)
            CN3 = RATIO(POC3, PON3)
            CN4 = RATIO(POC4, PON4)
            CP1 = RATIO(POC1, POP1)
            CP2 = RATIO(POC2, POP2)
            CP3 = RATIO(POC3, POP3)
            CP4 = RATIO(POC4, POP4)
            CS1 = RATIO(POC1, POS1)
            CS2 = RATIO(POC2, POS2)
            CS3 = RATIO(POC3, POS3)
            CS4 = RATIO(POC4, POS4)
            !
            PMSA (IP(44)) = TIM + AlgDM + POMnoa
            PMSA (IP(45)) = TIM + POMnoa
            PMSA (IP(46)) = TIM + AlgDM + POMnoa
            PMSA (IP(47)) = TIM
            !
            PMSA (IP(48)) = AlgDM + POMnoa
            PMSA (IP(49)) = TOC
            PMSA (IP(50)) = POC
            !
            PMSA (IP(51)) = TotN
            PMSA (IP(52)) = Kjel
            PMSA (IP(53)) = DIN
            PMSA (IP(54)) = TON
            PMSA (IP(55)) = PON
            !
            PMSA (IP(56)) = TotP
            PMSA (IP(57)) = TOP
            PMSA (IP(58)) = POP
            PMSA (IP(59)) = PIP
            PMSA (IP(60)) = FrAAP
            PMSA (IP(61)) = FrVAP
            !
            PMSA (IP(62)) = TotSi
            !
            PMSA (IP(63)) = POCnoa
            PMSA (IP(64)) = POMnoa
            PMSA (IP(65)) = PONnoa
            PMSA (IP(66)) = POPnoa
            PMSA (IP(67)) = POSnoa
            PMSA (IP(68)) = TOSnoa
            !
            PMSA (IP(69)) = CN1
            PMSA (IP(70)) = CN2
            PMSA (IP(71)) = CN3
            PMSA (IP(72)) = CN4
            PMSA (IP(73)) = CP1
            PMSA (IP(74)) = CP2
            PMSA (IP(75)) = CP3
            PMSA (IP(76)) = CP4
            PMSA (IP(77)) = CS1
            PMSA (IP(78)) = CS2
            PMSA (IP(79)) = CS3
            PMSA (IP(80)) = CS4
            !
            !        ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP = IP + INCREM
            !
        end do
        !
        RETURN
        !
    CONTAINS
        REAL FUNCTION RATIO(X, Y)
            REAL :: X, Y

            RATIO = 0.0
            IF (Y > 0.0 .AND. X > 0.0) THEN
                IF (X < Y) THEN
                    RATIO = X / Y
                ELSE
                    RATIO = Y / X
                    IF (RATIO > TINY(RATIO)) THEN
                        RATIO = 1.0 / RATIO
                    ELSE
                        RATIO = HUGE(RATIO)
                    ENDIF
                ENDIF
            ENDIF
        END FUNCTION RATIO
    END

end module m_wkcomp
