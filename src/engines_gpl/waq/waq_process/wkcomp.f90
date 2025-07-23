!!  Copyright (C)  Stichting Deltares, 2012-2025.
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


    subroutine wkcomp (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
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

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(80), INCREM(80), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

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
        DO ISEG = 1, num_cells

            NO3 = process_space_real(IP(1))
            NH4 = process_space_real(IP(2))
            PO4 = process_space_real(IP(3))
            Si = process_space_real(IP(4))
            IM1 = process_space_real(IP(5))
            IM2 = process_space_real(IP(6))
            IM3 = process_space_real(IP(7))
            Phyt = process_space_real(IP(8))
            AlgN = process_space_real(IP(9))
            AlgP = process_space_real(IP(10))
            AlgSi = process_space_real(IP(11))
            AlgDM = process_space_real(IP(12))
            POC1 = process_space_real(IP(13))
            POC2 = process_space_real(IP(14))
            POC3 = process_space_real(IP(15))
            POC4 = process_space_real(IP(16))
            PON1 = process_space_real(IP(17))
            DOC = process_space_real(IP(18))
            DON = process_space_real(IP(19))
            DOP = process_space_real(IP(20))
            DOS = process_space_real(IP(21))
            AAP = process_space_real(IP(22))
            VIVP = process_space_real(IP(23))
            APATP = process_space_real(IP(24))
            !
            DmIM1 = process_space_real(IP(25))
            DmIM2 = process_space_real(IP(26))
            DmIM3 = process_space_real(IP(27))
            !
            PON2 = process_space_real(IP(28))
            PON3 = process_space_real(IP(29))
            PON4 = process_space_real(IP(30))
            POP1 = process_space_real(IP(31))
            POP2 = process_space_real(IP(32))
            POP3 = process_space_real(IP(33))
            POP4 = process_space_real(IP(34))
            POS1 = process_space_real(IP(35))
            POS2 = process_space_real(IP(36))
            POS3 = process_space_real(IP(37))
            POS4 = process_space_real(IP(38))
            !
            POSnoa = process_space_real(IP(39))
            !
            DmPOC1 = process_space_real(IP(40))
            DmPOC2 = process_space_real(IP(41))
            DmPOC3 = process_space_real(IP(42))
            DmPOC4 = process_space_real(IP(43))
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
            process_space_real (IP(44)) = TIM + AlgDM + POMnoa
            process_space_real (IP(45)) = TIM + POMnoa
            process_space_real (IP(46)) = TIM + AlgDM + POMnoa
            process_space_real (IP(47)) = TIM
            !
            process_space_real (IP(48)) = AlgDM + POMnoa
            process_space_real (IP(49)) = TOC
            process_space_real (IP(50)) = POC
            !
            process_space_real (IP(51)) = TotN
            process_space_real (IP(52)) = Kjel
            process_space_real (IP(53)) = DIN
            process_space_real (IP(54)) = TON
            process_space_real (IP(55)) = PON
            !
            process_space_real (IP(56)) = TotP
            process_space_real (IP(57)) = TOP
            process_space_real (IP(58)) = POP
            process_space_real (IP(59)) = PIP
            process_space_real (IP(60)) = FrAAP
            process_space_real (IP(61)) = FrVAP
            !
            process_space_real (IP(62)) = TotSi
            !
            process_space_real (IP(63)) = POCnoa
            process_space_real (IP(64)) = POMnoa
            process_space_real (IP(65)) = PONnoa
            process_space_real (IP(66)) = POPnoa
            process_space_real (IP(67)) = POSnoa
            process_space_real (IP(68)) = TOSnoa
            !
            process_space_real (IP(69)) = CN1
            process_space_real (IP(70)) = CN2
            process_space_real (IP(71)) = CN3
            process_space_real (IP(72)) = CN4
            process_space_real (IP(73)) = CP1
            process_space_real (IP(74)) = CP2
            process_space_real (IP(75)) = CP3
            process_space_real (IP(76)) = CP4
            process_space_real (IP(77)) = CS1
            process_space_real (IP(78)) = CS2
            process_space_real (IP(79)) = CS3
            process_space_real (IP(80)) = CS4
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
