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
module m_raatra
    use m_waq_precision

    implicit none

contains


    subroutine raatra(nosys, ndmpq, noraai, ntraaq, ioraai, nqraai, iqraai, iqdmp, dmpq, trraai)

        !! Fills transport terms for raaien

        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSYS   INTEGER       1     INPUT   Total number of active substances
        !     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
        !     NORAAI  INTEGER       1     INPUT   Number of raaien
        !     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
        !     IORAAI  INTEGER       *     INPUT   Output option for raai
        !     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
        !     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai

        use timers

        integer(kind = int_wp) :: nosys, ndmpq, noraai, ntraaq
        integer(kind = int_wp) :: ioraai(*), nqraai(*), iqraai(*), iqdmp(*)
        real(kind = real_wp) :: dmpq(nosys, ndmpq, *), trraai(nosys, *)

        ! local
        integer(kind = int_wp) :: itel1, isys, iraai, nqc, integration_id, iqc, iq, ipq

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("raatra", ithandl)

        ! loop over the raaien
        itel1 = 0
        do iraai = 1, noraai

            ! the exchange contributes
            nqc = nqraai(iraai)
            integration_id = ioraai(iraai)
            do iqc = 1, nqc
                itel1 = itel1 + 1
                iq = iqraai(itel1)
                if (iq > 0) then
                    ipq = iqdmp(iq)
                    do isys = 1, nosys
                        if (integration_id == 1) then
                            trraai(isys, iraai) = trraai(isys, iraai) + &
                                    dmpq(isys, ipq, 1) - &
                                    dmpq(isys, ipq, 2)
                        elseif (integration_id == 2) then
                            trraai(isys, iraai) = trraai(isys, iraai) + &
                                    dmpq(isys, ipq, 1)
                        elseif (integration_id == 3) then
                            trraai(isys, iraai) = trraai(isys, iraai) - &
                                    dmpq(isys, ipq, 2)
                        endif
                    end do
                else
                    ipq = iqdmp(-iq)
                    do isys = 1, nosys
                        if (integration_id == 1) then
                            trraai(isys, iraai) = trraai(isys, iraai) - &
                                    dmpq(isys, ipq, 1) + &
                                    dmpq(isys, ipq, 2)
                        elseif (integration_id == 2) then
                            trraai(isys, iraai) = trraai(isys, iraai) + &
                                    dmpq(isys, ipq, 2)
                        elseif (integration_id == 3) then
                            trraai(isys, iraai) = trraai(isys, iraai) - &
                                    dmpq(isys, ipq, 1)
                        endif
                    end do
                endif
            end do
        end do

        if (timon) call timstop (ithandl)

    end subroutine raatra

end module m_raatra
