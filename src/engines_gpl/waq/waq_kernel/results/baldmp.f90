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
module m_baldmp
    use m_waq_precision

    implicit none

contains


    SUBROUTINE BALDMP (NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
            NDMPS, NTDMPQ, IQDMP, ISDMP, IPDMP, &
            DMPQ, MASS, DMPS, FLXDMP, ASMASS, &
            FLXINT)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            : march 1995 by Jan van Beek
        !
        !     FUNCTION            : Fills balances for sub-area's
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     COMMON BLOCKS       : -
        !
        !     PARAMETERS          : 15
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOSYS   INTEGER       1     INPUT   Total number of active substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Number of dump areas
        !     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
        !     NDMPS   INTEGER       1     INPUT   Number of dump segments
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
        !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     DMPS    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
        !     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 OUTPUT  Mass balance terms
        !     FLXINT  REAL  NOFLUX*NDMPAR OUTPUT  Integrated fluxes
        !
        !     Declaration of arguments
        !
        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
                NDMPS, NTDMPQ
        INTEGER(kind = int_wp) :: IQDMP(*), ISDMP(*), &
                IPDMP(*)
        REAL(kind = real_wp) :: DMPQ(NOSYS, NDMPQ, *), MASS(NOTOT, *), &
                DMPS(NOTOT, NDMPS, *), FLXDMP(NOFLUX, *), &
                ASMASS(NOTOT, NDMPAR, *), FLXINT(NOFLUX, *)
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: ITEL1, ITEL2, IP1, IDUMP, NQC, &
                IQC, IQ, IPQ, ISYS, NSC, &
                ISC, ISEG, IPS
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("baldmp", ithandl)

        !
        !     Loop over the dump area's
        !
        ITEL1 = NDMPAR
        IP1 = NDMPAR + NTDMPQ
        ITEL2 = NDMPAR + NTDMPQ + NDMPAR
        DO IDUMP = 1, NDMPAR
            !
            !        the exchange contributes
            !
            NQC = IPDMP(IDUMP)
            DO IQC = 1, NQC
                ITEL1 = ITEL1 + 1
                IQ = IPDMP(ITEL1)
                IF (IQ > 0) THEN
                    IPQ = IQDMP(IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 2)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 1)
                    end do
                ELSE
                    IPQ = IQDMP(-IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 1)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 2)
                    end do
                ENDIF
            end do
            !
            !        the segment contributes
            !
            DO ISYS = 1, NOTOT
                ASMASS(ISYS, IDUMP, 1) = 0.0
            ENDDO
            NSC = IPDMP(IP1 + IDUMP)
            DO ISC = 1, NSC
                ITEL2 = ITEL2 + 1
                ISEG = IPDMP(ITEL2)
                IF (ISEG > 0) THEN
                    IPS = ISDMP(ISEG)
                    DO ISYS = 1, NOTOT
                        ASMASS(ISYS, IDUMP, 1) = ASMASS(ISYS, IDUMP, 1) + &
                                MASS(ISYS, ISEG)
                        ASMASS(ISYS, IDUMP, 2) = ASMASS(ISYS, IDUMP, 2) + &
                                DMPS(ISYS, IPS, 1)
                        ASMASS(ISYS, IDUMP, 3) = ASMASS(ISYS, IDUMP, 3) + &
                                DMPS(ISYS, IPS, 2)
                        ASMASS(ISYS, IDUMP, 4) = ASMASS(ISYS, IDUMP, 4) + &
                                DMPS(ISYS, IPS, 3)
                    end do
                ENDIF
                !
            end do
            !
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
    END

end module m_baldmp
