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
module m_dlwqj2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQJ2 (LUNWR, NOBRK, NOTOT, ITAL, IAR, &
            RAR, IFILSZ, JFILSZ)
        !
        !
        !     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : Writes blocks of breakpoint data
        !
        !     SUBROUTINES CALLED : none
        !
        !     LOGICAL UNITS      : LUNWR   = binary/unformatted work file
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     LUNWR   INTEGER     1       INPUT   unit number output work file
        !     NOBRK   INTEGER     1       INPUT   nr of breakpoints to write
        !     NOTOT   INTEGER     1       INPUT   size of one matrix of data
        !     ITAL    INTEGER     1       INPUT   nr of integers per breakpoint
        !     IAR     INTEGER     *       INPUT   breakpoint timers
        !     RAR     REAL*4      *       INPUT   matrix storage
        !     IFILSZ  INTEGER     1       IN/OUT  cumulative integer space count
        !     JFILSZ  INTEGER     1       IN/OUT  cumulative real space count
        !
        !
        use timers       !   performance timers

        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: ITAL
        integer(kind = int_wp) :: k, I, NOTOT
        integer :: lunwr, nobrk, itel, jtel, iar(:), ifilsz, jfilsz
        real(kind = real_wp) :: rar(:)
        if (timon) call timstrt("dlwqj2", ithndl)
        !
        !           Write nr of breakpoints first
        !
        WRITE (LUNWR) NOBRK
        !
        !           Initialize counters for the loop
        !
        ITEL = 0
        JTEL = 0
        DO I = 1, NOBRK
            WRITE (LUNWR) (IAR(ITEL + K), K = 1, ITAL), &
                    (RAR(JTEL + K), K = 1, NOTOT)
            ITEL = ITEL + ITAL
            JTEL = JTEL + NOTOT
        end do
        !
        !           Update the space count
        !
        IFILSZ = IFILSZ + NOBRK * ITAL + 1
        JFILSZ = JFILSZ + NOBRK * NOTOT
        !
        if (timon) call timstop(ithndl)
        RETURN
    END

end module m_dlwqj2
