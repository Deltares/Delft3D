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
module m_dhpfil
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DHPFIL(LUNREP, TEKST, FILNAM)
        !
        !     print a filename to file
        !
        use timers       !   performance timers

        INTEGER(kind = int_wp) :: LUNREP
        CHARACTER*(*) TEKST, FILNAM

        integer(kind = int_wp) :: i, ilen, iend
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("dhpfil", ithndl)
        !
        ILEN = LEN(FILNAM)
        DO I = ILEN, 1, -1
            IEND = I
            IF (FILNAM(I:I) /= ' ') GOTO 10
        ENDDO
        10 CONTINUE
        !
        WRITE(LUNREP, '(A,A)') TEKST, FILNAM(1:IEND)
        !
        if (timon) call timstop(ithndl)
        RETURN
    END

end module m_dhpfil
