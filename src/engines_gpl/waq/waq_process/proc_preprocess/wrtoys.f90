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
module m_wrtoys
    use m_waq_precision

    implicit none

contains


    subroutine wrtoys (lchar, lun, notot, syname, noutp, &
            ioutps, outputs)

        !     Deltares Software Centre

        !>/File
        !>      writes altoys input files.

        !     Created   : Nov   1994 by Jan van Beek
        !     Modified  : Aug   2012 by Jan van Beek, use results structure, modern look and feel

        use timers         !< performance timers
        use results, only : OutputPointers, ihi3, ihi4, ibal

        implicit none

        character(len = *), intent(in) :: lchar(*)               !< filenames
        integer(kind = int_wp), intent(in) :: lun(*)                 !< unit numbers
        integer(kind = int_wp), intent(in) :: notot                  !< number of substances
        character(len = 20), intent(in) :: syname(*)              !< substance names
        integer(kind = int_wp), intent(in) :: noutp                  !< total number of output files
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure

        ! local

        integer(kind = int_wp) :: isys
        integer(kind = int_wp) :: ivar
        integer(kind = int_wp) :: ioffv
        character*255 :: filpst
        character*255 :: filstu
        integer(kind = int_wp) :: lunwrk
        integer(kind = int_wp) :: nrvar
        integer(kind = int_wp) :: indx
        integer(kind = int_wp) :: indx2
        integer(kind = int_wp) :: ilen
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("wrtoys", ithndl)
        !
        lunwrk = 81
        !
        !     write altoys.inp for all output in history file
        !
        open (lunwrk, file = 'altoys.inp')
        if (ioutps(5, 3) == ihi3) then
            do isys = 1, notot
                write(lunwrk, 1000) syname(isys), syname(isys)
            end do
        endif
        if (ioutps(5, 3) == ihi3 .or. ioutps(5, 3) == ihi4) then
            ioffv = ioutps(4, 1) + ioutps(4, 2)
            nrvar = ioutps(4, 3) / 2
            do ivar = 1, nrvar
                write(lunwrk, 1000) outputs%names(ioffv + ivar), outputs%names(ioffv + ivar)
            end do
        endif
        close (lunwrk)
        !
        !     write batoys.inp for all substances
        !
        if (ioutps(5, 5) == ibal) then
            open (lunwrk, file = 'batoys.inp')
            write(lunwrk, 1010) notot
            do isys = 1, notot
                write(lunwrk, 1020) syname(isys)
            end do
            do isys = 1, notot
                write(lunwrk, 1030) syname(isys), syname(isys)
            end do
            close (lunwrk)
        endif
        !
        !     construct filename pst en stu file
        !
        filpst = lchar(21)
        filstu = filpst
        indx = index(filpst, '.his ')
        if (indx > 0) then
            filpst(indx:) = '.pst'
            filstu(indx:) = '.stu'
        endif
        indx = indx + 3
        indx = min (indx, 255)
        ilen = len(lchar(36))
        indx2 = index(lchar(36), ' ')
        if (indx2 == 0) then
            indx2 = ilen
        else
            indx2 = indx2 - 1
            indx2 = max(indx2, 1)
        endif
        !
        !     write altoys.fil, file filetje for altoys
        !
        open (lunwrk, file = 'altoys.fil')
        write(lunwrk, 1040) lchar(36)(1:indx2)
        write(lunwrk, 1040) lchar(37)(1:indx)
        write(lunwrk, 1040) 'batoys.inp'
        write(lunwrk, 1040) lchar(21)(1:indx)
        write(lunwrk, 1040) filstu(1:indx)
        write(lunwrk, 1040) filpst(1:indx)
        write(lunwrk, 1040) 'altoys.mes'
        write(lunwrk, 1040) 'altoys.inp'
        write(lunwrk, 1040) '      .   '
        close (lunwrk)
        !
        !     write altoys.ini default settings
        !
        open (lunwrk, file = 'altoys.ini')
        write(lunwrk, 1050)
        write(lunwrk, 1060)
        write(lunwrk, 1070)
        write(lunwrk, 1070)
        close (lunwrk)
        !
        if (timon) call timstop(ithndl)
        return
        1000 format (a20, 'a', 9x, a20, '1.0')
        1010 format (i6, ' * number of balances')
        1020 format (a10)
        1030 format (a10, 2x, a10, 2x, '1.0')
        1040 format ('''', a, '''')
        1050 format ('86400')
        1060 format ('''day''')
        1070 format ('0')
    end

end module m_wrtoys
