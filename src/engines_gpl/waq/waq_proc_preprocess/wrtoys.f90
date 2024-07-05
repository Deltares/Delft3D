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


    subroutine wrtoys(file_name_list, file_unit_list, notot, syname, noutp, ioutps, outputs)
        !! writes altoys input files.

        use timers         !< performance timers
        use results, only : OutputPointers, ihi3, ihi4, ibal

        character(len = *), intent(in) :: file_name_list(*)               !< filenames
        integer(kind = int_wp), intent(in) :: file_unit_list(*)                 !< unit numbers
        integer(kind = int_wp), intent(in) :: notot                  !< number of substances
        character(len = 20), intent(in) :: syname(*)              !< substance names
        integer(kind = int_wp), intent(in) :: noutp                  !< total number of output files
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure

        integer(kind = int_wp) :: isys
        integer(kind = int_wp) :: ivar
        integer(kind = int_wp) :: ioffv
        character(len = 255) :: filpst
        character(len = 255) :: filstu
        integer(kind = int_wp) :: file_unit
        integer(kind = int_wp) :: nrvar
        integer(kind = int_wp) :: indx
        integer(kind = int_wp) :: indx2
        integer(kind = int_wp) :: ilen
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("wrtoys", ithndl)

        file_unit = 81

        ! write altoys.inp for all output in history file
        open (file_unit, file = 'altoys.inp')
        if (ioutps(5, 3) == ihi3) then
            do isys = 1, notot
                write(file_unit, 1000) syname(isys), syname(isys)
            end do
        endif
        if (ioutps(5, 3) == ihi3 .or. ioutps(5, 3) == ihi4) then
            ioffv = ioutps(4, 1) + ioutps(4, 2)
            nrvar = ioutps(4, 3) / 2
            do ivar = 1, nrvar
                write(file_unit, 1000) outputs%names(ioffv + ivar), outputs%names(ioffv + ivar)
            end do
        endif
        close (file_unit)

        ! write batoys.inp for all substances
        if (ioutps(5, 5) == ibal) then
            open (file_unit, file = 'batoys.inp')
            write(file_unit, 1010) notot
            do isys = 1, notot
                write(file_unit, 1020) syname(isys)
            end do
            do isys = 1, notot
                write(file_unit, 1030) syname(isys), syname(isys)
            end do
            close (file_unit)
        endif

        ! construct filename pst en stu file
        filpst = file_name_list(21)
        filstu = filpst
        indx = index(filpst, '.his ')
        if (indx > 0) then
            filpst(indx:) = '.pst'
            filstu(indx:) = '.stu'
        endif
        indx = indx + 3
        indx = min(indx, 255)
        ilen = len(file_name_list(36))
        indx2 = index(file_name_list(36), ' ')
        if (indx2 == 0) then
            indx2 = ilen
        else
            indx2 = indx2 - 1
            indx2 = max(indx2, 1)
        endif

        ! write altoys.fil, file filetje for altoys
        open (file_unit, file = 'altoys.fil')
        write(file_unit, 1040) file_name_list(36)(1:indx2)
        write(file_unit, 1040) file_name_list(37)(1:indx)
        write(file_unit, 1040) 'batoys.inp'
        write(file_unit, 1040) file_name_list(21)(1:indx)
        write(file_unit, 1040) filstu(1:indx)
        write(file_unit, 1040) filpst(1:indx)
        write(file_unit, 1040) 'altoys.mes'
        write(file_unit, 1040) 'altoys.inp'
        write(file_unit, 1040) '      .   '
        close (file_unit)
        !
        !     write altoys.ini default settings
        !
        open (file_unit, file = 'altoys.ini')
        write(file_unit, 1050)
        write(file_unit, 1060)
        write(file_unit, 1070)
        write(file_unit, 1070)
        close (file_unit)
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
    end subroutine wrtoys

end module m_wrtoys
