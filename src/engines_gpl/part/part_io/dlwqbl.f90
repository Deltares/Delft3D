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
module m_dlwqbl
    use m_stop_exit

    implicit none

contains


    subroutine dlwqbl (lunin, lunout, itime, idtime, itime1, &
            &                    itime2, ihdel, nftot, nrtot, array1, &
            &                    result, ipnt, luntxt, isflag, ifflag, &
            &                    update)

        !     Deltares Software Centre

        !>/File
        !>            Steps along in a dataset with blockwave property (flows)
        !>
        !>            This routine distinguishes from flwqtd that also supports
        !>            blockwaves in that it only needs one array.\n
        !>            Because of support of active-only files this have become
        !>            two arrays, would otherwise need 3 arrays.\n
        !>            The price paid is that:
        !>            - the series need to be equidistant
        !>            - the last record before rewind should contain only zeros
        !>            probably an additional array and use of expands_vol_area_for_bottom_cells would be simpler

        !     system administration : Antoon Koster

        !     created               : September 1996 by Robert Vos

        !     modified              : June      2011 by Leo Postma : support for active only hydrodynamics

        !     logical unitnumbers   : lunin  - input unit number hydrodynamic file
        !                             lunout - monitor file

        !     subroutines called    : stop_exit   , stops execution

        use m_waq_precision         ! single/double precision
        use timers

        implicit none

        !     Arguments           :

        !     kind           function         name               description

        integer  (int_wp), intent(in) :: lunin            !< unit number intermediate file
        integer  (int_wp), intent(in) :: lunout           !< unit number report file
        integer  (int_wp), intent(in) :: itime            !< current time in the model
        integer  (int_wp), intent(inout) :: idtime           !< time offset: > 0 after rewind
        integer  (int_wp), intent(inout) :: itime1           !< lower time in file
        integer  (int_wp), intent(inout) :: itime2           !< higher time in file
        integer  (int_wp), intent(in) :: ihdel            !< time step size in file
        integer  (int_wp), intent(in) :: nftot            !< array size in the file
        integer  (int_wp), intent(in) :: nrtot            !< array size to be delivered
        real     (sp), intent(inout) :: array1(nftot)    !< record at lower time in file
        real     (sp), intent(inout) :: result(nrtot)    !< record as delivered to Delpar
        integer  (int_wp), intent(in) :: ipnt  (nftot)    !< pointer from nftot to nrtot
        character(*), intent(in) :: luntxt           !< text with this unit number
        integer  (int_wp), intent(in) :: isflag           !< if 1 then 'dddhhmmss' format
        integer  (int_wp), intent(in) :: ifflag           !< if 1 then this is first invokation
        logical, intent(out) :: update           !< true if record is updated

        character(16), dimension(4) :: &
                &     msgtxt(4) = (/' Rewind on      ', ' Warning reading', &
                &                   ' Rewind error on', ' Error reading  ' /)

        !     locals

        integer(int_wp) :: i, iskip, messge, mod

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("dlwqbl", ithndl)
        !
        update = .false.
        messge = 0
        if (nftot  == 0) goto 100
        if (ifflag == 1) then
            read (lunin, end = 30, err = 30) itime1, array1
            itime2 = itime1 + ihdel
            idtime = 0
            update = .true.
        endif

        !         check for start time simulation before start time file

        if (itime < itime1) messge = 2

        !         a new record required?

        10 do while (itime - idtime >= itime2)
            update = .true.
            read (lunin, end = 50, err = 30) itime1, array1
            if (itime2 /= itime1) then
                write (lunout, *) 'Error: hydrodynamic database not equidistant'
                write (lunout, *) 'in time                                    '
                write (lunout, *) itime1, itime, idtime, itime2
            endif
            itime2 = itime1 + ihdel

            !.. check if the last record (all zero's) must be skipped

            iskip = 0
            do i = 1, nftot
                if (array1(i) /= 0.0) iskip = 1
            enddo
            if (iskip == 0) goto 50
        enddo

        !         block interpolation : stick to the old record

        result = 0.0
        !result(ipnt(:)) = array1(:) -- this caused a stack overflow ...
        do i = 1, size(ipnt)
            result(ipnt(i)) = array1(i)
        enddo
        goto 100

        !         normal rewind.

        20 rewind lunin
        idtime = idtime + itime1
        read (lunin, end = 40, err = 40) itime1, array1
        itime2 = itime1 + ihdel
        idtime = idtime - itime1
        goto 10

        !         error processing

        30 messge = 4              !    ' Error reading  '
        goto 100

        40 messge = 3              !    ' Rewind error on'
        goto 100

        50 messge = 1              !    ' Rewind on      '

        !         write the messages

        100 if (messge /= 0) then
            if (isflag /= 1) then
                write(lunout, 2000) msgtxt(messge), lunin, trim(luntxt), &
                        itime, itime1
            else
                write(lunout, 2010) msgtxt(messge), lunin, trim(luntxt), &
                        itime / 86400, mod(itime, 86400) / 3600, &
                        mod(itime, 3600) / 60, mod(itime, 60), &
                        itime1 / 86400, mod(itime1, 86400) / 3600, &
                        mod(itime1, 3600) / 60, mod(itime1, 60)
            endif
            if (messge == 1) then
                messge = 0
                goto 20
            endif
            call stop_exit(1)
        endif

        if (timon) call timstop (ithndl)
        return

        2000 format (a16, ' unit: ', i3, ', reading: ', a, /            &
                ' at simulation time:', i12, ' !', /, &
                ' time in file:      ', i12, ' !')
        2010 format (a16, ' unit: ', i3, ', reading: ', a, /            &
                ' at simulation time:', i5, 'd ', i2, 'h ', i2, 'm ', i2, 's !', / &
                ' time in file:      ', i5, 'd ', i2, 'h ', i2, 'm ', i2, 's !')

    end subroutine

end module m_dlwqbl
