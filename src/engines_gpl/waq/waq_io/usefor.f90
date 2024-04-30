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
module m_usefor
    use m_waq_precision

    implicit none

    private
    public :: compact_usefor, compact_usefor_list

contains


    subroutine compact_usefor(lunut, waq_param, data_param, i, icnt)
        !!compacts usefor lists if unresolved externals

        use m_waq_data_structure !   for definition and storage of data
        use timers        !   performance timers

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        type(t_waq_item), intent(inout) :: waq_param  !! list of param items to be set in this block ( substances etc )
        type(t_waq_item), intent(inout) :: data_param   ! list of param items in the data
        integer(kind = int_wp), intent(in) :: i             ! item index
        integer(kind = int_wp), intent(inout) :: icnt          ! shift in item index

        character(len = 20) :: charachter_output        ! item name
        integer(kind = int_wp) :: nitm          ! number of items in data
        integer(kind = int_wp) :: ishft         ! number of items shifted in data
        integer(kind = int_wp) :: i1            ! item index
        integer(kind = int_wp) :: i2            ! item index
        integer(kind = int_wp) :: i3            ! item index
        integer(kind = int_wp) :: i4            ! item index
        integer(kind = int_wp) :: i5            ! item index
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("compact_usefor", ithndl)

        ! write message
        write (lunut, *)
        write (lunut, 1010) i + icnt, data_param%name(i)
        nitm = data_param%no_item

        ! look backwards
        do i1 = i, 1, -1
            i2 = data_param%ipnt(i1)
            if (i2 > -100000) exit
        enddo

        ! additional messages for this sequence
        if (i2 <= 0 .and. i2 > -100000) then
            ! try to find the reference
            i4 = 0
            do i3 = 1, i
                i5 = data_param%ipnt(i3)
                if (i5 > 0) i4 = i5
                if (i5 <= 0 .and. i5 > -100000) i4 = i4 + 1
            enddo
            charachter_output = waq_param%name(i4)
            if (data_param%name(i) /= charachter_output) then
                write (lunut, 1030) i4, charachter_output
            endif
        endif
        if (i2 > 0 .and. i2 <  100000) then
            i4 = i2
            charachter_output = waq_param%name(i2)
            if (data_param%name(i) /= charachter_output) then
                write (lunut, 1030)  i2, charachter_output
            endif
        endif
        i2 = i4

        ! determine the shift in locations
        ishft = 1
        do i4 = i1 + 1, nitm
            i3 = data_param%ipnt(i4)
            if (i3 > -1000000) exit
            ishft = ishft + 1
        enddo

        ! shift the items in data_param
        do i4 = i1, nitm
            data_param%name(i4) = data_param%name(i4 + ishft)
            data_param%ipnt(i4) = data_param%ipnt(i4 + ishft)
            data_param%sequence(i4) = data_param%sequence(i4 + ishft)
            data_param%constant(i4) = data_param%constant(i4 + ishft)
        enddo
        data_param%no_item = data_param%no_item - ishft
        icnt = icnt + ishft

        ! shift the items in waq_param
        do i5 = i2, waq_param%no_item
            data_param%name(i5) = data_param%name(i5 + 1)
            data_param%ipnt(i5) = data_param%ipnt(i5 + 1)
            data_param%sequence(i5) = data_param%sequence(i5 + 1)
            data_param%constant(i5) = data_param%constant(i5 + 1)
        enddo

        ! renumber in data_param the reference to waq_param
        do i4 = i1, data_param%no_item
            if (data_param%ipnt(i4) > i2) data_param%ipnt(i4) = data_param%ipnt(i4) - 1
        enddo

        if (timon) call timstop(ithndl)
        return

        1010 format (' warning: input item : ', i3, ' not resolved: ', a)
        1030 format (' warning: item number: ', i3, ' also not resolved: ', a)

    end subroutine compact_usefor

    subroutine compact_usefor_list(lunut, int_array, itmnr, noitm, idmnr, &
            nodim, iorder, char_arr, ioffi, ioffc, &
            iods, ioffd, idx_missing, count_missing, ierr, status)

        !! Compacts USEFOR lists if unresolved externals
        !!
        !!     Logical Units      : file_unit_list(27) = unit stripped DELWAQ input file
        !!                          file_unit_list(29) = unit formatted output file


        !     Parameters    :
        !
        !     Name    Kind     Length     Funct.  Description
        !     ---------------------------------------------------------
        !     lunut   integer    1         input   unit number for ascii output
        !     int_array     integer  max_int_size       in/out  integer   workspace
        !     itmnr   integer    1         in/out  nr of items for assignment
        !     noitm   integer    1         in      nr of items in computational rule
        !     idmnr   integer    1         in/out  nr of subst for assignment
        !     nodim   integer    1         in      nr of subst in computational rule
        !     iorder  integer    1         in      1 = items first, 2 is subst first
        !     char_arr     char*(*)  nitm       input   items to check for presence
        !     ioffi   integer    1         in/out  offset in input array
        !     ioffc   integer    1         in/out  offset in character array
        !     ioffd   integer    1         in/out  base offset in both arrays
        !     iods    integer    1         input   shift counter ods files
        !     idx_missing       integer    1   input   loop counter (index of missing table header, in list USEFOR's)
        !     count_missing    integer    1         in/out  counter of missing column headers (wrt USEFOR's)

        use timers       !   performance timers
        use m_waq_precision
        use m_error_status

        character(len=*) char_arr(:)
        integer(kind = int_wp), intent(inout) :: int_array(:)
        character(len=20)  charachter_output, message_type
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: i1, i3, i4, i5
        integer(kind = int_wp) :: lunut, idx_missing, count_missing, ioffc, iorder, ntt, idmnr, nitm, nodim
        integer(kind = int_wp) :: itmnr, noitm, i2, ioffd, ishft, ioffi, iods

        integer(kind = int_wp) :: ierr
        type(error_status), intent(inout) :: status !< current error status

        ierr = -1
        if (timon) call timstrt("compact_usefor_list", ithndl)

        ! Write message
        write (lunut, *)

        if (iorder == 1) then ! items first
            ntt = idmnr
            nitm = nodim
        else ! subst first
            ntt = itmnr
            nitm = noitm
        endif

        ! look backwards
        i4 = 0
        do i1 = idx_missing, 1, -1
            i2 = int_array(ioffc + i1)
            if (i2 > -100000) exit
        end do

        ! additional messages for this sequence
        if (i2 > -100000 .and. i2 <= 0) then
            ! try to find the reference
            do i3 = 1, idx_missing
                i5 = int_array(ioffc + i3)
                if (i5 > -100000 .and. i5 <= 0)  i4 = i4 + 1
                if (i5 > 0)                      i4 = int_array(ioffc + i3)
            end do
            charachter_output = char_arr(ioffd + i4)
            if (char_arr(ioffc + idx_missing) /= charachter_output) then ! log not resolved
                if (iorder == 2) then
                    write (lunut, 1030) i4, charachter_output
                else
                    write (lunut, 1040) i4, charachter_output
                end if
            end if
        else if (i2 > 0 .and. i2 <  100000) then
            i4 = i2
            charachter_output = char_arr(ioffd + i2)
            if (char_arr(ioffc + idx_missing) == charachter_output) then
                call status%increase_warning_count()
                message_type = "WARNING"
                write (lunut, 1010) message_type, idx_missing + count_missing, char_arr(ioffc + idx_missing)
            else ! pseudo-error
                message_type = "ERROR"
                write (lunut, 1010) message_type, idx_missing + count_missing, char_arr(ioffc + idx_missing)
                ierr = 1
                if (iorder == 2) then
                    write (lunut, 1030)  message_type, i2, charachter_output
                else
                    write (lunut, 1040)  message_type, i2, charachter_output
                end if
            end if
        endif
        i2 = i4

        ! determine the shift in locations
        ishft = 1
        do i4 = i1 + 1, nitm
            i3 = int_array(ioffc + i4)
            if (i3 > -1000000) exit
            ishft = ishft + 1
        end do

        ! shift the third array heap
        do i4 = i1, nitm
            int_array(ioffi + i4) = int_array(ioffi + i4 + ishft)
        end do

        ! shift the second array heap
        do i4 = i1, nitm * 2 + iods
            int_array(ioffc + i4) = int_array(ioffc + i4 + ishft)
            char_arr(ioffc + i4) = char_arr(ioffc + i4 + ishft)
        end do
        nitm = nitm - ishft
        ioffi = ioffi - ishft
        ioffc = ioffc - 1
        ioffi = ioffi - 1
        count_missing = count_missing + ishft

        ! shift the base array heap
        do i5 = ioffd + i2, ioffd + ntt + nitm * 2 + iods
            int_array(i5) = int_array(i5 + 1)
            char_arr(i5) = char_arr(i5 + 1)
        end do

        ! renumber the second array heap
        do i4 = i1, nitm
            if (int_array(ioffc + i4) > i2) then
                int_array(ioffc + i4) = int_array(ioffc + i4) - 1
            end if
        end do

        ! update totals
        if (iorder == 1 .or.  iods > 0) then
            idmnr = idmnr - 1
            nodim = nodim - ishft
        else if (iorder == 2 .and. iods == 0) then
            itmnr = itmnr - 1
            noitm = noitm - ishft
        endif

        if (timon) call timstop(ithndl)
        return

        1010 format (' ', A, ': Input item : ', I3, ' not resolved: ', A)
        1030 format (' ', A, ': Item number: ', I3, ' also not resolved: ', A)
        1040 format (' ', A, ': Substance  : ', I3, ' also not resolved: ', A)

    end subroutine compact_usefor_list

end module m_usefor
