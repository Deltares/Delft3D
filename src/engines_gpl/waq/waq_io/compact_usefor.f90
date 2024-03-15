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
module m_compact_usefor
    use m_waq_precision

    implicit none

contains


    subroutine compact_usefor (lunut, waq_param, data_param, i, icnt)

        !     Deltares Software Centre

        !     function : compacts usefor lists if unresolved externals

        !     Global declarations

        use dlwq_hyd_data !   for definition and storage of data
        use timers        !   performance timers

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        type(t_dlwq_item), intent(inout) :: waq_param    ! list of param items to be set in this block ( substances etc )
        type(t_dlwq_item), intent(inout) :: data_param   ! list of param items in the data
        integer(kind = int_wp), intent(in) :: i             ! item index
        integer(kind = int_wp), intent(inout) :: icnt          ! shift in item index

        ! local declaration

        character(len = 20) :: chulp        ! item name
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
            chulp = waq_param%name(i4)
            if (data_param%name(i) /= chulp) then
                write (lunut, 1030) i4, chulp
            endif
        endif
        if (i2 > 0 .and. i2 <  100000) then
            i4 = i2
            chulp = waq_param%name(i2)
            if (data_param%name(i) /= chulp) then
                write (lunut, 1030)  i2, chulp
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

    end

end module m_compact_usefor
