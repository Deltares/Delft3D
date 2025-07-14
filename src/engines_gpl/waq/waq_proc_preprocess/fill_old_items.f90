!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
module m_fill_old_items
    use m_waq_precision

    implicit none

contains

    subroutine fill_old_items(old_items)

        !     Deltares Software Centre

        !>/File
        !>      add the old items to the collection

        use timers         !< performance timers
        use processet      !< use processet definitions
        use m_process_lib_data !< tables read from proces definition file

        implicit none

        ! decalaration of arguments

        type(old_item_coll) :: old_items         !< the old_items table to be filled

        ! local declaration

        type(old_item) :: a_old_item        ! single process
        integer(kind = int_wp) :: i                 ! loop counter old items
        integer(kind = int_wp) :: i2                ! index in collection
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("fill_old_items", ithndl)

        do i = 1, n_old_items

            a_old_item%old_name = old_items_old_name(i)
            a_old_item%new_name = old_items_new_name(i)
            a_old_item%old_default = old_items_old_default(i)
            a_old_item%configuration = old_items_configuration(i)
            a_old_item%serial = old_items_serial(i)
            a_old_item%action_type = old_items_action_type(i)

            i2 = old_item_coll_add(old_items, a_old_item)

        enddo

        if (timon) call timstop(ithndl)
        return
    end

end module m_fill_old_items
