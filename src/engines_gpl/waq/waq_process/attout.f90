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
module m_attout
    use m_waq_precision

    implicit none

contains


    subroutine attout (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Returns the selected attribute

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! IDX     I*4 1 I  selected attribute number (note the type!)         [-]
        ! ATTRIB  I*4 1 O  value of the attribute                             [-]
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(2), increm(2), num_cells, noflux
        integer(kind = int_wp) :: iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        integer(kind = int_wp) :: ip (2)
        integer(kind = int_wp) :: iseg
        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: attrib

        ip = ipoint

        do iseg = 1, num_cells
            idx = process_space_real(ip(1))
            if (idx==0) then
                attrib = iknmrk(iseg)
            else
                call extract_waq_attribute(idx, iknmrk(iseg), attrib)
            endif
            ! Store the value
            process_space_real(ip(2)) = attrib

            ! Next segment
            ip = ip + increm
        end do
        return
    end

end module m_attout
