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


    subroutine attout (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
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

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(2), increm(2), noseg, noflux
        integer(kind = int_wp) :: iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        integer(kind = int_wp) :: ip (2)
        integer(kind = int_wp) :: iseg
        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: attrib

        ip = ipoint

        do iseg = 1, noseg
            idx = pmsa(ip(1))
            if (idx==0) then
                attrib = iknmrk(iseg)
            else
                call extract_waq_attribute(idx, iknmrk(iseg), attrib)
            endif
            ! Store the value
            pmsa(ip(2)) = attrib

            ! Next segment
            ip = ip + increm
        end do
        return
    end

end module m_attout
