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
module m_get_index

    implicit none

contains


    integer function get_index(nosubs, subst, fract)
        !
        !     programmer : antoon koster
        !     function   : returns the index for substance fract in the
        !                  substance array subst(1:nosubs)
        !     date       : may 2004
        !
        use timers
        implicit none
        integer, intent(in) :: nosubs
        integer :: lensub, lenfract
        character(len = *) :: fract
        character(len = *) :: subst(nosubs)

        integer :: indx, is
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("get_index", ithndl)

        indx = -1
        do is = 1, nosubs
            lensub = len_trim(subst(is))
            lenfract = len_trim(fract)
            if (subst(is)(:lensub) == fract(:lenfract)) then
                indx = is
                exit
            endif
        enddo
        get_index = indx
        if (timon) call timstop (ithndl)
        return
    end function get_index


end module m_get_index
