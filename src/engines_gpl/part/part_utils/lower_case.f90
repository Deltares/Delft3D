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
module m_lower_case

    implicit none

contains


    subroutine lower_case(string)
        use timers
        implicit none           !   force explicit typing
        integer :: i, j, newlen
        character(len = *) string
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("lower_case", ithndl)
        !
        newlen = len(string)
        do i = 1, newlen
            j = ichar (string(i:i))
            if ((j > 64) .and. (j <91)) then
                j = j + 32
                string(i:i) = char (j)
            endif
        enddo
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine lower_case

end module m_lower_case
