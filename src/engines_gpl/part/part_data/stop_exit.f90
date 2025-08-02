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
module m_stop_exit

    implicit none

contains


    subroutine stop_exit(iexit)

        ! stops execution if possible with return value
        integer :: iexit    !! input   return value
        integer :: lunfil

        if (iexit == 0) then
            write (*, *) 'Normal end'
        else
            write (*, *) 'Simulation stopped because of errors - check the report'
        endif

        open  (newunit = lunfil, file = 'delpar.rtn') !     logical units      : 13 - return file
        write (lunfil, *) iexit
        close (lunfil)

        select case (iexit)
        case (:0)
            stop 0
        case (1)
            stop 1
        case (2)
            stop 2
        case (3)
            stop 3
        case (4)
            stop 4
        case (5)
            stop 5
        case (6)
            stop 6
        case (7)
            stop 7
        case (8)
            stop 8
        case default
            stop 255
        end select

    end subroutine

end module m_stop_exit
