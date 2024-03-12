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
module m_delwaq1_startup_screen

    implicit none

contains

    !>\file
    !>                    delwaq1_startup_screen
    !     SUBROUTINES CALLED :
    !                         *startup_screen, writes startup screen

    subroutine delwaq1_startup_screen()
        use m_startup_screen
        use m_delwaq1_data

        implicit none

        !     Show startup screen
        call startup_screen(lunrep)
        write (*, *)
        write (*, '(A9,A)') '  runid: ', trim(runid)
        write (*, *)
    end subroutine delwaq1_startup_screen
end module m_delwaq1_startup_screen
