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
module m_delwaq1
    use m_waq_precision
    use m_delwaq1_write_messages
    use m_delwaq1_startup_screen
    use m_delwaq1_read_input_data, only : delwaq1_read_input_data
    use m_delwaq1_init
    use m_delwaq1_close_lunfiles

    implicit none

contains

    function delwaq1(argv) result(success)
        !> Reads the DELWAQ inputfiles and generates
        !> a consistent set of binairy intermediate files.

        use m_delwaq1_allocate_workspace
        !DEC$ ATTRIBUTES DLLEXPORT::delwaq1

        character(len = *), intent(in), dimension(:) :: argv !< arguments as strings
        logical :: success !< if the run was successful

        type(error_status) :: status

        call status%initialize(0, 0, 0)

        ! create the lst, delwaq04.wrk, harmonic.wrk, pointers.wrk, and filenaam.wrk files
        call delwaq1_init(argv)

        call delwaq1_startup_screen()
        call delwaq1_allocate_workspace(argv, status)

        if (status%ierr == 0) then
            call delwaq1_read_input_data(status)
            call delwaq1_write_messages(status)
        end if

        call delwaq1_close_lunfiles()

        success = status%ierr == 0
        ! Delwaq1_lib should never use a stop, but must be modified to return an error code instead (0 = normal end)

    end function delwaq1

end module m_delwaq1
