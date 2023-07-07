!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

subroutine delwaq1(argc, argv, errorcode)

!DEC$ ATTRIBUTES DLLEXPORT::delwaq1

!>\file
!>                    DELWAQ - INPUT PROGRAMME
!>
!>                    Reads the DELWAQ inputfiles and generates
!>                    a consistent set of binairy intermediate files.
!
!     SUBROUTINES CALLED :
!                         delwaq1_init, initializes timer and values
!                         delwaq1_startup_screen
!                         delwaq1_allocate_workspace
!                         delwaq1_read_user_data
!                         delwaq1_write_messages
!                         delwaq1_close_lunfiles

    implicit none

    integer, intent(in)                           :: argc
    character(len=*), dimension(argc), intent(in) :: argv
    integer, intent(out)                          :: errorcode

    errorcode = 0

    call delwaq1_init(argc, argv)
    call delwaq1_startup_screen()
    call delwaq1_allocate_workspace(argc, argv, errorcode)
    if (errorcode==0) then
       call delwaq1_read_input_data()
       call delwaq1_write_messages(errorcode)
    endif
    call delwaq1_close_lunfiles()

    ! Delwaq1_lib should never use a stop, but must be modified to return an error code instead (0 = normal end)
    return
end subroutine delwaq1
