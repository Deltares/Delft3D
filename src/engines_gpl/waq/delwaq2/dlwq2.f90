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

program dlwq2

    use m_delwaq2
    use delwaq2_version_module

    implicit none

    integer :: argc
    character(len = 256), allocatable :: argv(:)
    character(len = 120) :: idstr
    integer(4) :: errorcode
    integer(4) :: i
    integer(4) :: lunfil

    call getfullversionstring_delwaq2(idstr)

    argc = command_argument_count() + 1

    allocate (argv (argc))
    do i = 1, argc
        call get_command_argument(i - 1, argv(i))
    end do

    call delwaq2(argc, argv, errorcode)

    open  (newunit = lunfil, file = 'delwaq.rtn')
    write (lunfil, *) errorcode
    close (lunfil)

    if (errorcode == 0) then
        write (*, *) ' Normal end'
    else
        write (*, *) ' Error code:', errorcode
        stop 1
    end if

end program
