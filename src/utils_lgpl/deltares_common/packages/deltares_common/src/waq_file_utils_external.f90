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
module waq_file_utils_external

    implicit none

contains

    subroutine create_new_file_unit_number(start_index, file_unit_number)
        !! sets next free unit number, starting at start_index till start_index + 98

        integer, intent(in) :: start_index        !< start looking from here
        integer, intent(out) :: file_unit_number   !< next free unit number

        integer :: ilun   !< loop counter
        logical :: lopen  !< opened indicator

        file_unit_number = 0
        do ilun = start_index, start_index + 100000
            inquire(ilun, opened = lopen)
            if (.not. lopen) then
                file_unit_number = ilun
                exit
            endif
        enddo
    end subroutine create_new_file_unit_number

    subroutine get_filepath_and_pathlen(file_name, path, path_len)
        !! get file path and path length including last separator

        integer :: path_len
        character(len = *) file_name, path

        integer :: name_len, ich
        character :: dirsep_dos, dirsep_ux

        dirsep_dos = char(92)
        dirsep_ux = char(47)

        ! blank name get out of here
        path = ' '
        path_len = 0
        if (file_name == ' ') return

        name_len = len(file_name)

        ! get last directory seperator
        do ich = name_len, 1, -1
            if (file_name(ich:ich) == dirsep_dos .or. file_name(ich:ich) == dirsep_ux) then
                path_len = ich
                exit
            endif
        enddo

        if (path_len > 0) then
            path = file_name(1:path_len)
        endif

    end subroutine get_filepath_and_pathlen
end module waq_file_utils_external
