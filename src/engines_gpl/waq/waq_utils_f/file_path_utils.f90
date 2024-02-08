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
module m_file_path_utils
    use m_waq_precision

    implicit none

contains


    SUBROUTINE extract_file_extension(file_name, file_extension, extension_dot_position, extension_length)
        !! get file extension and extension dot position an extension length without dot if no extension position is
        !! last blank
        character*(*), intent(in) :: file_name
        integer(kind = int_wp), intent(out) :: extension_length !! extension length without dot
        character*(*), intent(out) :: file_extension
        integer(kind = int_wp), intent(out) :: extension_dot_position !! the position of the extension dot in the
        !! filename. If there's no extension, it is set to the position of the last blank space.

        integer(kind = int_wp) :: file_name_length, last_non_blank_position, char_index
        character :: dir_separator_windows, dir_separator_unix

        dir_separator_windows = CHAR(92) ! ASCII code for backslash
        dir_separator_unix = CHAR(47) ! ASCII code for forward slash

        ! Initialize output variables
        file_extension = ' '
        extension_dot_position = 1
        extension_length = 0
        ! Return if filename is blank
        IF (file_name == ' ') RETURN

        file_name_length = LEN(file_name)

        ! get last non blank, last point after directory seperator

        ! Iterating through the filename to find the last non-blank character and the position of the last dot
        extension_dot_position = 0
        last_non_blank_position = 0
        do char_index = file_name_length, 1, -1
            if (last_non_blank_position == 0 .AND. file_name(char_index:char_index) /= ' ') then
                last_non_blank_position = char_index
            end if

            if (file_name(char_index:char_index) == dir_separator_windows .OR. &
                    file_name(char_index:char_index) == dir_separator_unix) exit

            IF (file_name(char_index:char_index) == '.') THEN
                extension_dot_position = char_index
                exit
            end if
        end do

        ! Extracting the extension if a dot is found
        if (extension_dot_position > 0) then
            extension_length = last_non_blank_position - extension_dot_position
            IF (extension_length > 0) file_extension = file_name(extension_dot_position + 1:last_non_blank_position)
        else
            extension_dot_position = MIN(last_non_blank_position + 1, file_name_length)
        endif

    end subroutine extract_file_extension
end module m_file_path_utils
