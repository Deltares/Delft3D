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

    private
    public extract_file_extension, get_file_name, get_file_name_without_extension

    character(len=1), parameter :: path_separator_unix = '/'
    character(len=1), parameter :: path_separator_windows = '\\'

contains

    !> get file extension and extension dot position an extension length without dot if no extension position is last blank
    subroutine extract_file_extension(file_name, file_extension, extension_dot_position, extension_length)
        character(len=*), intent(in) :: file_name
        integer(kind=int_wp), intent(out) :: extension_length !! extension length without dot
        character(len=*), intent(out) :: file_extension
        integer(kind=int_wp), intent(out) :: extension_dot_position !! the position of the extension dot in the
        !! filename. If there's no extension, it is set to the position of the last blank space.

        integer(kind=int_wp) :: file_name_length, last_non_blank_position, char_index

        ! Initialize output variables
        file_extension = ' '
        extension_dot_position = 1
        extension_length = 0
        ! Return if filename is blank
        if (file_name == ' ') return

        file_name_length = len(file_name)

        ! get last non blank, last point after directory seperator

        ! Iterating through the filename to find the last non-blank character and the position of the last dot
        extension_dot_position = 0
        last_non_blank_position = 0
        do char_index = file_name_length, 1, -1
            if (last_non_blank_position == 0 .and. file_name(char_index:char_index) /= ' ') then
                last_non_blank_position = char_index
            end if

            if (file_name(char_index:char_index) == path_separator_windows .or. &
                file_name(char_index:char_index) == path_separator_unix) exit

            if (file_name(char_index:char_index) == '.') then
                extension_dot_position = char_index
                exit
            end if
        end do

        ! Extracting the extension if a dot is found
        if (extension_dot_position > 0) then
            extension_length = last_non_blank_position - extension_dot_position
            if (extension_length > 0) file_extension = file_name(extension_dot_position + 1:last_non_blank_position)
        else
            extension_dot_position = min(last_non_blank_position + 1, file_name_length)
        end if

    end subroutine extract_file_extension

    !> Gets the file name of a path
    function get_file_name(path) result(file_name)
        character(len=*), intent(in) :: path !< Path to extract file name from
        character(:), allocatable :: file_name

        integer(kind=int_wp) :: index_path_separator
        integer(kind=int_wp) :: index_linux
        integer(kind=int_wp) :: index_windows

        index_linux = index(path, path_separator_unix, back=.true.)
        index_windows = index(path, path_separator_windows, back=.true.)
        index_path_separator = max(index_linux, index_windows)

        if (index_path_separator == 0) then
            file_name = path
            return
        end if

        file_name = path(index_path_separator + 1:)
    end function

    !> Gets the file name of a path without the extension
    function get_file_name_without_extension(path) result(file_name_without_extension)
        character(len=*), intent(in) :: path !< Name of the file
        character(:), allocatable :: file_name_without_extension !< File name without the extension

        ! local
        integer(kind=int_wp) :: index_extension
        character(:), allocatable :: file_name

        file_name = get_file_name(path)

        index_extension = index(file_name, '.', back=.true.)
        if (index_extension == 0) then
            file_name_without_extension = file_name
            return
        end if

        file_name_without_extension = file_name(1:index_extension-1)
    end function
end module m_file_path_utils
