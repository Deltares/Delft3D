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

module nefis_data
    use m_waq_precision
    implicit none
    private
    public :: manage_nefis_data_character

contains

    !    subroutine manage_nefis_data_numeric(def_file_name, data_file_name, group_name, num_elements, &
    !            element_names, element_dims, element_types, element_sizes, &
    !            element_name, cell_id, write_log, error_flag, &
    !            buffer, file_descriptor)
    !        ! Handles reading from and writing to NEFIS files based on input parameters.
    !
    !        integer, intent(in) :: file_descriptor, num_elements
    !        integer, intent(in) :: buffer(*), cell_id
    !        integer, intent(in) :: element_dims(6, *), element_sizes(*)
    !        character(len = *), intent(in) :: element_names(num_elements), element_types(num_elements)
    !        character(len = *), intent(in) :: element_name, group_name
    !        character(len = *), intent(in) :: def_file_name, data_file_name
    !        logical, intent(in) :: write_log
    !        integer, intent(out) :: error_flag
    !
    !        ! local declarations
    !        integer, parameter :: start = 1, stopp = 2, increment = 3
    !
    !        integer :: buffer_length, num_dims_element
    !        integer :: i, error_code, j, n
    !        integer :: element_number
    !        integer :: dimension_element(5), user_index(3)
    !
    !        character(len = 2) :: access_mode
    !        character(len = 1) :: coding_scheme
    !        character(len = 16) :: element_quantity, element_anticipation
    !        character(len = 64) :: element_description
    !        character(len = 134) :: error_string
    !
    !
    !        !-External Functions
    !        integer :: clsnef, credat, crenef, defcel, defelm, &
    !                defgrp, getelt, inqelm, neferr, putelt
    !        external :: clsnef, credat, crenef, defcel, defelm, &
    !                defgrp, getelt, inqelm, neferr, putelt
    !
    !        ! Initialization
    !        coding_scheme = 'N'
    !        num_dims_element = 5
    !        user_index(start) = cell_id
    !        user_index(stopp) = cell_id
    !        user_index(increment) = 1
    !        element_anticipation = '  '
    !        element_quantity = '   '
    !        element_description = '    '
    !
    !        ! write or read data from nefis files
    !        if (write_log) then
    !            access_mode = 'u'
    !        else
    !            access_mode = 'r'
    !        endif
    !
    !        if (file_descriptor < 0) then
    !            error_flag = CRENEF(file_descriptor, data_file_name, def_file_name, coding_scheme, access_mode)
    !            if (error_flag /= 0 .and. .not.write_log) then
    !                error_flag = -211
    !                goto 10000
    !            endif
    !            if (error_flag /= 0) goto 9999
    !        endif
    !
    !        if (write_log) then
    !            error_flag = putelt(file_descriptor, group_name, element_name, user_index, 1, buffer)
    !        else
    !            j = 0
    !            123    continue
    !            j = j + 1
    !            if (element_name == element_names(j)) goto 124
    !            goto 123
    !            124    continue
    !            buffer_length = element_sizes(j) ! size single precision integer
    !            do i = 1, element_dims(1, j)
    !                buffer_length = buffer_length * element_dims(i + 1, j)
    !            enddo
    !            error_flag = getelt(file_descriptor, group_name, element_name, user_index, 1, buffer_length, buffer)
    !            if (error_flag /= 0) goto 9999
    !        endif
    !
    !        ! error_flag:
    !        !   writing: most likely error_flag non existing group, so define it
    !        !   reading: error_flag, no error_flag expected
    !
    !        if (error_flag /= 0 .and. write_log) then
    !            ! Create elements
    !            do element_number = 1, num_elements
    !                error_flag = DEFELM(file_descriptor, element_names(element_number), &
    !                        element_types(element_number), element_sizes(element_number), &
    !                        element_quantity, element_anticipation, &
    !                        element_description, element_dims(1, element_number), &
    !                        element_dims(2, element_number))
    !                !      most likely error_flag, element already exist
    !                error_flag = 0
    !            end do
    !            ! Create cells
    !            error_flag = DEFCEL(file_descriptor, group_name, num_elements, element_names)
    !            if (error_flag /= 0) goto 9999
    !            ! Create group on definition file
    !            error_flag = DEFGRP(file_descriptor, group_name, group_name, 1, 0, 1)
    !            if (error_flag /= 0) goto 9999
    !            ! Create group on data       file
    !            error_flag = CREDAT(file_descriptor, group_name, group_name)
    !            if (error_flag /= 0) goto 9999
    !            ! try again to write data
    !            error_flag = putelt(file_descriptor, group_name, element_name, user_index, 1, buffer)
    !            if (error_flag /= 0) goto 9999
    !        endif
    !
    !        ! No error_flag when reading elements
    !        if (error_flag == 0 .and. .not.write_log) then
    !            write(*, *) 'manage_nefis_data_numeric'
    !            write(*, *) element_name
    !            write(*, *) element_types
    !            write(*, *) element_quantity
    !            write(*, *) element_anticipation
    !            write(*, *) element_description
    !            error_flag = INQELM(file_descriptor, element_name, element_types, element_sizes, element_quantity, element_anticipation, element_description, num_dims_element, dimension_element)
    !
    !            if (error_flag  /= 0) goto 9999
    !            element_number = 0
    !            do n = 1, num_elements
    !                if (element_name == element_names(n)) then
    !                    element_number = n
    !                    exit
    !                endif
    !            end do
    !
    !            if (element_number /= 0) goto 9999
    !
    !            do i = 1, num_dims_element
    !
    !                ! Compare local and global dimensions, not equal
    !                ! => new error_flag number and exit
    !                if (dimension_element(i) /= element_dims(1 + i, element_number)) then
    !                    error_flag = -15025
    !                    exit
    !                endif
    !            end do
    !        endif
    !        goto 10000
    !
    !        9999 continue
    !        if (error_flag /= 0) error_code = Neferr(1, error_string)
    !        10000 continue
    !
    !    end subroutine manage_nefis_data_numeric

    subroutine manage_nefis_data_character(def_file_name, data_file_name, group_name, num_elements, &
            element_names, element_dims, element_types, element_sizes, &
            element_name, cell_id, write_log, error_flag, &
            buffer, file_descriptor)

        integer, intent(in) :: file_descriptor, num_elements
        integer, intent(in) :: cell_id
        character(len = *), intent(in) :: buffer(*)
        integer, intent(in) :: element_dims(6, *), element_sizes(*)
        character(len = *), intent(in) :: element_names(num_elements), element_types(num_elements)
        character(len = *), intent(in) :: element_name, group_name
        character(len = *), intent(in) :: def_file_name, data_file_name
        logical, intent(in) :: write_log
        integer, intent(out) :: error_flag

        ! local declarations
        integer, parameter :: start = 1, stopp = 2, increment = 3
        integer(kind = int_wp) :: buffer_length, num_dims_element
        integer(kind = int_wp) :: i, j, n
        integer(kind = int_wp) :: error_code
        integer(kind = int_wp) :: element_number, ind
        integer(kind = int_wp) :: dimension_element(5), user_index(3)

        character(len = 2) :: access_mode
        character(len = 1) :: coding_scheme
        character(len = 16) :: element_quantity, element_anticipation
        character(len = 64) :: element_description
        character(len = 134) :: error_string

        ! External Functions
        integer(kind = int_wp) :: clsnef, credat, crenef, defcel, defelm, &
                defgrp, getels, inqelm, neferr, putels
        external        clsnef, credat, crenef, defcel, defelm, &
                defgrp, getels, inqelm, neferr, putels
        ! Initialization

        coding_scheme = 'N'
        num_dims_element = 5
        user_index(start) = cell_id
        user_index(stopp) = cell_id
        user_index(increment) = 1

        element_quantity = ' '
        element_description = '  '
        element_anticipation = '   '

        ! write or read data from nefis files
        if (write_log) then
            access_mode = 'u'
        else
            access_mode = 'r'
        endif

        if (file_descriptor < 0) then
            error_flag = CRENEF (file_descriptor, data_file_name, def_file_name, coding_scheme, access_mode)
            if (error_flag/=0 .and. .not.write_log) then
                error_flag = -211
                goto 10000
            endif
            if (error_flag/=0) goto 9999
        endif

        if (write_log) then
            error_flag = putels(file_descriptor, group_name, element_name, user_index, 1, buffer)
        else
            j = 0
            123    continue
            j = j + 1
            if (element_name == element_names(j)) goto 124
            goto 123
            124    continue
            buffer_length = element_sizes(j) ! size single precision integer
            do i = 1, element_dims(1, j)
                buffer_length = buffer_length * element_dims(i + 1, j)
            enddo
            error_flag = getels(file_descriptor, group_name, element_name, user_index, 1, buffer_length, buffer)
            if (error_flag/=0) goto 9999
        endif

        ! error_flag:
        !     writing: most likely error_flag non existing group, so define it
        !     reading: error_flag, no error_flag expected

        if (error_flag /= 0 .and. write_log) then
            ! Create elements
            do element_number = 1, num_elements
                error_flag = DEFELM(file_descriptor, element_names(element_number), &
                        element_types(element_number), element_sizes(element_number), &
                        ' ', ' ', &
                        ' ', element_dims(1, element_number), &
                        element_dims(2, element_number))
                ! most likely error_flag, element already exist
                error_flag = 0
            end do
            ! Create cells
            error_flag = DEFCEL(file_descriptor, group_name, num_elements, element_names)
            if (error_flag /= 0) goto 9999
            ! Create group on definition file
            error_flag = DEFGRP(file_descriptor, group_name, group_name, 1, 0, 1)
            if (error_flag /= 0) goto 9999
            ! Create group on data       file
            error_flag = CREDAT(file_descriptor, group_name, group_name)
            if (error_flag /= 0) goto 9999
            ! try again to write data
            error_flag = putels(file_descriptor, group_name, element_name, user_index, 1, buffer)
            if (error_flag /= 0) goto 9999
        endif
        !
        !     No error_flag when reading elements
        !
        if (error_flag == 0 .and. .not.write_log) then
            write(*, *) 'putget'
            write(*, *) element_name
            write(*, *) element_types
            write(*, *) element_quantity
            write(*, *) element_anticipation
            write(*, *) element_description
            error_flag = INQELM(file_descriptor, element_name, element_types, element_sizes, &
                    element_quantity, element_anticipation, element_description, num_dims_element, dimension_element)

            if (error_flag /= 0) goto 9999
            element_number = 0
            do n = 1, num_elements
                if (element_name == element_names(n)) then
                    element_number = n
                    goto 220
                endif
            end do
            220   continue
            if (element_number /= 0) goto 9999

            do i = 1, num_dims_element

                ! Compare local and global dimensions, not equal
                ! => new error_flag number and exit

                if (dimension_element(i) /= element_dims(1 + i, element_number)) then
                    error_flag = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000

        9999 continue
        if (error_flag /= 0) error_code = Neferr(1, error_string)
        10000 continue

        return
    end subroutine manage_nefis_data_character

end module nefis_data