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
module m_read_version_number
    use m_waq_precision
    use m_logger_helper, only : stop_with_error

    implicit none

contains


    subroutine read_version_number (input_file, lfile, lunut, npos, input_version_number, output_verbose_level)

        !! Searches and reads the input file for the version string
        !! The version string looks like DELWAQ_VERSION_n.nnn\n
        !! It can be placed anywhere in the input file\n
        !! The n.nnn number is used to determine how to parse the
        !! input file

        integer(kind = int_wp), intent(in) :: input_file             !< unit number input file
        character(len = *), intent(in) :: lfile             !< file name
        integer(kind = int_wp), intent(in) :: lunut             !< unit number report file
        integer(kind = int_wp), intent(in) :: npos              !< number of significant positions in one line
        real(kind = real_wp), intent(out) :: input_version_number            !< Version number
        integer(kind = int_wp), intent(out) :: output_verbose_level            !< Output option

        ! Local
        character(len = npos) char_arr                              !  read buffer
        character(len = 1)      ctrlz, ch_cr                    !  special characters
        integer(kind = int_wp) :: i, i2                            !  loop counter
        integer(kind = int_wp) :: status                           !  iostatus

        ch_cr = char(13)
        ctrlz = char(26)

        input_version_number = 0.0
        output_verbose_level = -1
        status = 0
        do while (status == 0)
            read (input_file, '(a)', iostat = status) char_arr

            ! search the tokens, read the numbers

            do i = 1, npos - 19
                if (char_arr(i:i + 14) == 'DELWAQ_VERSION_') then
                    do i2 = i + 15, i + 19
                        if (char_arr(i2:i2) == ctrlz .or. &
                                char_arr(i2:i2) == ch_cr) char_arr(i2:i2) = ' '
                    enddo
                    read  (char_arr(i + 15:i + 19), '(f5.0)') input_version_number
                    write (lunut, '(a,a,f6.3)') '       ---------->', &
                            ' Version number of the input file is: ', input_version_number
                endif
                if (char_arr(i:i + 19) == 'PRINT_OUTPUT_OPTION_') then
                    read (char_arr(i + 20:i + 20), '(i1)') output_verbose_level
                    write (lunut, '(a,a,i1)') '       ---------->', &
                            ' Output level of the listfile is: ', output_verbose_level
                endif
            enddo
        enddo

        if (status < 0) then    !        end of file encountered
            rewind input_file
            read (input_file, '(a)') char_arr
            return
        else                         !        errors during read
            write (lunut, 2000) input_file, lfile
            call stop_with_error()
        endif

        ! output formats

        2000 format (' ERROR, reading file on unit', I3, ' !!', &
                /' Filename is: ', A20, &
                /' ********** EXECUTION TERMINATED ********')

    end subroutine read_version_number


    subroutine compare_version_number_to_lower_limit (input_version_number, lunut)

        !! Compares version number with lower limit
        !! if input file is too old -> exit program

        real(kind = real_wp), intent(in) :: input_version_number              ! Version number
        real(kind = real_wp) :: lower_limit_version ! lower limit of version numbers
        integer(kind = int_wp), intent(in) :: lunut               ! unit number report file

        lower_limit_version = 4.91

        if (input_version_number < lower_limit_version) then
            write (lunut, 2010) input_version_number, lower_limit_version
            call stop_with_error()
        endif

        ! output formats

        2010 format (' ERROR, version number of input file is: ', F4.2, &
                /' Lowest supported version is: ', F4.2, &
                /' ********** EXECUTION TERMINATED ********')

    end subroutine compare_version_number_to_lower_limit

end module m_read_version_number
