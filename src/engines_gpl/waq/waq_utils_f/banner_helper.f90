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

!> Module for methods related to creating a banner
module m_banner_helper
    use m_waq_precision
    use m_banner_information
    use iso_c_binding, only: c_new_line, c_char

    implicit none

    private

    character(kind=c_char), parameter :: new_line = c_new_line

    public :: generate_banner

contains

    !> Generates a banner based on the provided banner information
    function generate_banner(information) result(banner)
        use m_string_utils, only: centre_text
        type(banner_information), intent(in) :: information !< Information regarding the program
        character(:), allocatable :: banner !< Resulting banner

        integer(kind=int_wp) :: text_width     !< Total width for text
        integer(kind=int_wp) :: tot_text_width !< Total width needed for the banner
        integer(kind=int_wp) :: margin = 2     !< Required margin for the banner

        character(:), allocatable :: empty_banner_line
        character(:), allocatable :: rule_banner_line

        text_width = information%get_max_width()
        tot_text_width = (margin * 2) + text_width

        empty_banner_line = create_banner_line('', tot_text_width, margin)
        rule_banner_line = '+'//repeat('-', tot_text_width)//'+'//new_line

        banner = rule_banner_line
        banner = banner//'|'//centre_text(information%suite_name, tot_text_width)//'|'//new_line
        banner = banner//rule_banner_line
        banner = banner//empty_banner_line

        banner = banner//create_banner_line(information%name, tot_text_width, margin)
        banner = banner//create_banner_line(repeat('-', len(information%name)), tot_text_width, margin)

        banner = banner//create_banner_line(information%description, tot_text_width, margin)
        banner = banner//empty_banner_line
        banner = banner//create_banner_line('Version: '//information%version_string, tot_text_width, margin)
        banner = banner//create_banner_line('Built on: '//information%built_on, tot_text_width, margin)

        banner = banner//empty_banner_line
        banner = banner//create_banner_line(information%copyright, tot_text_width, margin)

        banner = banner//rule_banner_line
    end function generate_banner

    !> Creates a line for the banner containing the provided text
    function create_banner_line(text, line_width, margin) result(banner_line)
        character(*), intent(in) :: text                !< Text for banner line
        integer(kind=int_wp), intent(in) :: line_width  !< Width of the line
        integer(kind=int_wp), intent(in) :: margin      !< Margin left and right

        character(:), allocatable :: banner_line !< Resulting text

        banner_line = '|'//repeat(' ', margin)//text//repeat(' ', line_width - (len(text) + margin))//'|'//new_line

    end function create_banner_line

end module m_banner_helper
