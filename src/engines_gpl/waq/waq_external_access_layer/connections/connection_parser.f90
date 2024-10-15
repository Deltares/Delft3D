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

!> module containing methods for parsing (bmi) connection strings
!! to connection_data objects
module m_connection_parser
    use m_waq_precision
    use m_connection_data

    implicit none

    public :: get_category_by_name, parse_connection_string

    character(*), parameter :: incoming_string = "TO_DELWAQ"
    character(*), parameter :: outgoing_string = "FROM_DELWAQ"

contains

    !> Parses the provided connection string to a connecion_data object
    !! examples :
    !! TO_DELWAQ|WASTE|1|FLOW - FROM_DELWAQ|SEGMN|9|OXY
    !! TO_DELWAQ|CONST|VWind - FROM_DELWAQ|SEGMN|9|OXY
    function parse_connection_string(connection_string) result(connection)
        use m_string_utils, only: string_equals, split_string

        character(*), intent(in) :: connection_string    !< key to create connection from
        type(connection_data), allocatable :: connection !< created connection

        character(:), dimension(:), allocatable :: string_parts
        type(connection_data) :: new_connection
        integer(kind=int_wp) :: location_index

        ! We require at least three parts, separated by a vertical bar ("|")
        string_parts = split_string(connection_string, "|")
        if (size(string_parts) < 3) then
            return
        end if

        new_connection%exchange_name = connection_string

        if (string_equals(trim(string_parts(1)), incoming_string, .true., .false.)) then
            new_connection%incoming = .true.
        else if (string_equals(trim(string_parts(1)), outgoing_string, .true., .false.)) then
            new_connection%incoming = .false.
        else
            return
        end if

        new_connection%category = get_category_by_name(trim(string_parts(2)))
        new_connection%has_location_filter = (size(string_parts) == 4)

        if (new_connection%has_location_filter) then
            new_connection%location_text = trim(string_parts(3))
            if (parse_location_index(trim(string_parts(3)), location_index)) then
                new_connection%location_index = location_index
            else
                ! set location index to -1 (not set yet)
                ! index will be determined later by parsing location_text property
                new_connection%location_index = -1
            end if
            new_connection%subst_name = trim(string_parts(4))
        else
            new_connection%subst_name = trim(string_parts(3))
        end if

        connection = new_connection
    end function

    !> parses category name to a category enum (see m_waq_api_categories)
    function get_category_by_name(category_name) result(category)
        use m_waq_api_categories

        character(*), intent(in) :: category_name   !< name of the category
        integer(kind=int_wp) :: category !< number of the category enum (see m_waq_api_categories)

        select case (category_name)
        case ('BOUND')
            category = category_boundary
        case ('WASTE')
            category = category_wasteload
        case ('SEGMN')
            category = category_segment
        case ('OBSRV')
            category = category_monitorpoint
        case ('CONST')
            category = category_procparam
        case ('HYDRO')
            category = category_hydrodynamics
        end select
    end function

    !> parses location index to an integer (location_index)
    function parse_location_index(text, location_index) result(is_int)
        use m_waq_api_categories

        character(*), intent(in) :: text !< text to parse
        integer(kind=int_wp), intent(out) :: location_index  !< parsed location index
        logical :: is_int !< parsing could be done to int

        integer(kind=int_wp) :: ierr

        read (text, *, iostat=ierr) location_index

        is_int = ierr == 0
    end function

end module m_connection_parser
