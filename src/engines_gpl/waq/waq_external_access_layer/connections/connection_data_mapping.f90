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

!> Module containing methods for setting data for connection_data objects
!! (getting and setting the pointers to and from Delwaq data)
module m_connection_data_mapping
    use m_waq_precision
    use m_connection_data

    implicit none

    private
    public :: set_connection_data

contains

    !> Set the connection data for the given connection
    !! (connection.buffer_idx)
    subroutine set_connection_data(connection)
        use m_waq_api_categories
        type(connection_data), intent(inout) :: connection !< connection to set

        ! currently supported categories
        select case (connection%category)
        case (category_wasteload)
            call set_wasteload_connection_data(connection)
        case (category_segment)
            call set_segment_connection_data(connection)
        case (category_monitorpoint)
            call set_monitorpoint_connection_data(connection)
        case (category_procparam)
            call set_procparam_connection_data(connection)
        end select

        allocate (connection%p_value)

    end subroutine set_connection_data

    !> Set the connection data for a wasteload connection
    subroutine set_wasteload_connection_data(connection)
        use delwaq2_global_data, only: load_name
        use m_real_array_indices, only: iwste
        use m_string_utils, only: string_equals

        type(connection_data), intent(inout) :: connection !< connection to set

        character(:), allocatable :: location_id
        character(:), allocatable :: substance_id
        integer(kind=int_wp) :: location_index

        if (.not. connection%has_location_filter) then
            ! todo : add error if no location filter is specified
            ! this is required for wasteloads
            return
        end if

        connection%data_index = get_data_index(connection, load_name)

        ! The substance/parameter may be "FLOW": handle this carefully.
        if (string_equals(connection%subst_name, 'FLOW')) then
            connection%substance_index = 1
        else
            ! add 1 for "flow" substance
            connection%substance_index = get_substance_index(connection) + 1
        end if

        ! If the waste load is to be set, we set the flow, hence store the index
        ! of the waste load, not that of the individual value
        if (connection%incoming) then
            connection%buffer_idx = connection%data_index
        else
            connection%buffer_idx = get_connection_buffer_index(connection, iwste, 1)
        end if
    end subroutine

    !> Set the connection data for a segment connection
    subroutine set_segment_connection_data(connection)
        use m_real_array_indices, only: iconc
        type(connection_data), intent(inout) :: connection !< connection to set

        if ((.not. connection%has_location_filter) .or. connection%location_index <= 0) then
            ! todo : add error if no location filter is specified
            ! this is required for segments
            return
        end if

        connection%data_index = connection%location_index
        connection%substance_index = get_substance_index(connection)

        connection%buffer_idx = get_connection_buffer_index(connection, iconc)
    end subroutine

    !> Set the connection data for a monitorpoint connection
    subroutine set_monitorpoint_connection_data(connection)
        use delwaq2_global_data, only: monitor_name, monitor_cell
        use m_real_array_indices, only: iconc
        type(connection_data), intent(inout) :: connection !< connection to set

        integer(kind=int_wp) :: monitorpoint_index

        connection%data_index = get_data_index(connection, monitor_name, monitor_cell)
        connection%substance_index = get_substance_index(connection)

        connection%buffer_idx = get_connection_buffer_index(connection, iconc)
    end subroutine

    !> Set the connection data for a procparam connection
    subroutine set_procparam_connection_data(connection)
        use delwaq2_global_data, only: procparam_const
        use m_real_array_indices, only: icons
        use m_string_utils, only: index_in_array

        type(connection_data), intent(inout) :: connection !< connection to set

        ! look up the index of the parameter in the procparam_const array
        connection%data_index = index_in_array(connection%subst_name, procparam_const)
        connection%buffer_idx = icons - 1 + connection%data_index
    end subroutine

    !> Determine substance index based on subst_name
    function get_substance_index(connection) result(substance_index)
        use delwaq2_global_data, only: substance_name
        use m_string_utils, only: index_in_array

        type(connection_data), intent(in) :: connection !< connection to use
        integer(kind=int_wp) :: substance_index !< index of the substance in the substances array

        substance_index = index_in_array(connection%subst_name, substance_name)
    end function

    !> Determine location index based on location_text (id), if needed
    function get_data_index(connection, location_names, location_lookup) result(data_index)
        use m_string_utils, only: index_in_array
        type(connection_data), intent(in) :: connection !< connection to use
        character(len=*), dimension(:), intent(in) :: location_names !< list names to find the index in
        integer(kind=int_wp), dimension(:), intent(in), allocatable, optional :: location_lookup !< lookup for location index

        integer(kind=int_wp) :: data_index
        character(:), allocatable :: location_id

        ! check if index has already been set
        if (connection%data_index > 0) then
            data_index = -1
            return
        end if

        ! check for location index
        if (connection%location_index > 0) then
            data_index = connection%location_index
        else
            data_index = index_in_array(connection%location_text, location_names)
        end if

        if (present(location_lookup)) then
            data_index = location_lookup(data_index)
        end if

        ! todo : add error if index can not be found
    end function

    !> Determine the buffer index for the connection based on the index offset, substance index and connection location index
    function get_connection_buffer_index(connection, index_offset, extra_substances_count) result(buffer_idx)
        use m_waq_memory_dimensions, only: num_substances_total

        type(connection_data), intent(in) :: connection  !< connection to set
        integer(kind=int_wp), intent(in) :: index_offset    !< offset to use for the index
        integer(kind=int_wp), intent(in), optional :: extra_substances_count !< number of extra substances
        integer(kind=int_wp) :: buffer_idx !< number of extra substances

        integer(kind=int_wp) :: substance_count

        if (present(extra_substances_count)) then
            substance_count = num_substances_total + extra_substances_count
        else
            substance_count = num_substances_total
        end if

        buffer_idx = index_offset - 1 + connection%substance_index + &
                     (connection%data_index - 1) * substance_count
    end function

end module m_connection_data_mapping
