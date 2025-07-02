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

module m_waq_external_access_layer_utils
    use m_waq_precision
    use m_connection_data

    implicit none
contains

    !> Find the key in the registered connections and return its index
    !!
    !! Result: Index into the connection array or 0 if not found
    !!
    !! Note: Internal to get_var_ptr
    integer function key_index(key_name, connection)
        character(len=*), intent(in) :: key_name    !< Connection key to find
        type(connection_data), dimension(:), intent(in) :: connection  !< Array storing the connection information

        integer(kind=int_wp) :: i

        key_index = 0
        do i = 1, size(connection)
            if (connection(i)%exchange_name == key_name) then
                key_index = i
                exit
            end if
        end do
    end function key_index

    !> Update the internal data in case we have incoming data
    subroutine update_from_incoming_data(data_manager)
        use delwaq_loads
        use m_waq_api_categories
        use m_connection_manager

        type(connection_manager), intent(in) :: data_manager

        !! external components
        type(connection_data), allocatable :: connection
        type(connection_wrapper), dimension(:), allocatable :: category_connections
        integer(kind=int_wp) :: i

        ! Handle waste loads
        category_connections = data_manager%get_incoming_connections_by_category(category_wasteload)
        do i = 1, size(category_connections)
            call update_wasteload(category_connections(i)%connection_ptr)
        end do

        ! Handle constants (process parameters)
        category_connections = data_manager%get_incoming_connections_by_category(category_procparam)
        do i = 1, size(category_connections)
            call update_process_parameters(category_connections(i)%connection_ptr)
        end do
    end subroutine update_from_incoming_data

    subroutine update_process_parameters(connection)
        use delwaq2_global_data
        type(connection_data), intent(in) :: connection !< Connection to update

        dlwqd%buffer%rbuf(connection%buffer_idx) = connection%p_value
    end subroutine update_process_parameters

    subroutine update_wasteload(connection)
        use delwaq_loads, only: wasteloads, wasteload
        use delwaq2_global_data, only: substance_name
        use m_string_utils, only: index_in_array
        type(connection_data), intent(in) :: connection !< Connection to update

        type(wasteload), pointer :: waste_load
        integer(kind=int_wp) :: substance_index

        if (.not. allocated(wasteloads)) then
            return
        end if

        waste_load => wasteloads(connection%buffer_idx)

        if (.not. allocated(waste_load%set_factor)) then
            allocate (waste_load%set_factor(1:size(substance_name) + 1))
            waste_load%set_factor = 1.0
        end if

        waste_load%set_factor(connection%substance_index) = connection%p_value
    end subroutine update_wasteload
end module m_waq_external_access_layer_utils
