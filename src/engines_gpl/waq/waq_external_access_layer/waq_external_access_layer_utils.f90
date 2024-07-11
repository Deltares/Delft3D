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

module  m_waq_external_access_layer_utils
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
    !!
    !!Note: Used by update()
    subroutine update_from_incoming_data(connection)
        use delwaq_loads
        use m_waq_api_categories
        use delwaq2_global_data

        type(connection_data), dimension(:), intent(in) :: connection !< Information about the connections to
        !! external components

        integer(kind=int_wp) :: idx, isys, iwaste

        !
        ! Handle waste loads
        !
        if (allocated(wasteloads)) then
            do idx = 1, size(connection)
                if (connection(idx)%category == category_wasteload .and. connection(idx)%incoming) then
                    iwaste = connection(idx)%buffer_idx
                    if (.not. allocated(wasteloads(iwaste)%set_factor)) then
                        allocate (wasteloads(iwaste)%set_factor(1:size(substance_name) + 1))
                        wasteloads(iwaste)%set_factor = 1.0
                    end if

                    isys = connection(idx)%system_idx
                    wasteloads(iwaste)%set_factor(isys) = connection(idx)%p_value
                end if
            end do
        end if

        !
        ! Handle constants (process parameters)
        !
        do idx = 1, size(connection)
            if (connection(idx)%category == category_procparam .and. connection(idx)%incoming) then
                dlwqd%buffer%rbuf(connection(idx)%buffer_idx) = connection(idx)%p_value
            end if
        end do

    end subroutine update_from_incoming_data

    !> Split a (new) connection key into its components and add to the array
    !!
    !! Result: Index into the connection array and updated array
    !!
    !! Note: Internal to get_var_ptr
    subroutine split_key(key_name, connection, newidx)
        use m_waq_memory_dimensions
        use m_real_array_indices
        use m_waq_api_categories
        use delwaq2_global_data
        use m_string_utils

        character(len=*), intent(in) :: key_name     !< Connection key to find
        type(connection_data), dimension(:), allocatable, intent(inout) :: connection   !< Array storing the connection information

        integer(kind=int_wp), intent(out) :: newidx        !< Index into the connection array to be used
        !! for the new connection

        type(connection_data) :: new_connection
        character(len=len(key_name)) :: copy_key, component, item_name, subst_param
        integer(kind=int_wp) :: i, k
        integer(kind=int_wp) :: iseg, isys, monidx, conidx
        integer(kind=int_wp) :: ierr
        logical :: error

        ! Analyse the connection string
        newidx = 0
        isys = -999 ! Just to be safe

        if (key_name(1:10) /= 'TO_DELWAQ|' .and. key_name(1:12) /= 'FROM_DELWAQ|') then
            return
        end if

        new_connection%exchange_name = key_name
        new_connection%incoming = key_name(1:10) == 'TO_DELWAQ|' ! Otherwise automatically outgoing!

        !
        ! We require four parts, separated by a vertical bar ("|")
        ! By requiring the position to be larger than 1 and removing spaces on the left
        ! we ensure that there is something between the bars.
        !
        error = .false.
        copy_key = adjustl(key_name)
        do i = 1, 3
            k = index(copy_key, '|')
            if (k > 1) then
                copy_key = adjustl(copy_key(k + 1:))
            else
                if (index(key_name, '|CONST|') == 0) then
                    error = .true.
                end if
                exit
            end if
        end do

        if (copy_key == ' ') then
            error = .true.
        end if

        if (error) then
            return
        end if

        copy_key = key_name

        !
        ! Strip off the first part
        !
        k = index(copy_key, '|')
        copy_key = copy_key(k + 1:)

        !
        ! The category
        !
        k = index(copy_key, '|')
        component = copy_key(1:k - 1)
        copy_key = copy_key(k + 1:)

        !
        ! The item name or index
        !
        k = index(copy_key, '|')
        item_name = copy_key(1:k - 1)
        copy_key = copy_key(k + 1:)

        !
        ! The substance name or parameter name
        ! (for constants this is the previous part of the connection string)
        !
        if (copy_key == ' ') then
            if (index(key_name, '|CONST|') /= 0) then
                subst_param = item_name
            else
                return
            end if
        else
            subst_param = copy_key
        end if

        !
        ! Create the connection
        !
        selection: &
            block
            select case (component)
            case ('BOUND')
                new_connection%category = category_boundary
                ! TODO = boundary cell - to be worked out
            case ('WASTE')
                new_connection%category = category_wasteload
                !
                ! Index would be index in the list, name is also allowed
                !
                read (item_name, *, iostat=ierr) iseg
                if (ierr == 0) then
                    if (iseg < 1 .or. iseg > num_cells) then
                        newidx = 0
                        exit selection
                    end if
                else
                    iseg = index_in_array(item_name(:len(load_name)), load_name)

                    if (iseg < 1) then
                        newidx = 0
                        exit selection
                    end if
                end if

                !
                ! The substance/parameter may be "FLOW": handle this carefully.
                !
                if (subst_param == 'FLOW') then
                    isys = 1
                else
                    isys = index_in_array(subst_param(:len(substance_name)), substance_name)
                    if (isys <= 0) then
                        newidx = 0
                        exit selection
                    end if

                    isys = isys + 1
                end if

                !
                ! If the waste load is to be set, we set the flow, hence store the index
                ! of the waste load, not that of the individual value
                !
                if (new_connection%incoming) then
                    new_connection%buffer_idx = iseg
                else
                    new_connection%buffer_idx = iwste - 1 + isys + (iseg - 1) * num_waste_loads * (num_substances_total + 1)
                end if

            case ('SEGMN')
                new_connection%category = category_segment
                !
                ! Segments are indicated by segment number. We also need the substance index
                !
                read (item_name, *, iostat=ierr) iseg
                if (ierr == 0) then
                    if (iseg < 1 .or. iseg > num_cells) then
                        newidx = 0
                        exit selection
                    end if
                else
                    newidx = 1
                    exit selection
                end if

                isys = index_in_array(subst_param(:20), substance_name(:num_substances_total))
                if (isys <= 0) then
                    newidx = 0
                    exit selection
                end if

                new_connection%buffer_idx = iconc - 1 + isys + (iseg - 1) * num_substances_total

            case ('OBSRV')
                new_connection%category = category_monitorpoint
                !
                ! Observation points
                ! Index would be index in the list, name is also allowed
                !
                read (item_name, *, iostat=ierr) monidx
                if (ierr == 0) then
                    if (monidx < 1 .or. monidx > size(monitor_name)) then
                        newidx = 0
                        exit selection
                    end if
                else
                    monidx = index_in_array(item_name(:len(monitor_name)), monitor_name)
                    if (monidx < 1) then
                        newidx = 0
                        exit selection
                    end if
                end if

                iseg = monitor_cell(monidx)

                isys = index_in_array(subst_param(:20), substance_name(:num_substances_total))
                if (isys <= 0) then
                    newidx = 0
                    exit selection
                end if
                new_connection%buffer_idx = iconc - 1 + isys + (iseg - 1) * num_substances_total

            case ('CONST')

                ! NOTE: parameters/segment functions not supported yet

                new_connection%category = category_procparam
                !
                ! Constant (timeseries) or parameter (segment function)
                ! Index would be index in the list, name is also allowed
                !

                conidx = index_in_array(subst_param(:len(procparam_const)), procparam_const) ! procparam_const is the name of constants.
                if (conidx < 1) then
                    newidx = 0
                    exit selection
                end if
                new_connection%buffer_idx = icons - 1 + conidx

            case ('HYDRO')
                new_connection%category = category_hydrodynamics
                ! TODO - get arrays like volume etc.
            end select

            ! Fill in the details
            ! Implementation note:
            ! The component p_value must be a pointer, as otherwise the automatic reallocation
            ! below would result in new memory locations that we cannot correct.
            ! Alternative implementations are possible, but short of a fixed size, they all
            ! involve pointers as far as I (AM) can tell. This seems the simplest one.
            !
            new_connection%system_idx = isys
            allocate (new_connection%p_value)

            connection = [connection, new_connection]
            newidx = size(connection)

        end block &
            selection

    end subroutine split_key

end module m_waq_external_access_layer_utils
