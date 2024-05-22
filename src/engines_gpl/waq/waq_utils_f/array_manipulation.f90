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
module m_array_manipulation
    use m_waq_precision
    implicit none
    private
    ! Type for managing memory partitions.
    type memory_partition
        integer :: index_pointer = 1
        integer :: alpha_pointer = 1
        integer :: char_pointer = 1
    end type memory_partition
    ! Constants representing different data types.
    ! Taken from fsm-fix.i
    integer, parameter :: int_type = 1
    integer, parameter :: real_type = 2
    integer, parameter :: double_type = 3
    integer, parameter :: double_complex_type = 4
    integer, parameter :: complex_type = 5
    integer, parameter :: logical_type = 6
    integer, parameter :: char_type = 7

    real, parameter :: no_data_value = -999.0

    !    interface
    !        subroutine resize_integer_array(array_pointer, new_length, old_length)
    !            integer, pointer :: array_pointer(:)
    !            integer, intent(in) :: old_length, new_length
    !        end subroutine
    !
    !        subroutine resize_character_array(array_pointer, new_length, old_length)
    !            character(len=20), pointer :: array_pointer(:)
    !            integer, intent(in) :: old_length, new_length
    !        end subroutine
    !
    !    end interface

    public :: shift_array_right, copy_real_array_elements, copy_integer_array_elements, &
            initialize_real_array, initialize_integer_array, make_pointer, resize_character_array, &
            resize_integer_array, is_missing, fill_element_dimensions, set_array_parameters, &
            create_pointer_table

    public :: memory_partition, int_type, real_type, double_type, double_complex_type, complex_type, logical_type, &
            char_type
contains

    subroutine shift_array_right(arr, start_index, end_index)
        !!  shifts an array of integers 1 locations
        integer(kind = int_wp), intent(inout) :: arr(:)   !! arr(nomax) array to be shifted
        integer(kind = int_wp), intent(in) :: end_index   !! end_index - end index of the shift.
        integer(kind = int_wp), intent(in) :: start_index !! start of the shift
        integer(kind = int_wp) :: i

        ! shift the elements of the array
        do i = end_index, start_index, -1
            arr(i + 1) = arr(i)
        end do

    end subroutine shift_array_right

    subroutine copy_real_array_elements(source_array, destination_array, size)
        !! copy values from source_array to destination_array
        real(kind = real_wp), intent(in) :: source_array(:)        !! array to be copied
        real(kind = real_wp), intent(out) :: destination_array(:)  !! destination_array  size      array to copy to
        integer(kind = int_wp), intent(in) :: size                 !! total number of entries
        integer(kind = int_wp) :: i

        do  i = 1, size
            destination_array(i) = source_array(i)
        end do

    end subroutine copy_real_array_elements

    subroutine copy_integer_array_elements(source_array, destination_array, size)
        !! copy values from source_array to destination_array
        integer(kind = real_wp), intent(in) :: source_array(*)        !! source_array
        integer(kind = real_wp), intent(out) :: destination_array(*)  !! destination_array
        integer(kind = int_wp), intent(in) :: size                    !! total number of entries
        integer(kind = int_wp) :: i

        do  i = 1, size
            destination_array(i) = source_array(i)
        end do

    end subroutine copy_integer_array_elements

    subroutine initialize_real_array(array, size)
        !! initialize array with a zero value
        real(kind = real_wp), intent(inout) :: array(*)     !! array to be zeroed
        integer(kind = int_wp), intent(in) :: size          !! array size
        integer(kind = int_wp) :: i

        do  i = 1, size
            array(i) = 0.0
        end do

    end subroutine initialize_real_array

    subroutine initialize_integer_array(array, size)
        !! initialize array with a zero value
        integer(kind = real_wp), intent(inout) :: array(*)     !! array to be zeroed
        integer(kind = int_wp), intent(in) :: size             !! array size
        integer(kind = int_wp) :: i

        do  i = 1, size
            array(i) = 0
        end do

    end subroutine initialize_integer_array

    integer function make_pointer(partition, var_type, num_elements)
        !! Submodule for partitioning large arrays into smaller pieces.
        !! Mimics the old FMM library behavior for memory partitioning.
        !! Designed for multithreaded context, avoids local, saved variables.
        !! Uses a type(memory_partition) argument for managing partitions.
        ! Function to make a pointer for a specific part of the memory partition.
        use m_logger, only : terminate_execution  ! Error handling module.

        type(memory_partition), intent(inout) :: partition
        integer, intent(in) :: var_type
        integer, intent(in) :: num_elements

        select case (var_type)
        case (int_type)
            make_pointer = partition%index_pointer
            partition%index_pointer = partition%index_pointer + max(num_elements, 1)
        case (real_type)
            make_pointer = partition%alpha_pointer
            partition%alpha_pointer = partition%alpha_pointer + max(num_elements, 1)
        case (char_type)
            make_pointer = partition%char_pointer
            partition%char_pointer = partition%char_pointer + max(num_elements, 1)
        case default
            write(*, *) 'Fatal error in make_pointer: Unimplemented variable type: ', var_type
            call terminate_execution(1)
        end select
    end function make_pointer

    subroutine resize_integer_array(array_pointer, new_length, old_length)
        ! reallocate an integer array pointed to by a pointer variable pint. it adjusts the size of this array to a new
        ! length specified by new_length, preserving the data up to the minimum of the old and new lengths.

        integer, pointer :: array_pointer(:)                !! pointer to the integer array
        integer, intent(in) :: old_length, new_length       !! old and new length of the array

        logical :: is_associated                            !! flag to check if the array is associated
        integer, pointer :: temp_pointer(:)
        integer :: min_length

        min_length = min(old_length, new_length)
        is_associated = associated(array_pointer)
        if (is_associated) then
            if (min_length > 0) then
                allocate(temp_pointer(new_length))
                temp_pointer(1:min_length) = array_pointer(1:min_length)
                deallocate(array_pointer)
                array_pointer => temp_pointer
            else
                deallocate(array_pointer)
                allocate(array_pointer(new_length))
            endif
        else
            allocate(array_pointer(new_length))
        endif

    end subroutine resize_integer_array

    subroutine resize_character_array(array_pointer, new_length, old_length)
        !! reallocate an array of character strings, each string being of a fixed length of 20 characters. the
        !! subroutine handles the dynamic allocation and reallocation of this array, ensuring that data is preserved
        !! up to the minimum of the old and new lengths when the array is resized

        character(len = 20), pointer :: array_pointer(:)    !! pointer to the character array
        integer, intent(in) :: old_length, new_length       !! old and new length of the array

        logical :: is_associated
        character(len = 20), pointer :: temp_pointer(:)
        integer :: min_length

        min_length = min(old_length, new_length)
        is_associated = associated(array_pointer)

        if (is_associated) then
            if (min_length > 0) then
                allocate(temp_pointer(new_length))
                temp_pointer(1:min_length) = array_pointer(1:min_length)
                deallocate(array_pointer)
                array_pointer => temp_pointer
            else
                deallocate(array_pointer)
                allocate(array_pointer(new_length))
            endif
        else
            allocate(array_pointer(new_length))
        endif

    end subroutine resize_character_array

    logical function is_missing(value)
        ! Checks if a real value is considered "missing", defined as -999.

        real, intent(in) :: value

        is_missing = (value == no_data_value)

    end function is_missing

    subroutine set_array_parameters(var_index, array_index, array_kind, value_index, array_dims_1, &
            array_dims_2, array_pointer, grid_index, sys_index, total_elements, &
            pointer_var)

        use m_logger, only : get_log_unit_number, terminate_execution

        integer(kind = int_wp), intent(in) :: var_index
        integer(kind = int_wp), intent(in) :: array_kind                    !! kind of array (2 or 3)

        integer(kind = int_wp), intent(in) :: grid_index                    !! grid index
        integer(kind = int_wp), intent(in) :: array_dims_1, array_dims_2    !! dimensions of the array (idim1, idim2)
        integer(kind = int_wp), intent(inout) :: total_elements            !! total number of elements in the array
        integer(kind = int_wp), intent(inout) :: sys_index                  !! system index
        integer(kind = int_wp), intent(in) :: array_pointer
        integer(kind = int_wp), intent(out) :: pointer_var
        integer(kind = int_wp) :: array_index
        integer(kind = int_wp) :: value_index
        integer(kind = int_wp) :: file_unit

        select case (array_kind)
        case (2)
            sys_index = value_index
            total_elements = array_dims_1
            pointer_var = array_pointer + (grid_index - 1) * array_dims_1 * array_dims_2
        case (3)
            sys_index = 1
            total_elements = 1
            pointer_var = array_pointer + (grid_index - 1) * array_dims_1 * array_dims_2 + (value_index - 1) * array_dims_1
        case default
            ! error , undefined kind of array
            call get_log_unit_number(file_unit)
            write(file_unit, 2000) array_kind, array_index, var_index
            call terminate_execution(1)
        end select

        return
        2000 format (' error: undefined kind of array :', i8, &
                /'        array number             ', i8, &
                /'        variable number          ', i8)
    end subroutine set_array_parameters

    subroutine fill_element_dimensions(element_dims, element_index, dim1, dim2, dim3, dim4, dim5, dim6)
        ! Write element dimensions in array element_dims
        ! Populates an array with dimension information for a specific element.

        integer(kind = int_wp), intent(out) :: element_dims(6, *)      !! array containing info about the
        !! element dimensions element_dims(1,*) is the number of dimensions
        !! element_dims(2-element_dims(1,*),*) is the size of each dimension. the size of the
        !! array is (6,nelems).
        integer(kind = int_wp), intent(in) :: element_index               !! index number of element in group
        integer(kind = int_wp), intent(in) :: dim1, dim2, dim3, dim4, dim5, dim6 !! ! dimension sizes
        ! define element dimensions
        element_dims(1, element_index) = dim1
        element_dims(2, element_index) = dim2
        element_dims(3, element_index) = dim3
        element_dims(4, element_index) = dim4
        element_dims(5, element_index) = dim5
        element_dims(6, element_index) = dim6

    end subroutine fill_element_dimensions

    subroutine create_pointer_table(grid_dim_x, grid_dim_y, num_layers, num_volumes, num_boundaries, &
            total_exchanges, exchanges_x, exchanges_y, active_grid, ipoint, &
            volume_pointers, flow_pointers)

        !! Makes from-to pointer table from regular matrix grid table
        !!
        !! The routine scans the grid table to identify from-to entries./n
        !! If the active grid is 'active-only' contracted, then the pointer is.
        !! This is checked by comparing the obtained exchanges_x, exchanges_y and noq3 with
        !! those in the lga file./n
        !! A backpointer from num_volumes to mnmaxk is constructed, to fill in the
        !! volumes in the complete matrix for solvers 18, 19 and Delpar./n
        !! A backpointer from total_exchanges to 3*mnmaxk - mnmax is constructed, to fill in
        !! areas and flows into the complete matrix for solvers 18, 19 and Delpar.

        integer(kind = int_wp), intent(in) :: grid_dim_x               !< first grid dimension
        integer(kind = int_wp), intent(in) :: grid_dim_y               !< second grid dimension
        integer(kind = int_wp), intent(in) :: num_layers               !< number of layers
        integer(kind = int_wp), intent(in) :: num_volumes              !< total number of volumes
        integer(kind = int_wp), intent(in) :: num_boundaries              !< total number of boundaries
        integer(kind = int_wp), intent(in) :: total_exchanges                !< total number of exchanges
        integer(kind = int_wp), intent(in) :: exchanges_x               !< total number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: exchanges_y               !< total number of exchanges in second direction
        integer(kind = int_wp), intent(in) :: active_grid(grid_dim_x, grid_dim_y)  !< active grid table
        integer(kind = int_wp), intent(out) :: ipoint(4, total_exchanges)      !< from to pointer
        integer(kind = int_wp), intent(out) :: volume_pointers(num_volumes)     !< from to pointer
        integer(kind = int_wp), intent(out) :: flow_pointers(total_exchanges)     !< from to pointer

        ! Local declarations
        integer(kind = int_wp) :: n, m, layer         !  loop variables in the matrix
        integer(kind = int_wp) :: exchange_index, layer_exchange_count   !  help variable exchange nr.
        integer(kind = int_wp) :: from_index, to_index          !  from and to exchange pointers
        integer(kind = int_wp) :: from_index_adj, to_index_adj  !  from-1 and to+1 exchange pointers
        integer(kind = int_wp) :: num_layers_adjusted           !  number of layers
        integer(kind = int_wp) :: volumes_per_layer          !  number of volumes per layer
        integer(kind = int_wp) :: boundaries_per_layer          !  number of boundaries per layer
        integer(kind = int_wp) :: exchanges_x_per_layer           !  number of exchanges first direction per layer
        integer(kind = int_wp) :: exchanges_y_per_layer           !  number of exchanges second direction per layer
        logical :: is_contracted      !  if true, it is an 'active only' grid
        integer(kind = int_wp) :: grid_area           !  help variable grid_dim_x*grid_dim_y (horizontal size of the matrix)
        integer(kind = int_wp) :: total_grid_volume          !  help variable grid_area*num_layers (total size of the cube)
        integer(kind = int_wp) :: non_contracted_count, contracted_count        !  non contracted and contracted counters

        num_layers_adjusted = max(num_layers, 1)
        volumes_per_layer = num_volumes / num_layers_adjusted
        boundaries_per_layer = num_boundaries / num_layers_adjusted
        exchanges_x_per_layer = exchanges_x / num_layers_adjusted
        exchanges_y_per_layer = exchanges_y / num_layers_adjusted
        grid_area = grid_dim_x * grid_dim_y
        total_grid_volume = grid_area * num_layers_adjusted
        is_contracted = .false.
        if (volumes_per_layer < grid_area) is_contracted = .true.

        ! construct backpointers from active-only grid to total grid, both segments and exchanges
        volume_pointers = 0
        flow_pointers = 0
        non_contracted_count = 0
        do m = 1, grid_dim_y
            do n = 1, grid_dim_x
                non_contracted_count = non_contracted_count + 1
                if (active_grid(n, m) > 0) then
                    do layer = 1, num_layers_adjusted
                        volume_pointers(active_grid(n, m) + (layer - 1) * volumes_per_layer) = non_contracted_count + &
                        (layer - 1) * grid_area
                    enddo
                endif
            enddo
        enddo
        do n = 1, num_volumes
            if (volume_pointers(n) <= 0) volume_pointers(n) = n       ! happens if 1-1 coupling
        enddo

        ! initialise all pointers as inactive

        ipoint = 0

        ! Horizontal 1st direction
        non_contracted_count = 0
        contracted_count = 0
        do m = 1, grid_dim_y
            do n = 1, grid_dim_x - 1
                non_contracted_count = non_contracted_count + 1
                from_index = active_grid(n, m)
                if (from_index == 0) cycle
                to_index = active_grid(n + 1, m)
                if (to_index   == 0) cycle
                if (from_index < 0 .and. to_index   < 0) cycle
                contracted_count = contracted_count + 1
                from_index_adj = 0
                to_index_adj = 0
                if (n >      1) from_index_adj = active_grid(n - 1, m)
                if (n < grid_dim_x - 1) to_index_adj = active_grid(n + 2, m)
                layer_exchange_count = non_contracted_count
                if (is_contracted) layer_exchange_count = contracted_count
                do layer = 1, num_layers_adjusted
                    exchange_index = (layer - 1) * exchanges_x_per_layer + layer_exchange_count
                    flow_pointers(exchange_index) = (layer - 1) * grid_area + non_contracted_count
                    if (from_index_adj > 0) then
                        ipoint(3, exchange_index) = (layer - 1) * volumes_per_layer + from_index_adj
                    else
                        ipoint(3, exchange_index) = (1 - layer) * boundaries_per_layer + from_index_adj
                    endif
                    if (from_index  > 0) then
                        ipoint(1, exchange_index) = (layer - 1) * volumes_per_layer + from_index
                    else
                        ipoint(1, exchange_index) = (1 - layer) * boundaries_per_layer + from_index
                    endif
                    if (to_index    > 0) then
                        ipoint(2, exchange_index) = (layer - 1) * volumes_per_layer + to_index
                    else
                        ipoint(2, exchange_index) = (1 - layer) * boundaries_per_layer + to_index
                    endif
                    if (to_index_adj > 0) then
                        ipoint(4, exchange_index) = (layer - 1) * volumes_per_layer + to_index_adj
                    else
                        ipoint(4, exchange_index) = (1 - layer) * boundaries_per_layer + to_index_adj
                    endif
                enddo
            enddo
            if (.not. is_contracted) non_contracted_count = non_contracted_count + 1
        enddo
        layer_exchange_count = non_contracted_count
        if (is_contracted) layer_exchange_count = contracted_count
        if (layer_exchange_count /= exchanges_x_per_layer) write (338, *) ' ERROR1 in create_pointer_table: ',&
            layer_exchange_count, exchanges_x_per_layer

        ! Horizontal 2nd direction
        non_contracted_count = total_grid_volume
        contracted_count = exchanges_x
        do m = 1, grid_dim_y - 1
            do n = 1, grid_dim_x
                non_contracted_count = non_contracted_count + 1
                from_index = active_grid(n, m)
                if (from_index == 0) cycle
                to_index = active_grid(n, m + 1)
                if (to_index   == 0) cycle
                if (from_index < 0 .and. to_index   < 0) cycle
                from_index_adj = 0
                to_index_adj = 0
                if (m >      1) from_index_adj = active_grid(n, m - 1)
                if (m < grid_dim_y - 1) to_index_adj = active_grid(n, m + 2)
                contracted_count = contracted_count + 1
                layer_exchange_count = non_contracted_count
                if (is_contracted) layer_exchange_count = contracted_count
                do layer = 1, num_layers_adjusted
                    exchange_index = (layer - 1) * exchanges_y_per_layer + layer_exchange_count
                    flow_pointers(exchange_index) = (layer - 1) * grid_area + non_contracted_count - total_grid_volume
                    if (from_index_adj > 0) then
                        ipoint(3, exchange_index) = (layer - 1) * volumes_per_layer + from_index_adj
                    else
                        ipoint(3, exchange_index) = (1 - layer) * boundaries_per_layer + from_index_adj
                    endif
                    if (from_index  > 0) then
                        ipoint(1, exchange_index) = (layer - 1) * volumes_per_layer + from_index
                    else
                        ipoint(1, exchange_index) = (1 - layer) * boundaries_per_layer + from_index
                    endif
                    if (to_index    > 0) then
                        ipoint(2, exchange_index) = (layer - 1) * volumes_per_layer + to_index
                    else
                        ipoint(2, exchange_index) = (1 - layer) * boundaries_per_layer + to_index
                    endif
                    if (to_index_adj > 0) then
                        ipoint(4, exchange_index) = (layer - 1) * volumes_per_layer + to_index_adj
                    else
                        ipoint(4, exchange_index) = (1 - layer) * boundaries_per_layer + to_index_adj
                    endif
                enddo
            enddo
        enddo
        non_contracted_count = non_contracted_count + grid_dim_x
        layer_exchange_count = non_contracted_count
        if (is_contracted) layer_exchange_count = contracted_count
        if (layer_exchange_count /= exchanges_x + exchanges_y_per_layer) write (338, *) ' ERROR2 in create_pointer_table: ',&
           layer_exchange_count, exchanges_x + exchanges_y_per_layer

        !     Vertical 3d direction
        non_contracted_count = total_grid_volume * 2
        contracted_count = exchanges_x + exchanges_y
        do layer = 1, num_layers_adjusted - 1
            do m = 1, grid_dim_y
                do n = 1, grid_dim_x
                    non_contracted_count = non_contracted_count + 1
                    from_index = active_grid(n, m)
                    if (from_index > 0) then
                        contracted_count = contracted_count + 1
                        layer_exchange_count = non_contracted_count
                        if (is_contracted) layer_exchange_count = contracted_count
                        exchange_index = layer_exchange_count
                        flow_pointers(exchange_index) = non_contracted_count - 2 * total_grid_volume
                        from_index_adj = 0
                        to_index_adj = 0
                        if (layer >       1) from_index_adj = (layer - 2) * volumes_per_layer + from_index
                        if (layer < num_layers_adjusted - 1) to_index_adj = (layer + 1) * volumes_per_layer + from_index
                        ipoint(1, exchange_index) = (layer - 1) * volumes_per_layer + from_index
                        ipoint(2, exchange_index) = layer * volumes_per_layer + from_index
                        ipoint(3, exchange_index) = from_index_adj
                        ipoint(4, exchange_index) = to_index_adj
                    endif
                enddo
            enddo
        enddo
        layer_exchange_count = non_contracted_count
        if (is_contracted) layer_exchange_count = contracted_count
        if (layer_exchange_count /= total_exchanges) write (338, *) ' ERROR3 in create_pointer_table: ',&
            layer_exchange_count, total_exchanges
        do n = 1, exchanges_x
            if (flow_pointers(n) <= 0) flow_pointers(n) = n       ! happens if 1-1 coupling
        enddo
        do n = exchanges_x + 1, exchanges_x + exchanges_y
            if (flow_pointers(n) <= 0) flow_pointers(n) = n - exchanges_x
        enddo
        do n = exchanges_x + exchanges_y + 1, total_exchanges
            if (flow_pointers(n) <= 0) flow_pointers(n) = n - exchanges_x - exchanges_y
        enddo

        return
    end subroutine create_pointer_table

end module m_array_manipulation
