!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  

module m_utils_waqgeom
 use MessageHandling
 use m_alloc
 use io_ugrid
 
 implicit none
    
    contains 
    
!> determine all edgex and edgey
 subroutine add_edgexy_waqgeom(waqgeom)
 
 type(t_ug_meshgeom), intent(inout)       :: waqgeom

 integer                                  :: i_edge
 
 call reallocP(waqgeom%edgex, waqgeom%numedge, keepExisting = .false.)
 call reallocP(waqgeom%edgey, waqgeom%numedge, keepExisting = .false.)
 
 do i_edge = 1, waqgeom%numedge
     waqgeom%edgex(i_edge) = (waqgeom%nodex(waqgeom%edge_nodes(1, i_edge)) + waqgeom%nodex(waqgeom%edge_nodes(2, i_edge))) / 2.0
     waqgeom%edgey(i_edge) = (waqgeom%nodey(waqgeom%edge_nodes(1, i_edge)) + waqgeom%nodey(waqgeom%edge_nodes(2, i_edge))) / 2.0
 end do
 
 end subroutine add_edgexy_waqgeom
 
!> Determine all mass centers (facex, facey) of a waqgeom
 subroutine add_facexy_waqgeom(waqgeom)
 
 use geometry_module
 
 type(t_ug_meshgeom), intent(inout)       :: waqgeom

 integer, parameter                       :: missing_value = -999
 integer                                  :: i_face
 integer                                  :: i_node
 integer                                  :: node_count
 
 integer, dimension(:), allocatable       :: nodes !< Helper arrays.
 double precision                         :: area !< Output of subroutine comp_masscenter (not used here).
 integer                                  :: counterclockwise !< Output of subroutine comp_masscenter (not used here).

 call realloc(nodes, waqgeom%maxnumfacenodes)
 call reallocP(waqgeom%facex, waqgeom%numface, keepExisting = .false.)
 call reallocP(waqgeom%facey, waqgeom%numface, keepExisting = .false.)

 do i_face = 1,waqgeom%numface
    node_count = 0
    do i_node = 1, waqgeom%maxnumfacenodes
        if (waqgeom%face_nodes(i_node, i_face) == missing_value) exit
        node_count = i_node
    end do

    ! Reset nodes.
    nodes = missing_value
    nodes(1:node_count) = waqgeom%face_nodes(1:node_count, i_face)
!
    ! Note that passed xs and ys arrays are larger than the passed polygon size (extra elements are not used in subroutine comp_masscenter).
    call comp_masscenter(node_count, waqgeom%nodex(nodes(1:node_count)), waqgeom%nodey(nodes(1:node_count)), &
            waqgeom%facex(i_face), waqgeom%facey(i_face), area, counterclockwise, 0, 0, -999.0D0)
!    ! Face z coordinates are unknown.
 end do
 end subroutine add_facexy_waqgeom
    
!> Determine all face_links of a waqgeom
 subroutine add_facelinks_waqgeom(waqgeom)
 
 type(t_ug_meshgeom), intent(inout)       :: waqgeom
 
 integer                                  :: i_face
 integer                                  :: i_node
 integer                                  :: node_count
 integer, parameter                       :: missing_value = -999

 call reallocP(waqgeom%face_links, (/ waqgeom%maxnumfacenodes, waqgeom%numface /), fill=missing_value)
 do i_face = 1,waqgeom%numface
    node_count = 0
    do i_node = 1, waqgeom%maxnumfacenodes
        if (waqgeom%face_nodes(i_node, i_face) == missing_value) exit
        node_count = i_node
    end do
    ! Get output faces that are adjacent to the current output_face.
    call get_adjacent_faces(i_face, waqgeom%face_edges, waqgeom%edge_faces, waqgeom%face_links(1:node_count, i_face))
 end do
 end subroutine add_facelinks_waqgeom
 
!> Sorts the given edges of the current face in counter clockwise order.
!! At the same time stores the sorted nodes of the current face in the given nodes array.
!! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
function sort_edges(current_face, edges, nodes, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, &
                    reverse_edge_mapping_table, node_mapping_table, output_edge_nodes) result(success)

    implicit none

    integer, intent(in)                  :: current_face !< Current face.
    integer, dimension(:), intent(inout) :: edges !< Edges of the current face.
    integer, dimension(:), intent(out)   :: nodes !< Array to store the nodes of the current face.
    integer, dimension(:,:), intent(in)  :: input_edge_nodes, input_face_nodes, input_edge_faces, output_edge_nodes !< Connectivity arrays.
    integer, dimension(:), intent(in)    :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.
    logical                              :: success !< Result status, true if successful.

    character(len=255)    :: message !< Temporary variable for writing log messages.
    integer               :: first_node, current_node, number_of_edges, k, i !< Counters.
    integer, dimension(2) :: next_nodes !< Helper array.
    logical               :: found
    
    success = .false.

    ! Start with the edge that happens to be listed first, this will stay in the first position.
    ! First sort the two nodes of the first edge in CCW order, so that all subsequent edges will also be sorted in CCW order.
    next_nodes = sort_first_two_nodes(current_face, edges(1), input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table)

    first_node = next_nodes(1)
    current_node = next_nodes(2)
    nodes(1) = first_node
    number_of_edges = size(edges)
    do k = 2,number_of_edges
        nodes(k) = current_node

        ! Error if arrive at the first edge and there are still un-used edges leftover.
        if (current_node == first_node) then
            write(message, *) 'For face ', current_face, ' there are unconnected edges in aggregated mesh.' 
            call mess(LEVEL_ERROR, trim(message))
            write(message, *) 'This can happen if the aggregated cell consists of cells that are not connected,'
            call mess(LEVEL_ERROR, trim(message))
            write(message, *) 'or if the aggregated cell is shaped like a ring. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if

        ! Get next neighbor edge, i.e. another edge that is connected to the current node.
        found = .false.
        do i = k,number_of_edges
            next_nodes = output_edge_nodes(1:2, edges(i))

            if (next_nodes(1) == current_node) then
                found = .true.
                if (i /= k) then
                    call swap(edges(i), edges(k))
                end if
                ! Continue with node on the other side of next edge.
                current_node = next_nodes(2)
                exit
            else if (next_nodes(2) == current_node) then
                found = .true.
                if (i /= k) then
                    call swap(edges(i), edges(k))
                end if
                ! Continue with node on the other side of next edge.
                current_node = next_nodes(1)
                exit
            end if
        end do ! i

        if (.not. found) then
            write(message, *) 'For face ', current_face, ' cannot find edge connected to node ', current_node
            call mess(LEVEL_ERROR, trim(message))
            write(message, *) 'of edge ', edges(k-1), ' in aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do ! k

    ! Error if last edge is not connected to first edge.
    if (current_node /= first_node) then
        write(message, *) 'For face ', current_face, ' node ', current_node, ' of last edge ', edges(number_of_edges)
        call mess(LEVEL_ERROR, trim(message))
        write(message, *) ' is not connected to node ', first_node, ' of first edge ', edges(1), ' in aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

    success = .true.

end function sort_edges

!> The given edge in the aggregated mesh has two nodes. The returned array contains these two nodes sorted in CCW order,
!! i.e. in the same order as these two nodes would be encountered when traversing the nodes of the given face in CCW order.
!! The order will be opposite for the two faces that the given edge connects, therefore the given face is also needed as input.
!! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
function sort_first_two_nodes(output_face, output_edge, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table) result(sorted_output_nodes)
    use m_alloc

    implicit none

    integer, intent(in)                 :: output_face !< Current face.
    integer, intent(in)                 :: output_edge !< First edge of the current face.
    integer, dimension(:,:), intent(in) :: input_edge_nodes, input_face_nodes, input_edge_faces !< Connectivity arrays.
    integer, dimension(:), intent(in)   :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.

    character(len=255)                 :: message !< Temporary variable for writing log messages.
    integer, parameter                 :: missing_value = -999
    integer                            :: input_edge, input_face, max_nodes_per_face, nodes_per_face, node, next_node, previous_node
    integer, dimension(2)              :: input_nodes, input_faces
    integer, dimension(:), allocatable :: nodes
    integer                            :: i !< Counter.
    logical                            :: sorted
    integer, dimension(2)              :: sorted_output_nodes !< The two nodes of the first edge of the current face in sorted order.
    integer                            :: input_face1, input_face2


    ! Get input edge, nodes and faces that correspond to the given output edge.
    input_edge = reverse_edge_mapping_table(output_edge)
    input_nodes = input_edge_nodes(1:2, input_edge)
    input_faces = input_edge_faces(1:2, input_edge)

    ! Get the input face of the input edge that is part of the given output face.
    input_face1=-1
    input_face2=-1
    if (input_faces(1) /= missing_value) input_face1 = face_mapping_table(input_faces(1))
    if (input_faces(2) /= missing_value) input_face2 = face_mapping_table(input_faces(2))
    if (input_face1 == output_face) then
        input_face = input_faces(1)
    else if (input_face2 == output_face) then
        input_face = input_faces(2)
    else
        write(message, *) 'Cannot find input face for output face ', output_face, ' and output edge ', output_edge, ' in un-aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

    ! Get input nodes of input face.
    max_nodes_per_face = size(input_face_nodes(:, input_face))
    call realloc(nodes, max_nodes_per_face)
    nodes = input_face_nodes(:, input_face)
    ! Determine nodes_per_face.
    nodes_per_face = max_nodes_per_face
    do i = 1,max_nodes_per_face
        if (nodes(i) == missing_value) then
            nodes_per_face = i - 1
            exit
        end if
    end do
    if (nodes_per_face < 3) then
        call mess(LEVEL_ERROR, 'Nodes per face in un-aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if

    ! Sort input_nodes.
    ! Find input nodes in input face nodes of input face and sort the input nodes in the same order as they are found in input face nodes.
    sorted = .false.
    do i = 1,nodes_per_face
        node = nodes(i)

        if (node == input_nodes(1)) then
            next_node = nodes(modulo(i, nodes_per_face) + 1)
            if (next_node == input_nodes(2)) then
                sorted = .true.
                exit
            end if

            previous_node = nodes(modulo(i + nodes_per_face - 2, nodes_per_face) + 1)
            if (previous_node == input_nodes(2)) then
                call swap(input_nodes(1), input_nodes(2))
                sorted = .true.
                exit
            end if

            write(message, *) 'Cannot find node ', input_nodes(2), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do
    if (.not. sorted) then
        write(message, *) 'Cannot find node ', input_nodes(1), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
        call mess(LEVEL_ERROR, trim(message))
        return
    end if

    ! Get output nodes corresponding to sorted input nodes.
    sorted_output_nodes = node_mapping_table(input_nodes)

    deallocate(nodes)

end function sort_first_two_nodes

!> All faces that are adjacent to the given face are stored in the given array adjacent_faces.
!! The length of the given array adjacent_faces must be equal to the number of edges of the given face.
subroutine get_adjacent_faces(face, face_edges, edge_faces, adjacent_faces)

    implicit none

    integer, intent(in)                 :: face !< Input face.
    integer, dimension(:,:), intent(in) :: face_edges !< Face edge connectivity.
    integer, dimension(:,:), intent(in) :: edge_faces !< Edge face connectivity.
    integer, dimension(:), intent(out)  :: adjacent_faces !< Output array.

    integer               :: edge, i
    integer, dimension(2) :: faces

    ! Determine faces that are adjacent to the current face.
    do i = 1,size(adjacent_faces)
        edge = face_edges(i, face)

        ! Store neighboring face for this edge.
        ! Note that some face links can be out_of_mesh (i.e. missing value).
        faces = edge_faces(1:2, edge)
        ! Of the two faces, one is the given face and the other is the neighboring face (or missing value).
        if (faces(1) == face) then
            adjacent_faces(i) = faces(2)
        else ! If faces(2) == face
            adjacent_faces(i) = faces(1)
        end if
    end do ! i

end subroutine get_adjacent_faces

!> Swap the values of the given integers a and b.
subroutine swap(a, b)

    implicit none

    integer, intent(inout) :: a, b !< Integers to swap.

    integer :: temp

    temp = a
    a = b
    b = temp

end subroutine swap

 subroutine getdxdy(x1,y1,x2,y2,dx,dy)
 implicit none
 double precision :: x1, y1, x2, y2, dx, dy
 integer :: jsferic = 0 ! xy pair is in : 0=cart, 1=sferic coordinates (no taken into account yet)
 double precision, external :: getdx, getdy
 if (Jsferic == 1) then 
  
!    if (jasferdistance == 1) then  ! this is a fix 
!       call sferdistance(x1,y1,x2,y1,dx)  
!       if (x2 < x1) dx = -dx
!       call sferdistance(x1,y1,x1,y2,dy)
!       if (y2 < y1) dy = -dy
!     else
!       dx = getdx(x1,y1,x2,y2)  
!       dy = getdy(x1,y1,x2,y2) 
!    endif
    
 else
    dx = x2-x1
    dy = y2-y1
 endif
 
 end subroutine getdxdy

end module    
