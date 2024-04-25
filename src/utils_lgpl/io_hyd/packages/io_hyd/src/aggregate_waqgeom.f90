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
!!--description-----------------------------------------------------------------
!> Aggregates the given mesh geometry using the given aggregation table.
!!--pseudo code and references--------------------------------------------------
! Dependencies:
!   io_netcdf module for derived type t_ug_meshgeom
!!--declarations----------------------------------------------------------------
module m_aggregate_waqgeom
    use MessageHandling
    use m_utils_waqgeom
    
    implicit none

contains    
!> Aggregates the given mesh geometry and edge type array using the given aggregation table.
!! The mesh aggregation algorithm removes edges, but preserves the order of the edges. The edge type of a given edge stays the same.
!! So if the edges in the un-aggregated mesh are ordered (first flow links, then closed edges),
!! then the edges in the aggregated mesh will still be ordered (first flow links, then closed edges).
!!
!! since array pointers will become disassociated, possibly causing memory leaks.
function aggregate_ugrid_geometry(input, output, input_edge_type, output_edge_type, face_mapping_table) result(success)
    use io_ugrid
    use geometry_module
    use m_alloc

    implicit none

    type(t_ug_meshgeom), intent(in)                 :: input !< The mesh geometry to be aggregated.
    type(t_ug_meshgeom), intent(inout)              :: output !< Aggregated mesh geometry.
    integer, dimension(:), intent(in)               :: input_edge_type !< The edge type array to be aggregated.
    integer, dimension(:), pointer, intent(out)     :: output_edge_type !< Aggregated edge type array.
    integer, dimension(:), intent(in)               :: face_mapping_table !< Mapping table flow cells -> waq cells.
    logical                                         :: success !< Result status, true if successful.

    character(len=255)                       :: message !< Temporary variable for writing log messages.
    integer, parameter                       :: missing_value = -999
    integer, dimension(:), allocatable       :: node_mapping_table, reverse_node_mapping_table, reverse_edge_mapping_table !< Mapping tables.
    integer                                  :: input_edge_count, output_edge_count, output_node_count, output_face_count, max_nodes_per_face, node_count !< Counters.
    integer                                  :: i, j, input_edge, output_edge, input_node, output_node, output_face !< Counters.
    integer, dimension(2)                    :: faces !< Helper array.
    integer, dimension(:,:), allocatable     :: input_edge_output_faces !< Helper array.
    integer, dimension(:), allocatable       :: face_edge_count, nodes !< Helper arrays.
    double precision                         :: area !< Output of subroutine comp_masscenter (not used here).
    integer                                  :: counterclockwise !< Output of subroutine comp_masscenter (not used here).

    success = .false.
    output%start_index = 1

    ! 1. Determine output edge_faces and edge_nodes.
    ! Apply face mapping table to edge faces.
    input_edge_count = input%numEdge
    call realloc(input_edge_output_faces, (/ 2, input_edge_count /), fill=missing_value)
    do input_edge = 1,input_edge_count
        do i = 1,2
            if (input%edge_faces(i, input_edge) /= missing_value) then
                input_edge_output_faces(i, input_edge) = face_mapping_table(input%edge_faces(i, input_edge))
            end if
        end do ! i
    end do ! input_edge
    ! Create edge mapping table and output edge_faces and edge_nodes.
    call realloc(reverse_edge_mapping_table, input_edge_count)
    call reallocP(output%edge_faces, (/ 2, input_edge_count /))
    call reallocP(output%edge_nodes, (/ 2, input_edge_count /))
    output_edge = 0
    do input_edge = 1,input_edge_count
        ! If edge points to the same aggregated face on either side, then edge is not needed anymore in the aggregated mesh.
        if (input_edge_output_faces(1, input_edge) /= input_edge_output_faces(2, input_edge)) then ! Edge that should stay.
            ! The remaining output edges have a different numbering.
            output_edge = output_edge + 1
            reverse_edge_mapping_table(output_edge) = input_edge
            output%edge_faces(1:2, output_edge) = input_edge_output_faces(1:2, input_edge)
            output%edge_nodes(1:2, output_edge) = input%edge_nodes(1:2, input_edge)
        end if
    end do
    output_edge_count = output_edge
    if (output_edge_count < 3) then
        call mess(LEVEL_ERROR, 'Edge count in aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if
    ! At this point edges have been renumbered automatically from input edge numbers to output edge numbers.
    ! Truncate arrays.
    call realloc(reverse_edge_mapping_table, output_edge_count, keepExisting=.true.)
    call reallocP(output%edge_faces, (/ 2, output_edge_count /), keepExisting=.true.)
    call reallocP(output%edge_nodes, (/ 2, output_edge_count /), keepExisting=.true.)


    ! 2. Determine output edge coordinates and types.
    call reallocP(output%edgex, output_edge_count)
    call reallocP(output%edgey, output_edge_count)
    call reallocP(output_edge_type, output_edge_count)
    do output_edge = 1,output_edge_count
        output%edgex(output_edge) = input%edgex(reverse_edge_mapping_table(output_edge))
        output%edgey(output_edge) = input%edgey(reverse_edge_mapping_table(output_edge))
        ! Edge z coordinates are unknown.
        output_edge_type(output_edge) = input_edge_type(reverse_edge_mapping_table(output_edge))
    end do


    ! 3. Create node mapping table.
    call realloc(node_mapping_table, input%numNode, fill=missing_value)
    ! All nodes that are present in output edge_nodes should remain, all other nodes are not needed anymore in the aggregated mesh.
    ! First create mask of remaining nodes in node_mapping_table.
    do output_edge = 1,output_edge_count
        node_mapping_table(output%edge_nodes(1:2, output_edge)) = 1
    end do
    output_node_count = count(node_mapping_table == 1)
    if (output_node_count < 3) then
        call mess(LEVEL_ERROR, 'Node count in aggregated mesh < 3. Mesh will not be aggregated.')
        return
    end if
    ! Change mask into mapping table.
    call realloc(reverse_node_mapping_table, output_node_count)
    output_node = 0
    do input_node = 1,input%numNode
        if (node_mapping_table(input_node) == 1) then ! Node that should stay.
            ! The remaining output nodes have a different numbering.
            output_node = output_node + 1
            node_mapping_table(input_node) = output_node
            reverse_node_mapping_table(output_node) = input_node
        end if
    end do
    ! Renumber input node numbers to output node numbers in output edge_nodes, using node_mapping_table.
    do output_edge = 1,output_edge_count
        output%edge_nodes(1, output_edge) = node_mapping_table(output%edge_nodes(1, output_edge))
        output%edge_nodes(2, output_edge) = node_mapping_table(output%edge_nodes(2, output_edge))
    end do


    ! 4. Determine output node coordinates.
    call reallocP(output%nodex, output_node_count)
    call reallocP(output%nodey, output_node_count)
    call reallocP(output%nodez, output_node_count)
    do output_node = 1,output_node_count
        output%nodex(output_node) = input%nodex(reverse_node_mapping_table(output_node))
        output%nodey(output_node) = input%nodey(reverse_node_mapping_table(output_node))
        output%nodez(output_node) = input%nodez(reverse_node_mapping_table(output_node))
    end do


    ! 5. Determine output face_edges.
!    ! Convert output edge_faces to a flat table with two columns: edges column and faces column.
!    call realloc(edges_column, output_edge_count * 2)
!    call realloc(faces_column, output_edge_count * 2)
!    forall (i = 1:output_edge_count*2)
!        edges_column(i) = (i + 1) / 2
!    end forall
!    faces_column = reshape(output_edge_faces, (/ output_edge_count * 2 /))
!    ! Sort table on faces column.
!    ! TODO use quicksort? AK
!    qsort(faces_column, sorted_faces_column, sorted_indices)
!    sorted_edges_column = edges_column(sorted_indices)
    ! This code assumes that output faces are numbered 1, 2, 3, etc. without gaps.
    ! TODO remove -1, -2, etc. by making temp pointer to first part of face_mapping_table
    output_face_count = maxval(face_mapping_table)
    if (output_face_count < 1) then
        call mess(LEVEL_ERROR, 'Face count in aggregated mesh < 1. Mesh will not be aggregated.')
        return
    end if
    ! Count edges for each face.
    call realloc(face_edge_count, output_face_count, fill=0)
    do output_edge = 1,output_edge_count
        faces = output%edge_faces(1:2, output_edge)
        ! Add 1 edge for both faces.
        do i=1,2
            if (faces(i) == missing_value) then
                cycle
            end if

            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
        end do ! i
    end do ! output_edge
    do output_face = 1,output_face_count
        if (face_edge_count(output_face) < 3) then
            write(message, *) 'Face edge count in aggregated mesh < 3 for face ', output_face, '. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
        end if
    end do
    ! Determine max_nodes_per_face.
    max_nodes_per_face = maxval(face_edge_count)
    ! Determine nodes, edges and faces for each output face.
    call reallocP(output%face_edges, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    ! Re-use face_edge_count array to put edges in the next available spot in the output%face_edges array.
    face_edge_count = 0
    do output_edge = 1,output_edge_count
        faces = output%edge_faces(1:2, output_edge)
        do i = 1,2
            if (faces(i) == missing_value) then
                cycle
            end if

            ! Keep track of current number of edges for this face.
            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
            ! Put current edge in the next available spot in output%face_edges for this face.
            output%face_edges(face_edge_count(faces(i)), faces(i)) = output_edge
        end do ! i
    end do ! output_edge
    ! At this point the edges for each face are in random order.


    ! 6. Sort edges for each face in counter clockwise order.
    ! At the same time store sorted nodes of sorted edges in output%face_nodes array.
    call reallocP(output%face_nodes, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    do output_face = 1,output_face_count
        ! Sort edges for current output face.
        success = sort_edges(output_face, output%face_edges(1:face_edge_count(output_face), output_face), output%face_nodes(1:face_edge_count(output_face), output_face), &
                input%edge_nodes, input%face_nodes, input%edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output%edge_nodes)
        if (.not. success) return
    end do


    ! 7. Determine output face_links.
    call reallocP(output%face_links, (/ max_nodes_per_face, output_face_count /), fill=missing_value)
    do output_face = 1,output_face_count
        ! Get output faces that are adjacent to the current output_face.
        call get_adjacent_faces(output_face, output%face_edges, output%edge_faces, output%face_links(1:face_edge_count(output_face), output_face))
    end do


    ! 8. Determine output face coordinates.
    ! Here calculate the cell centroids (cell "centers of mass").
    call realloc(nodes, max_nodes_per_face)
    call reallocP(output%facex, output_face_count)
    call reallocP(output%facey, output_face_count)
    do output_face = 1,output_face_count
        node_count = face_edge_count(output_face)

        ! Reset nodes.
        nodes = missing_value
        nodes(1:node_count) = output%face_nodes(1:node_count, output_face)

        ! Note that passed xs and ys arrays are larger than the passed polygon size (extra elements are not used in subroutine comp_masscenter).
        call comp_masscenter(node_count, output%nodex(nodes(1:node_count)), output%nodey(nodes(1:node_count)), &
                output%facex(output_face), output%facey(output_face), area, counterclockwise, 0, 0, -999.0D0)
        ! Face z coordinates are unknown.
    end do


    ! Store remaining output variables in output mesh geometry.
    output%meshName = trim(input%meshName)//'_agg'
    output%dim = input%dim

    output%numNode = output_node_count
    output%numEdge = output_edge_count
    output%numFace = output_face_count

    !TODO deallocate temporary arrays

    success = .true.

end function aggregate_ugrid_geometry

!
!------------------------------------------------------------------------------
end module m_aggregate_waqgeom
