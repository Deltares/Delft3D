!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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

   private

   public :: aggregate_ugrid_geometry
   public :: aggregate_ugrid_layers_interfaces

contains
!> Aggregates the given mesh geometry and edge type array using the given aggregation table.
!! The mesh aggregation algorithm removes edges, but preserves the order of the edges. The edge type of a given edge stays the same.
!! So if the edges in the un-aggregated mesh are ordered (first flow links, then closed edges),
!! then the edges in the aggregated mesh will still be ordered (first flow links, then closed edges).
!!
!! since array pointers will become disassociated, possibly causing memory leaks.
   function aggregate_ugrid_geometry(input, output, input_edge_type, output_edge_type, &
                                     face_mapping_table, layer_mapping_table) result(success)
      use io_ugrid
      use geometry_module
      use m_alloc
      use m_missing, only: dmiss, missing_value => imiss

      implicit none

      type(t_ug_meshgeom), intent(in) :: input !< The mesh geometry to be aggregated.
      type(t_ug_meshgeom), intent(inout) :: output !< Aggregated mesh geometry.
      integer, dimension(:), intent(in) :: input_edge_type !< The edge type array to be aggregated.
      integer, dimension(:), pointer, intent(out) :: output_edge_type !< Aggregated edge type array.
      integer, dimension(:), intent(in) :: face_mapping_table !< Mapping table flow cells -> waq cells.
      integer, dimension(:), optional, intent(in) :: layer_mapping_table !< Mapping table input layers and interfaces -> waq layers and interfaces.
      logical :: success !< Result status, true if successful.

      character(len=255) :: message !< Temporary variable for writing log messages.
      integer, dimension(:), allocatable :: node_mapping_table, reverse_node_mapping_table, reverse_edge_mapping_table !< Mapping tables.
      integer :: input_edge_count, output_edge_count, output_node_count, output_face_count, max_nodes_per_face, node_count !< Counters.
      integer :: i, j, input_edge, output_edge, input_node, output_node, output_face !< Counters.
      integer, dimension(2) :: faces !< Helper array.
      integer, dimension(:, :), allocatable :: input_edge_output_faces !< Helper array.
      integer, dimension(:), allocatable :: face_edge_count, nodes !< Helper arrays.
      double precision :: area !< Output of subroutine comp_masscenter (not used here).
      integer :: counterclockwise !< Output of subroutine comp_masscenter (not used here).

      success = .false.
      output%start_index = 1

      ! 1. Determine output edge_faces and edge_nodes.
      ! Apply face mapping table to edge faces.
      input_edge_count = input%numEdge
      call realloc(input_edge_output_faces, (/2, input_edge_count/), fill=missing_value)
      do input_edge = 1, input_edge_count
         do i = 1, 2
            if (input%edge_faces(i, input_edge) /= missing_value) then
               input_edge_output_faces(i, input_edge) = face_mapping_table(input%edge_faces(i, input_edge))
            end if
         end do ! i
      end do ! input_edge
      ! Create edge mapping table and output edge_faces and edge_nodes.
      call realloc(reverse_edge_mapping_table, input_edge_count)
      call reallocP(output%edge_faces, (/2, input_edge_count/))
      call reallocP(output%edge_nodes, (/2, input_edge_count/))
      output_edge = 0
      do input_edge = 1, input_edge_count
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
      call reallocP(output%edge_faces, (/2, output_edge_count/), keepExisting=.true.)
      call reallocP(output%edge_nodes, (/2, output_edge_count/), keepExisting=.true.)

      ! 2. Determine output edge coordinates and types.
      call reallocP(output%edgex, output_edge_count)
      call reallocP(output%edgey, output_edge_count)
      call reallocP(output_edge_type, output_edge_count)
      do output_edge = 1, output_edge_count
         output%edgex(output_edge) = input%edgex(reverse_edge_mapping_table(output_edge))
         output%edgey(output_edge) = input%edgey(reverse_edge_mapping_table(output_edge))
         ! Edge z coordinates are unknown.
         output_edge_type(output_edge) = input_edge_type(reverse_edge_mapping_table(output_edge))
      end do

      ! 3. Create node mapping table.
      call realloc(node_mapping_table, input%numNode, fill=missing_value)
      ! All nodes that are present in output edge_nodes should remain, all other nodes are not needed anymore in the aggregated mesh.
      ! First create mask of remaining nodes in node_mapping_table.
      do output_edge = 1, output_edge_count
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
      do input_node = 1, input%numNode
         if (node_mapping_table(input_node) == 1) then ! Node that should stay.
            ! The remaining output nodes have a different numbering.
            output_node = output_node + 1
            node_mapping_table(input_node) = output_node
            reverse_node_mapping_table(output_node) = input_node
         end if
      end do
      ! Renumber input node numbers to output node numbers in output edge_nodes, using node_mapping_table.
      do output_edge = 1, output_edge_count
         output%edge_nodes(1, output_edge) = node_mapping_table(output%edge_nodes(1, output_edge))
         output%edge_nodes(2, output_edge) = node_mapping_table(output%edge_nodes(2, output_edge))
      end do

      ! 4. Determine output node coordinates.
      call reallocP(output%nodex, output_node_count)
      call reallocP(output%nodey, output_node_count)
      call reallocP(output%nodez, output_node_count)
      do output_node = 1, output_node_count
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
      do output_edge = 1, output_edge_count
         faces = output%edge_faces(1:2, output_edge)
         ! Add 1 edge for both faces.
         do i = 1, 2
            if (faces(i) == missing_value) then
               cycle
            end if

            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
         end do ! i
      end do ! output_edge
      do output_face = 1, output_face_count
         if (face_edge_count(output_face) < 3) then
            write (message, *) 'Face edge count in aggregated mesh < 3 for face ', output_face, '. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end do
      ! Determine max_nodes_per_face.
      max_nodes_per_face = maxval(face_edge_count)
      ! Determine nodes, edges and faces for each output face.
      call reallocP(output%face_edges, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      ! Re-use face_edge_count array to put edges in the next available spot in the output%face_edges array.
      face_edge_count = 0
      do output_edge = 1, output_edge_count
         faces = output%edge_faces(1:2, output_edge)
         do i = 1, 2
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
      call reallocP(output%face_nodes, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      do output_face = 1, output_face_count
         ! Sort edges for current output face.
         success = sort_edges(output_face, output%face_edges(1:face_edge_count(output_face), output_face), output%face_nodes(1:face_edge_count(output_face), output_face), &
                              input%edge_nodes, input%face_nodes, input%edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output%edge_nodes)
         if (.not. success) return
      end do

      ! 7. Determine output face_links.
      call reallocP(output%face_links, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      do output_face = 1, output_face_count
         ! Get output faces that are adjacent to the current output_face.
         call get_adjacent_faces(output_face, output%face_edges, output%edge_faces, output%face_links(1:face_edge_count(output_face), output_face))
      end do

      ! 8. Determine output face coordinates.
      ! Here calculate the cell centroids (cell "centers of mass").
      call realloc(nodes, max_nodes_per_face)
      call reallocP(output%facex, output_face_count)
      call reallocP(output%facey, output_face_count)
      do output_face = 1, output_face_count
         node_count = face_edge_count(output_face)

         ! Reset nodes.
         nodes = missing_value
         nodes(1:node_count) = output%face_nodes(1:node_count, output_face)

         ! Note that passed xs and ys arrays are larger than the passed polygon size (extra elements are not used in subroutine comp_masscenter).
         call comp_masscenter(node_count, output%nodex(nodes(1:node_count)), output%nodey(nodes(1:node_count)), &
                              output%facex(output_face), output%facey(output_face), area, counterclockwise, 0, 0, dmiss)
         ! Face z coordinates are unknown.
      end do

      ! Store remaining output variables in output mesh geometry.
      output%meshName = trim(input%meshName)//'_agg'
      output%dim = input%dim

      output%numNode = output_node_count
      output%numEdge = output_edge_count
      output%numFace = output_face_count

      if (present(layer_mapping_table)) then
         success = aggregate_ugrid_layers_interfaces(input, output, layer_mapping_table)
         if (.not. success) then
            write (message, *) 'There was a problem when trying to aggregate the layer information.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end if
      
      success = .true.

   end function aggregate_ugrid_geometry

   function aggregate_ugrid_layers_interfaces(input, output, layer_mapping_table) result(success)
      use io_ugrid
      use m_alloc

      implicit none

      type(t_ug_meshgeom), intent(in) :: input !< The layers and interfaces to be aggregated.
      type(t_ug_meshgeom), intent(inout) :: output !< Aggregated layers and interfaces.
      integer, dimension(:), intent(in) :: layer_mapping_table !< Mapping table input layers and interfaces -> waq layers and interfaces.
      logical :: success !< Result status, true if successful.
      logical :: no_aggregation !< Is there no aggregation at all?
      logical :: to_2D !< Is there aggregation to 2D?
      logical :: top_to_bottom !< Are layers defined from top to bottom?
      integer :: i, old_layer, new_layer, increment !< Loop variable and increment variable.
      character(len=255) :: message !< Temporary variable for writing log messages.

      ! Set defaults
      success = .false.
      no_aggregation = .true.
      to_2D = .true.

      ! Check the validity of the layer mapping table
      ! Is the size equal to the number of layer ins the input?
      if (size(layer_mapping_table) /= input%num_layers) then
         write (message, *) 'Definition of vertical layer mapping does not match the number of layers.'
         call mess(LEVEL_ERROR, trim(message))
         return
      end if

      ! Does it start with one?
      if (layer_mapping_table(1) /= 1) then
         write (message, *) 'Definition of vertical layer mapping should start with one.'
         call mess(LEVEL_ERROR, trim(message))
         return
      end if

      ! Does the vertical layer mapping can only contain increments of one or stays the same between layers?
      do i = 1, input%num_layers - 1
         increment = layer_mapping_table(i + 1) - layer_mapping_table(i)
         if (increment > 1) then
            write (message, *) 'Definition of vertical layer mapping can only contain increments of one or remain '// &
               'the same between layers.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
         if (increment < 0) then
            write (message, *) 'Definition of vertical layer mapping can not decrease between layers.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
         if (increment == 0) then
            no_aggregation = .false.
         end if
         if (increment == 1) then
            to_2D = .false.
         end if
      end do

      ! For z-sigma-layers, the last sigma-layer can not be merged with the first z-layer unless we aggregate to 2D.
      if (input%layertype == LAYERTYPE_OCEAN_SIGMA_Z .and. .not. to_2D) then
         if (layer_mapping_table(input%numtopsig)==layer_mapping_table(input%numtopsig + 1)) then
            write (message, *) 'Z and sigma layers in z-sigma-layers model can not be merged!'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end if

      ! When we aggregate to 2D, then the new %num_layers is 0, the rest is the default. We don't need to aggregate the layers.
      if (to_2D) then
         output%num_layers = 0
         success = .true.
         return
      end if

      ! When there is no aggregation, just copy the input to the output and return.
      if (no_aggregation) then
         output = input
         success = .true.
         return
      end if
      
      ! The layer type always stays the same
      output%layertype = input%layertype
      
      ! The new number of layers is equal to the last value in the layer mapping table.
      output%num_layers = layer_mapping_table(input%num_layers)
      
      ! For z-sigma-layers, the new numtopsig is equal to the value in the layer mapping table of the old numtopsig.
      if (input%layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
         output%numtopsig = layer_mapping_table(input%numtopsig)
      end if

      ! Check if layers are defined from top to bottom based on the first two values.
      top_to_bottom = (input%interface_zs(1) > input%interface_zs(2))

      ! Allocate output arrays for layer_zs and interface_zs.
      call reallocP(output%layer_zs, output%num_layers)
      call reallocP(output%interface_zs, output%num_layers + 1)

      ! Copy the interfaces we need to keep to the output array.
      ! Always copy the first interface from the input.
      output%interface_zs(1) = input%interface_zs(1)
      new_layer = 1
      ! Loop over input layers.
      do i = 2, input%num_layers
         ! Skip if consecutive layers end up in the same new layer.
         if (top_to_bottom) then
            if (layer_mapping_table(i - 1) == layer_mapping_table(i)) then
               cycle
            end if
         else
            if (layer_mapping_table(input%num_layers - i + 1) == layer_mapping_table(input%num_layers - i + 2)) then
               cycle
            end if
         end if
         ! Copy the interfaces that we still need.
         new_layer = new_layer + 1
         output%interface_zs(new_layer) = input%interface_zs(i)
      end do
      ! Always copy the last interface from the input.
      output%interface_zs(new_layer + 1) = input%interface_zs(input%num_layers + 1)

      ! Calculate the output layers as the average of the two surrounding interfaces.
      do i = 1, output%num_layers
         output%layer_zs(i) = (output%interface_zs(i) + output%interface_zs(i + 1)) / 2.0d0
      end do
      
      ! Correct the last sigma layer in layer_zs in case of z-sigma-layers. We need this because the bottom interface of
      ! the last sigma-layer of -1.0 is not in interface_zs. It overlaps with the top interface of the first z-layer.
      if (output%layertype == LAYERTYPE_OCEAN_SIGMA_Z) then
         if (top_to_bottom) then
            output%layer_zs(output%numtopsig) = (output%interface_zs(output%numtopsig) - 1.0d0) / 2.0d0
         else
            output%layer_zs(output%num_layers - output%numtopsig + 1) = & 
               (output%interface_zs(output%num_layers - output%numtopsig + 2) - 1.0d0) / 2.0d0
         end if
      end if

      success = .true.

   end function aggregate_ugrid_layers_interfaces

!
!------------------------------------------------------------------------------
end module m_aggregate_waqgeom
