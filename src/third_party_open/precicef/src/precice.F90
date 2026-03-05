module precice
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double
  implicit none

  interface

    subroutine precicef_create(participantName, configFileName, &
      &                        solverProcessIndex, solverProcessSize, &
      &                        participantNameLength, configFileNameLength) &
      &  bind(c, name='precicef_create_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: participantName
      character(kind=c_char), dimension(*), intent(in) :: configFileName
      integer(kind=c_int), intent(in) :: solverProcessIndex
      integer(kind=c_int), intent(in) :: solverProcessSize
      integer(kind=c_int), value, intent(in) :: participantNameLength
      integer(kind=c_int), value, intent(in) :: configFileNameLength
    end subroutine precicef_create

    subroutine precicef_create_with_communicator(participantName, configFileName, &
      &                        solverProcessIndex, solverProcessSize, &
      &                        communicator, &
      &                        participantNameLength, configFileNameLength) &
      &  bind(c, name='precicef_create_with_communicator_')

      import :: c_char, c_int
      character(kind=c_char), dimension(*), intent(in) :: participantName
      character(kind=c_char), dimension(*), intent(in) :: configFileName
      integer(kind=c_int), intent(in) :: solverProcessIndex
      integer(kind=c_int), intent(in) :: solverProcessSize
      integer(kind=c_int), intent(in) :: communicator
      integer(kind=c_int), value, intent(in) :: participantNameLength
      integer(kind=c_int), value, intent(in) :: configFileNameLength
    end subroutine precicef_create_with_communicator

    subroutine precicef_initialize() &
      &  bind(c, name='precicef_initialize_')

    end subroutine precicef_initialize

    subroutine precicef_advance(timestepLengthLimit) &
      &  bind(c, name='precicef_advance_')

      import :: c_double
      real(kind=c_double), intent(in) :: timestepLengthLimit
    end subroutine precicef_advance

    subroutine precicef_finalize() &
      & bind(c, name='precicef_finalize_')

    end subroutine precicef_finalize

    subroutine precicef_requires_reading_checkpoint(isRequired) &
       &  bind(c, name='precicef_requires_reading_checkpoint_')

       import :: c_int
       integer(kind=c_int), intent(out) :: isRequired
    end subroutine precicef_requires_reading_checkpoint

    subroutine precicef_requires_writing_checkpoint(isRequired) &
       &  bind(c, name='precicef_requires_writing_checkpoint_')

       import :: c_int
       integer(kind=c_int), intent(out) :: isRequired
    end subroutine precicef_requires_writing_checkpoint

    subroutine precicef_get_mesh_dimensions(meshName, dimensions, &
      & meshNameLength) &
      & bind(c, name='precicef_get_mesh_dimensions_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(out) :: dimensions
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_get_mesh_dimensions

    subroutine precicef_get_data_dimensions(meshName, dataName, &
      & dimensions, meshNameLength, dataNameLength) &
      & bind(c, name='precicef_get_data_dimensions_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(out) :: dimensions
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_get_data_dimensions

    subroutine precicef_is_coupling_ongoing(isOngoing) &
      &  bind(c, name='precicef_is_coupling_ongoing_')

      import :: c_int
      integer(kind=c_int), intent(out) :: isOngoing
    end subroutine precicef_is_coupling_ongoing

    subroutine precicef_is_time_window_complete(isComplete) &
      &  bind(c, name='precicef_is_time_window_complete_')

      import :: c_int
      integer(kind=c_int), intent(out) :: isComplete
    end subroutine precicef_is_time_window_complete

    subroutine precicef_get_max_time_step_size(maxTimeStepSize) &
      & bind(c, name='precicef_get_max_time_step_size_')

      import :: c_double
      real(kind=c_double), intent(out) :: maxTimeStepSize
    end subroutine precicef_get_max_time_step_size

    subroutine precicef_requires_mesh_connectivity_for(meshName, required, meshNameLength) &
      & bind(c, name='precicef_requires_mesh_connectivity_for_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(out) :: required
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_requires_mesh_connectivity_for

    subroutine precicef_set_vertex(meshName, coordinates, id, meshNameLength) &
      &  bind(c, name='precicef_set_vertex_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      real(kind=c_double), intent(in) :: coordinates(3)
      integer(kind=c_int), intent(out) :: id
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_vertex

    subroutine precicef_get_mesh_vertex_size(meshName, meshSize, meshNameLength) &
      &  bind(c, name='precicef_get_mesh_vertex_size_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(out) :: meshSize
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_get_mesh_vertex_size

    subroutine precicef_set_vertices(meshName, size, coordinates, ids, meshNameLength) &
      &  bind(c, name='precicef_set_vertices_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      real(kind=c_double), intent(in) :: coordinates(*)
      integer(kind=c_int), intent(out) :: ids(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_vertices

    subroutine precicef_set_edge(meshName, firstVertexID, secondVertexID, &
      &                          meshNameLength) &
      &  bind(c, name='precicef_set_edge_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: firstVertexID
      integer(kind=c_int), intent(in) :: secondVertexID
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_edge

    subroutine precicef_set_mesh_edges(meshName, size, ids, meshNameLength) &
      & bind(c, name='precicef_set_mesh_edges_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_mesh_edges

    subroutine precicef_set_triangle(meshName, firstEdgeID, secondEdgeID, &
      &                              thirdEdgeID, meshNameLength) &
      &  bind(c, name='precicef_set_triangle_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: firstEdgeID
      integer(kind=c_int), intent(in) :: secondEdgeID
      integer(kind=c_int), intent(in) :: thirdEdgeID
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_triangle

    subroutine precicef_set_mesh_triangles(meshName, size, ids, meshNameLength) &
      & bind(c, name='precicef_set_mesh_triangles_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_mesh_triangles

    subroutine precicef_set_quad(meshName, firstVertexID, secondVertexID, &
      &                          thirdVertexID, fourthVertexID, &
      &                          meshNameLength ) &
      &  bind(c, name='precicef_set_quad_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: firstVertexID
      integer(kind=c_int), intent(in) :: secondVertexID
      integer(kind=c_int), intent(in) :: thirdVertexID
      integer(kind=c_int), intent(in) :: fourthVertexID
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_quad

    subroutine precicef_set_mesh_quads(meshName, size, ids, meshNameLength) &
      & bind(c, name='precicef_set_mesh_quads_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_mesh_quads

    subroutine precicef_set_tetrahedron(meshName, firstVertexID, secondVertexID, &
      &                                 thirdVertexID, fourthVertexID, &
      &                                 meshNameLength) &
      &  bind(c, name='precicef_set_tetrahedron_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: firstVertexID
      integer(kind=c_int), intent(in) :: secondVertexID
      integer(kind=c_int), intent(in) :: thirdVertexID
      integer(kind=c_int), intent(in) :: fourthVertexID
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_tetrahedron

    subroutine precicef_set_mesh_tetrahedra(meshName, size, ids, meshNameLength) &
      & bind(c, name='precicef_set_mesh_tetrahedra_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_mesh_tetrahedra

    subroutine precicef_requires_initial_data(isRequired) &
      & bind(c, name='precicef_requires_initial_data_')

      import :: c_int
      integer(kind=c_int), intent(out) :: isRequired
    end subroutine precicef_requires_initial_data

    subroutine precicef_write_data(meshName, dataName, size, ids, &
      &                            values, meshNameLength, dataNameLength) &
      & bind(c, name='precicef_write_data_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      real(kind=c_double), intent(in) :: values(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_write_data

    subroutine precicef_read_data(meshName, dataName, size, ids, &
      &                           relativeReadTime, values, meshNameLength, &
      &                           dataNameLength) &
      & bind(c, name='precicef_read_data_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      real(kind=c_double), intent(in) :: relativeReadTime
      real(kind=c_double), intent(out) :: values(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_read_data

    subroutine precicef_write_and_map_data(meshName, dataName, size, coordinates, &
      &                            values, meshNameLength, dataNameLength) &
      & bind(c, name='precicef_write_and_map_data_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(in) :: size
      real(kind=c_double), intent(in) :: coordinates(*)
      real(kind=c_double), intent(in) :: values(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_write_and_map_data

    subroutine precicef_map_and_read_data(meshName, dataName, size, coordinates, &
      &                           relativeReadTime, values, meshNameLength, &
      &                           dataNameLength) &
      & bind(c, name='precicef_map_and_read_data_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(in) :: size
      real(kind=c_double), intent(in) :: coordinates(*)
      real(kind=c_double), intent(in) :: relativeReadTime
      real(kind=c_double), intent(out) :: values(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_map_and_read_data

    subroutine precicef_set_mesh_access_region(meshName, boundingBox, &
      &                                        meshNameLength) &
      & bind(c, name='precicef_set_mesh_access_region_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      real(kind=c_double), intent(in) :: boundingBox(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_set_mesh_access_region

    subroutine precicef_get_mesh_vertex_ids_and_coordinates(meshName, size, &
      &                                                     ids, coordinates, &
      &                                                     meshNameLength) &
      & bind(c, name='precicef_get_mesh_vertex_ids_and_coordinates_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(out) :: ids(*)
      real(kind=c_double), intent(out) :: coordinates(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
    end subroutine precicef_get_mesh_vertex_ids_and_coordinates

    subroutine precicef_requires_gradient_data_for(meshName, dataName, &
      &                                            required, meshNameLength, &
      &                                            dataNameLength) &
      & bind(c, name='precicef_requires_gradient_data_for_')

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(out) :: required
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_requires_gradient_data_for

    subroutine precicef_write_gradient_data(meshName, dataName, size, ids, &
      &                                     gradients, meshNameLength, &
      &                                     dataNameLength) &
      & bind(c, name='precicef_write_gradient_data_')

      import :: c_int, c_char, c_double
      character(kind=c_char), dimension(*), intent(in) :: meshName
      character(kind=c_char), dimension(*), intent(in) :: dataName
      integer(kind=c_int), intent(in) :: size
      integer(kind=c_int), intent(in) :: ids(*)
      real(kind=c_double), intent(in) :: gradients(*)
      integer(kind=c_int), value, intent(in) :: meshNameLength
      integer(kind=c_int), value, intent(in) :: dataNameLength
    end subroutine precicef_write_gradient_data

    subroutine precicef_start_profiling_section(sectionName, sectionNameLength) &
      & bind(c, name="precicef_start_profiling_section_")

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(in) :: sectionName
      integer(kind=c_int), value, intent(in) :: sectionNameLength
    end subroutine precicef_start_profiling_section

    subroutine precicef_stop_last_profiling_section() &
      & bind(c, name="precicef_stop_last_profiling_section_")

    end subroutine precicef_stop_last_profiling_section

    subroutine precicef_get_version_information(versionInfo, lengthVersionInfo) &
      & bind(c, name="precicef_get_version_information_")

      import :: c_int, c_char
      character(kind=c_char), dimension(*), intent(out) :: versionInfo
      integer(kind=c_int), value, intent(in) :: lengthVersionInfo
    end subroutine precicef_get_version_information

  end interface

end module precice
