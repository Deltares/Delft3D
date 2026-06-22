!> @file adapter.F90
!! @brief preCICE adapter implementation for D-Flow FM.
!!
!! This module implements a concrete adapter that extends the
!! `precice_adapter_interface_t` and provides routines to construct,
!! initialize, update (advance) and finalize a preCICE coupling instance.
!! It also contains utilities to publish model quantities to preCICE meshes.
module precice_adapter
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precision, only: dp
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   real(kind=dp), save :: summed_time_progress !> Cumulative time progress since the last preCICE advance, used to determine when to call precicef_advance.
   public :: precice_adapter_t

   !> Maximum length for preCICE standard name strings stored in quantity_t.
   integer, parameter :: MAX_STANDARD_NAME_LENGTH = 50

   !> A single quantity that can be exchanged with preCICE.
   type :: quantity_t
      character(kind=c_char, len=MAX_STANDARD_NAME_LENGTH) :: standard_name
      logical :: is_active !> Flag indicating if this quantity should be published to preCICE
   end type quantity_t

   !> Container with all quantities used by the adapter.
   type :: quantities_t
      type(quantity_t) :: bl = quantity_t(standard_name="sea_floor_depth_below_geoid", is_active=.true.)
      type(quantity_t) :: s1 = quantity_t(standard_name="sea_surface_height", is_active=.true.)
      type(quantity_t) :: hs = quantity_t(standard_name="sea_floor_depth_below_sea_surface", is_active=.false.)
      type(quantity_t) :: rho = quantity_t(standard_name="sea_water_potential_density", is_active=.true.)
   end type quantities_t

   !> Concrete preCICE adapter implementation.
   !! Extends `precice_adapter_interface_t`.
   type, extends(precice_adapter_interface_t) :: precice_adapter_t
      character(kind=c_char, len=:), allocatable :: config_file
      character(kind=c_char, len=:), allocatable :: name
      character(kind=c_char, len=:), allocatable :: cell_center_mesh_name
      character(kind=c_char, len=:), allocatable :: cell_center_mesh_3d_name
      type(quantities_t) :: quantities
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids_3d
      logical :: is_communicator_set = .false.
      integer(kind=c_int) :: communicator
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: number_of_ranks = 1_c_int
      real(kind=c_double), dimension(:), allocatable :: cell_center_mesh_coordinates_2d ! Mesh coordinates: x1,y1,x2,y2,...,xN,yN
      real(kind=c_double), dimension(:), allocatable :: cell_center_mesh_coordinates_3d ! Mesh coordinates: x1,y1,z1,x2,y2,z2,...,xN,yN,zN
      integer(kind=c_int) :: mesh_size = 0_c_int ! Number of vertices in the mesh: N
      integer(kind=c_int) :: mesh_3d_size = 0_c_int ! Number of vertices in the 3D mesh: N*kmax
   contains
      procedure :: initialize => precice_adapter_initialize
      procedure :: update => precice_adapter_update
      procedure :: finalize => precice_adapter_finalize
   end type precice_adapter_t

   !> Constructor interface for `precice_adapter_t`.
   interface precice_adapter_t
      procedure :: precice_adapter_constructor
   end interface

contains

   !> Constructor for `precice_adapter_t`.
   !! Allocates and populates a new adapter instance with the provided settings and mesh coordinates.
   !! @return pointer to newly allocated `precice_adapter_t` instance.
   function precice_adapter_constructor(config_file, name, is_communicator_set, communicator, my_rank, number_of_ranks, cell_center_mesh_name, cell_center_mesh_3d_name, &
                                        mesh_size, mesh_3d_size, cell_center_mesh_coordinates_2d, cell_center_mesh_coordinates_3d) result(adapter_instance)
      use precision, only: dp
      use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

      implicit none(type, external)

      character(kind=c_char, len=*), intent(in) :: config_file
      character(kind=c_char, len=*), intent(in) :: name
      logical, intent(in) :: is_communicator_set
      integer(kind=c_int), intent(in) :: communicator
      integer(kind=c_int), intent(in) :: my_rank
      integer(kind=c_int), intent(in) :: number_of_ranks
      character(kind=c_char, len=*), intent(in) :: cell_center_mesh_name
      character(kind=c_char, len=*), intent(in) :: cell_center_mesh_3d_name
      integer(kind=c_int), intent(in) :: mesh_size
      integer(kind=c_int), intent(in) :: mesh_3d_size
      real(kind=c_double), dimension(:), intent(in), allocatable :: cell_center_mesh_coordinates_2d
      real(kind=c_double), dimension(:), intent(in), allocatable :: cell_center_mesh_coordinates_3d
      type(precice_adapter_t), pointer :: adapter_instance

      allocate (adapter_instance)
      adapter_instance%config_file = config_file
      adapter_instance%name = name
      adapter_instance%is_communicator_set = is_communicator_set
      adapter_instance%my_rank = my_rank
      adapter_instance%number_of_ranks = number_of_ranks
      adapter_instance%communicator = communicator
      adapter_instance%cell_center_mesh_name = cell_center_mesh_name
      adapter_instance%cell_center_mesh_3d_name = cell_center_mesh_3d_name

      adapter_instance%mesh_size = mesh_size
      adapter_instance%mesh_3d_size = mesh_3d_size
      adapter_instance%cell_center_mesh_coordinates_2d = cell_center_mesh_coordinates_2d
      adapter_instance%cell_center_mesh_coordinates_3d = cell_center_mesh_coordinates_3d

   end function precice_adapter_constructor

   !> Initialize the preCICE adapter and preCICE participant.
   !! Creates the preCICE participant (with or without communicator), registers mesh vertices,
   !! produces initial data if required, and calls preCICE initialize.
   subroutine precice_adapter_initialize(self)
      use precice, only: precicef_set_vertices, precicef_initialize, &
                         precicef_create_with_communicator, precicef_create, &
                         precicef_requires_initial_data, precicef_set_mesh_access_region
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      integer(kind=c_int) :: is_initial_data_required = 0_c_int
      character(kind=c_char, len=19) :: sources_sinks_mesh_name = "sources_sinks_nodes"
      real(kind=c_double), dimension(4) :: sources_sinks_bounding_box = [-1.0, 1.0, -1.0, 1.0]

      if (self%is_communicator_set) then
         call precicef_create_with_communicator(self%name, self%config_file, self%my_rank, &
                                                self%number_of_ranks, self%communicator, len(self%name), &
                                                len(self%config_file))
      else
         call precicef_create(self%name, self%config_file, self%my_rank, self%number_of_ranks, &
                              len(self%name), len(self%config_file))
      end if

      allocate (self%vertex_ids(self%mesh_size))
      allocate (self%vertex_ids_3d(self%mesh_3d_size))
      call precicef_set_vertices(self%cell_center_mesh_name, self%mesh_size, self%cell_center_mesh_coordinates_2d, self%vertex_ids, len(self%cell_center_mesh_name))
      call precicef_set_vertices(self%cell_center_mesh_3d_name, self%mesh_3d_size, self%cell_center_mesh_coordinates_3d, self%vertex_ids_3d, len(self%cell_center_mesh_3d_name))

      call precicef_set_mesh_access_region(sources_sinks_mesh_name, sources_sinks_bounding_box, len(sources_sinks_mesh_name))

      call precicef_requires_initial_data(is_initial_data_required)
      if (is_initial_data_required /= 0) then
         call precice_adapter_write_data(self)
      end if

      call precicef_initialize()
      summed_time_progress = 0.0
   end subroutine precice_adapter_initialize

   !> Advance the coupling when the accumulated model_time reaches the preCICE time_window.
   !! Accumulate timesteps, if the preCICE time_window is reached:
   !! - Remesh the 3D mesh
   !! - Write data (2D and 3D) to preCICE
   !! - Advance preCICE
   subroutine precice_adapter_update(self, timestep)
      use precice, only: precicef_get_max_time_step_size, precicef_advance, &
                         precicef_is_coupling_ongoing, precicef_is_time_window_complete, &
                         precicef_get_max_time_step_size, precicef_write_data, precicef_reset_mesh, precicef_set_vertices
      use precice_adapter_utils, only: set_cell_center_mesh_zcoords
      use precision, only: dp
      use MessageHandling, only: mess, LEVEL_ERROR
      use m_flow, only: kmx, zws

      implicit none(type, external)

      class(precice_adapter_t), intent(inout) :: self
      real(kind=dp), intent(in) :: timestep

      integer(kind=c_int) :: is_ongoing
      integer(kind=c_int) :: is_time_window_complete
      real(kind=c_double) :: max_timestep

      call precicef_is_coupling_ongoing(is_ongoing)
      if (is_ongoing == 0) then
         return ! Skip if the connection is no longer ongoing.
      end if

      ! Update summed time progress and check if we need to advance preCICE
      call precicef_get_max_time_step_size(max_timestep)
      summed_time_progress = summed_time_progress + timestep
      if (summed_time_progress > max_timestep + 1.0) then
         call mess(LEVEL_ERROR, "Summed user time steps are beyond the preCICE coupling window!")
      end if
      if (summed_time_progress > max_timestep - 1.0e-5) then
         ! Remesh the 3D mesh; that only works if the time window is complete (i.e. preCICE is ready for a new mesh).
         call precicef_is_time_window_complete(is_time_window_complete)
         if (is_time_window_complete == 1) then
            call precicef_reset_mesh(self%cell_center_mesh_3d_name, len(self%cell_center_mesh_3d_name))
            call set_cell_center_mesh_zcoords(self%mesh_size, kmx, zws, self%cell_center_mesh_coordinates_3d)
            call precicef_set_vertices(self%cell_center_mesh_3d_name, self%mesh_3d_size, self%cell_center_mesh_coordinates_3d, self%vertex_ids_3d, len(self%cell_center_mesh_3d_name))
         end if
         call precice_adapter_write_data(self)
         call precicef_advance(max_timestep)
         ! Reset summed_time_progress after advancing
         summed_time_progress = 0.0
      else
         write (*, *) "Not advancing preCICE yet, summed_time_progress = ", summed_time_progress, " max_timestep = ", max_timestep
      end if
   end subroutine precice_adapter_update

   !> Finalize the preCICE adapter and perform preCICE shutdown.
   subroutine precice_adapter_finalize(self)
      use precice, only: precicef_finalize
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      if (loc(self) >= 0) continue ! Suppress unused error.

      call precicef_finalize()
   end subroutine precice_adapter_finalize

   !> Write the currently active model quantities to the registered preCICE meshes.
   !! Quantities published:
   !! - `hs` (sea_floor_depth_below_sea_surface) written to 2D mesh when active.
   !! - `s1` (sea_surface_height) written to 2D mesh when active.
   !! - `bl` (sea_floor_depth_below_geoid) written to 2D mesh when active (note the sign inversion).
   !! - `rho` (sea_water_potential_density) written to 3D mesh when active.
   subroutine precice_adapter_write_data(self)
      use precice, only: precicef_write_data
      use precision, only: dp
      use MessageHandling, only: mess, LEVEL_ERROR
      use m_flow, only: hs, s1
      use m_flowgeom, only: bl, ndx2d
      use m_turbulence, only: potential_density
      implicit none(type, external)
      class(precice_adapter_t), intent(in) :: self

      if (self%quantities%hs%is_active) then
         call precicef_write_data(self%cell_center_mesh_name, self%quantities%hs%standard_name, &
                                  size(self%vertex_ids), self%vertex_ids, &
                                  hs, len(self%cell_center_mesh_name), len(trim(self%quantities%hs%standard_name)))
      end if
      if (self%quantities%s1%is_active) then
         call precicef_write_data(self%cell_center_mesh_name, self%quantities%s1%standard_name, &
                                  size(self%vertex_ids), self%vertex_ids, &
                                  s1, len(self%cell_center_mesh_name), len(trim(self%quantities%s1%standard_name)))
      end if
      if (self%quantities%bl%is_active) then
         call precicef_write_data(self%cell_center_mesh_name, self%quantities%bl%standard_name, &
                                  size(self%vertex_ids), self%vertex_ids, &
                                  -1 * bl(1:ndx2d), len(self%cell_center_mesh_name), len(trim(self%quantities%bl%standard_name)))
      end if
      if (self%quantities%rho%is_active) then
         call precicef_write_data(self%cell_center_mesh_3d_name, self%quantities%rho%standard_name, &
                                  size(self%vertex_ids_3d), self%vertex_ids_3d, &
                                  potential_density, len(self%cell_center_mesh_3d_name), len(trim(self%quantities%rho%standard_name)))
      end if
   end subroutine precice_adapter_write_data

end module precice_adapter
