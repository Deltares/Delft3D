module precice_adapter
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precision, only: dp
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   public :: precice_adapter_t

   type, extends(precice_adapter_interface_t) :: precice_adapter_t
      character(kind=c_char, len=:), allocatable :: config_file
      character(kind=c_char, len=:), allocatable :: name
      character(kind=c_char, len=:), allocatable :: cell_center_mesh_name
      character(kind=c_char, len=27) :: bed_levels_name = "sea_floor_depth_below_geoid" ! unstruc_netcdf:id_bldepth
      character(kind=c_char, len=18) :: water_levels_name = "sea_surface_height" ! unstruc_netcdf:id_s1
      character(kind=c_char, len=33) :: water_depths_name = "sea_floor_depth_below_sea_surface" ! unstruc_netcdf:id_hs
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      logical :: is_communicator_set = .false.
      integer(kind=c_int) :: communicator
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: number_of_ranks = 1_c_int
      real(kind=c_double), dimension(:), allocatable :: cell_center_mesh_coordinates_2d ! Mesh coordinates: x1,y1,x2,y2,...,xN,yN
      integer(kind=c_int) :: mesh_size = 0_c_int ! Number of vertices in the mesh: N
   contains
      procedure :: initialize => precice_adapter_initialize
      procedure :: update => precice_adapter_update
      procedure :: finalize => precice_adapter_finalize
   end type precice_adapter_t

   interface precice_adapter_t
      procedure :: precice_adapter_constructor
   end interface

contains

   function precice_adapter_constructor(config_file, name, is_communicator_set, communicator, my_rank, number_of_ranks, cell_center_mesh_name, &
                                        mesh_size, cell_center_mesh_coordinates_2d) result(adapter_instance)
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
      integer(kind=c_int), intent(in) :: mesh_size
      real(kind=c_double), dimension(:), intent(in), allocatable :: cell_center_mesh_coordinates_2d
      type(precice_adapter_t), pointer :: adapter_instance

      allocate (adapter_instance)
      adapter_instance%config_file = config_file
      adapter_instance%name = name
      adapter_instance%is_communicator_set = is_communicator_set
      adapter_instance%my_rank = my_rank
      adapter_instance%number_of_ranks = number_of_ranks
      adapter_instance%communicator = communicator
      adapter_instance%cell_center_mesh_name = cell_center_mesh_name

      adapter_instance%mesh_size = mesh_size
      adapter_instance%cell_center_mesh_coordinates_2d = cell_center_mesh_coordinates_2d

   end function precice_adapter_constructor

   subroutine precice_adapter_initialize(self)
      use precice, only: precicef_set_vertices, precicef_initialize, &
                         precicef_create_with_communicator, precicef_create, &
                         precicef_requires_initial_data
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      integer(kind=c_int) :: is_initial_data_required = 0_c_int

      if (self%is_communicator_set) then
         call precicef_create_with_communicator(self%name, self%config_file, self%my_rank, &
                                                self%number_of_ranks, self%communicator, len(self%name), &
                                                len(self%config_file))
      else
         call precicef_create(self%name, self%config_file, self%my_rank, self%number_of_ranks, &
                              len(self%name), len(self%config_file))
      end if

      allocate (self%vertex_ids(self%mesh_size))
      call precicef_set_vertices(self%cell_center_mesh_name, self%mesh_size, self%cell_center_mesh_coordinates_2d, self%vertex_ids, len(self%cell_center_mesh_name))

      call precicef_requires_initial_data(is_initial_data_required)
      if (is_initial_data_required /= 0) then
         call precice_adapter_write_data(self)
      end if

      ! Finally, call initialise.
      call precicef_initialize()
   end subroutine precice_adapter_initialize

   subroutine precice_adapter_update(self, timestep)
      use precice, only: precicef_get_max_time_step_size, precicef_advance, &
                         precicef_is_coupling_ongoing, &
                         precicef_get_max_time_step_size
      use precision, only: dp
      use MessageHandling, only: mess, LEVEL_ERROR

      implicit none(type, external)

      class(precice_adapter_t), intent(inout) :: self
      real(kind=dp), intent(in) :: timestep

      integer(kind=c_int) :: is_ongoing
      real(kind=c_double) :: max_timestep

      call precicef_is_coupling_ongoing(is_ongoing)
      if (.not. is_ongoing) then
         return ! Skip if the connection is no longer ongoing.
      end if

      ! TODO: Implement precice stuff including possible read/write etc.
      call precice_adapter_write_data(self)

      ! Actually advance time
      call precicef_get_max_time_step_size(max_timestep)
      if (timestep > max_timestep) then
         call mess(LEVEL_ERROR, "User time step will skip end of preCICE coupling window!")
      end if

      if (abs(max_timestep - timestep) <= 1E-5) then
         call precicef_advance(max_timestep)
      else
         call precicef_advance(timestep)
      end if

      ! TODO: Read latest state here ?
   end subroutine precice_adapter_update

   subroutine precice_adapter_finalize(self)
      use precice, only: precicef_finalize
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      if (loc(self) >= 0) continue ! Suppress unused error.

      call precicef_finalize()
   end subroutine precice_adapter_finalize

   subroutine precice_adapter_write_data(self)
      use precice, only: precicef_write_data
      use precision, only: dp
      use MessageHandling, only: mess, LEVEL_ERROR
      use m_flow, only: hs, s1
      use m_flowgeom, only: bl, ndx2d
      implicit none(type, external)
      class(precice_adapter_t), intent(in) :: self

      ! Write water depths (do we need to consider active nodes?)
      call precicef_write_data(self%cell_center_mesh_name, self%water_depths_name, &
                               size(self%vertex_ids), self%vertex_ids, &
                               hs, len(self%cell_center_mesh_name), len(self%water_depths_name))
      call precicef_write_data(self%cell_center_mesh_name, self%water_levels_name, &
                               size(self%vertex_ids), self%vertex_ids, &
                               s1, len(self%cell_center_mesh_name), len(self%water_levels_name))
      call precicef_write_data(self%cell_center_mesh_name, self%bed_levels_name, &
                               size(self%vertex_ids), self%vertex_ids, &
                               -1 * bl(1:ndx2d), len(self%cell_center_mesh_name), len(self%bed_levels_name))
   end subroutine precice_adapter_write_data

end module precice_adapter
