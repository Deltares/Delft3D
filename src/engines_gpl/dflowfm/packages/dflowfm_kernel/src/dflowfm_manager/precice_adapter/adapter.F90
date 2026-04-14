module precice_adapter
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precision, only: dp
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   public :: precice_adapter_t

   type, extends(precice_adapter_interface_t) :: precice_adapter_t
      character(kind=c_char, len=250) :: configfile
      character(kind=c_char, len=20) :: name = "fm"
      character(kind=c_char, len=13) :: meshname = "fm_flow_nodes"
      character(kind=c_char, len=10) :: bed_levels_name = "bed_levels"
      character(kind=c_char, len=12) :: water_levels_name = "water_levels"
      character(kind=c_char, len=12) :: water_depths_name = "water_depths"
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      logical :: is_comm_set = .false.
      integer(kind=c_int) :: comm
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: numranks = 1_c_int
      real(kind=c_double), dimension(:), allocatable :: mesh_coordinates ! Mesh coordinates: x1,y1,x2,y2,...,xN,yN
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

   function precice_adapter_constructor(configfile, name, is_comm_set, comm, my_rank, numranks, meshname, &
                                        mesh_size, mesh_coordinates) result(adapter_instance)
      use precision, only: dp
      use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

      implicit none(type, external)

      character(kind=c_char, len=*), intent(in) :: configfile
      character(kind=c_char, len=*), intent(in) :: name
      logical, intent(in) :: is_comm_set
      integer(kind=c_int), intent(in) :: comm
      integer(kind=c_int), intent(in) :: my_rank
      integer(kind=c_int), intent(in) :: numranks
      character(kind=c_char, len=*), intent(in) :: meshname
      integer(kind=c_int), intent(in) :: mesh_size
      real(KIND=c_double), dimension(:), intent(in), allocatable :: mesh_coordinates
      type(precice_adapter_t), pointer :: adapter_instance

      allocate (adapter_instance)
      adapter_instance%configfile = configfile
      adapter_instance%name = name
      adapter_instance%is_comm_set = is_comm_set
      adapter_instance%my_rank = my_rank
      adapter_instance%numranks = numranks
      adapter_instance%comm = comm
      adapter_instance%meshname = meshname

      adapter_instance%mesh_size = mesh_size
      adapter_instance%mesh_coordinates = mesh_coordinates

   end function precice_adapter_constructor

   subroutine precice_adapter_initialize(self)
      use precice, only: precicef_set_vertices, precicef_initialize, &
                         precicef_create_with_communicator, precicef_create, &
                         precicef_requires_initial_data
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      integer(kind=c_int) :: is_initial_data_required = 0_c_int

      if (self%is_comm_set) then
         call precicef_create_with_communicator(self%name, self%configfile, self%my_rank, &
                                                self%numranks, self%comm, len(self%name), &
                                                len(self%configfile))
      else
         call precicef_create(self%name, self%configfile, self%my_rank, self%numranks, &
                              len(self%name), len(self%configfile))
      end if

      allocate (self%vertex_ids(self%mesh_size))
      call precicef_set_vertices(self%meshname, self%mesh_size, self%mesh_coordinates, self%vertex_ids, len(self%meshname))

      call precicef_requires_initial_data(is_initial_data_required)
      if (is_initial_data_required /= 0) then
         ! TODO: Write initial data
      end if

      ! Finally, call initialise.
      call precicef_initialize()
   end subroutine precice_adapter_initialize

   subroutine precice_adapter_update(self, timestep)
      use precice, only: precicef_get_max_time_step_size, precicef_advance, &
                         precicef_is_coupling_ongoing, &
                         precicef_get_max_time_step_size, precicef_write_data
      use precision, only: dp
      use m_flow, only: hs
      ! TODO: Import more (global) data structs here.

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

      ! Write water depths (do we need to consider active nodes?)
      call precicef_write_data(self%meshname, self%water_depths_name, &
                               size(self%vertex_ids), self%vertex_ids, &
                               hs, len(self%meshname), len(self%water_depths_name))

      ! Actually advance time
      call precicef_get_max_time_step_size(max_timestep)
      if (max_timestep < timestep) then
         call precicef_advance(max_timestep)
         ! ... and now what?
         call precicef_advance(timestep - max_timestep) ! This works?
      else
         call precicef_advance(timestep)
      end if

      ! TODO: Read latest state here ?
   end subroutine precice_adapter_update

   subroutine precice_adapter_finalize(self)
      use precice, only: precicef_finalize
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      if ( loc(self) >=0 ) continue ! Suppress unused error.

      call precicef_finalize()
   end subroutine precice_adapter_finalize

end module precice_adapter
