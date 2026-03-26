! TODO:
! Component name : character(kind=c_char, len=2) :: component_name = "fm" <- needs to be configurable for FM-FM connections.
! Mesh name :      character(kind=c_char, len=13) :: mesh_name = "fm_flow_nodes" <- needs configuring?
! Quantity names : e.g. character(kind=c_char, len=12) :: water_levels_name = "water_levels" <- as number of static names, in future they may be remappable.
! Vertex ID's :    integer(kind=c_int), dimension(:), allocatable :: flow_vertex_ids <- allocated and set at runtime.
module precice_adapter
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precice, only: precicef_finalize
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
      character(kind=c_char, len=13) :: flow_velocity_name = "flow_velocity"
      character(kind=c_char, len=13) :: wind_velocity_name = "wind_velocity"
      character(kind=c_char, len=23) :: vegetation_stem_density_name = "vegetation_stem_density"
      character(kind=c_char, len=19) :: vegetation_diameter_name = "vegetation_diameter"
      character(kind=c_char, len=17) :: vegetation_height_name = "vegetation_height"
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      character(kind=c_char, len=2) :: fx_name = "fx"
      character(kind=c_char, len=2) :: fy_name = "fy"
      character(kind=c_char, len=7) :: wsbodyu_name = "wsbodyu"
      character(kind=c_char, len=7) :: wsbodyv_name = "wsbodyv"
      character(kind=c_char, len=2) :: mx_name = "mx"
      character(kind=c_char, len=2) :: my_name = "my"
      character(kind=c_char, len=7) :: dissip2_name = "dissip2" ! dsurf
      character(kind=c_char, len=7) :: dissip3_name = "dissip3" ! dwcap
      character(kind=c_char, len=4) :: ubot_name = "ubot"
      character(kind=c_char, len=4) :: hrms_name = "hrms"
      character(kind=c_char, len=2) :: tp_name = "tp"
      character(kind=c_char, len=4) :: pdir_name = "pdir"
      logical :: is_comm_set
      integer(kind=c_int) :: comm
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: numranks = 1_c_int
      real(kind=c_double), dimension(:), allocatable :: mesh_coordinates ! Mesh coordinates: x1,y1,x2,y2,...,xN,yN
      integer(kind=c_int) :: active_count = 0_c_int
      logical :: needs_triangulation
      integer(kind=c_int), dimension(:), allocatable :: triangle_nodes
      integer(kind=c_int) :: num_triangles = 0_c_int
   contains
      procedure :: initialize => precice_adapter_initialize
      procedure :: update => precice_adapter_update
      procedure :: finalize => precice_adapter_finalize
      final :: precice_adapter_destructor
   end type precice_adapter_t

   interface precice_adapter_t
      procedure :: precice_adapter_constructor
   end interface

contains

   function precice_adapter_constructor(configfile, name, is_comm_set, comm, my_rank, numranks, meshname, &
                                        mesh_coordinates_x, mesh_coordinates_y, needs_triangulation) &
      result(adapter_instance)
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
      real(KIND=c_double), dimension(:), intent(in), pointer :: mesh_coordinates_x
      real(KIND=c_double), dimension(:), intent(in), pointer :: mesh_coordinates_y
      logical, intent(in) :: needs_triangulation
      type(precice_adapter_t), pointer :: adapter_instance
      ! Local variables
      integer :: i
      ! Dummy items for tricall(). TODO: Refactor into triangle specific function?
      !real(kind=c_double), dimension(1) :: dummy_xs3, dummy_ys3
      !integer(kind=c_int), dimension(1) :: dummy_edge_nodes, dummy_triangle_edges
      !integer(kind=c_int) :: dummy_ns3, dummy_num_edges
      !real(kind=c_double) :: dummy_trisize
      !integer :: one = 1

      allocate (adapter_instance)
      adapter_instance%configfile = configfile
      adapter_instance%name = name
      adapter_instance%is_comm_set = is_comm_set
      adapter_instance%my_rank = my_rank
      adapter_instance%numranks = numranks
      adapter_instance%comm = comm
      adapter_instance%meshname = meshname
      adapter_instance%needs_triangulation = needs_triangulation

      adapter_instance%active_count = min(size(mesh_coordinates_x), size(mesh_coordinates_x))
      allocate (adapter_instance%mesh_coordinates(adapter_instance%active_count * 2))

      do i = 1, adapter_instance%active_count
         adapter_instance%mesh_coordinates(2 * i - 1) = mesh_coordinates_x(i)
         adapter_instance%mesh_coordinates(2 * i) = mesh_coordinates_y(i)
      end do

      if (adapter_instance%needs_triangulation) then
         adapter_instance%num_triangles = adapter_instance%active_count * 2
         allocate (adapter_instance%triangle_nodes(3 * adapter_instance%num_triangles))
         ! TODO: Set up triangulation. HOW TO INCLUDE??
         !call tricall(one, mesh_coordinates_x, mesh_coordinates_y, adapter_instance%active_count, &
         !             adapter_instance%triangle_nodes, adapter_instance%num_triangles, dummy_edge_nodes, &
         !             dummy_num_edges, dummy_triangle_edges, dummy_xs3, dummy_ys3, dummy_ns3, dummy_trisize)
      else
         adapter_instance%num_triangles = 0_c_int
      end if

   end function precice_adapter_constructor

   subroutine precice_adapter_destructor(self)
      type(precice_adapter_t), intent(inout) :: self
      if (allocated(self%mesh_coordinates)) then
         deallocate (self%mesh_coordinates)
      end if
      if (allocated(self%triangle_nodes)) then
         deallocate (self%triangle_nodes)
      end if
   end subroutine precice_adapter_destructor

   subroutine precice_adapter_initialize(self)
      use precice, only: precicef_set_vertices, precicef_set_mesh_triangles, precicef_initialize, &
                         precicef_create_with_communicator, precicef_create, &
                         precicef_requires_initial_data
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self

      integer(kind=c_int), dimension(:), allocatable :: precice_triangle_nodes
      integer(kind=c_int) :: is_initial_data_required = 0_c_int
      integer :: i

      if (self%is_comm_set) then
         call precicef_create_with_communicator(self%name, self%configfile, self%my_rank, &
                                                self%numranks, self%comm, len(self%name), &
                                                len(self%configfile))
      else
         call precicef_create(self%name, self%configfile, self%my_rank, self%numranks, &
                              len(self%name), len(self%configfile))
      end if

      call precicef_set_vertices(self%meshname, self%active_count, self%mesh_coordinates, self%vertex_ids, len(self%meshname))

      if (self%needs_triangulation) then
         ! Take the triangles and tanslate them to precice vertex id's
         allocate (precice_triangle_nodes(3 * self%num_triangles))
         do i = 1, 3 * self%num_triangles
            precice_triangle_nodes(i) = self%vertex_ids(self%triangle_nodes(i))
         end do
         ! Register triangles with preCICE
         call precicef_set_mesh_triangles(self%meshname, self%num_triangles, precice_triangle_nodes, len(self%meshname))
      end if

      call precicef_requires_initial_data(is_initial_data_required)
      if (is_initial_data_required /= 0) then
         ! TODO: Write initial data
      end if

      ! Finally, call initialise.
      call precicef_initialize()
   end subroutine precice_adapter_initialize

   subroutine precice_adapter_update(self, timestep)
      use precice, only: precicef_get_max_time_step_size, precicef_advance
      use precision, only: dp
      ! TODO: Import (global) data structs here?

      implicit none(type, external)

      class(precice_adapter_t), intent(inout) :: self
      real(kind=dp), intent(in) :: timestep

      ! TODO: Implement precice advace and stuff including possible read/write, max timestep checking etc.
      call precicef_advance(timestep)
   end subroutine precice_adapter_update

   subroutine precice_adapter_finalize(self)
      implicit none(type, external)
      class(precice_adapter_t), intent(inout) :: self
      if (allocated(self%mesh_coordinates)) then
         deallocate (self%mesh_coordinates)
      end if
      call precicef_finalize()
   end subroutine precice_adapter_finalize

end module precice_adapter
