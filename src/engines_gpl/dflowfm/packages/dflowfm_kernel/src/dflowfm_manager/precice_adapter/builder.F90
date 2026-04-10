module precice_adapter_builder
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precice_adapter, only: precice_adapter_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   public :: precice_adapter_builder_t

   type :: precice_adapter_builder_t
      character(kind=c_char, len=250) :: configfile ! preCICE XML config file path
      character(kind=c_char, len=20) :: name ! Participant name.
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: numranks = 1_c_int
      integer(kind=c_int) :: comm = 0_c_int
      logical :: is_comm_set = .false.
      character(kind=c_char, len=20) :: mesh_name ! mesh name
      integer(kind=c_int) :: mesh_size = 0_c_int ! mesh size (number of points)
      real(kind=c_double), dimension(:), pointer :: mesh_coordinates_x ! mesh X coordinates
      real(kind=c_double), dimension(:), pointer :: mesh_coordinates_y ! mesh Y coordinates
   contains
      procedure :: set_configfile => builder_set_configfile
      procedure :: set_name => builder_set_name
      procedure :: set_mpi_rank => builder_set_mpi_rank
      procedure :: set_mpi_comm => builder_set_mpi_comm
      procedure :: add_mesh2d => builder_add_mesh_2d
      procedure :: build => builder_build
   end type

   interface precice_adapter_builder_t
      procedure :: precice_adapter_builder_constructor
   end interface

contains

   function precice_adapter_builder_constructor() result(builder_instance)
      type(precice_adapter_builder_t), pointer :: builder_instance

      allocate (builder_instance)
   end function precice_adapter_builder_constructor

   subroutine builder_destructor(self)
      class(precice_adapter_builder_t), intent(inout) :: self
      deallocate (self%mesh_coordinates_x)
      deallocate (self%mesh_coordinates_y)
   end subroutine builder_destructor

   subroutine builder_set_configfile(self, configfile)
      class(precice_adapter_builder_t), intent(inout) :: self
      character(kind=c_char, len=*), intent(in) :: configfile

      self%configfile = configfile
   end subroutine builder_set_configfile

   subroutine builder_set_name(self, name)
      class(precice_adapter_builder_t), intent(inout) :: self
      character(kind=c_char, len=*), intent(in) :: name

      self%name = name
   end subroutine builder_set_name

   subroutine builder_set_mpi_rank(self, my_rank, numranks)
      class(precice_adapter_builder_t), intent(inout) :: self
      integer(kind=c_int), intent(in) :: my_rank
      integer(kind=c_int), intent(in) :: numranks

      self%is_comm_set = .true.
      self%my_rank = my_rank
      self%numranks = numranks
   end subroutine builder_set_mpi_rank

   subroutine builder_set_mpi_comm(self, comm)
      class(precice_adapter_builder_t), intent(inout) :: self
      integer(kind=c_int), intent(in) :: comm

      self%is_comm_set = .true.
      self%comm = comm
   end subroutine builder_set_mpi_comm

   subroutine builder_add_mesh_2d(self, mesh_name, mesh_size, mesh_coordinates_x, mesh_coordinates_y)
      use precision, only: dp
      class(precice_adapter_builder_t), intent(inout) :: self
      character(len=*) :: mesh_name
      integer(kind=c_int), intent(in) :: mesh_size
      real(kind=c_double), dimension(:), intent(in), pointer :: mesh_coordinates_x
      real(kind=c_double), dimension(:), intent(in), pointer :: mesh_coordinates_y

      self%mesh_name = mesh_name
      self%mesh_size = mesh_size
      self%mesh_coordinates_x => mesh_coordinates_x
      self%mesh_coordinates_y => mesh_coordinates_y
   end subroutine builder_add_mesh_2d

   function builder_build(self) result(adapter_instance)
      class(precice_adapter_builder_t), intent(inout) :: self
      type(precice_adapter_t), pointer :: adapter_instance

      adapter_instance => precice_adapter_t(self%configfile, self%name, self%is_comm_set, self%comm, &
                                            self%my_rank, self%numranks, self%mesh_name, &
                                            self%mesh_size, self%mesh_coordinates_x, self%mesh_coordinates_y)
   end function builder_build

end module precice_adapter_builder
