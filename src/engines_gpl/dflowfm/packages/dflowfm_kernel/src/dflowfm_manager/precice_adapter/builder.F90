module precice_adapter_builder
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precice_adapter, only: precice_adapter_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   public :: precice_adapter_builder_t

   type :: precice_adapter_builder_t
      character(kind=c_char, len=250) :: config_file ! preCICE XML config file path
      character(kind=c_char, len=20) :: name ! Participant name.
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: number_of_ranks = 1_c_int
      integer(kind=c_int) :: communicator = 0_c_int
      logical :: is_communicator_set = .false.
      character(kind=c_char, len=20) :: mesh_name ! mesh name
      integer(kind=c_int) :: mesh_size = 0_c_int ! mesh size (number of points): N
      real(kind=c_double), dimension(:), allocatable :: mesh_coordinates ! mesh coordinates: x1,y1,x2,y2,...,xN,yN
   contains
      procedure :: set_config_file => builder_set_config_file
      procedure :: set_name => builder_set_name
      procedure :: set_mpi_rank => builder_set_mpi_rank
      procedure :: set_mpi_communicator => builder_set_mpi_communicator
      procedure :: add_mesh_2d => builder_add_mesh_2d
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

   subroutine builder_set_config_file(self, config_file)
      class(precice_adapter_builder_t), intent(inout) :: self
      character(kind=c_char, len=*), intent(in) :: config_file

      self%config_file = config_file
   end subroutine builder_set_config_file

   subroutine builder_set_name(self, name)
      class(precice_adapter_builder_t), intent(inout) :: self
      character(kind=c_char, len=*), intent(in) :: name

      self%name = name
   end subroutine builder_set_name

   subroutine builder_set_mpi_rank(self, my_rank, number_of_ranks)
      class(precice_adapter_builder_t), intent(inout) :: self
      integer(kind=c_int), intent(in) :: my_rank
      integer(kind=c_int), intent(in) :: number_of_ranks

      self%is_communicator_set = .true.
      self%my_rank = my_rank
      self%number_of_ranks = number_of_ranks
   end subroutine builder_set_mpi_rank

   subroutine builder_set_mpi_communicator(self, communicator)
      class(precice_adapter_builder_t), intent(inout) :: self
      integer(kind=c_int), intent(in) :: communicator

      self%is_communicator_set = .true.
      self%communicator = communicator
   end subroutine builder_set_mpi_communicator

   subroutine builder_add_mesh_2d(self, mesh_name, mesh_size, mesh_coordinates_x, mesh_coordinates_y)
      use precision, only: dp
      class(precice_adapter_builder_t), intent(inout) :: self
      character(len=*) :: mesh_name
      integer(kind=c_int), intent(in) :: mesh_size
      real(kind=c_double), dimension(:), intent(in) :: mesh_coordinates_x
      real(kind=c_double), dimension(:), intent(in) :: mesh_coordinates_y
      ! Local variables
      integer :: i

      self%mesh_name = mesh_name
      self%mesh_size = mesh_size

      allocate (self%mesh_coordinates(mesh_size * 2))

      do i = 1, mesh_size
         self%mesh_coordinates(2 * i - 1) = mesh_coordinates_x(i)
         self%mesh_coordinates(2 * i) = mesh_coordinates_y(i)
      end do

   end subroutine builder_add_mesh_2d

   function builder_build(self) result(adapter_instance)
      class(precice_adapter_builder_t), intent(inout) :: self
      type(precice_adapter_t), pointer :: adapter_instance

      adapter_instance => precice_adapter_t(self%config_file, self%name, self%is_communicator_set, self%communicator, &
                                            self%my_rank, self%number_of_ranks, self%mesh_name, &
                                            self%mesh_size, self%mesh_coordinates)
   end function builder_build

end module precice_adapter_builder
