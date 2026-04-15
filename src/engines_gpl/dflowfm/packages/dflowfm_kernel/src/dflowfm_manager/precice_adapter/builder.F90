module precice_adapter_builder
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precice_adapter, only: precice_adapter_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double

   implicit none(type, external)

   private
   public :: precice_adapter_builder_t

   type :: precice_adapter_builder_t
      character(kind=c_char, len=:), allocatable :: config_file ! preCICE XML config file path
      character(kind=c_char, len=:), allocatable :: name ! Participant name.
      integer(kind=c_int) :: my_rank = 0_c_int
      integer(kind=c_int) :: number_of_ranks = 1_c_int
      integer(kind=c_int) :: communicator = 0_c_int
      logical :: is_communicator_set = .false.
      character(kind=c_char, len=:), allocatable :: cell_center_mesh_name ! mesh name
      integer(kind=c_int) :: cell_center_mesh_size = 0_c_int ! mesh size (number of points): N
      real(kind=c_double), dimension(:), allocatable :: cell_center_mesh_coordinates_2d ! mesh coordinates: x1,y1,x2,y2,...,xN,yN
   contains
      procedure :: set_config_file => builder_set_config_file
      procedure :: set_name => builder_set_name
      procedure :: set_mpi_rank => builder_set_mpi_rank
      procedure :: set_mpi_communicator => builder_set_mpi_communicator
      procedure :: set_cell_center_mesh_2d => builder_set_cell_center_mesh_2d
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

   subroutine builder_set_cell_center_mesh_2d(self, cell_center_mesh_name, cell_center_mesh_size, cell_center_mesh_coordinates_2d_x, cell_center_mesh_coordinates_2d_y)
      use precision, only: dp
      class(precice_adapter_builder_t), intent(inout) :: self
      character(len=*) :: cell_center_mesh_name
      integer(kind=c_int), intent(in) :: cell_center_mesh_size
      real(kind=c_double), dimension(:), intent(in) :: cell_center_mesh_coordinates_2d_x
      real(kind=c_double), dimension(:), intent(in) :: cell_center_mesh_coordinates_2d_y
      ! Local variables
      integer :: i

      self%cell_center_mesh_name = cell_center_mesh_name
      self%cell_center_mesh_size = cell_center_mesh_size

      if (allocated(self%cell_center_mesh_coordinates_2d)) then
         deallocate(self%cell_center_mesh_coordinates_2d)
      end if
      allocate (self%cell_center_mesh_coordinates_2d(cell_center_mesh_size * 2))

      do i = 1, cell_center_mesh_size
         self%cell_center_mesh_coordinates_2d(2 * i - 1) = cell_center_mesh_coordinates_2d_x(i)
         self%cell_center_mesh_coordinates_2d(2 * i) = cell_center_mesh_coordinates_2d_y(i)
      end do

   end subroutine builder_set_cell_center_mesh_2d

   function builder_build(self) result(adapter_instance)
      class(precice_adapter_builder_t), intent(inout) :: self
      type(precice_adapter_t), pointer :: adapter_instance

      adapter_instance => precice_adapter_t(self%config_file, self%name, self%is_communicator_set, self%communicator, &
                                            self%my_rank, self%number_of_ranks, self%cell_center_mesh_name, &
                                            self%cell_center_mesh_size, self%cell_center_mesh_coordinates_2d)
   end function builder_build

end module precice_adapter_builder
