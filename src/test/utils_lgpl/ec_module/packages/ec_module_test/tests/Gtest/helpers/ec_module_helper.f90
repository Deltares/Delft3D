module ec_module_helper
   use m_ec_connection, only: ecConnectionAddSourceItem, ecConnectionAddTargetItem, ecConnectionSetConverter
   use m_ec_field, only: ecFieldCreate1dArray
   use m_ec_instance, only: ecInstanceCreate, ecInstanceFree, ecInstanceCreateConnection, ecInstanceCreateConverter, &
                            ecInstanceCreateElementSet, ecInstanceCreateField, ecInstanceCreateItem, ecInstanceCreateQuantity
   use m_ec_parameters, only: convType_spiderweb, EC_OPERAND_REPLACE, elmSetType_spheric, itemType_source, itemType_target
   use m_ec_support, only: ecSupportFindConnection, ecSupportFindConverter, ecSupportFindElementSet, &
                           ecSupportFindField, ecSupportFindItem, ecSupportFindQuantity
   use m_ec_typedefs, only: tEcInstance, tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem, tEcQuantity
   use precision, only: dp
   use precision_basics, only: comparereal
   implicit none(type, external)
   private

   public :: ec_connection_builder, spiderweb_data

   !> Contains spiderweb geometry and source data.
   !! Beware: The second dimension of `data` should be ordered in the same way as the source quantities are registered.
   type spiderweb_data
      integer :: n_rows !< The number of "rows" (fixed-width circular shells around the eye).
      integer :: n_cols !< The number of "columns" (wedges of the circle with uniform angle).
      real(dp) :: radius !< Outer radius in metres.
      real(dp), dimension(2) :: eye !< Longitude and latitude in degrees.
      real(dp), dimension(:, :), allocatable :: data !< Flattened grid points by source quantity. The order 
   end type spiderweb_data

   !> Builder that initializes the EC-module and does all the wiring required to call `ecConverterPerformConversions`.
   type :: ec_connection_builder
      private
      type(tEcInstance), pointer :: instance => null()
      character(len=:), allocatable :: error_message
   contains
      procedure :: build_spiderweb_connection
      procedure :: get_error_message
      procedure, private :: check_spiderweb_input
      procedure, private :: add_quantity
      procedure, private :: add_item
      procedure, private :: add_source_item
      procedure, private :: add_target_item
      procedure, private :: add_field
      procedure, private :: add_connection
      procedure, private :: add_converter
      procedure, private :: add_element_set
      procedure, private :: create_instance
      final :: clear_instance
   end type ec_connection_builder

contains

   !> Build a connection with one target point and a spiderweb converter.
   !! On failure, return null and store a diagnostic.
   function build_spiderweb_connection( &
         self, source_names, target_name, target_xy, t0, t1, &
         spw_t0, spw_t1, initial_values) result(connection)
      class(ec_connection_builder), intent(inout) :: self
      character(len=*), dimension(:), intent(in) :: source_names
      character(len=*), intent(in) :: target_name
      real(dp), dimension(2), intent(in) :: target_xy
      real(dp), intent(in) :: t0, t1 !< Source times as Modified Julian Dates.
      type(spiderweb_data), intent(in) :: spw_t0, spw_t1
      real(dp), dimension(:), intent(in) :: initial_values !< One initial value per target item.
      type(tEcConnection), pointer :: connection, candidate
      type(tEcConverter), pointer :: converter
      type(tEcElementSet), pointer :: source_set, target_set
      type(tEcQuantity), pointer :: quantity
      type(tEcField), pointer :: field_t0, field_t1, target_field
      integer :: i, n_grid_points

      nullify (connection)
      self%error_message = ''
      call clear_instance(self)
      if (len(self%error_message) /= 0) then
         return
      end if

      call self%check_spiderweb_input(spw_t0, spw_t1, size(source_names), size(initial_values))
      if (len(self%error_message) /= 0) then
         return
      end if

      ! Create the EC instance.
      call self%create_instance()
      if (len(self%error_message) /= 0) then
         return
      end if

      ! Create connection and add converter
      converter => self%add_converter(converter_type=convType_spiderweb, operand_type=EC_OPERAND_REPLACE)
      candidate => self%add_connection(converter)
      if (.not. associated(candidate)) then
         self%error_message = 'could not create the spiderweb connection'
         return
      end if
      source_set => self%add_element_set(n_rows=spw_t0%n_rows, n_cols=spw_t0%n_cols, radius=spw_t0%radius)
      target_set => self%add_element_set(ofType=elmSetType_spheric, target_xy=target_xy)

      ! Add source items
      n_grid_points = spw_t0%n_rows * spw_t0%n_cols
      do i = 1, size(source_names)
         quantity => self%add_quantity(source_names(i))
         field_t0 => self%add_field(n_grid_points)
         field_t1 => self%add_field(n_grid_points)
         call set_spiderweb_field(field_t0, spw_t0, t0, i)
         call set_spiderweb_field(field_t1, spw_t1, t1, i)
         call self%add_source_item(candidate, quantity, source_set, field_t0, field_t1)
         if (len(self%error_message) /= 0) then
            return
         end if
      end do

      ! Add target items
      quantity => self%add_quantity(target_name)
      do i = 1, size(initial_values)
         target_field => self%add_field(1)
         target_field%arr1dPtr(1) = initial_values(i)
         call self%add_target_item(candidate, quantity, target_set, target_field)
         if (len(self%error_message) /= 0) then
            return
         end if
      end do
      connection => candidate
   end function build_spiderweb_connection

   !> Set the builder's diagnostic if the snapshots or item counts are incompatible.
   pure subroutine check_spiderweb_input(self, spw_t0, spw_t1, n_sources, n_targets)
      class(ec_connection_builder), intent(inout) :: self
      type(spiderweb_data), intent(in) :: spw_t0, spw_t1
      integer, intent(in) :: n_sources, n_targets
      integer, dimension(2) :: source_shape
      integer :: n_rows, n_cols

      n_rows = spw_t0%n_rows
      n_cols = spw_t0%n_cols

      self%error_message = ''
      if (n_rows < 2 .or. n_cols < 2 .or. n_targets < 1 .or. n_sources < 1) then
         self%error_message = 'spiderweb wiring requires at least two rows and columns, and sources and targets'
         return
      end if
      if (n_rows /= spw_t1%n_rows .or. n_cols /= spw_t1%n_cols) then
         self%error_message = 'spiderweb snapshots must have matching rows and columns'
         return
      end if
      source_shape = [n_rows * n_cols, n_sources]
      if (any(shape(spw_t0%data) /= source_shape) .or. any(shape(spw_t1%data) /= source_shape)) then
         self%error_message = 'spiderweb source snapshots must match the grid and source count'
         return
      end if
      if (spw_t0%radius <= 0.0_dp .or. spw_t1%radius <= 0.0_dp .or. comparereal(spw_t0%radius, spw_t1%radius) /= 0) then
         self%error_message = 'spiderweb snapshot radii must be positive and match'
         return
      end if
   end subroutine check_spiderweb_input

   !> Populate the `tEcField` with spiderweb data.
   subroutine set_spiderweb_field(field, spw, time, source_index)
      type(tEcField), intent(inout) :: field
      type(spiderweb_data), intent(in) :: spw
      real(dp), intent(in) :: time
      integer, intent(in) :: source_index

      field%timesteps = time
      field%x_spw_eye = spw%eye(1)
      field%y_spw_eye = spw%eye(2)
      field%arr1dPtr(:) = spw%data(:, source_index)
   end subroutine set_spiderweb_field

   !> Return the error message, or an empty string if not set.
   pure function get_error_message(self) result(message)
      class(ec_connection_builder), intent(in) :: self
      character(len=:), allocatable :: message

      message = ''
      if (allocated(self%error_message)) then
         message = self%error_message
      end if
   end function get_error_message

   !> Create the ec_module instance.
   subroutine create_instance(self)
      class(ec_connection_builder), intent(inout) :: self

      if (.not. ecInstanceCreate(self%instance)) then
         self%error_message = 'could not create the EC test instance'
      end if
   end subroutine create_instance

   !> Create and add a quantity.
   function add_quantity(self, name) result(quantity)
      class(ec_connection_builder), intent(inout) :: self
      character(len=*), intent(in) :: name
      type(tEcQuantity), pointer :: quantity
      integer :: id

      id = ecInstanceCreateQuantity(self%instance)
      quantity => ecSupportFindQuantity(self%instance, id)
      quantity%name = name
   end function add_quantity

   !> Create and add an item with the given quantity and element set.
   function add_item(self, quantity, element_set, role) result(item)
      class(ec_connection_builder), intent(inout) :: self
      type(tEcQuantity), pointer, intent(in) :: quantity
      type(tEcElementSet), pointer, intent(in) :: element_set
      integer, intent(in) :: role
      type(tEcItem), pointer :: item
      integer :: id

      id = ecInstanceCreateItem(self%instance)
      item => ecSupportFindItem(self%instance, id)
      if (associated(item)) then
         item%quantityPtr => quantity
         item%elementSetPtr => element_set
         item%role = role
      end if
   end function add_item

   !> Create and add an item to existing source fields and attach it to the connection.
   subroutine add_source_item(self, connection, quantity, element_set, field_t0, field_t1)
      class(ec_connection_builder), intent(inout) :: self
      type(tEcConnection), pointer, intent(in) :: connection
      type(tEcQuantity), pointer, intent(in) :: quantity
      type(tEcElementSet), pointer, intent(in) :: element_set
      type(tEcField), pointer, intent(in) :: field_t0, field_t1
      type(tEcItem), pointer :: item

      item => self%add_item(quantity, element_set, itemType_source)
      item%sourceT0FieldPtr => field_t0
      item%sourceT1FieldPtr => field_t1
      if (.not. ecConnectionAddSourceItem(self%instance, connection%id, item%id)) then
         self%error_message = 'could not attach the source item'
      end if
   end subroutine add_source_item

   !> Create and add an item to an existing target field and attach it to the connection.
   subroutine add_target_item(self, connection, quantity, element_set, target_field)
      class(ec_connection_builder), intent(inout) :: self
      type(tEcConnection), pointer, intent(in) :: connection
      type(tEcQuantity), pointer, intent(in) :: quantity
      type(tEcElementSet), pointer, intent(in) :: element_set
      type(tEcField), pointer, intent(in) :: target_field
      type(tEcItem), pointer :: item

      item => self%add_item(quantity, element_set, itemType_target)
      item%targetFieldPtr => target_field
      if (.not. ecConnectionAddTargetItem(self%instance, connection%id, item%id)) then
         self%error_message = 'could not attach the target item'
      end if
   end subroutine add_target_item

   !> Create and add a field and allocate its storage.
   function add_field(self, n_values) result(field)
      class(ec_connection_builder), intent(inout) :: self
      integer, intent(in) :: n_values
      type(tEcField), pointer :: field
      integer :: id

      nullify (field)
      id = ecInstanceCreateField(self%instance)
      if (.not. ecFieldCreate1dArray(self%instance, id, n_values)) then
         return
      end if
      field => ecSupportFindField(self%instance, id)
   end function add_field

   !> Create and add a connection with the requested converter. Return null on failure.
   function add_connection(self, converter) result(connection)
      class(ec_connection_builder), intent(inout) :: self
      type(tEcConverter), pointer, intent(in) :: converter
      type(tEcConnection), pointer :: connection
      integer :: id

      nullify (connection)
      if (.not. associated(converter)) then
         return
      end if
      if (.not. associated(ecSupportFindConverter(self%instance, converter%id), converter)) then
         return
      end if
      id = ecInstanceCreateConnection(self%instance)
      connection => ecSupportFindConnection(self%instance, id)
      if (.not. associated(connection)) then
         return
      end if
      if (.not. ecConnectionSetConverter(self%instance, connection%id, converter%id)) then
         nullify (connection)
      end if
   end function add_connection

   !> Create and add a converter with the given converter and operand types.
   function add_converter(self, converter_type, operand_type) result(converter)
      class(ec_connection_builder), intent(inout) :: self
      integer, intent(in) :: converter_type, operand_type
      type(tEcConverter), pointer :: converter
      integer :: id

      id = ecInstanceCreateConverter(self%instance)
      converter => ecSupportFindConverter(self%instance, id)
      if (associated(converter)) then
         converter%ofType = converter_type
         converter%operandType = operand_type
      end if
   end function add_converter

   !> Create and add an element set, optionally setting a few of its attributes.
   function add_element_set(self, n_rows, n_cols, ofType, radius, target_xy) result(element_set)
      class(ec_connection_builder), intent(inout) :: self
      integer, intent(in), optional :: n_rows, n_cols, ofType
      real(dp), intent(in), optional :: radius
      real(dp), dimension(2), intent(in), optional :: target_xy
      type(tEcElementSet), pointer :: element_set
      integer :: id

      id = ecInstanceCreateElementSet(self%instance)
      element_set => ecSupportFindElementSet(self%instance, id)
      if (present(n_rows)) then
         element_set%n_rows = n_rows
      end if
      if (present(n_cols)) then
         element_set%n_cols = n_cols
      end if
      if (present(ofType)) then
         element_set%ofType = ofType
      end if
      if (present(radius)) then
         element_set%radius = radius
      end if
      if (present(target_xy)) then
         element_set%nCoordinates = 1
         allocate (element_set%x(1), element_set%y(1))
         element_set%x(1) = target_xy(1)
         element_set%y(1) = target_xy(2)
      end if
   end function add_element_set

   !> Destructor: Free the EC test instance.
   subroutine clear_instance(self)
      type(ec_connection_builder), intent(inout) :: self

      if (associated(self%instance)) then
         if (.not. ecInstanceFree(self%instance)) then
            self%error_message = 'could not free the EC test instance'
         end if
      end if
   end subroutine clear_instance

end module ec_module_helper