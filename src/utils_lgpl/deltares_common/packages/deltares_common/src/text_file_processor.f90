module text_file_processor
   use messageHandling, only: warn_flush, err_flush, msgbuf, LEVEL_FATAL, LEVEL_ERROR, LEVEL_WARN, LEVEL_INFO
   use properties
   use tree_data_types
   use string_module, only: str_tolower

   implicit none

   type :: TextFileProcessor
      character(len=:), allocatable :: filename
      logical :: is_error = .false.
      type(tree_data), pointer   :: tree
   contains
      procedure :: init => text_file_processor_init
      procedure :: parse => text_file_processor_parse
   end type TextFileProcessor

   interface TextFileProcessor
      module procedure :: text_file_processor_constructor
   end interface TextFileProcessor

   !> Abstract interface for verification
   type, abstract :: TextFileProcessorVerifier
   contains
      procedure(verify_interface), deferred :: verify
   end type TextFileProcessorVerifier

   abstract interface
      function verify_interface(this, processor) result(is_valid)
         import TextFileProcessorVerifier, TextFileProcessor
         class(TextFileProcessorVerifier), intent(in) :: this
         type(TextFileProcessor), intent(in) :: processor
         logical :: is_valid
      end function verify_interface
   end interface

   !> String array verifier implementation
   type, extends(TextFileProcessorVerifier) :: ChapterPropsVerifier
      character(len=:), allocatable :: required_props(:)
      character(len=:), allocatable :: chapter_name
   contains
      procedure :: verify => string_array_verifier_verify
   end type ChapterPropsVerifier

   type, extends(TextFileProcessorVerifier) :: AndVerifier
      class(TextFileProcessorVerifier), allocatable :: verifiers(:)
   contains
      procedure :: verify => and_verifier_verify
   end type AndVerifier 

   type, extends(TextFileProcessorVerifier) :: ArraysLengthVerifier
      character(len=:), allocatable :: property_names(:)
      character(len=:), allocatable :: chapter_name
      character(len=:), allocatable :: expected_length
      logical :: check_specific_length
   contains
      procedure :: verify => arrays_length_verifier_verify
   end type ArraysLengthVerifier

   interface ChapterPropsVerifier
      module procedure :: string_array_verifier_constructor
   end interface ChapterPropsVerifier

   interface AndVerifier
      module procedure :: and_verifier_constructor
   end interface AndVerifier

   interface ArraysLengthVerifier
      module procedure :: arrays_length_verifier_constructor
   end interface ArraysLengthVerifier

contains

   !> Constructor for TextFileProcessor
   function text_file_processor_constructor(filename) result(processor)
      character(len=*), intent(in) :: filename
      type(TextFileProcessor) :: processor

      processor%filename = filename
   end function text_file_processor_constructor

   !> Constructor for ChapterPropsVerifier
   function string_array_verifier_constructor(chapter_name, strings) result(verifier)
      character(len=*), intent(in) :: chapter_name
      character(len=*), intent(in) :: strings(:)
      type(ChapterPropsVerifier) :: verifier
      integer :: n

      ! Allocate from source to preserve deferred character length
      allocate(verifier%required_props, source=strings)
      verifier%chapter_name = chapter_name
   end function string_array_verifier_constructor

   !> Constructor for AndVerifier
   function and_verifier_constructor(verifiers) result(verifier)
      class(TextFileProcessorVerifier), intent(in) :: verifiers(:)
      type(AndVerifier) :: verifier
      
      allocate(verifier%verifiers, source=verifiers)
   end function and_verifier_constructor

   !> Constructor for ArraysLengthVerifier
   function arrays_length_verifier_constructor(chapter_name, property_names, expected_length) result(verifier)
      character(len=*), intent(in) :: chapter_name
      character(len=*), intent(in) :: property_names(:)
      character(len=*), intent(in), optional :: expected_length
      type(ArraysLengthVerifier) :: verifier
      
      allocate(verifier%property_names, source=property_names)
      verifier%chapter_name = chapter_name
      
      if (present(expected_length)) then
         verifier%expected_length = expected_length
         verifier%check_specific_length = .true.
      else
         verifier%check_specific_length = .false.
      end if
   end function arrays_length_verifier_constructor

   !> Initialize method
   subroutine text_file_processor_init(this)
      class(TextFileProcessor), intent(inout) :: this
      logical :: file_exists

      ! Check if the file exists
      inquire (file=this%filename, exist=file_exists)

      if (.not. file_exists) then
         write (msgbuf, '(a,a,a)') 'File does not exist: ', trim(this%filename), '.'
         this%is_error = .true.
      end if

   end subroutine text_file_processor_init

   !> Parse method
   subroutine text_file_processor_parse(this)
      class(TextFileProcessor), intent(inout) :: this
      integer :: istat

      call prop_inifile(this%filename, this%tree, istat)
      ! Parse the file
      ! Add parsing logic here
      if (istat /= 0) then
         write (msgbuf, '(a,a,a)') 'Error parsing file: ', trim(this%filename), '.'
         this%is_error = .true.
      end if

   end subroutine text_file_processor_parse

   !> String array verifier implementation
   function string_array_verifier_verify(this, processor) result(is_valid)
      class(ChapterPropsVerifier), intent(in) :: this
      type(TextFileProcessor), intent(in) :: processor
      logical :: is_valid
      integer :: i, j
      integer :: num_items_in_file
      type(tree_data), pointer :: block_ptr
      character(len=:), allocatable :: group_name

      character(len=:), allocatable :: value
      logical :: found

      ! Check if processor is valid first
      is_valid = .not. processor%is_error

      num_items_in_file = tree_num_nodes(processor%tree)



      ! If valid, check required strings
      if (is_valid .and. allocated(this%required_props)) then
         do i = 1, num_items_in_file
            block_ptr => processor%tree%child_nodes(i)%node_ptr
            group_name = trim(tree_get_name(block_ptr))
            if (group_name == trim(this%chapter_name)) then
               do j = 1, size(this%required_props)
                  call prop_get_alloc_string(block_ptr, this%chapter_name, this%required_props(j), value, found)
                  ! print *, 'Verifying presence of string: ', trim(this%required_props(j))
                  if (.not. found) then
                     write (msgbuf, '(a,a,a)') 'Missing required property: ', trim(this%required_props(j)), '.'
                     is_valid = .false.
                  else if (allocated(value)) then
                     DEALLOCATE(value)
                  end if
                  ! For now, just return true
               end do
            end if
         end do
      end if

   end function string_array_verifier_verify

   !> AndVerifier implementation - verifies all sub-verifiers
   function and_verifier_verify(this, processor) result(is_valid)
      class(AndVerifier), intent(in) :: this
      type(TextFileProcessor), intent(in) :: processor
      logical :: is_valid
      integer :: i

      is_valid = .true.

      ! Call all verifiers and fail if any one fails
      if (allocated(this%verifiers)) then
         do i = 1, size(this%verifiers)
            if (.not. this%verifiers(i)%verify(processor)) then
               is_valid = .false.
               exit  ! Stop on first failure
            end if
         end do
      end if

   end function and_verifier_verify

   !> ArraysLengthVerifier implementation - verifies all specified arrays have consistent length
   function arrays_length_verifier_verify(this, processor) result(is_valid)
      class(ArraysLengthVerifier), intent(in) :: this
      type(TextFileProcessor), intent(in) :: processor
      logical :: is_valid
      integer :: i, j
      integer :: num_items_in_file
      integer :: first_length, current_length
      type(tree_data), pointer :: block_ptr
      character(len=:), allocatable :: group_name
      logical :: found
      logical :: anything_found
      integer :: num_values
      character(len=:), allocatable :: value
      integer :: read_expected_length
      
      is_valid = .not. processor%is_error
      if (.not. is_valid) return
      
      num_items_in_file = tree_num_nodes(processor%tree)
      first_length = -1
      
      ! Find the chapter and check array lengths
      if (allocated(this%property_names)) then
         do i = 1, num_items_in_file
            block_ptr => processor%tree%child_nodes(i)%node_ptr
            group_name = trim(tree_get_name(block_ptr))
            
            if (trim(adjustl(str_tolower(group_name))) == trim(adjustl(str_tolower(this%chapter_name)))) then
               if (this%check_specific_length) then
                  read_expected_length = 0
                  call prop_get(block_ptr, '', this%expected_length, read_expected_length, found)
                  if (.not. found) then
                     write (msgbuf, '(a,a,a)') 'Expected length property not found: ', trim(this%expected_length), '.'
                     is_valid = .false.
                     return
                  end if
               end if
               do j = 1, size(this%property_names)
                  ! Get the number of values for this property
                  call prop_get_alloc_string(block_ptr, '', this%property_names(j), value, found)
                  if (found ) then
                     anything_found = .true.
                     num_values = count_string_elements(value)
                     DEALLOCATE(value)

                     if (num_values <= 0) then
                        write (msgbuf, '(a,a,a)') 'Property empty: ', trim(this%property_names(j)), '.'
                        is_valid = .false.
                        return
                     end if
                     
                     current_length = num_values
                     
                     ! Store first length or compare against it
                     if (first_length == -1) then
                        first_length = current_length
                     else if (current_length /= first_length) then
                        write (msgbuf, '(a,a,a,i0,a,i0,a)') 'Array length mismatch for property: ', &
                           trim(this%property_names(j)), '. Expected ', first_length, ' but got ', current_length, '.'
                        is_valid = .false.
                        return
                     end if

                  end if
                  
               end do
               
               ! If checking for specific length, verify it
               if (anything_found .and. this%check_specific_length .and. first_length /= read_expected_length) then
                  write (msgbuf, '(a,i0,a,i0,a)') 'Array length mismatch. Expected ', &
                       read_expected_length, ' but got ', first_length, '.'
                  is_valid = .false.
                  return
               end if
               
               exit  ! Found the chapter, done checking
            end if
         end do
      end if
      
   end function arrays_length_verifier_verify

   !> Count the number of space/tab-separated elements in a string
   function count_string_elements(input_string) result(count)
      character(len=*), intent(in) :: input_string
      integer :: count
      integer :: i, len_str
      logical :: in_element

      count = 0
      in_element = .false.
      len_str = len_trim(input_string)

      do i = 1, len_str
         if (input_string(i:i) /= ' ' .and. input_string(i:i) /= char(9)) then
            if (.not. in_element) then
               count = count + 1
               in_element = .true.
            end if
         else
            in_element = .false.
         end if
      end do

   end function count_string_elements

end module text_file_processor
