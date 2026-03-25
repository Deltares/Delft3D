module m_text_file_validators
   use precision
   use messageHandling, only: warn_flush, err_flush, msgbuf, LEVEL_FATAL, LEVEL_ERROR, LEVEL_WARN, LEVEL_INFO
   use tree_data_types, only: tree_data
   use tree_structures, only: tree_num_nodes, tree_get_name
   use m_text_file_processor, only: TextFileProcessor
   use string_module, only: str_tolower
   use properties, only: prop_get, prop_get_alloc_string

   implicit none

   !> Abstract interface for verification
   type, abstract :: TextFileProcessorVerifier
   contains
      procedure(verify_interface), deferred :: verify
   end type TextFileProcessorVerifier

   !> Wrapper type for polymorphic verifier pointers
   type :: VerifierPtr
      class(TextFileProcessorVerifier), allocatable :: ptr
   end type VerifierPtr

   !> Abstract interface for chapter-level verification (operates on tree_data pointer for the chapter)
   type, abstract :: ChapterVerifier
   contains
      procedure(verify_chapter_interface), deferred :: verify
   end type ChapterVerifier

   abstract interface
      function verify_interface(this, processor) result(is_valid)
         import TextFileProcessorVerifier, TextFileProcessor
         class(TextFileProcessorVerifier), intent(in) :: this
         type(TextFileProcessor), intent(in) :: processor
         logical :: is_valid
      end function verify_interface

      function verify_chapter_interface(this, block_ptr) result(is_valid)
         import ChapterVerifier, TextFileProcessor, tree_data
         class(ChapterVerifier), intent(in) :: this
         type(tree_data), pointer, intent(in) :: block_ptr
         logical :: is_valid
      end function verify_chapter_interface
   end interface

   !> String array verifier implementation
   type, extends(TextFileProcessorVerifier) :: PropertiesVerifier
      character(len=:), allocatable :: chapter_name
      class(ChapterPropertiesVerifier), allocatable :: chapter_verifier
   contains
      procedure :: verify => properties_verifier_verify
   end type PropertiesVerifier

   interface PropertiesVerifier
      module procedure :: properties_verifier_constructor
   end interface PropertiesVerifier

   type, extends(ChapterVerifier) :: ChapterPropertiesVerifier
      character(len=:), allocatable :: required_props(:)
   contains
      procedure :: verify => chapter_properties_verifier_verify
   end type ChapterPropertiesVerifier

   !> Check if one of more arrays have the same length, and optionally check against expected length
   !> Chapter level
   type, extends(ChapterVerifier) :: ArraysLengthChapterVerifier
      character(len=:), allocatable :: property_names(:)
      character(len=:), allocatable :: expected_length
      logical :: check_specific_length
   contains
      procedure :: verify => arrays_length_chapter_verifier_verify
   end type ArraysLengthChapterVerifier

   interface ArraysLengthChapterVerifier
      module procedure :: arrays_length_chapter_verifier_constructor
   end interface ArraysLengthChapterVerifier

   !> Check if one of more arrays have the same length, and optionally check against expected length
   !> File Level
   type, extends(TextFileProcessorVerifier) :: ArraysLengthVerifier
      character(len=:), allocatable :: chapter_name
      class(ArraysLengthChapterVerifier), allocatable :: chapter_verifier
   contains
      procedure :: verify => arrays_length_verifier_verify
   end type ArraysLengthVerifier

   interface ArraysLengthVerifier
      module procedure :: arrays_length_verifier_constructor
   end interface ArraysLengthVerifier

   !> Combines multiple verifiers and requires all to pass
   type, extends(TextFileProcessorVerifier) :: AndVerifier
      type(VerifierPtr), allocatable :: verifiers(:)
   contains
      procedure :: verify => and_verifier_verify
   end type AndVerifier


contains

   function properties_verifier_constructor(chapter_name, strings) result(verifier)
      character(len=*), intent(in) :: chapter_name
      character(len=*), intent(in) :: strings(:)
      type(PropertiesVerifier) :: verifier
      integer :: n

      verifier%chapter_name = chapter_name
      verifier%chapter_verifier = ChapterPropertiesVerifier(strings)
   end function properties_verifier_constructor

   function arrays_length_chapter_verifier_constructor(property_names, expected_length) result(verifier)
      character(len=*), intent(in) :: property_names(:)
      character(len=*), intent(in), optional :: expected_length
      type(ArraysLengthChapterVerifier) :: verifier

      allocate (verifier%property_names, source=property_names)
      if (present(expected_length)) then
         verifier%expected_length = expected_length
         verifier%check_specific_length = .true.
      else
         verifier%check_specific_length = .false.
      end if
   end function arrays_length_chapter_verifier_constructor

   function arrays_length_verifier_constructor(chapter_name, property_names, expected_length) result(verifier)
      character(len=*), intent(in) :: chapter_name
      character(len=*), intent(in) :: property_names(:)
      character(len=*), intent(in), optional :: expected_length
      type(ArraysLengthVerifier) :: verifier

      verifier%chapter_name = chapter_name
      verifier%chapter_verifier = ArraysLengthChapterVerifier(property_names, expected_length)

   end function arrays_length_verifier_constructor

   function chapter_properties_verifier_verify(this, block_ptr) result(is_valid)
      class(ChapterPropertiesVerifier), intent(in) :: this
      type(tree_data), pointer, intent(in) :: block_ptr
      logical :: is_valid
      integer :: j
      character(len=:), allocatable :: value
      logical :: found

      is_valid = .true.

      if (allocated(this%required_props)) then
         do j = 1, size(this%required_props)
            call prop_get_alloc_string(block_ptr, '', this%required_props(j), value, found)
            if (.not. found) then
               write (msgbuf, '(a,a,a)') 'Missing required property: ', trim(this%required_props(j)), '.'
               is_valid = .false.
            else if (allocated(value)) then
               DEALLOCATE (value)
            end if
         end do
      end if

   end function chapter_properties_verifier_verify

   function properties_verifier_verify(this, processor) result(is_valid)
      class(PropertiesVerifier), intent(in) :: this
      type(TextFileProcessor), intent(in) :: processor
      logical :: is_valid

      ! Find the chapter and check array lengths
      if (allocated(this%chapter_verifier)) then
         is_valid = apply_chapter_verifier_to_chapter(processor%tree, this%chapter_name, this%chapter_verifier)
         if (.not. is_valid) then
             write (msgbuf, '(a,a,a)') 'Verification failed for chapter: ', trim(this%chapter_name), '.'
         end if
      end if

   end function properties_verifier_verify

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
            if (allocated(this%verifiers(i)%ptr)) then
               if (.not. this%verifiers(i)%ptr%verify(processor)) then
                  is_valid = .false.
                  exit  ! Stop on first failure
               end if
            end if
         end do
      end if

   end function and_verifier_verify

   function arrays_length_chapter_verifier_verify(this, block_ptr) result(is_valid)
      class(ArraysLengthChapterVerifier), intent(in) :: this
      type(tree_data), pointer, intent(in) :: block_ptr
      logical :: is_valid
      integer :: read_expected_length
      integer :: first_length, current_length
      logical :: found
      integer :: j
      logical :: anything_found
      integer :: num_values
      character(len=:), allocatable :: value
      character(len=:), allocatable :: prop_list

      is_valid = .true.
      first_length = -1

      ! Check the specified chapter for array length consistency
      if (allocated(this%property_names)) then
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
            if (found) then
               anything_found = .true.
               num_values = count_string_elements(value)
               DEALLOCATE (value)

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
            ! Build comma-separated list of property names
            prop_list = trim(this%property_names(1))
            do j = 2, size(this%property_names)
               prop_list = prop_list//', '//trim(this%property_names(j))
            end do

            write (msgbuf, '(a,i0,a,i0,a,a,a,a,a)') 'Array length mismatch. Expected ', &
               read_expected_length, ' but got ', first_length, &
               ' (expected_length property: ', trim(this%expected_length), &
               ', properties: ', trim(prop_list), ').'
            is_valid = .false.
            return
         end if
      end if

   end function arrays_length_chapter_verifier_verify

   !> ArraysLengthVerifier implementation - verifies all specified arrays have consistent length
   function arrays_length_verifier_verify(this, processor) result(is_valid)
      class(ArraysLengthVerifier), intent(in) :: this
      type(TextFileProcessor), intent(in) :: processor
      logical :: is_valid

      is_valid = .not. processor%is_error
      if (.not. is_valid) return

      ! Find the chapter and check array lengths
      if (allocated(this%chapter_verifier)) then
         is_valid = apply_chapter_verifier_to_chapter(processor%tree, this%chapter_name, this%chapter_verifier)
         if (.not. is_valid) then
             write (msgbuf, '(a,a,a)') 'Array length verification failed for chapter: ', trim(this%chapter_name), '.'
         end if
      end if

   end function arrays_length_verifier_verify

   !> Helper function to apply a chapter verifier to the specified chapter in the tree
   function apply_chapter_verifier_to_chapter(tree, chapter_name, chapter_verifier) result(is_valid)
      type(tree_data), pointer, intent(in) :: tree
      character(len=*), intent(in) :: chapter_name
      class(ChapterVerifier), intent(in) :: chapter_verifier
      logical :: is_valid
      integer :: i, num_items_in_file
      type(tree_data), pointer:: block_ptr
      character(len=:), allocatable :: group_name

      is_valid = .false.
      num_items_in_file = tree_num_nodes(tree)

      ! Find the chapter and check array lengths
      do i = 1, num_items_in_file
         block_ptr => tree%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(block_ptr))

         if (trim(adjustl(str_tolower(group_name))) == trim(adjustl(str_tolower(chapter_name)))) then
            is_valid = chapter_verifier%verify(block_ptr)
            exit  ! Found the chapter, done checking
         end if
      end do

   end function apply_chapter_verifier_to_chapter

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
end module m_text_file_validators
