module text_file_processor
   use messageHandling, only: warn_flush, err_flush, msgbuf, LEVEL_FATAL, LEVEL_ERROR, LEVEL_WARN, LEVEL_INFO
   use properties
   use tree_data_types

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

   interface ChapterPropsVerifier
      module procedure :: string_array_verifier_constructor
   end interface ChapterPropsVerifier

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

      character(len=:), allocatable :: value
      logical :: found

      ! Check if processor is valid first
      is_valid = .not. processor%is_error

      num_items_in_file = tree_num_nodes(processor%tree)



      ! If valid, check required strings
      if (is_valid .and. allocated(this%required_props)) then
         do i = 1, num_items_in_file
            block_ptr => processor%tree%child_nodes(i)%node_ptr
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
         end do
      end if

   end function string_array_verifier_verify

end module text_file_processor
