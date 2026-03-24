module m_text_file_processor
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
      procedure, private :: init => text_file_processor_init
      procedure, private :: parse => text_file_processor_parse

   end type TextFileProcessor

   interface TextFileProcessor
      module procedure :: text_file_processor_constructor
      module procedure :: text_file_processor_constructor_with_tree
   end interface TextFileProcessor


contains

   !> Constructor for TextFileProcessor
   function text_file_processor_constructor(filename) result(processor)
      character(len=*), intent(in) :: filename
      type(TextFileProcessor) :: processor

      processor%filename = filename
      call processor%init()
      if (.not. processor%is_error) then
         call processor%parse()
      end if
   end function text_file_processor_constructor

   function text_file_processor_constructor_with_tree(tree) result(processor)
      type(tree_data), pointer :: tree
      type(TextFileProcessor) :: processor

      processor%tree => tree
   end function text_file_processor_constructor_with_tree

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


end module m_text_file_processor
