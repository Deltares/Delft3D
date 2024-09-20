!> Module that contains general functions to check for deprecated keywords
module m_deprecation
   implicit none
   private

   integer, parameter :: UNDEFINED = 0 !< integer parameter indicating that deprecation status is undefined
   integer, parameter, public :: DEPRECATED = 1 !< integer parameter indicating that the keyword is deprecated
   integer, parameter, public :: OBSOLETE = 2 !< integer parameter indicating that the keyword is obsolete

   type, public :: deprecated_keyword
      character(len=255) :: chapter !< chapter name
      character(len=255) :: key !< keyword name
      integer :: deprecation_level !< whether the keyword is OBSOLETE (error) or DEPRECATED (warning)
      character(len=255) :: additional_information !< additional information about the keyword
   end type

   type, public :: deprecated_keyword_set
      type(deprecated_keyword), dimension(:), allocatable :: deprecated_keyword_list !< array of deprecated keyword types
      integer :: count = 0 !< number of keywords in the keyword set
      character(len=256) :: additional_information !< string with extra information for this keyword set
   end type

   public :: is_deprecated, is_obsolete, add_deprecated_keyword, check_file_tree_for_deprecated_keywords

contains

   !> Either allocate the list or double the size if it is already allocated.
   subroutine reallocate_deprecated_keyword_list(list)
      type(deprecated_keyword), dimension(:), allocatable, intent(inout) :: list !< list of deprecated keywords that needs reallocation

      type(deprecated_keyword), dimension(:), allocatable :: temp_list

      if (.not. allocated(list)) then
         allocate (list(100))
      else
         allocate (temp_list(2 * size(list)))
         temp_list(1:size(list)) = list(1:size(list))
         call move_alloc(temp_list, list)
      end if

   end subroutine reallocate_deprecated_keyword_list

   !> Retrieve a deprecated keyword from the set, or an undefined deprecated keyword if not found.
   function get_keyword(chapter, key, set, deprecation_level) result(res)
      use string_module, only: strcmpi
      character(len=*), intent(in) :: chapter !< chapter name
      character(len=*), intent(in) :: key !< keyword name
      type(deprecated_keyword_set), intent(in) :: set !< keyword set in which to check whether it is deprecated or not
      integer, optional, intent(in) :: deprecation_level !< which level to check (DEPRECATED or OBSOLETE)
      type(deprecated_keyword) :: res !< deprecated keyword if found, otherwise a deprecated keyword with deprecation_level UNDEFINED

      integer :: i

      res = deprecated_keyword(chapter='', key='', deprecation_level=UNDEFINED, additional_information='')
      associate (list => set%deprecated_keyword_list)
         do i = 1, set%count
            if (strcmpi(list(i)%chapter, chapter) .and. strcmpi(list(i)%key, key)) then
               if (present(deprecation_level)) then
                  if (list(i)%deprecation_level == deprecation_level) then
                     res = list(i)
                  end if
               else
                  res = list(i)
               end if
               exit
            end if
         end do
      end associate
   end function get_keyword

   !> Check if a keyword is deprecated (but still supported).
   function is_deprecated(chapter, key, set) result(res)
      character(len=*), intent(in) :: chapter !< chapter name
      character(len=*), intent(in) :: key !< keyword name
      type(deprecated_keyword_set), intent(in) :: set !< keyword set in which to check whether it is deprecated or not
      logical :: res !< whether the keyword is deprecated

      type(deprecated_keyword) :: keyword

      keyword = get_keyword(chapter, key, set, DEPRECATED)
      res = keyword%deprecation_level /= UNDEFINED
   end function is_deprecated

   !> Check if a keyword is obsolete and cannot be used anymore.
   function is_obsolete(chapter, key, set) result(res)
      character(len=*), intent(in) :: chapter !< chapter name
      character(len=*), intent(in) :: key !< keyword name
      type(deprecated_keyword_set), intent(in) :: set !< keyword set in which to check whether it is deprecated or not
      logical :: res !< whether the keyword is obsolete

      type(deprecated_keyword) :: keyword

      keyword = get_keyword(chapter, key, set, OBSOLETE)
      res = keyword%deprecation_level /= UNDEFINED
   end function is_obsolete

   !> Retrieve optional additional information for a keyword.
   subroutine print_additional_keyword_information(chapter, key, set, prefix)
      use messagehandling, only: LEVEL_INFO, mess
      character(len=*), intent(in) :: chapter !< chapter name
      character(len=*), intent(in) :: key !< keyword name
      character(len=*), intent(in) :: prefix !< Message string prefix
      type(deprecated_keyword_set), intent(in) :: set !< keyword set in which to check whether it is deprecated or not

      type(deprecated_keyword) :: keyword

      keyword = get_keyword(chapter, key, set, OBSOLETE)
      if (len_trim(keyword%additional_information) /= 0) then
         call mess(LEVEL_INFO, prefix//': keyword ['//trim(chapter)//'] '//trim(key)//': '//keyword%additional_information)
      end if
   end subroutine print_additional_keyword_information

   !> Add a deprecated or obsolete keyword to a deprecated keyword set.
   subroutine add_deprecated_keyword(deprecated_keywords, chapter, key, deprecation_level, additional_information)
      type(deprecated_keyword_set), intent(inout) :: deprecated_keywords !< keyword set to add the new keyword to
      character(len=*), intent(in) :: chapter !< chapter name
      character(len=*), intent(in) :: key !< keyword name
      integer, intent(in) :: deprecation_level !< whether the keyword is deprecated or obsolete
      character(len=*), optional, intent(in) :: additional_information !< additional information about the keyword

      associate (count => deprecated_keywords%count)
         if (count >= size(deprecated_keywords%deprecated_keyword_list)) then
            call reallocate_deprecated_keyword_list(deprecated_keywords%deprecated_keyword_list)
         end if
         count = count + 1
         associate (keyword => deprecated_keywords%deprecated_keyword_list(count))
            keyword%chapter = chapter
            keyword%key = key
            keyword%deprecation_level = deprecation_level
            if (present(additional_information)) then
               keyword%additional_information = additional_information
            else
               keyword%additional_information = ''
            end if
         end associate
      end associate
   end subroutine add_deprecated_keyword

   !> Check a file tree for all keywords that were not used, deprecated or obsolete.
   !! Throw an error for obsolete keywords, and otherwise print a warning.
   subroutine check_file_tree_for_deprecated_keywords(tree, keyword_set, status, prefix, excluded_chapters)
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_get_name, tree_get_data_string
      use unstruc_messages, only: threshold_abort, level_fatal, level_error, level_warn, mess
      use string_module, only: strcmpi

      implicit none
      type(TREE_DATA), pointer, intent(in) :: tree !< tree of content of the input file to check for deprecated keywords
      type(deprecated_keyword_set), intent(in) :: keyword_set !< keyword set that corresponds to the file type of the tree that is being checked
      integer, intent(out) :: status !< Result status (DFM_NOERR if no invalid (obsolete) entries were present)
      character(len=*), intent(in) :: prefix !< Message string prefix
      character(len=*), dimension(:), optional, intent(in) :: excluded_chapters !< Tree chapters to exclude when checking for deprecated or unused keywords

      type(TREE_DATA), pointer :: chapter !< tree data pointer for chapter level
      type(TREE_DATA), pointer :: node !< tree data pointer for keyword level
      integer :: node_index !< index of the keyword being processed
      integer :: num_nodes !< number of keywords in the chapter
      integer :: chapter_index !< index of the chapter being processed
      integer :: num_chapters !< number of chapters in the file
      character(len=30) :: node_name !< name of the keyword
      character(len=30) :: chapter_name !< name of the chapter
      character(len=100) :: node_string !< string containing the keyword value
      integer :: temp_threshold !< backup variable for default abort threshold level (temporarily overruled)
      logical :: success !< flag indicating successful completion of a call
      integer :: num_obsolete !< count the number of obsolete (removed) keywords
      integer :: num_deprecated !< count the number of deprecated keywords

      status = DFM_NOERR
      if (.not. associated(tree)) return

      num_obsolete = 0
      num_deprecated = 0

      temp_threshold = threshold_abort
      threshold_abort = LEVEL_FATAL

      num_chapters = size(tree%child_nodes)
      do chapter_index = 1, num_chapters
         chapter => tree%child_nodes(chapter_index)%node_ptr
         chapter_name = tree_get_name(chapter)
         if (present(excluded_chapters)) then
            if (any(strcmpi(excluded_chapters, trim(chapter_name)))) then !> do not check model chapter
               cycle
            end if
         end if
         if (associated(chapter%child_nodes)) then
            num_nodes = size(chapter%child_nodes)
         else
            num_nodes = 0
         end if
         do node_index = 1, num_nodes
            node => chapter%child_nodes(node_index)%node_ptr
            call tree_get_data_string(node, node_string, success)
            if (success) then
               node_name = tree_get_name(node)
               if (size(node%node_data) > 0) then
                  if (node%node_visit < 1) then
                     if (is_obsolete(trim(chapter_name), trim(node_name), keyword_set)) then
                        num_obsolete = num_obsolete + 1
                        call mess(LEVEL_ERROR, prefix//': keyword ['//trim(chapter_name)//'] '//trim(node_name)//' is obsolete.')
                        call print_additional_keyword_information(trim(chapter_name), trim(node_name), keyword_set, prefix)
                     else
                        ! keyword unknown, or known keyword that was not accessed because of the reading was switched off by the value of another keyword
                        call mess(LEVEL_WARN, prefix//': keyword ['//trim(chapter_name)//'] '//trim(node_name)//'='//trim(node_string)//' was in file, but not used. Check possible typo.')
                     end if
                  else
                     ! keyword is known and used (node_visit >= 1)
                     if (is_deprecated(trim(chapter_name), trim(node_name), keyword_set)) then
                        num_deprecated = num_deprecated + 1
                        call mess(LEVEL_WARN, prefix//': keyword ['//trim(chapter_name)//'] '//trim(node_name)//' is deprecated and may be removed in a future release.')
                        call print_additional_keyword_information(trim(chapter_name), trim(node_name), keyword_set, prefix)
                     end if
                  end if
               end if
            end if
         end do
      end do

      if (num_deprecated > 0) then
         call mess(LEVEL_WARN, prefix//': Deprecated keywords used:  '//trim(keyword_set%additional_information))
      end if

      threshold_abort = temp_threshold !> restore threshold_abort

      if (num_obsolete > 0) then
         call mess(LEVEL_ERROR, prefix//': Old unsupported keywords used: '//trim(keyword_set%additional_information))
         status = DFM_WRONGINPUT
      end if

   end subroutine check_file_tree_for_deprecated_keywords

end module m_deprecation
