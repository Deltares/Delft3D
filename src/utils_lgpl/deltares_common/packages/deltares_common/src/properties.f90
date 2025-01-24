module properties
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use tree_structures
   use string_module, only: str_tolower, str_lower, get_version_major_minor_integer

   implicit none
   private

   integer, parameter :: max_length = 256
   integer, parameter :: xml_buffer_length = 1000
   character(len=1), parameter :: space = ' '
   character(len=1), parameter :: tab = achar(9)
   character(len=1), parameter :: indent = tab
   character(len=10), dimension(2, 3), parameter :: entities = &
                                                    reshape((/'&    ', '&amp;', &
                                                              '>    ', '&gt; ', &
                                                              '<    ', '&lt; '/), (/2, 3/))
   !
   ! Define the XML data type that holds the parser information
   !
   type, public :: xml_parse
      integer :: lun ! LU-number of the XML-file
      integer :: level ! Indentation level (output)
      integer :: lineno ! Line in file
      logical :: ignore_whitespace ! Ignore leading blanks etc.
      logical :: no_data_truncation ! Do not allow data truncation
      logical :: too_many_attribs ! More attributes than could be stored?
      logical :: too_many_data ! More lines of data than could be stored?
      logical :: eof ! End of file?
      logical :: error ! Invalid XML file or other error?
      character(len=xml_buffer_length) :: line ! buffer
   end type xml_parse
   !
   interface max_keylength
      module procedure max_keylength
   end interface
   interface leaf_keylength
      module procedure leaf_keylength
   end interface
   interface print_initree
      module procedure print_initree
   end interface

   interface prop_get
      module procedure prop_get_string
      module procedure prop_get_strings
      module procedure prop_get_integer
      module procedure prop_get_integers
      module procedure prop_get_integers_2d
      module procedure prop_get_real
      module procedure prop_get_reals
      module procedure prop_get_logical
      module procedure prop_get_double
      module procedure prop_get_doubles
      module procedure prop_get_subtree_string
      module procedure prop_get_subtree_integer
      module procedure prop_get_subtree_integers
      module procedure prop_get_subtree_real
      module procedure prop_get_subtree_reals
      module procedure prop_get_subtree_logical
      module procedure prop_get_subtree_double
      module procedure prop_get_subtree_doubles
   end interface

   interface prop_set
      module procedure prop_set_data
      module procedure prop_set_string
      module procedure prop_set_integer
      module procedure prop_set_integers
      module procedure prop_set_double
      module procedure prop_set_doubles
   end interface

   interface get_version_number
      module procedure prop_get_version_number
   end interface

   public :: readIniFile, max_keylength, leaf_keylength, print_initree, has_key, prop_get, prop_set, get_version_number, count_occurrences
   public :: prop_file, prop_inifile, prop_write_xmlfile, prop_write_inifile, prop_get_alloc_string
   public :: node_value, tree_data, tree_get_name, tree_create, tree_create_node, tree_put_data, tree_get_node_by_name, tree_get_data_ptr, tree_remove_child_by_name
   public :: tree_destroy, tree_add_node, tree_disconnect_node, tree_get_data_string, tree_num_nodes, tree_traverse_level, tree_count_nodes_byname, tree_fold, tree_traverse, maxlen

contains
   ! ====================================================================
   !> Loads an .ini file into a tree structure.
   !! Some optional arguments may be used to obtain error status + message.
   subroutine readIniFile(filename, ini_ptr, filetypename, errmsg, istat)
      character(len=*), intent(in) :: filename !< File to be read
      type(tree_data), pointer :: ini_ptr !< tree_data structure into which file is read, subsequently ready for prop_get calls.
      character(len=*), optional, intent(in) :: filetypename !< Optional name of this file type, used in a possible error message. Default: 'ini file'.
      character(len=*), optional, intent(out) :: errmsg !< Optional character string that will be filled with a printable error message (to be done on call site).
      integer, optional, intent(out) :: istat !< Optional result status (0 if successful).

      integer :: istat_
      character(len=64) :: filetypename_

      istat_ = 0 ! Success

      ! Prepare error message:
      if (present(filetypename)) then
         filetypename_ = filetypename
      else
         filetypename_ = 'ini file'
      end if

      ! Construct + fill tree
      call tree_create(trim(filename), ini_ptr)
      call prop_file('ini', trim(filename), ini_ptr, istat_)

      ! Handle possible error
      if (istat_ /= 0 .and. present(errmsg)) then
         write (errmsg, '(a," ''",a,"'' not found. Code: ",i0)') trim(filetypename), trim(filename), istat_
      end if
      if (present(istat)) then
         istat = istat_
      end if

   end subroutine readIniFile

   ! ====================================================================
   subroutine prop_file(filetype, filename, tree, error, errmsg)
      use tree_structures
      !
      ! Parameters
      !
      character(*), intent(in) :: filetype
      character(*), intent(in) :: filename
      type(tree_data), pointer :: tree
      integer, intent(out) :: error
      character(len=*), intent(out), optional :: errmsg
      !
      ! Local variables
      !
      integer :: lu
      character(10) :: ftype

      ftype = str_tolower(filetype)
      error = 0

      ! Get the error message first if requested - keeps the interface for the individual routines the same as before
      if (present(errmsg)) then
         errmsg = ' '
         open (newunit=lu, file=filename, iostat=error, status='old', iomsg=errmsg)
         close (lu)
      end if

      select case (trim(ftype))
      case ('ini')
         call prop_inifile(filename, tree, error)
      case ('tekal')
         call prop_tekalfile(filename, tree, error)
      case ('xml')
         call prop_xmlfile(filename, tree, error)
      case default
         write (*, *) 'file type ', filetype, ' not supported'
         if (present(errmsg)) then
            errmsg = 'file type '//trim(filetype)//' not supported'
         end if
         error = 5
      end select
   end subroutine prop_file

   ! --------------------------------------------------------------------
   !   Subroutine: prop_file
   !   Author:     Arjen Markus
   !   Purpose:    Read the props from file
   !   Context:    Called before calls to prop_get
   !   Summary:
   !               Read the props file, store the lines with
   !               chapters and key-value pairs.
   !   Arguments:
   !   filename    Name of the file to read
   !   error       0: no error occured
   !               1: file does not exist
   !               2: unable to open file
   !               3: no properties found in file
   !               4: more than max_properties found
   !   Restrictions:
   !               - One file at a time can be handled
   !               - Maximum number of properties in a file: 200
   !               - Maximum length of a line in a file    : 256
   !   Multi-line values:
   !               Lines ending with '\' are continued on the next line.
   !   Comment lines:
   !               Chapters are recognised by the character "[" in the first column of a line.
   !               Keywords are recognised by the character "=" somewhere in the line.
   !               Comments behind line continuation *must* be preceded by '#' and may
   !               not contain further '#'s. Same for last line of multi-line block.
   !               All other lines are assumed to be comments.
   ! --------------------------------------------------------------------
   !
   subroutine prop_inifile(filename, tree, error, japreproc)
      use tree_structures
      !
      ! Parameters
      !
      character(*), intent(in) :: filename !< File name
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      integer, intent(out) :: error !< Placeholder for file errors
      logical, intent(in), optional :: japreproc !< Run the file through a preprocessor
      !
      ! Local variables
      !
      integer :: lu

      lu = 0
      if (present(japreproc)) then ! If preprocessor was requested
         if (japreproc) then
            lu = preprocINI(filename, error) ! do preprocessing
         end if
      end if
      if (lu == 0) then ! if lu has not been assigned a valid unit number
         ! open existing file only
         open (newunit=lu, file=filename, iostat=error, status='old')
         if (error /= 0) then
            return
         end if
      end if

      call prop_inifile_pointer(lu, tree)
      close (lu)
      error = 0
      return
   end subroutine prop_inifile
   !
   !
   ! ====================================================================
   subroutine prop_inifile_pointer(lu, tree)
      use tree_structures
      use string_module
      !
      ! Parameters
      !
      integer, intent(in) :: lu !< File unit
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      !
      ! Local variables
      !
      integer :: eof
      integer :: eqpos, valend
      integer :: k, k2, i
      integer :: lend, lcend, num_bs
      logical :: multiple_lines
      character(max_length) :: key

      character(len=:), allocatable :: line
      character(len=:), allocatable :: linecont !< Placeholder for continued line
      character(len=:), allocatable :: lineconttemp
      character(len=:), allocatable :: value

      type(tree_data), pointer :: achapter
      type(tree_data), pointer :: anode
      integer :: num_hash

      allocate (character(maxlen) :: line)
      allocate (character(maxlen) :: linecont)
      allocate (character(maxlen) :: value)
      allocate (character(maxlen) :: lineconttemp)

      if (.not. associated(tree)) then
         allocate (tree)
      end if
      if (.not. associated(tree%node_name)) then
         allocate (tree%node_name(0))
      end if

      !
    !! executable statements -------------------------------------------------------
      !
      achapter => tree
      !
      ! To do:
      !   Get rid of leading blanks
      do
         line = ''
         lend = 0
         multiple_lines = .false.

         do ! Check on line continuation
            call GetLine(lu, lineconttemp, eof)
            if (index(lineconttemp, 'END PARAMETERS') == 1) then
               eof = -1
               return ! stop reading from file
            end if
            linecont = adjustl(lineconttemp)
            lcend = len_trim(linecont)
            if (lcend == 0) then
               ! Empty line, leave continuation loop
               exit
            end if
            if (linecont(1:1) == '#' .or. linecont(1:1) == '*') then
               ! Comment line, leave continuation loop
               exit
            end if
            ! There could be a comment (started by #) after line continuation backslash
            num_hash = 0
            do i = 1, lcend ! count number of #
               if (linecont(i:i) == '#') num_hash = num_hash + 1
            end do
            if (num_hash == 0) then ! if none, it is easy
               lcend = len_trim(linecont)
            else
               if (num_hash == 1) then ! if only one, then this is THE comment mark
                  lcend = index(linecont(1:lcend), '#') - 1
               else ! if more than one
                  !    if nothing between '=' and the first '#', cut after second '#'
                  if (len_trim(linecont(index(linecont(1:lcend), '=') + 1:index(linecont(1:lcend), '#') - 1)) == 0) then
                     lcend = index(linecont(index(linecont(1:lcend), '#') + 1:lcend), '#') + index(linecont(1:lcend), '#')
                  else
                     lcend = index(linecont(1:lcend), '#') - 1 !    else cut before the first
                  end if
               end if
            end if
            lcend = len_trim(linecont(1:lcend)) ! finally, remove trailing blanks
            linecont = linecont(1:lcend) ! and actually remove the end of the string
            if (lcend > 0) then
               num_bs = lcend - verify(linecont(1:lcend), char(92), .true.) ! nr of backslashes at end of line
               if (mod(num_bs, 2) == 1) then ! Odd nr of backslashes, indeed line continuation
                  multiple_lines = .true.
                  lcend = lcend - 1 ! Strip off single line cont character
                  line = line(1:lend)//' '//linecont(1:lcend)
                  lend = lend + lcend + 1
                  ! Line continuation, proceed to next line
               else
                  if (.not. multiple_lines) then
                     ! No continuation, so leave possible comment as well
                     lcend = len_trim(linecont)
                  end if
                  line = line(1:lend)//' '//linecont(1:lcend)
                  lend = lend + lcend + 1
                  exit ! No further lines for this value
               end if
            else
               ! Empty line, leave continuation loop
               exit
            end if

         end do

         if (eof /= 0) exit
         !
         ! Remove carriage returns and tabs
         !
         do k = 1, len_trim(line)
            if (line(k:k) == char(13)) then
               line(k:k) = ' '
            else if (line(k:k) == char(9)) then
               line(k:k) = ' '
            end if
         end do
         !
         ! Remove leading spaces, cycle when line is empty
         !
         line = adjustl(line)
         if (len_trim(line) == 0) cycle
         !
         ! Chapters
         !
         if (line(1:1) == '[') then
            k = index(line, ']')
            if (k <= 0) then
               cycle
            end if
            !
            call str_lower(line(2:k - 1), k - 2)
            call tree_create_node(tree, line(2:k - 1), achapter)
         else
            !
            ! Key-value pairs
            !
            eqpos = index(line, '=')
            if (eqpos <= 0) then
               call tree_create_node(achapter, " ", anode)
               call tree_put_data(anode, transfer(trim(adjustl(line)), node_value), "STRING")
               cycle
            end if
            k = index('ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz0123456789', &
                     & line(1:1))
            if (k <= 0) then
               call tree_create_node(achapter, " ", anode)
               call tree_put_data(anode, transfer(trim(adjustl(line)), node_value), "STRING")
               cycle
            end if
            key = str_tolower(adjustl(line(1:eqpos - 1)))

            ! Strip off comments that start with #
            ! To allow lines like: FileName = somefile.ext # Comment text
            ! and prevent that comment text also ends up in file name.
            k = index(line(eqpos + 1:), '#')
            if (k > 0) then ! # found, could be first delimiter of #..string..#, OR start of comment
               k2 = index(line(eqpos + k + 1:), '#')
               if (k2 > 0) then ! Second # found: ##-delimited value
                  valend = eqpos + k + k2 ! Last value char is second #.
               else ! No second # found: it was a value followed by # + comment
                  valend = eqpos + k - 1 ! Last value char is just before first #.
               end if
            else
               valend = len_trim(line) ! Value is complete line after = char
            end if

            value = adjustl(line(eqpos + 1:valend))
            call tree_create_node(achapter, trim(key), anode)
            call tree_put_data(anode, transfer(trim(value), node_value), "STRING")

         end if
         !
         ! Get the next line
         !
      end do
      !
      ! End of file or procedure
      !
   end subroutine prop_inifile_pointer
   !
   !
   ! --------------------------------------------------------------------
   !   Subroutine: prop_tekal_file
   !   Author:     Adri Mourits
   !   Purpose:    Read the props from file
   !   Context:    Called before calls to prop_get
   !   Summary:
   !               Read the props file, store the lines with
   !               chapters and key-value pairs.
   !   Arguments:
   !   filename    Name of the file to read
   !   error       0: no error occured
   !               1: file does not exist
   !               2: unable to open file
   !               3: no properties found in file
   !               4: more than max_properties found
   !   Restrictions:
   !               - One file at a time can be handled
   !               - Maximum number of properties in a file: 200
   !               - Maximum length of a line in a file    : 256
   !   Comment lines:
   !               Chapters are recognised by the character "[" in the first column of a line.
   !               Keywords are recognised by the character "=" somewhere in the line.
   !               All other lines are assumed to be comments.
   ! --------------------------------------------------------------------
   !
   subroutine prop_tekalfile(filename, tree, error)
      use tree_structures
      !
      ! Parameters
      !
      character(*), intent(in) :: filename !< File name
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      integer, intent(out) :: error !< Placeholder for file errors
      !
      ! Local variables
      !
      integer :: lu

      open (newunit=lu, file=filename, iostat=error)
      if (error /= 0) then
         return
      end if

      call prop_tekalfile_pointer(lu, tree)
      close (lu)
      return
   end subroutine prop_tekalfile
   !
   !
   ! ====================================================================
   subroutine prop_tekalfile_pointer(lu, tree)
      use tree_structures
      !
      ! Parameters
      !
      integer, intent(in) :: lu !< File unit
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      !
      ! Local variables
      !
      integer :: eof
      integer :: k
      integer, dimension(2) :: blockdims
      real, dimension(:), allocatable :: arow
      character(len=:), allocatable :: line
      character(len=:), allocatable :: linetemp
      type(tree_data), pointer :: atekalblock
      type(tree_data), pointer :: anode

      allocate (character(maxlen) :: line)
      allocate (character(maxlen) :: linetemp)

      if (.not. associated(tree)) then
         allocate (tree)
      end if
      if (.not. associated(tree%node_name)) then
         allocate (tree%node_name(0))
      end if

      !
    !! executable statements -------------------------------------------------------
      !
      blockdims = 0
      do
         !
         ! Skip the commentary in the header, lines starting with a '*'
         !
         do
            read (lu, '(a)', iostat=eof) linetemp
            if (eof /= 0 .or. linetemp(1:1) /= '*') exit
         end do
         if (eof /= 0) exit
         !
         ! Remove carriage returns
         !
         k = index(linetemp, char(13))
         if (k > 0) then
            linetemp(k:k) = ' '
         end if
         !
         ! Remove leading spaces
         !
         line = str_tolower(adjustl(linetemp))
         !
         ! Assumption: this line contains the name of a tekal block
         !
         call tree_create_node(tree, trim(line), atekalblock)
         !
         ! Assumption: nest line contains the dimensions of the tekal block
         !
         read (lu, *, iostat=eof) blockdims
         if (eof /= 0) exit
         call tree_put_data(atekalblock, transfer(blockdims, node_value), "INTEGER ARRAY")
         allocate (arow(blockdims(2)))
         do k = 1, blockdims(1)
            read (lu, *, iostat=eof) arow
            if (eof /= 0) exit
            write (line, '(a,i0)') 'row_', k
            call tree_create_node(atekalblock, trim(line), anode)
            call tree_put_data(anode, transfer(arow, node_value), "REAL ARRAY")
         end do
         deallocate (arow)
         !
         ! Get the next tekal block
         !
      end do
      !
      ! End of file or procedure
      !
   end subroutine prop_tekalfile_pointer
   !
   !
   ! ====================================================================
   subroutine prop_xmlfile(filename, tree, error)
      use tree_structures
      !
      ! Parameters
      !
      character(*), intent(in) :: filename !< File name
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      integer, intent(out) :: error !< Placeholder for file errors
      !
      ! Local variables
      !
      integer :: lu
      !
      lu = -1
      !
      ! if lu has not been assigned a valid unit number
      !
      if (lu < 0) then
         !
         !      open existing file only
         !
         open (newunit=lu, file=filename, iostat=error, status='old')
         if (error /= 0) then
            return
         end if
      end if
      !
      call prop_xmlfile_pointer(lu, tree, error)
      close (lu)
   end subroutine prop_xmlfile
   !
   !
   ! ====================================================================
   subroutine prop_xmlfile_pointer(lu, tree, error)
      use tree_structures
      use TREE_DATA_TYPES
      !
      ! Parameters
      !
      integer, intent(in) :: lu !< File unit
      type(tree_data), pointer, intent(inout) :: tree !< Tree object generated
      integer, intent(out) :: error !< Placeholder for file errors
      !
      ! Local variables
      !
      integer :: i
      integer :: k
      integer :: kend
      integer :: ierr
      integer :: noattribs
      integer :: nodata
      logical :: xmlendtag
      character(10) :: inttostring
      character(80) :: xmltag
      character(80), dimension(:, :), allocatable :: xmlattribs
      character(xml_buffer_length), dimension(:), allocatable :: xmldata
      type(xml_parse) :: xmlinfo
      type(tree_data_ptr), dimension(:), allocatable :: xmllevel ! Store a pointer to each xml element in the "path"
      !
    !! executable statements -------------------------------------------------------
      !
      ! 100 xml levels must be enough
      !
      allocate (xmlattribs(2, 10), stat=ierr)
      if (allocated(xmldata)) deallocate (xmldata, stat=ierr)
      allocate (xmllevel(0:100), stat=ierr)
      xmllevel(0)%node_ptr => tree
      xmlinfo%lun = lu
      xmlinfo%level = 0
      xmlinfo%lineno = 0
      xmlinfo%ignore_whitespace = .true.
      xmlinfo%no_data_truncation = .true.
      xmlinfo%too_many_attribs = .false.
      xmlinfo%too_many_data = .false.
      xmlinfo%eof = .false.
      xmlinfo%error = .false.
      xmlinfo%line = ' '
      !
      ! Read first line separately
      !
      k = 1
      do while (k >= 1)
         read (xmlinfo%lun, '(a)', iostat=ierr) xmlinfo%line
         xmlinfo%lineno = xmlinfo%lineno + 1
         if (ierr == 0) then
            xmlinfo%line = adjustl(xmlinfo%line)
            k = index(xmlinfo%line, '<?')
            !
            ! Assume (for now at least) that <?xml ... ?> appears on a single line!
            !
            if (k >= 1) then
               kend = index(xmlinfo%line, '?>')
               call tree_create_node(xmllevel(0)%node_ptr, trim(xmlinfo%line(2:kend)), xmllevel(1)%node_ptr)
               if (kend <= 0) then
                  write (*, '(a,i0)') 'ERROR: XML_OPEN: error reading file with LU-number: ', xmlinfo%lun
                  write (*, '(a)') 'ERROR: Line starting with "<?xml" should end with "?>"'
                  xmlinfo%error = .true.
                  exit
               end if
            end if
         else
            write (*, '(a,i0)') 'ERROR: XML_OPEN: error reading file with LU-number: ', xmlinfo%lun
            write (*, '(a)') 'ERROR: Possibly no line starting with "<?xml"'
            xmlinfo%error = .true.
            exit
         end if
      end do
      if (xmlinfo%error) return
      !
      do
         call xml_get(xmlinfo, xmltag, xmlendtag, xmlattribs, noattribs, xmldata, nodata)
         if (.not. xml_ok(xmlinfo)) then
            exit
         end if
         if (xml_error(xmlinfo)) then
            error = 1
            exit
         end if
         if (xmlinfo%level >= ubound(xmllevel, DIM=1)) then
            error = 1
            exit
         end if
         !
         ! End tag: nothing to add to tree
         !
         if (xmlendtag) cycle
         !
         ! Skip comments
         !
         if (xmltag(1:3) == '!--') cycle
         !
         ! Create a node for the tag itself
         !
         xmltag = str_tolower(xmltag)
         call tree_create_node(xmllevel(xmlinfo%level - 1)%node_ptr, trim(xmltag), xmllevel(xmlinfo%level)%node_ptr)
         do k = 1, noattribs
            !
            ! Create a subnode for each attribute
            ! The only difference between an attribute and a child tag is the type specification: "STRING:XMLATTRIBUTE" vs "STRING:XMLDATA"
            ! The type specification must start with STRING (needed in tree_struct)
            !
            xmlattribs(1, k) = str_tolower(xmlattribs(1, k))
            call tree_create_node(xmllevel(xmlinfo%level)%node_ptr, trim(xmlattribs(1, k)), xmllevel(xmlinfo%level + 1)%node_ptr)
            call tree_put_data(xmllevel(xmlinfo%level + 1)%node_ptr, transfer(trim(xmlattribs(2, k)), node_value), "STRING:XMLATTRIBUTE")
         end do
         if (nodata == 1) then
            call tree_put_data(xmllevel(xmlinfo%level)%node_ptr, transfer(xmldata(1), node_value), "STRING:XMLDATA")
         elseif (nodata > 1) then
            write (inttostring, '(i0)') nodata
            call tree_put_data(xmllevel(xmlinfo%level)%node_ptr, transfer(trim(inttostring), node_value), "STRING:XMLNUMDATALINES")
            do i = 1, nodata
               write (inttostring, '(i0)') i
               call tree_create_node(xmllevel(xmlinfo%level)%node_ptr, trim(inttostring), xmllevel(xmlinfo%level + 1)%node_ptr)
               call tree_put_data(xmllevel(xmlinfo%level + 1)%node_ptr, transfer(trim(xmldata(i)), node_value), "STRING:XMLDATALINE")
            end do
         else
            nullify (xmllevel(xmlinfo%level)%node_ptr%node_data)
         end if
      end do
      !
      ! End of file or procedure
      !
      deallocate (xmlattribs, stat=ierr)
      deallocate (xmldata, stat=ierr)
      deallocate (xmllevel, stat=ierr)
   end subroutine prop_xmlfile_pointer
   !
   !
   ! ====================================================================
   ! xml_get --
   !    Routine to get the next bit of information from an XML file
   ! Arguments:
   !    info        Structure holding information on the XML-file
   !    tag         Tag that was encountered
   !    endtag      Whether the end of the element was encountered
   !    attribs     List of attribute-value pairs
   !    no_attribs  Number of pairs in the list
   !    data        Lines of character data found
   !    no_data     Number of lines of character data
   !
   subroutine xml_get(info, tag, endtag, attribs, no_attribs, data, no_data)
      use m_alloc
      !
      type(xml_parse), intent(inout) :: info
      character(len=*), intent(out) :: tag
      logical, intent(out) :: endtag
      character(len=*), intent(out), dimension(:, :) :: attribs
      integer, intent(out) :: no_attribs
      character(xml_buffer_length), intent(inout), dimension(:), allocatable :: data
      integer, intent(out) :: no_data
      !
      integer :: kspace
      integer :: kend
      integer :: keq
      integer :: kfirst, kfirst1, kfirst2
      integer :: ksecond
      integer :: idxat
      integer :: idxdat
      integer :: ierr
      logical :: close_bracket
      logical :: comment_tag
      logical :: endtag_nodata
      character(len=xml_buffer_length) :: nextline
      character(len=1) :: closing_char
      !
      ! Initialise the output
      !
      endtag = .false.
      endtag_nodata = .false.
      no_attribs = 0
      no_data = 0
      info%too_many_attribs = .false.
      info%too_many_data = .false.
      !
      ! From the previous call or the call to xmlopen we have
      ! the line that we need to parse already in memory:
      ! <tag attrib1="..." attrib2="..." />
      !
      comment_tag = .false.
      close_bracket = .false.
      kspace = index(info%line, ' ')
      kend = index(info%line, '>')
      do while (kend <= 0)
         read (info%lun, '(a)', iostat=ierr) nextline
         info%lineno = info%lineno + 1
         if (ierr == 0) then
            info%line = trim(info%line)//' '//adjustl(nextline)
         else
            info%error = .true.
            write (*, '(a,i0)') 'ERROR: XML_GET - end of tag not found (buffer too small?). Line: ', info%lineno
            return
         end if
         kend = index(info%line, '>')
      end do
      if (kend > kspace) then
         kend = kspace
      else
         close_bracket = .true.
      end if
      !
      ! Check for the end of an ordinary tag and of
      ! a comment tag
      !
      if (info%line(1:3) == '-->') then
         endtag = .true.
         tag = '<!--'
         info%level = info%level - 1
      else if (info%line(1:2) == '</') then
         endtag = .true.
         tag = info%line(3:kend - 1)
         info%level = info%level - 1
      else if (info%line(1:4) == '<!--') then
         kend = 4
         tag = info%line(2:4)
         info%level = info%level + 1
         comment_tag = .true.
      else if (info%line(1:1) == '<') then
         ! tag found
         tag = info%line(2:kend - 1)
         info%level = info%level + 1
      else
         kend = 0 ! Beginning of data!
      end if
      if (info%level < 0) then
         write (*, '(a,i0,a)') 'ERROR: xml_get - level dropped below zero: ', info%lineno, trim(info%line)
      end if
      info%line = adjustl(info%line(kend + 1:))
      idxat = 0
      idxdat = 0
      if (.not. allocated(data)) then
         allocate (data(100), stat=ierr)
      end if
      data = ' '
      do while (info%line /= ' ' .and. .not. close_bracket .and. .not. comment_tag)
         keq = index(info%line, '=')
         kend = index(info%line, '>')
         if (keq > kend) keq = 0 ! Guard against multiple tags with attributes on one line
         !
         ! No attributes any more?
         !
         if (keq < 1) then
            kend = index(info%line, '/>')
            if (kend >= 1) then
               kend = kend + 1 ! To go beyond the ">" character
               endtag = .true.
               endtag_nodata = .true.
            else
               kend = index(info%line, '>')
               if (kend < 1) then
                  write (*, '(a,i0,a)') 'ERROR: XML_GET - wrong ending of tag ', info%lineno, trim(info%line)
                  info%error = .true. ! Wrong ending of line!
                  return
               else
                  close_bracket = .true.
               end if
            end if
            if (kend >= 1) then
               info%line = adjustl(info%line(kend + 1:))
            end if
            exit
         end if
         idxat = idxat + 1
         if (idxat <= size(attribs, 2)) then
            no_attribs = idxat
            attribs(1, idxat) = adjustl(info%line(1:keq - 1)) ! Use adjustl() to avoid multiple spaces, etc
            info%line = adjustl(info%line(keq + 1:))
            !
            ! We have almost found the start of the attribute's value
            !
            kfirst1 = index(info%line, '''')
            kfirst2 = index(info%line, '"')
            kfirst = 0
            if (kfirst1 > 0 .and. (kfirst2 <= 0 .or. kfirst1 < kfirst2)) then
               closing_char = ''''
               kfirst = kfirst1
            end if
            if (kfirst2 > 0 .and. (kfirst1 <= 0 .or. kfirst2 < kfirst1)) then
               closing_char = '"'
               kfirst = kfirst2
            end if
            if (kfirst < 1) then
               write (*, '(a,i0,a)') 'ERROR: XML_GET - malformed attribute-value pair: ', info%lineno, trim(info%line)
               info%error = .true. ! Wrong form of attribute-value pair
               return
            end if
            ksecond = index(info%line(kfirst + 1:), closing_char) + kfirst
            if (ksecond < 1) then
               write (*, '(a,i0,a)') 'ERROR: XML_GET - malformed attribute-value pair: ', info%lineno, trim(info%line)
               info%error = .true. ! Wrong form of attribute-value pair
               return
            end if
            attribs(2, idxat) = info%line(kfirst + 1:ksecond - 1)
            info%line = adjustl(info%line(ksecond + 1:))
         end if
         if (idxat > size(attribs, 2)) then
            write (*, '(a,i0,a)') 'ERROR: XML_GET - more attributes than could be stored: ', info%lineno, trim(info%line)
            info%too_many_attribs = .true.
            info%line = ' '
            exit
         end if
      end do
      !
      ! Now read the data associated with the current tag
      ! - all the way to the next "<" character
      !
      do
         if (comment_tag) then
            kend = index(info%line, '-->')
         else
            kend = index(info%line, '<')
         end if
         idxdat = idxdat + 1
         if (idxdat > size(data)) then
            call realloc(data, size(data) + 100, lindex=1, stat=ierr, fill=' ', keepExisting=.true.)
         end if
         if (.not. endtag) no_data = idxdat ! The endtag was set because of "/>"?
         if (kend >= 1) then
            data(idxdat) = info%line(1:kend - 1)
            info%line = info%line(kend:)
         else
            data(idxdat) = info%line
         end if
         no_data = idxdat
         !
         ! No more data? Otherwise, read on
         !
         if (kend >= 1) then
            exit
         else
            read (info%lun, '(a)', iostat=ierr) info%line
            info%lineno = info%lineno + 1
            if (ierr < 0) then
               ! write(*,'(a,i0)') 'ERROR:XML_GET - end of file found - LU-number: ', info%lun
               info%eof = .true.
            elseif (ierr > 0) then
               write (*, '(a,2i0)') 'ERROR:XML_GET - error reading file with LU-number ', info%lun, info%lineno
               info%error = .true.
            end if
            if (ierr /= 0) then
               exit
            end if
         end if
      end do
      !
      ! If we encountered the "/>" pattern, we still need to read the next line
      ! from the file, but there is no actual data ...
      !
      if (endtag_nodata) then
         no_data = 0
      end if
      !
      ! Compress the data?
      !
      if (info%ignore_whitespace) then
         call xml_compress_(data, no_data)
      end if
      !
      ! Replace the entities, if any
      !
      call xml_replace_entities_(data, no_data)
      !
      !write(*,'(a,i0)') 'XML_GET - number of attributes: ', no_attribs
      !write(*,'(a,i0)') 'XML_GET - number of data lines: ', no_data
      !
   end subroutine xml_get
   !
   !
   ! ====================================================================
   ! xml_ok --
   !    Function that returns whether all was okay or not
   ! Arguments:
   !    info                Structure holding information on the XML-file
   ! Returns:
   !    .true. if there was no error, .false. otherwise
   !
   logical function xml_ok(info)
      type(xml_parse), intent(in) :: info

      xml_ok = info%eof .or. info%error .or. &
               (info%no_data_truncation .and. &
                (info%too_many_attribs .or. info%too_many_data))
      xml_ok = .not. xml_ok
   end function xml_ok
   !
   !
   ! ====================================================================
   ! xml_error --
   !    Function that returns whether there was an error
   ! Arguments:
   !    info                Structure holding information on the XML-file
   ! Returns:
   !    .true. if there was an error, .false. if there was none
   !
   logical function xml_error(info)
      type(xml_parse), intent(in) :: info

      xml_error = info%error .or. &
                  (info%no_data_truncation .and. &
                   (info%too_many_attribs .or. info%too_many_data))
   end function xml_error
   !
   !
   ! ====================================================================
   ! --------------------------------------------------------------------
   !
   ! xml_compress_ --
   !    Routine to remove empty lines from the character data and left-align
   !    all others. Left-align in this case, also includes removing tabs!
   ! Arguments:
   !    data        Lines of character data found
   !    no_data     (Nett) number of lines of character data
   !
   subroutine xml_compress_(data, no_data)
      character(len=*), intent(inout), dimension(:) :: data
      integer, intent(inout) :: no_data

      integer :: i
      integer :: j
      integer :: k

      j = 0
      do i = 1, no_data
         do k = 1, len(data(i))
            if (data(i) (k:k) /= space .and. data(i) (k:k) /= tab) then
               j = j + 1
               data(j) = data(i) (k:)
               exit
            end if
         end do
      end do

      no_data = j ! Make sure the empty lines do not count anymore

   end subroutine xml_compress_
   !
   !
   ! ====================================================================
   ! xml_replace_entities_ --
   !    Routine to replace entities such as &gt; by their
   !    proper character representation
   ! Arguments:
   !    data        Lines of character data found
   !    no_data     (Nett) number of lines of character data
   !
   subroutine xml_replace_entities_(data, no_data)
      character(len=*), intent(inout), dimension(:) :: data
      integer, intent(inout) :: no_data

      integer :: i
      integer :: j
      integer :: j2
      integer :: k
      integer :: pos
      logical :: found

      do i = 1, no_data
         j = 1
         do
            do k = 1, size(entities, 2)
               found = .false.
               pos = index(data(i) (j:), trim(entities(2, k)))
               if (pos > 0) then
                  found = .true.
                  j = j + pos - 1
                  j2 = j + len_trim(entities(2, k))
                  data(i) (j:) = trim(entities(1, k))//data(i) (j2:)
                  j = j2
               end if
            end do
            if (.not. found) exit
         end do
      end do

   end subroutine xml_replace_entities_
   !
   !
   ! ====================================================================
   ! --------------------------------------------------------------------
   !   Subroutine: expand
   !   Purpose:    Expand keys ${key} in subject, given a set of key-value pairs
   !   Context:    Called by parse_directives
   !   Summary:
   !               Non-recursive (first level) expansion
   !               Defnames and defstrings form a list of ndef (key,value)-pairs
   !               used in the substitution upon encountering $key or ${key} in the string
   !               keys starting with an underscore refer to environment variables
   !               e.g. ${_PATH} or $_PATH refers to the path variable
   !   Arguments:
   !   subject     Character string subjected to replacements
   !   defnames    keys
   !   defstrings  replacement strings
   !   ndef        number of keys = number of replacement strings
   !
   !   Restrictions:
   !               - Single pass, replacement strings are not subject to expansion themselves (i.e. no recursion)
   !               - keys and replacement strings can at max hold 50 characters
   ! --------------------------------------------------------------------
   !
   subroutine expand(subject, defnames, defstrings, ndef)
      !
      ! Parameters
      !
      character(*), intent(inout) :: subject !< subject to replacments
      character(len=50), intent(in) :: defnames(:) !< defined constants: names
      character(len=50), intent(in) :: defstrings(:) !< defined constants: content
      integer, intent(in) :: ndef !< length of the definition list
      !
      ! Local variables
      !
      character(len=300) :: envstring ! environment strings can be lengthy sometimes  ...
      character(len=600) :: outstring ! so the output must support that. Adapt if still insufficient.
      character(len=50) :: defstring
      integer :: s1
      integer :: s2
      integer :: l1
      integer :: idef

      !
    !! executable statements -------------------------------------------------------
      !
      s1 = 1
      s2 = 1
      l1 = len(trim(subject))
      outstring = ''
      do while (s1 <= l1)
         if (subject(s1:s1) == '$') then
            if (subject(s1 + 1:s1 + 1) == '{') then
               read (subject(s1 + 2:index(subject(s1 + 1:l1), '}') + s1 - 1), *) defstring
               s1 = s1 + len(trim(defstring)) + 2 + 1
            else
               read (subject(s1 + 1:l1), *) defstring
               s1 = s1 + len(trim(defstring)) + 1
            end if
            if (defstring(1:1) == '_') then ! environment variable
               defstring = defstring(2:len(trim(defstring)))
               call getenv(trim(defstring), envstring) ! is left empty if not existent
               outstring = outstring(1:s2 - 1)//trim(envstring)
               s2 = s2 + len(trim(envstring))
            else
               do idef = 1, ndef
                  if (trim(defstring) == trim(defnames(idef))) then
                     outstring = outstring(1:s2 - 1)//trim(defstrings(idef))
                     s2 = s2 + len(trim(defstrings(idef)))
                     exit
                  end if
               end do
            end if
         else
            outstring(s2:s2) = subject(s1:s1)
            s2 = s2 + 1
            s1 = s1 + 1
         end if
      end do
      subject = trim(outstring)
   end subroutine expand
   ! --------------------------------------------------------------------
   !   Subroutine: preprocINI
   !   Purpose:    INI-file preprocessor, cpp-style
   !   Context:    preceeds processing an ini-files into a tree
   !   Summary:
   !            * (nested) file inclusion through include-directive
   !                 #include filename
   !                 #include <filename>
   !            * aliases, defined
   !                  #define aliasname content
   !              and invoked by placing $aliasname or ${aliasname} in the text
   !            * $_aliasname and ${_aliasname} refer to environment variables
   !            * conditionals#ifdef, #ifndef #endif
   !            *#include and #define work recursive, but preprocessing is 'single-pass' (begin to end of files)
   !            Resulting file (expanded) is written to filename_out
   !            Return error code: -5 -> file not found
   !                               -6 -> trying to open an already opened file
   !                               positive error codes refer to iostats
   !            filename_out is optional. If provided, the file is created (or overwritten if existing),
   !                otherwise a scratchfile is used, which is automatically unlinked upon closure
   !            Non-recursive (first level) expansion
   !            Defnames and defstrings form a list of ndef (key,value)-pairs
   !            used in the substitution upon encountering $key or ${key} in the string
   !            keys starting with an underscore refer to environment variables
   !            e.g. ${_PATH} or $_PATH refers to the path variable
   !   Arguments:
   !   infilename      Original file subjected to preprocessing
   !   error           Reports final status:
   !                           0  : no error
   !                         -55  : file not found (passed from parse_directives)
   !                         -66  : tryng to open a file that has already been opened (passed from parse_directives)
   !                         -33  : available unit numbers ran out
   !                   otherwise  : iostat from the last failed file operation
   !   filename_out    Resulting file, optional (if not given, a scratch file is used)
   !
   !   Result:
   !   outfilenumber   Handle to a file open for reading, containing the preprocessors result
   !   Restrictions:
   !               - the maximum file unit number to be returned is 500
   !               - keys and replacement strings can at hold up to 50 characters only
   !               - no more than 100 replacements can be defined
   ! --------------------------------------------------------------------
   !
   integer function preprocINI(infilename, error, outfilename) result(outfilenumber)
      use MessageHandling
      !
      ! Parameters
      !
      character(*), intent(in) :: infilename !< basic config file
      integer, intent(out) :: error !< error code
      character(*), intent(in), optional :: outfilename !< resulting input to build tree
      !
      ! Local variables
      !
      character(50) :: defnames(100) ! definition database
      character(50) :: defstrings(100)
      integer :: ndef
      integer :: iostat

      !
    !! executable statements -------------------------------------------------------
      !
      error = 0
      ndef = 0

      if (present(outfilename)) then
         open (newunit=outfilenumber, file=trim(outfilename), iostat=iostat)
         if (iostat /= 0) then
            outfilenumber = 0
            error = iostat !       ERROR : Intermediate ini-file could not be written.
            return
         end if
      else
         open (newunit=outfilenumber, status='SCRATCH', IOSTAT=iostat)
         if (iostat /= 0) then
            outfilenumber = 0
            error = iostat
            return
         end if
      end if

      error = parse_directives(trim(infilename), outfilenumber, defnames, defstrings, ndef, 1)
      if (error /= 0) then ! either something went wrong ...
         close (outfilenumber) ! close the file
         outfilenumber = 0 ! return 0 as a filenumber
      else ! ... or we're all clear  ....
         rewind (outfilenumber) ! rewind the file just written and return the number to caller
      end if
   end function preprocINI
   ! --------------------------------------------------------------------
   !   Subroutine: parse_directives
   !   Purpose:    part of the INI-file preprocessor, handles preprocessor directives
   !   Context:    called by preprocINI
   !   Summary:    see preprocINI

   !   Arguments:
   !   filename_in     Character string subjected to replacements
   !   infilename      Original file subjected to preprocessing (can also be an included file at deeper level)
   !   outfilenumber   Handle to a file open for reading, containing the preprocessors result
   !   defnames        keys
   !   defstrings      replacement strings
   !   ndef            number of keys = number of replacement strings
   !   level           keeps track of the recursive depth (we could enforce a max on this depth if desired)
   !
   !   Result:
   !   error           Reports final status:
   !                           0  : no error
   !                         -55  : file not found
   !                         -66  : tryng to open a file that has already been opened
   !                         -33  : ran out of available unit numbers
   !                   otherwise  : iostat from the last failed file operation
   !   Restrictions:   see preprocINI
   ! --------------------------------------------------------------------
   !
   recursive integer function parse_directives(infilename, outfilenumber, defnames, defstrings, ndef, level) result(error)
      use MessageHandling
      !
      ! Parameters
      !
      character(len=*), intent(in) :: infilename !< subject file parsed
      integer, intent(in) :: outfilenumber !< unit nr. of output
      character(len=50), intent(inout) :: defnames(:) !< defined constants: names
      character(len=50), intent(inout) :: defstrings(:) !< defined constants: content
      integer, intent(inout) :: ndef !< keeps track of the number of definitions
      integer, intent(in) :: level !< nesting level
      !
      ! Local variables
      !
      character(len=200) :: s
      character(len=50) :: includefile
      character(len=50) :: dumstr
      character(len=50) :: defname
      character(len=50) :: defstring
      integer :: writing
      integer :: idef
      integer :: infilenumber
      integer :: iostat
      logical :: opened
      logical :: exist

      !
    !! executable statements
      !
      error = 0
      inquire (file=trim(infilename), opened=opened, exist=exist)
      if (opened) then
         error = -66 ! ERROR : included file is already open (circular dependency), ignore file
         return
      end if
      if (.not. exist) then
         error = -55 ! ERROR : included file not found
         return
      end if

      open (newunit=infilenumber, file=trim(infilename), iostat=iostat)
      if (iostat /= 0) then
         error = iostat ! ERROR : file was encountered, but for some reason cannot be opened ....
         return
      end if

      writing = 1
      do
         read (infilenumber, '(a200)', end=666) s
         if (index(s, '#include') == 1) then
            read (s, *) dumstr, includefile
            if (includefile(1:1) == '<') then
               includefile = includefile(2:index(includefile, '>') - 1)
            end if
            if (writing > 0) then
               error = parse_directives(includefile, outfilenumber, defnames, defstrings, ndef, level + 1)
               if (error /= 0) return ! first error stops the process
            end if ! is returned to higher levels
         elseif (index(s, '#define') == 1) then
            read (s, *) dumstr, defname, defstring
            call expand(defstring, defnames, defstrings, ndef) ! first expand names
            ndef = ndef + 1
            defnames(ndef) = trim(defname)
            call expand(defstring, defnames, defstrings, ndef)
            defstrings(ndef) = trim(defstring)
         elseif (index(s, '#ifdef') == 1) then
            read (s, *) dumstr, defname
            writing = writing - 1
            do idef = 1, ndef
               if (trim(defname) == trim(defnames(idef))) then
                  writing = writing + 1
                  exit
               end if
            end do
         elseif (index(s, '#endif') == 1) then
            writing = writing + 1
         elseif (index(s, '#ifndef') == 1) then
            read (s, *) dumstr, defname
            do idef = 1, ndef
               if (trim(defname) == trim(defnames(idef))) then
                  writing = writing - 1
               end if
            end do
         else ! Just process this line
            if (writing > 0) then
               if (index(s, '$') > 0) call expand(s, defnames, defstrings, ndef) ! first expand names
               write (outfilenumber, '(a)') trim(s)
            end if
         end if
      end do
666   continue
      close (infilenumber)
   end function parse_directives

   ! ====================================================================
   !> Writes a property tree to file in ini format.
   subroutine prop_write_inifile(mout, tree, error)
      integer, intent(in) :: mout !< File pointer where to write to.
      type(TREE_DATA), pointer :: tree !< Tree to be written.
      integer, intent(out) :: error !< Return status.

      character(len=1), allocatable :: lenmaxdata(:)
      logical :: dummylog

      allocate (lenmaxdata(size(transfer(123, node_value)))) ! Fit a single integer into char array (generally 4 bytes)

      error = 0

      ! Determine maximum key stringlength (used for prettyprinting/alignment in print_initree)
      call tree_fold(tree, max_keylength, leaf_keylength, lenmaxdata, dummylog)

      ! Print the tree by traversing it depth-first, pass mout and lenmax by transfer into data variable.
      call tree_traverse(tree, print_initree, transfer((/mout, transfer(lenmaxdata, 123)/), node_value), dummylog)

   end subroutine prop_write_inifile

   ! ====================================================================
   !> Writes a property tree to file in xml format.
   recursive subroutine prop_write_xmlfile(mout, tree, level, error)
      integer, intent(in) :: mout !< File pointer where to write to.
      type(TREE_DATA), pointer :: tree !< Tree to be written.
      integer, intent(in) :: level !< Indentation level.
      integer, intent(out) :: error !< Return status.
      !
      ! local
      integer :: i
      integer :: numatt
      integer :: numchild
      character(len=1), dimension(:), pointer :: data_ptr
      character(40) :: type_string
      character(40) :: formatstring
      character(80) :: tag
      character(max_length) :: string
      character(xml_buffer_length) :: buffer
      logical :: success
      !
      ! body
      tag = tree_get_name(tree)
      !
      ! Number of children:
      !
      if (.not. associated(tree%child_nodes)) then
         numchild = 0
      else
         numchild = size(tree%child_nodes)
      end if
      !
      ! Count the number of children with type = attribure
      !
      numatt = 0
      do i = 1, numchild
         call tree_get_data_ptr(tree%child_nodes(i)%node_ptr, data_ptr, type_string)
         if (type_string == "STRING:XMLATTRIBUTE") then
            numatt = numatt + 1
         end if
      end do
      call tree_get_data_ptr(tree, data_ptr, type_string)
      if (numchild == numatt) then
         !
         ! Everything related to this tag fits on one line
         !
         buffer = ' '
         if (level > 0) then
            if (tag(1:1) == '?') then
               !
               ! Special treatment for first line: <?xml version="1.0"?>
               !
               write (buffer, '(3a)') "<", trim(tag), ">"
            elseif (tag(1:4) == '<!--') then
               !
               ! Comment line; write it as is
               write (buffer, '(a)') trim(tag)
            else
               !
               ! <tagname att1="val1" att2="val2">data</tagname>
               !
               write (buffer, '(2a)') "<", trim(tag)
               do i = 1, numchild
                  call tree_get_data_ptr(tree%child_nodes(i)%node_ptr, data_ptr, type_string)
                  if (type_string == "STRING:XMLATTRIBUTE") then
                     string = ' '
                     if (associated(data_ptr)) then
                        call tree_get_data_string(tree%child_nodes(i)%node_ptr, string, success)
                     end if
                     write (buffer, '(6a)') trim(buffer), " ", trim(tree_get_name(tree%child_nodes(i)%node_ptr)), &
                        "=""", trim(string), """"
                  end if
               end do
               string = ' '
               call tree_get_data_ptr(tree, data_ptr, type_string)
               if (associated(data_ptr)) then
                  call tree_get_data_string(tree, string, success)
               end if
               if (type_string == "STRING:XMLDATALINE") then
                  write (buffer, '(a)') trim(string)
               else
                  write (buffer, '(6a)') trim(buffer), ">", trim(string), "</", trim(tag), ">"
               end if
            end if
            !
            ! Indentation
            !
            i = (level - 1) * 4
            if (i == 0) then
               write (formatstring, '(a)') '(a)'
            else
               write (formatstring, '(a,i0,a)') '(', i, 'x,a)'
            end if
            write (mout, trim(formatstring)) trim(buffer)
         end if
      else
         !
         ! - Write start tag                  <tagname att1="val1" att2="val2">
         ! - process child_nodes recursively     <children>
         ! - Write end tag                    </tagname>
         !
         buffer = ' '
         if (level > 0) then
            write (buffer, '(2a)') "<", trim(tag)
            do i = 1, numchild
               call tree_get_data_ptr(tree%child_nodes(i)%node_ptr, data_ptr, type_string)
               if (type_string == "STRING:XMLATTRIBUTE") then
                  string = ' '
                  if (associated(data_ptr)) then
                     call tree_get_data_string(tree%child_nodes(i)%node_ptr, string, success)
                  end if
                  write (buffer, '(6a)') trim(buffer), " ", trim(tree_get_name(tree%child_nodes(i)%node_ptr)), "=""", trim(string), """"
               end if
            end do
            write (buffer, '(6a)') trim(buffer), ">"
            !
            ! Indentation
            !
            i = (level - 1) * 4
            if (i == 0) then
               write (formatstring, '(a)') '(a)'
            else
               write (formatstring, '(a,i0,a)') '(', i, 'x,a)'
            end if
            write (mout, trim(formatstring)) trim(buffer)
         end if
         !
         ! process child_nodes recursively
         ! skip children with type=attribute
         !
         do i = 1, numchild
            call tree_get_data_ptr(tree%child_nodes(i)%node_ptr, data_ptr, type_string)
            if (type_string /= "STRING:XMLATTRIBUTE") then
               call prop_write_xmlfile(mout, tree%child_nodes(i)%node_ptr, level + 1, error)
            end if
         end do
         !
         ! Write end tag
         !
         buffer = ' '
         if (level > 0) then
            write (buffer, '(3a)') "</", trim(tag), ">"
            !
            ! Indentation
            !
            i = (level - 1) * 4
            if (i == 0) then
               write (formatstring, '(a)') '(a)'
            else
               write (formatstring, '(a,i0,a)') '(', i, 'x,a)'
            end if
            write (mout, trim(formatstring)) trim(buffer)
         end if
      end if
   end subroutine prop_write_xmlfile

   ! ====================================================================
   !> Selects the maximum keylength from childdata.
   !! to be used in call to tree_fold.
   subroutine max_keylength(tree, childdata, data, stop)
      type(TREE_DATA), pointer :: tree
      character(len=1), dimension(:, :), intent(in) :: childdata
      character(len=1), dimension(:), intent(out) :: data
      logical, intent(inout) :: stop

      integer :: i, lenmax, n
      lenmax = 0
      n = size(childdata, 2)

      do i = 1, size(childdata, 2)
         lenmax = max(lenmax, transfer(childdata(:, i), lenmax))
      end do

      data = transfer(lenmax, data)
   end subroutine max_keylength

   ! ====================================================================
   !> Selects the keylength from a tree leave.
   !! to be used in call to tree_fold.
   subroutine leaf_keylength(tree, data, stop)
      type(TREE_DATA), pointer :: tree
      character(len=1), dimension(:), intent(out) :: data
      logical, intent(inout) :: stop

      character(len=1), dimension(:), pointer :: data_ptr
      character(len=40) :: type_string
      integer :: keylen

      call tree_get_data_ptr(tree, data_ptr, type_string)

      if (associated(data_ptr)) then ! This is a key-value pair
         keylen = len_trim(tree_get_name(tree))
      else
         keylen = 0 ! Don't include chapter names
      end if

      data = transfer(keylen, data)
   end subroutine leaf_keylength

   ! ====================================================================
   !> Prints the root of a tree (either as chapter or as key-value pair)
   !! to be used in call to tree_traverse
   subroutine print_initree(tree, data, stop)
      type(TREE_DATA), pointer :: tree !< Tree whose root should be printed.
      character(len=1), dimension(:), intent(in) :: data !< Help data (max key length, used for alignment).
      logical, intent(inout) :: stop !< Whether to continue or stop.

      integer, dimension(2) :: inputdata
      integer :: mout
      integer :: maxkeylength
      character(len=1), dimension(:), pointer :: data_ptr
      character(len=max_length) :: string
      character(len=40) :: type_string
      logical :: success
      integer :: level

      inputdata = transfer(data, inputdata)
      mout = inputdata(1) !< File pointer
      maxkeylength = inputdata(2)

      level = tree_traverse_level()
      if (level == 0) return

      call tree_get_data_ptr(tree, data_ptr, type_string)
      if (.not. associated(data_ptr)) then
         write (mout, '(a)') ''
         write (mout, '(a,a,a)') '[', trim(tree_get_name(tree)), ']'
         return
      else
         string = tree_get_name(tree)
         write (mout, '(a,a)', advance='no') &
            trim(string), repeat(' ', max(0, maxkeylength - len_trim(string)))
         if (len_trim(string) > 0) then
            write (mout, '(a)', advance='no') ' = '
         else ! For comment lines that appear as key/value with key==''
            write (mout, '(a)', advance='no') '   '
         end if
      end if

      select case (type_string)
      case ('STRING')
         string = '(no data)'
         call tree_get_data_string(tree, string, success)
         write (mout, '(a)') trim(string)
      case default
         string = '(unknown data type)'
         write (mout, '(a,a,a,a)') '# ', trim(string), ' -- ', trim(type_string)
      end select
   end subroutine print_initree

   !> Test if a key exists in the property tree.
   function has_key(tree, chapter_in, key_in) result(res)
      type(tree_data), pointer, intent(in) :: tree !< The property tree
      character(*), intent(in) :: chapter_in !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key_in !< Name of the key (case-insensitive)
      logical :: res
      character(len=:), allocatable :: local_value
      call prop_get_alloc_string(tree, chapter_in, key_in, local_value, res)
   end function has_key

   !> Get the string value for a property
   !!    Go through the list of props to check the
   !!    chapter. When the right chapter is found, check for the key.
   !!    Only set the value if the key matches
   !!  Delimiters:
   !!    If the value starts with the character "#", this character is removed.
   !!    If a second character "#" is found , this character and everything behind this character is removed.
   !!  Comments on this line:
   !!    Use the delimiters "#". Example:
   !!    StringIn = # AFileName # Comments are allowed behind the second "#"
   subroutine prop_get_string(tree, chapterin, keyin, value, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapterin !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: keyin !< Name of the key (case-insensitive)
      character(*), intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      !
      ! Local variables
      !
      character(len=:), allocatable :: localvalue
      logical :: success_

      call prop_get_alloc_string(tree, chapterin, keyin, localvalue, success_)

      if (success_) then
         value = localvalue
         success_ = (len(value) >= len(localvalue))
      end if

      if (present(success)) then
         success = success_
      end if

   end subroutine prop_get_string

   !> Get the string value for a property
   !!    Go through the list of props to check the chapter.
   !!    When the right chapter is found, check for the key.
   !!    Only set the value if the key matches
   !!
   !!  Delimiters:
   !!    If the value starts with the character "#", this character is removed.
   !!    If a second character "#" is found , this character and everything behind this character is removed.
   !!
   !!  Comments on this line:
   !!    Use the delimiters "#". Example:
   !!    StringIn = # AFileName # Comments are allowed behind the second "#"
   subroutine prop_get_alloc_string(tree, chapterin, keyin, value, success)
      type(tree_data), pointer, intent(in) :: tree !< The property tree
      character(*), intent(in) :: chapterin !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: keyin !< Name of the key (case-insensitive)
      character(:), allocatable, intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      !
      ! Local variables
      !
      logical :: ignore
      logical :: success_
      logical :: isFirst
      integer :: i ! Childnode number with node_name = key
      ! All following child nodes with node_name = " " are also added
      integer :: k
      character(len=80) :: nodename
      character(len=:), allocatable :: chapter
      character(len=:), allocatable :: key
      character(len=:), allocatable :: localvalue
      character(len=:), allocatable :: localvaluetemp
      type(tree_data), pointer :: thechapter
      type(tree_data), pointer :: anode

      !
     !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      chapter = str_tolower(chapterin)
      key = str_tolower(keyin)
      !
      ! Handle chapters
      !
      if (len_trim(chapter) == 0) then
         ignore = .true.
      else
         ignore = chapter(1:1) == '*'
      end if
      !
      ! Find the chapter first
      !
      thechapter => tree
      if (.not. ignore) then
         call tree_get_node_by_name(tree, trim(chapter), thechapter)
         if (.not. associated(thechapter)) then
            thechapter => tree
         end if
      end if
      !
      ! Find the key
      ! To do:
      !    Remove leading blanks
      ! Note:
      !    Work around an apparent problem with the SUN Fortran 90 compiler
      !
      call tree_get_node_by_name(thechapter, trim(key), anode, i)
      isFirst = .true.
      if (associated(anode)) then
         do
            call tree_get_data_alloc_string(anode, localvaluetemp, success_)
            localvalue = localvaluetemp

            ! tree_get_data_string only checks whether key exists (success_ = .true.)
            ! but this prop_get_string is more strict: if value was empty, success_ = .false.
            if (len_trim(localvalue) == 0) then
               success_ = .false.
            else
               !
               ! Remove possible delimiters #
               !
               if (localvalue(1:1) == '#') then
                  localvalue = localvalue(2:)
                  k = index(localvalue, '#')
                  if (k > 0) then
                     localvalue = localvalue(1:k - 1)
                  end if
                  localvalue = adjustl(localvalue)
               end if
               !
               ! Write to parameter "value"
               !
               if (isFirst) then
                  value = localvalue
                  isFirst = .false.
               else
                  value = value//' '//localvalue
               end if
            end if ! empty(localvalue)
            !
            ! Check if the next child node has name " "
            !
            i = i + 1
            if (associated(thechapter%child_nodes) .and. i <= size(thechapter%child_nodes)) then
               nodename = "dummy value"
               nodename = tree_get_name(thechapter%child_nodes(i)%node_ptr)
               if (nodename == " ") then
                  ! Yes: add this data string to parameter "value" (in the next do-loop)
                  anode => thechapter%child_nodes(i)%node_ptr
               else
                  ! No: exit do-loop
                  exit
               end if
            else
               exit
            end if
         end do
         if (size(anode%node_data) > 0) then
            anode%node_visit = anode%node_visit + 1 ! Count visits (request of the value)
         end if
      else
         ! Key not found
      end if
      ! success var is not optional in tree_struct, so we used local placeholder first
      if (present(success)) then
         success = success_
      end if
   end subroutine prop_get_alloc_string
   !
   !
   ! ====================================================================
   subroutine visit_tree(tree, direction)
      type(TREE_DATA), pointer :: tree
      character(len=1), dimension(0) :: data
      logical :: stop
      integer, intent(in) :: direction
      if (direction > 0) then
         call tree_traverse(tree, node_visit, data, stop)
      else
         call tree_traverse(tree, node_unvisit, data, stop)
      end if
   end subroutine visit_tree
   !
   !
   ! ====================================================================
   subroutine node_visit(node, data, stop)
      use TREE_DATA_TYPES
      type(TREE_DATA), pointer :: node
      character(len=1), dimension(:), intent(in) :: data
      logical, intent(inout) :: stop
      if (size(node%node_data) > 0) then
         node%node_visit = node%node_visit + 1 ! Update visit count
      end if
   end subroutine node_visit
   !
   !
   ! ====================================================================
   subroutine node_unvisit(node, data, stop)
      use TREE_DATA_TYPES
      type(TREE_DATA), pointer :: node
      character(len=1), dimension(:), intent(in) :: data
      logical, intent(inout) :: stop
      if (size(node%node_data) > 0) then
         node%node_visit = max(0, node%node_visit - 1) ! Update visit count
      end if
   end subroutine node_unvisit

   !> Get the integer value for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to integer.
   !!
   !!  Comments on this line:
   !!    Only after integer if valuesfirst = .true., otherwise:
   !!    Value is set with the first integer found behind the character "=".
   !!    The following example is allowed:
   !!    IntegerIn = Index 8, denoting the startpoint for searches
   subroutine prop_get_integer(tree, chapter, key, value, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      integer, intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether value should be specified before any comments (optional)
      !
      ! Local variables
      !
      integer, dimension(1) :: valuearray
      !
    !! executable statements -------------------------------------------------------
      !
      valuearray(1) = value
      call prop_get_integers(tree, chapter, key, valuearray, 1, success, valuesfirst)
      value = valuearray(1)
   end subroutine prop_get_integer

   !> Get the array of integer values for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to integers.
   !!    If the string contains less integers than valuelength,
   !!    only the integers found are set in value.
   !!    If the string contains more integers than valuelength,
   !!    only valuelength integers are set in value
   !!
   !!  Comments on this line:
   !!    Only after valuelength integers if valuesfirst = .true., otherwise:
   !!    Everywhere behind the character "=".
   !!    The following example is allowed:
   !!    IntegersIn = (n): 4
   subroutine prop_get_integers(tree, chapter, key, value, valuelength, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      integer, intent(in) :: valuelength !< Size of the array value
      integer, dimension(:), intent(inout) :: value !< Values of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether values should be specified before any comments (optional)
      !
      ! Local variables
      !
      integer :: i
      integer :: k
      integer :: length
      integer :: valcount
      integer :: ierr
      character(12) :: intchars = '0123456789-+'
      character(2) :: spacechars = ' '//achar(9)
      character(20) :: fmt
      character(255) :: avalue
      character(len=:), allocatable :: prop_value
      logical :: valuesfirst_

      !
    !! executable statements -------------------------------------------------------
      !
      call prop_get_alloc_string(tree, chapter, key, prop_value, success)
      if (.not. allocated(prop_value)) prop_value = ' '
      if (present(valuesfirst)) then
         valuesfirst_ = valuesfirst
      else
         valuesfirst_ = .false.
      end if
      !
      ! Extract the integer part
      ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
      ! Has another behaviour as the following implementation:
      !
      do valcount = 1, valuelength
         do
            !
            ! Remove everything before the first integer
            !
            k = 0
            do i = 1, len_trim(prop_value)
               k = index(intchars, prop_value(i:i))
               if (k > 0) exit
               if (valuesfirst_ .and. index(spacechars, prop_value(i:i)) == 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end do
            !
            ! k=0: no integer found
            !
            if (k == 0) return
            prop_value = prop_value(i:len(prop_value))
            !
            ! Move the first integer to avalue
            !
            do i = 1, len(prop_value)
               k = index(intchars, prop_value(i:i))
               if (k == 0) exit
            end do
            avalue = prop_value(1:i - 1)
            prop_value = prop_value(i:len(prop_value))
            length = len_trim(avalue)
            if (length /= 0) then
               write (fmt, '(a,i5,a)') '(i', length, ')'
               read (avalue, fmt, iostat=ierr) value(valcount)
               if (ierr /= 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
               exit
            else
               if (valuesfirst_) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end if
         end do
      end do
   end subroutine prop_get_integers

   !> Get the array of integer values for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to integers.
   !!    If the string contains less integers than valuelength,
   !!    only the integers found are set in value.
   !!    If the string contains more integers than valuelength,
   !!    only valuelength integers are set in value
   !!
   !!  Comments on this line:
   !!    Only after valuelength integers if valuesfirst = .true., otherwise:
   !!    Everywhere behind the character "=".
   !!    The following example is allowed:
   !!    IntegersIn = (n,m): 4,5
   subroutine prop_get_integers_2d(tree, chapter, key, value, valuelength, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      integer, dimension(:, :), intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      integer, intent(in) :: valuelength !< Size of the array value
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether value should be specified before any comments (optional)

      integer, allocatable, dimension(:) :: valuearray

      valuearray = reshape(value, [valuelength])
      call prop_get_integers(tree, chapter, key, valuearray, 1, success, valuesfirst)
      value = reshape(valuearray, shape(value))
   end subroutine prop_get_integers_2d

   !> Get the real value for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to real.
   !!
   !!  Comments on this line:
   !!    Only after real if valuesfirst = .true., otherwise:
   !!    Value is set with the first real found behind the character "=".
   !!    The following example is allowed:
   !!    RealIn = Gravity 9.8, m/s*2
   subroutine prop_get_real(tree, chapter, key, value, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      real(kind=sp), intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether value should be specified before any comments (optional)
      !
      ! Local variables
      !
      real, dimension(1) :: valuearray
      !
    !! executable statements -------------------------------------------------------
      !
      valuearray(1) = value
      call prop_get_reals(tree, chapter, key, valuearray, 1, success, valuesfirst)
      value = valuearray(1)
   end subroutine prop_get_real

   !> Get the array of real values for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to reals.
   !!    If the string contains less reals than valuelength,
   !!    only the reals found are set in value.
   !!    If the string contains more reals than valuelength,
   !!    only valuelength reals are set in value
   !!
   !!  Comments on this line:
   !!    Only after valuelength reals if valuesfirst = .true., otherwise:
   !!    Everywhere behind the character "=".
   !!    The following example is allowed:
   !!    RealsIn = (x,y): 4.5,5.9 Start point
   subroutine prop_get_reals(tree, chapter, key, value, valuelength, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      integer, intent(in) :: valuelength !< Size of the array value
      real, dimension(:), intent(inout) :: value !< Values of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether values should be specified before any comments (optional)
      !
      ! Local variables
      !
      integer :: i
      integer :: k
      integer :: length
      integer :: valcount
      integer :: ierr
      character(15) :: realchars = '0123456789-+.eE'
      character(2) :: spacechars = ' '//achar(9)
      character(20) :: fmt
      character(255) :: avalue
      character(len=:), allocatable :: prop_value
      logical :: digitfound
      logical :: valuesfirst_
      !
      !! executable statements -------------------------------------------------------
      !
      call prop_get_alloc_string(tree, chapter, key, prop_value, success)
      if (.not. allocated(prop_value)) prop_value = ' '
      if (present(valuesfirst)) then
         valuesfirst_ = valuesfirst
      else
         valuesfirst_ = .false.
      end if
      !
      ! Extract the real part
      ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
      ! Has another behaviour as the following implementation:
      !
      do valcount = 1, valuelength
         do
            !
            ! Remove everything before the first real
            !
            digitfound = .false.
            k = 0
            length = len_trim(prop_value)
            if (length < 1) exit
            do i = 1, length
               k = index(realchars, prop_value(i:i))
               if (k > 0) exit
               if (valuesfirst_ .and. index(spacechars, prop_value(i:i)) == 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end do
            !
            ! k=0: no real found
            !
            if (k == 0) return
            prop_value = prop_value(i:len(prop_value))
            !
            ! Move the first real to avalue
            !
            length = len_trim(prop_value)
            do i = 1, length
               k = index(realchars, prop_value(i:i))
               if (k == 0) exit
               if (k <= 10) digitfound = .true.
            end do
            avalue = prop_value(1:i - 1)
            prop_value = prop_value(i:len(prop_value))
            length = len_trim(avalue)
            !
            ! if avalue does not contain a digit, scan the rest of prop_value for reals
            !
            if (digitfound .and. length /= 0) then
               write (fmt, '(a,i0,a)') '(f', length, '.0)'
               read (avalue, fmt, iostat=ierr) value(valcount)
               if (ierr /= 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
               exit
            else
               if (valuesfirst_) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end if
         end do
      end do
   end subroutine prop_get_reals

   !> Get the double-precision real value for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to a double precision real.
   !!
   !!  Comments on this line:
   !!    Only after the double if valuesfirst = .true., otherwise:
   !!    Value is set with the first real found behind the character "=".
   !!    The following example is allowed:
   !!    RealIn = Gravity 9.8, m/s*2
   subroutine prop_get_double(tree, chapter, key, value, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      real(kind=hp), intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether value should be specified before any comments (optional)
      !
      ! Local variables
      !
      real(kind=hp), dimension(1) :: valuearray
      !
    !! executable statements -------------------------------------------------------
      !
      valuearray(1) = value
      call prop_get_doubles(tree, chapter, key, valuearray, 1, success, valuesfirst)
      value = valuearray(1)
   end subroutine prop_get_double

   !> Get the array of double precision real values for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to double precision reals.
   !!    If the string contains less reals than valuelength,
   !!    only the reals found are set in value.
   !!    If the string contains more reals than valuelength,
   !!    only valuelength reals are set in value.
   !!
   !!  Comments on this line:
   !!    Only after valuelength doubles if valuesfirst = .true., otherwise:
   !!    Everywhere behind the character "=".
   !!    The following example is allowed:
   !!    RealsIn = (x,y): 4.5,5.9 Start point
   subroutine prop_get_doubles(tree, chapter, key, value, valuelength, success, valuesfirst)
      implicit none
      type(tree_data), pointer :: tree !< The property tree
      integer, intent(in) :: valuelength !< Size of the array value
      real(kind=hp), dimension(:), intent(inout) :: value !< Values of the key (not set if the key is not found, so you can set a default value)
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, optional, intent(out) :: success !< Whether successful or not (optional)
      logical, optional, intent(in) :: valuesfirst !< Whether values should be specified before any comments (optional)
      !
      ! Local variables
      !
      integer :: i
      integer :: k
      integer :: length
      integer :: valcount
      integer :: ierr
      character(17) :: realchars = '0123456789-+.eEdD'
      character(2) :: spacechars = ' '//achar(9)
      character(20) :: fmt
      character(255) :: avalue
      character(len=:), allocatable :: prop_value
      logical :: digitfound
      logical :: valuesfirst_

      !
      !! executable statements -------------------------------------------------------
      !
      call prop_get_alloc_string(tree, chapter, key, prop_value, success)
      if (.not. allocated(prop_value)) prop_value = ' '
      if (present(valuesfirst)) then
         valuesfirst_ = valuesfirst
      else
         valuesfirst_ = .false.
      end if
      !
      ! Extract the real part
      ! Using read(prop_value,*,iostat=io) (value(i),i=1,valuelength)
      ! Has another behaviour as the following implementation:
      !
      do valcount = 1, valuelength
         do
            !
            ! Remove everything before the first real
            !
            digitfound = .false.
            k = 0
            length = len_trim(prop_value)
            if (length < 1) exit
            do i = 1, length
               k = index(realchars, prop_value(i:i))
               if (k > 0) exit
               if (valuesfirst_ .and. index(spacechars, prop_value(i:i)) == 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end do
            !
            ! k=0: no real found
            !
            if (k == 0) return
            prop_value = prop_value(i:len(prop_value))
            !
            ! Move the first real to avalue
            !
            length = len_trim(prop_value)
            do i = 1, length
               k = index(realchars, prop_value(i:i))
               if (k == 0) exit
               if (k <= 10) digitfound = .true.
            end do
            avalue = prop_value(1:i - 1)
            prop_value = prop_value(i:len(prop_value))
            length = len_trim(avalue)
            !
            ! if avalue does not contain a digit, scan the rest of prop_value for reals
            !
            if (digitfound .and. length /= 0) then
               write (fmt, '(a,i0,a)') '(f', length, '.0)'
               read (avalue, fmt, iostat=ierr) value(valcount)
               if (ierr /= 0) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
               exit
            else
               if (valuesfirst_) then
                  if (present(success)) then
                     success = .false.
                  end if
                  return
               end if
            end if
         end do
      end do
   end subroutine prop_get_doubles

   !> Get the logical value for a property
   !!    Use prop_get_string to get the string value.
   !!    Convert it to logical.
   !!    Allowed strings to detect the value true (case-insensitive):
   !!    |1|Y|YES|T|TRUE|J|JA|W|WAAR|ON|
   !!    Allowed strings to detect the value false (case-insensitive):
   !!    |0|N|NO|F|FALSE|N|NEE|O|ONWAAR|OFF|
   !!
   !!  Comments on this line:
   !!    Not allowed
   subroutine prop_get_logical(tree, chapter, key, value, success, value_parsed)
      use string_module, only: str_toupper
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter (case-insensitive) or "*" to get any key
      character(*), intent(in) :: key !< Name of the key (case-insensitive)
      logical, intent(inout) :: value !< Value of the key (not set if the key is not found, so you can set a default value)
      logical, optional, intent(out) :: success !< Whether key was present and value not empty
      logical, optional, intent(out) :: value_parsed !< Whether value was successfully parsed
      !
      ! Local variables
      !
      integer :: k1
      integer :: k2
      integer :: spacepos
      integer :: vallength
      character(100) :: falsity
      character(100) :: truth
      character(len=:), allocatable :: prop_value
      !
      if (present(value_parsed)) then
         value_parsed = .false.
      end if

      truth = '|1|Y|YES|T|TRUE|.TRUE.|J|JA|W|WAAR|ON|'
      falsity = '|0|N|NO|F|FALSE|.FALSE.|N|NEE|O|ONWAAR|OFF|'
      !
    !! executable statements -------------------------------------------------------
      !
      call prop_get_alloc_string(tree, chapter, key, prop_value, success)
      if (.not. allocated(prop_value)) prop_value = ' '
      vallength = len_trim(prop_value)
      !
      ! Leave immediately in case prop_value is empty
      !
      if (vallength == 0) return
      spacepos = index(prop_value, ' ')
      if (spacepos > 0) vallength = min(spacepos - 1, vallength)
      !
      ! Extract the logical part
      !
      k1 = index(truth, str_toupper(prop_value(1:vallength)))
      k2 = index(falsity, str_toupper(prop_value(1:vallength)))
      !
      ! The value must match a complete word in string truth or falsity, bordered by two '|'s
      !
      if (k1 > 0) then
         if (truth(k1 - 1:k1 - 1) == '|' .and. truth(k1 + vallength:k1 + vallength) == '|') then
            value = .true.
            if (present(value_parsed)) then
               value_parsed = .true.
            end if
         end if
      end if
      if (k2 > 0) then
         if (falsity(k2 - 1:k2 - 1) == '|' .and. falsity(k2 + vallength:k2 + vallength) == '|') then
            value = .false.
            if (present(value_parsed)) then
               value_parsed = .true.
            end if
         end if
      end if
   end subroutine prop_get_logical
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_string(tree, subtree, value)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      character(*), intent(inout) :: value
      character(*), intent(in) :: subtree
      !
      ! With the optional parameter success, prop_get_subtree_string, can not be distinguished from
      ! prop_get_logical anymore.
      ! Clean solution: make parameter success not optional in all prop_get subroutines
      !logical        , optional, intent (out)   :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
    !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_string(tree, "*", subtree, value, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_string(node_ptr, subtree(separator + 1:), value)
         end if
      end if
      ! if (present(success)) success = success_
   end subroutine prop_get_subtree_string
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_integer(tree, subtree, value, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      integer, intent(inout) :: value
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_integer(tree, "*", subtree, value, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_integer(node_ptr, subtree(separator + 1:), value, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_integer
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_integers(tree, subtree, value, valuelength, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      integer, dimension(:), intent(inout) :: value
      integer, intent(in) :: valuelength
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
    !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_integers(tree, "*", subtree, value, valuelength, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_integers(node_ptr, subtree(separator + 1:), value, valuelength, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_integers
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_real(tree, subtree, value, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      real(sp), intent(inout) :: value
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
    !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_real(tree, "*", subtree, value, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_real(node_ptr, subtree(separator + 1:), value, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_real
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_reals(tree, subtree, value, valuelength, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      real(sp), dimension(:), intent(inout) :: value
      integer, intent(in) :: valuelength
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_reals(tree, "*", subtree, value, valuelength, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_reals(node_ptr, subtree(separator + 1:), value, valuelength, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_reals
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_double(tree, subtree, value, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      real(hp), intent(inout) :: value
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_double(tree, "*", subtree, value, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_double(node_ptr, subtree(separator + 1:), value, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_double
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_doubles(tree, subtree, value, valuelength, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      real(hp), dimension(:), intent(inout) :: value
      integer, intent(in) :: valuelength
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_doubles(tree, "*", subtree, value, valuelength, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_doubles(node_ptr, subtree(separator + 1:), value, valuelength, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_doubles
   !
   !
   ! ====================================================================
   ! subtree: String containing multiple tree nodes, correctly ordered,
   !          separated by a forward slash (/)
   ! method: Recursively call this subroutine for each level in subtree,
   !         until there is only one level left. Then call the corresponding
   !         subroutine without parameter subtree, with chapter="*" and key=subtree
   recursive subroutine prop_get_subtree_logical(tree, subtree, value, success)
      !
      ! Parameters
      !
      type(tree_data), pointer :: tree
      logical, intent(inout) :: value
      character(*), intent(in) :: subtree
      logical, optional, intent(out) :: success
      !
      ! Local variables
      !
      integer :: separator
      type(tree_data), pointer :: node_ptr
      logical :: success_
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      separator = index(subtree, '/')
      if (separator == 0) then
         !
         ! No multiple level key (anymore)
         ! Handle it normally
         !
         call prop_get_logical(tree, "*", subtree, value, success_)
      else
         call tree_get_node_by_name(tree, subtree(:separator - 1), node_ptr)
         if (associated(node_ptr)) then
            call prop_get_subtree_logical(node_ptr, subtree(separator + 1:), value, success_)
         end if
      end if
      if (present(success)) success = success_
   end subroutine prop_get_subtree_logical
   !
   !
   ! ====================================================================
   !> The generic routine for setting key-value data in the tree.
   !! The value (of any type) should be transferred into the type of node_value.
   subroutine prop_set_data(tree, chapter, key, value, type_string, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      character(len=1), intent(in) :: value(:) !< Value of the property
      character(*), intent(in) :: type_string !< Data type of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful

      character(len=1), allocatable :: valueline(:)
      logical :: ignore
      logical :: success_
      type(tree_data), pointer :: thechapter
      type(tree_data), pointer :: anode
      integer :: i, ianno, nanno, nval, nvalanno

      success_ = .false.

      ignore = len(chapter) == 0
      if (.not. ignore) ignore = chapter(1:1) == '*'

      ! Find the chapter first
      if (ignore) then
         thechapter => tree
      else
         call tree_get_node_by_name(tree, trim(chapter), thechapter)

         ! If chapter does not exist, create it.
         if (.not. associated(thechapter)) then
            call tree_create_node(tree, trim(chapter), thechapter)
         end if
      end if

      ! Create the node for key (not looking for earlier definitions)
      call tree_create_node(thechapter, trim(key), anode)
      if (associated(anode)) then
         ! Determine value length and optional annotation length
         ! and combine the two in valueline(:) (separated by ' # ')
         nval = size(value)
         if (present(anno)) then
            ianno = max(20, nval) ! column nr where anno will start
            nanno = len_trim(anno)
            nvalanno = ianno + nanno + 2 ! Separate by ' # '
         else
            ianno = nval
            nanno = 0
            nvalanno = nval
         end if

         allocate (valueline(nvalanno))
         valueline = ' '

         do i = 1, nval
            valueline(i) = value(i)
         end do

         if (nanno > 0) then
            valueline(ianno + 1) = '#'
         end if

         do i = 1, nanno
            valueline(ianno + 2 + i) = anno(i:i)
         end do
         call tree_put_data(anode, transfer(valueline, node_value), trim(type_string), success_)
         deallocate (valueline)
      end if

      if (present(success)) then
         success = success_
      end if
   end subroutine prop_set_data
   !
   !
   ! ====================================================================
   !> Sets a string property in the tree.
   !! Take care of proper quoting (e.g., by "" or ##) at the call site.
   subroutine prop_set_string(tree, chapter, key, value, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      character(len=*), intent(in) :: value !< Value of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful
      logical :: success_

      if (present(anno)) then
         call prop_set_data(tree, chapter, key, transfer(value, node_value), 'STRING', anno=anno, success=success_)
      else
         call prop_set_data(tree, chapter, key, transfer(value, node_value), 'STRING', success=success_)
      end if

      if (present(success)) then
         success = success_
      end if
   end subroutine prop_set_string
   !
   !
   ! ====================================================================
   !> Sets a double precision array property in the tree.
   !! The property value is stored as a string representation.
   subroutine prop_set_doubles(tree, chapter, key, value, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      real(kind=hp), intent(in) :: value(:) !< Value of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful

      logical :: success_
      character(len=:), allocatable :: strvalue
      character(len=24) :: strscalar
      integer :: i, n

      n = size(value)
      if (n == 0) then
         strvalue = ' '
      else
         ! Pretty print all doubles into strvalue, separated by single spaces
         call pp_double(value(1), strscalar)
         strvalue = trim(strscalar)
         do i = 2, n
            call pp_double(value(i), strscalar)
            strvalue = strvalue//' '//trim(strscalar)
         end do
      end if

      ! Put the string representation into the tree
      if (present(anno)) then
         call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', anno=anno, success=success_)
      else
         call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', success=success_)
      end if

      if (present(success)) then
         success = success_
      end if

   end subroutine prop_set_doubles
   !
   !
   ! ====================================================================
   !> Sets a double precision property in the tree.
   !! The property value is stored as a string representation.
   subroutine prop_set_double(tree, chapter, key, value, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      double precision, intent(in) :: value !< Value of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful

      logical :: success_
      real(kind=hp) :: valuearray(1)

      valuearray(1) = value

      if (present(anno)) then
         call prop_set(tree, chapter, key, valuearray, anno=anno, success=success_)
      else
         call prop_set(tree, chapter, key, valuearray, success=success_)
      end if

      if (present(success)) then
         success = success_
      end if

   end subroutine prop_set_double
   !
   !
   ! ====================================================================
   !> Sets an integer array property in the tree.
   !! The property value is stored as a string representation.
   subroutine prop_set_integers(tree, chapter, key, value, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      integer, intent(in) :: value(:) !< Value of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful

      logical :: success_
      character(len=max_length) :: strvalue
      character(len=24) :: strscalar
      integer :: i, is, iv, n

      strvalue = ' '
      n = size(value)
      if (n > 0) then
         ! Pretty print all integers into strvalue, separated by single spaces
         write (strvalue, *) value(1)
         strvalue = adjustl(strvalue)
         iv = len_trim(strvalue)
         do i = 2, n
            write (strscalar, *) value(i)
            strscalar = adjustl(strscalar)
            is = len_trim(strscalar)
            strvalue(iv + 2:iv + is + 1) = strscalar(1:is)
            iv = iv + is + 1
         end do
      end if

      ! Put the string representation into the tree
      if (present(anno)) then
         call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', anno=anno, success=success_)
      else
         call prop_set_data(tree, chapter, key, transfer(trim(strvalue), node_value), 'STRING', success=success_)
      end if

      if (present(success)) then
         success = success_
      end if

   end subroutine prop_set_integers
   !
   !
   ! ====================================================================
   !> Sets an integer property in the tree.
   !! The property value is stored as a string representation.
   subroutine prop_set_integer(tree, chapter, key, value, anno, success)
      type(tree_data), pointer :: tree !< The property tree
      character(*), intent(in) :: chapter !< Name of the chapter under which to store the property ('' or '*' for global)
      character(*), intent(in) :: key !< Name of the property
      integer, intent(in) :: value !< Value of the property
      character(len=*), optional, intent(in) :: anno !< Optional annotation/comment
      logical, optional, intent(out) :: success !< Returns whether the operation was successful

      logical :: success_
      integer :: valuearray(1)

      valuearray(1) = value

      if (present(anno)) then
         call prop_set_integers(tree, chapter, key, valuearray, anno=anno, success=success_)
      else
         call prop_set_integers(tree, chapter, key, valuearray, success=success_)
      end if

      if (present(success)) then
         success = success_
      end if

   end subroutine prop_set_integer
   !
   !
   ! ====================================================================
   !> Prettyprints a double precision real to a character string
   !! Trailing zeros and leading blanks are removed.
   subroutine pp_double(value, strvalue)
      ! A bit ad-hoc prettyprinting, intended for easy readable output in settings files.
      real(kind=hp), intent(in) :: value
      character(len=*), intent(out) :: strvalue

      ! adjustl not working in gfortran, so writing to a temp array
      character(len=10000) :: strtmp

      integer :: i, iz, j, n

      write (strtmp, *) value

      i = index(strtmp, '.')
      if (i == 0) then
         strtmp = adjustl(strtmp)
         return
      end if

      n = len_trim(strtmp)
      iz = -1
      do
         i = i + 1
         if (i == n + 1) then
            ! End of number string, erase any trailing zeros.
            if (iz > 0) then
               strtmp(iz:n) = ' '
            end if
            exit
         end if

         ! Check for a zero, mark position if the previous char wasn't already a zero.
         if (strtmp(i:i) == '0') then
            if (iz < 0) then
               iz = i
            end if
            cycle
         else if (index('EeDd', strtmp(i:i)) > 0) then
            if (iz > 0) then ! Place exponent part over tail of trailing zeros.
               do j = i + 2, n
                  if (strtmp(j:j) /= '0') exit
               end do
               if (j == n + 1) then ! Entirely remove 'E+000'
                  strtmp(iz:n) = ' '
               else
                  strtmp(iz:iz) = 'd'
                  strtmp(iz + 1:iz + 1) = strtmp(i + 1:i + 1)
                  strtmp(iz + 2:iz + n - j + 2) = strtmp(j:n)
                  strtmp(iz + n - j + 3:n) = ' '
               end if
            end if
            exit
         else
            ! No zero nor exponent, unset the trailing zero position
            iz = -1
         end if
      end do

      strvalue = adjustl(trim(strtmp))
   end subroutine pp_double
   subroutine count_occurrences(input_ptr, group, keyword, npars)
      !
      ! Dummy variables
      !
      integer :: npars
      character(*) :: group
      character(*) :: keyword
      type(tree_data), pointer :: input_ptr
      !
      ! Local variables
      !
      integer :: i
      character(80) :: parname
      type(tree_data), pointer :: group_ptr
      type(tree_data), pointer :: node_ptr
      !
    !! executable statements -------------------------------------------------------
      !
      ! Initialise parameters
      !
      i = 0
      npars = 0
      !
      ! Find the group in the input tree
      !
      call tree_get_node_by_name(input_ptr, group, group_ptr)
      !
      ! Read dimensions from input tree
      !
      if (associated(group_ptr%child_nodes)) then
         do i = 1, size(group_ptr%child_nodes)
            !
            ! Does group_ptr contain one or more children with name keyword?
            !
            node_ptr => group_ptr%child_nodes(i)%node_ptr
            parname = tree_get_name(node_ptr)
            if (parname == keyword) then
               npars = npars + 1
            end if
         end do
      end if
   end subroutine count_occurrences

   subroutine prop_get_strings(tree, chapterin, keyin, valuelength, value, success, spChar)

      implicit none
      !
      ! Parameters
      !
      type(tree_data), pointer, intent(in) :: tree
      character(*), intent(in) :: chapterin
      character(*), intent(in) :: keyin
      integer, intent(in) :: valuelength
      character(len=*), dimension(:), intent(inout) :: value
      logical, intent(out) :: success
      character(1), optional :: spChar
      !
      ! Local variables
      !
      logical :: ignore
      logical :: success_
      integer :: i ! Childnode number with node_name = key
      ! All following child nodes with node_name = " " are also added
      character(255) :: chapter
      character(255) :: key
      character(1) :: sepChar
      character(len=:), allocatable :: localvalue
      type(tree_data), pointer :: thechapter
      type(tree_data), pointer :: anode
      integer :: icount
      integer :: ipos

      allocate (character(maxlen) :: localvalue)
      !
      !! executable statements -------------------------------------------------------
      !
      success_ = .false.
      chapter = str_tolower(chapterin)
      key = str_tolower(keyin)
      localvalue(1:maxlen) = ' '

      ! Determine separation character
      if (present(spChar)) then
         sepChar = spChar
      else
         sepChar = ';'
      end if
      !
      ! Handle chapters
      !
      ignore = chapter(1:1) == '*' .or. len_trim(chapter) == 0
      !
      ! Find the chapter first
      !
      thechapter => tree
      if (.not. ignore) then
         call tree_get_node_by_name(tree, trim(chapter), thechapter)
         if (.not. associated(thechapter)) then
            thechapter => tree
         end if
      end if
      !
      ! Find the key
      ! To do:
      !    Remove leading blanks
      ! Note:
      !    Work around an apparent problem with the SUN Fortran 90
      !    compiler
      !
      call tree_get_node_by_name(thechapter, trim(key), anode, i)

      if (associated(anode)) then

         call tree_get_data_string(anode, localvalue, success)
         if (.not. success) return
         localvalue = trim(localvalue)

         ipos = scan(localvalue, sepChar)
         icount = 0

         do while (ipos > 0 .and. icount < valuelength)

            icount = icount + 1

            value(icount) = trim(adjustl(localvalue(1:ipos - 1)))

            localvalue = localvalue(ipos + 1:)

            ipos = scan(localvalue, sepChar)

            ! Pickup the last one

         end do

         if (ipos == 0 .and. icount < valuelength) then
            icount = icount + 1
            value(icount) = trim(adjustl(localvalue))
         end if

      else
         success = .false.
      end if

   end subroutine prop_get_strings

   !> Returns the version number found in the ini file default location is "[General], fileVersion".
   !! FileVersion should contain <<major>>.<<minor>><<additional info>>.
   !! SUCCESS is set to false, when no '.' is found or when the key cannot be found.
   subroutine prop_get_version_number(tree, chapterin, keyin, major, minor, versionstring, success)
      use MessageHandling

      implicit none
      !
      ! Parameters
      !
      type(tree_data), pointer, intent(in) :: tree !< pointer to treedata structure of input
      character(*), intent(in), optional :: chapterin !< chapter for the fileVersion, if not present 'General' is used
      character(*), intent(in), optional :: keyin !< key for fileVersion, if not present 'fileVersion' is used
      integer, intent(out), optional :: major !< Major number of the fileVersion
      integer, intent(out), optional :: minor !< Minor number of the fileVersion
      character(len=*), intent(out), optional :: versionstring !< Version string
      logical, intent(out), optional :: success !< Returns whether fileVersion is found

      character(len=IdLen) :: chapterin_
      character(len=IdLen) :: keyin_
      character(len=IdLen) :: string
      logical :: isnum
      integer :: idot
      integer :: iend
      integer :: major_, minor_

      if (present(chapterin)) then
         chapterin_ = chapterin
      else
         chapterin_ = 'General'
      end if
      if (present(keyin)) then
         keyin_ = keyin
      else
         keyin_ = 'fileVersion'
      end if

      call prop_get_string(tree, chapterin_, keyin_, string, success)
      if (.not. success) then
         return
      end if

      if (present(versionstring)) then
         versionstring = string
      end if

      call get_version_major_minor_integer(string, major_, minor_, success)
      if (.not. success) then
         return
      end if
      if (present(major)) then
         major = major_
      end if
      if (present(minor)) then
         minor = minor_
      end if
   end subroutine prop_get_version_number
end module properties
