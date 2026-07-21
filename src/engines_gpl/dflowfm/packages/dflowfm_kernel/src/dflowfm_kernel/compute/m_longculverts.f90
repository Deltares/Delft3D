!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> Module for long culvert data in a dflowfm model.
!! Long culverts are read from the structures.ini file(s), and converted into
!! new netlinks and prof1D definitions.
module m_longculverts
   use precision, only: dp
   use m_getflowdir
   use MessageHandling
   use m_missing
   use iso_c_binding
   use m_longculverts_data

   implicit none

   private
   public realloc

   public default_longculverts
   public reset_longculverts
   public loadLongCulvertsAsNetwork
   public convertLongCulvertsAsNetwork
   public finalizeLongCulvertsInNetwork
   public LongCulvertsToProfs
   public setFrictionForLongculverts
   public reduceFlowAreaAtLongculverts
   public get_valve_relative_opening_c_loc
   public find1d2dculvertlinks
   public initialize_Long_Culverts
   public convert1D2DLongCulverts
   public makelongculverts_commandline

   interface realloc
      module procedure reallocLongCulverts
   end interface

contains

   !> Sets ALL (scalar) variables in this module to their default values.
   !! For a reinit prior to flow computation, call reset_longculverts() instead.
   subroutine default_longculverts()
      if (allocated(longculverts)) then
         deallocate (longculverts)
      end if

      nlongculverts = 0 !< Number of longculverts
      newculverts = .false.
      ! Remaining of variables is handled in reset_longculverts()
      call reset_longculverts()
   end subroutine default_longculverts

   !> Resets only long culverts variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, use default_longculverts() instead.
   subroutine reset_longculverts()
      ! NOT: intentionally not resetting nlongculverts counter here, because that is part of model loading.
   end subroutine reset_longculverts

   !> Loads the long culverts from a structures.ini file and
   !! creates extra netnodes+links for them.
   subroutine convertLongCulvertsAsNetwork(structurefile, jaKeepExisting, culvertprefix, structures_output, crsdef_output, ierr, crsdeffile, write_converted_files)
      use dfm_error, only: dfm_noerr, dfm_wronginput
      use m_missing, only: dmiss
      use m_Roughness, only: frictiontypestringtointeger, frictionTypeIntegerToString
      use m_readstructures, only: allowedflowdirtoint, get_value_or_addto_forcinglist
      use messagehandling, only: mess, level_error, msgbuf, err_flush, setmessage
      use properties, only: tree_create, prop_inifile, tree_create_node, tree_put_data, node_value, tree_num_nodes, tree_get_name, prop_get, prop_set, tree_remove_child_by_name, prop_write_inifile, tree_destroy
      use unstruc_channel_flow, only: st_longculvert, network
      use m_save_ugrid_state, only: nbranchids
      use system_utils, only: split_filename, cat_filename
      use string_module, only: strcmpi
      use m_filez, only: newfil
      use tree_data_types, only: tree_data
      use tree_structures, only: maxlen
      use m_readCrossSections, only: parseCrossSectionDefinitionFile
      use m_CrossSections, only: fill_hashtable
      use m_alloc, only: realloc
      use m_read_location_info, only: read_polyline_coordinates
      use m_physcoef, only: IFRCTYPUNI, FRCUNI1D

      implicit none

      character(len=*), intent(inout) :: structurefile !< File name of the structure.ini file.
      integer, intent(in) :: jaKeepExisting !< Whether or not (1/0) to keep the existing already read long culverts.
      character(len=*), intent(in) :: culvertprefix !< Command line argument prefix to add to the converted files
      character(len=:), allocatable, intent(out) :: structures_output !< structures ini output file ( = culvertprefix // structurefile )
      character(len=:), allocatable, intent(out) :: crsdef_output !< crs def ini output file
      character(len=*), optional, intent(in) :: crsdeffile !< File name of the original crsdef.ini file.
      logical, optional, intent(in) :: write_converted_files !< Whether or not to write the converted structures and cross-sections files. (default = .false.)
      integer, intent(out) :: ierr !< Result status, DFM_NOERR in case of success.

      character(len=128) :: crsdef_filename
      character(len=:), allocatable :: line
      type(tree_data), pointer :: prop_ptr
      type(tree_data), pointer :: block_ptr
      type(tree_data), pointer :: node_ptr
      type(tree_data), pointer :: strs_ptr
      type(tree_data), pointer :: str_ptr
      character(len=IdLen) :: typestr
      character(len=IdLen) :: st_id
      character(len=IdLen) :: csDefId
      character(len=IdLen) :: txt
      integer :: readerr, nstr, i, numcoords
      logical :: success
      integer :: istart
      integer :: nlongculverts0
      integer :: mout
      character(len=IdLen) :: temppath, tempname, tempext
      logical :: write_converted_files_
      integer :: longculvertindex
      real(kind=dp), allocatable :: x_coordinates(:), y_coordinates(:), z_coordinates(:)
      real(kind=dp), allocatable :: all_x_coordinates(:), all_y_coordinates(:), all_z_coordinates(:)
      integer :: all_coordinates_size
      integer :: num_columns

      ierr = DFM_NOERR

      crsdef_filename = 'crsdef.ini'
      ! Determine new crsdef file name
      if (present(crsdeffile)) then
         call split_filename(crsdeffile, temppath, tempname, tempext)
         crsdef_filename = trim(culvertprefix)//tempname
         crsdef_output = cat_filename(temppath, crsdef_filename, tempext)
      else
         crsdef_filename = trim(culvertprefix)//crsdef_filename
         crsdef_output = crsdef_filename
      end if

      write_converted_files_ = .false.
      if (present(write_converted_files)) then
         write_converted_files_ = write_converted_files
      end if

      ! Determine new structures file name
      call split_filename(structurefile, temppath, tempname, tempext)
      tempname = trim(culvertprefix)//tempname
      structures_output = cat_filename(temppath, tempname, tempext)

      allocate (character(maxlen) :: line)

      nlongculverts0 = nlongculverts ! Remember any old longculvert count

      if (jaKeepExisting == 0) then
         nlongculverts = 0
         if (allocated(longculverts)) then
            deallocate (longculverts)
         end if
      end if

      if (present(crsdeffile)) then
         call tree_create(trim(crsdeffile), prop_ptr)
         call prop_inifile(crsdeffile, prop_ptr, readerr)
         !check if file was successfully opened
         if (readerr /= 0) then
            ierr = DFM_WRONGINPUT
            call mess(LEVEL_ERROR, 'Error opening file ''', trim(crsdeffile), ''' for loading the long culverts.')
         end if
      else
         call tree_create(trim(crsdef_filename), prop_ptr)
         call tree_create_node(prop_ptr, 'General', block_ptr)
         call tree_create_node(block_ptr, 'fileVersion', node_ptr)
         call tree_put_data(node_ptr, transfer('3.00', node_value), 'STRING') ! fileVersion           = 3.00

         call tree_create_node(block_ptr, 'fileType', node_ptr)
         call tree_put_data(node_ptr, transfer("crossDef", node_value), 'STRING') !fileType = crossDef
      end if
      ! Temporarily put structures.ini file into a property tree
      call tree_create(trim(structurefile), strs_ptr)
      call prop_inifile(structurefile, strs_ptr, readerr)
      !check if file was successfully opened
      if (readerr /= 0) then
         ierr = DFM_WRONGINPUT
         call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
      end if

      nstr = tree_num_nodes(strs_ptr)
      call realloc(longculverts, nlongculverts + nstr)
      all_coordinates_size = 0

      do i = 1, nstr
         str_ptr => strs_ptr%child_nodes(i)%node_ptr

         success = .true.

         if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
            ! Only read [Structure] blocks, skip any other (e.g., [General]).
            cycle
         end if

         typestr = ' '
         call prop_get(str_ptr, '', 'type', typestr, success)
         if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
            cycle
         end if

         nlongculverts = nlongculverts + 1

         call prop_get(str_ptr, '', 'id', st_id, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
            call err_flush()
         end if

         if (success) then
            call read_polyline_coordinates(str_ptr, trim(st_id), structurefile, temppath, 'longCulvert', &
                                           x_coordinates, y_coordinates, z_coordinates, num_columns, success)
         end if

         if (success) then
            numcoords = size(x_coordinates)

            call tree_create_node(prop_ptr, 'Definition', block_ptr)
            csDefId = 'CsDef_longCulvert_'//trim(st_id)
            call prop_set(block_ptr, '', 'id', csDefId)
            call prop_set(str_ptr, '', 'csDefId', csDefId) ! Directly refer to this new csdef in the converted structure.
            longculverts(nlongculverts)%csdefid = csDefId
            call prop_set(block_ptr, '', 'type', 'rectangle')

            longculverts(nlongculverts)%id = st_id
            longculverts(nlongculverts)%numlinks = numcoords - 1
            allocate (longculverts(nlongculverts)%netlinks(longculverts(nlongculverts)%numlinks))
            allocate (longculverts(nlongculverts)%flowlinks(longculverts(nlongculverts)%numlinks))
            longculverts(nlongculverts)%flowlinks = -999

            longculverts(nlongculverts)%xcoords = x_coordinates
            longculverts(nlongculverts)%ycoords = y_coordinates
            allocate (longculverts(nlongculverts)%bl(numcoords))
            longculverts(nlongculverts)%bl = z_coordinates

            ! Append this culvert's coordinates (plus dmiss separator) to the local concatenated arrays.
            call realloc(all_x_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            call realloc(all_y_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            call realloc(all_z_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            all_x_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = x_coordinates
            all_y_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = y_coordinates
            all_z_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = z_coordinates
            ! all_x_coordinates(all_coordinates_size + numcoords + 1) stays dmiss: the segment separator.
            all_coordinates_size = all_coordinates_size + numcoords + 1

            txt = 'both'
            call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
            longculverts(nlongculverts)%allowed_flowdir = allowedFlowDirToInt(txt)

            call prop_get(str_ptr, '', 'width', longculverts(nlongculverts)%width, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'width not found for long culvert: '//st_id)
            end if
            call prop_get(str_ptr, '', 'width', typestr)
            call prop_set(block_ptr, '', 'width', trim(typestr))

            call prop_get(str_ptr, '', 'height', longculverts(nlongculverts)%height, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'height not found for long culvert: '//st_id)
            end if
            call prop_get(str_ptr, '', 'height', typestr)
            call prop_set(block_ptr, '', 'height', trim(typestr))

            call prop_set(block_ptr, '', 'closed', 'yes')

            call prop_get(str_ptr, '', 'frictionType', typestr, success)
            if (.not. success) then
               longculverts(nlongculverts)%friction_type = ifrctypuni
            else
               call frictionTypeStringToInteger(typestr, longculverts(nlongculverts)%friction_type)
            end if

            call tree_create_node(block_ptr, 'frictionType', node_ptr)
            call tree_put_data(node_ptr, transfer(frictionTypeIntegerToString(longculverts(nlongculverts)%friction_type), node_value), 'STRING')

            call prop_get(str_ptr, '', 'frictionValue', longculverts(nlongculverts)%friction_value, success)
            if (.not. success) then
               longculverts(nlongculverts)%friction_value = frcuni1D
            end if

            call prop_set(block_ptr, '', 'frictionValue', longculverts(nlongculverts)%friction_value)

            call get_value_or_addto_forcinglist(str_ptr, 'valveRelativeOpening', longculverts(nlongculverts)%valve_relative_opening, st_id, &
                                                ST_LONGCULVERT, network%forcinglist, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'valveRelativeOpening not found for long culvert: '//st_id)
            end if
         else
            call SetMessage(LEVEL_ERROR, 'numCoordinates not found for long culvert '//st_id)
         end if

         call tree_remove_child_by_name(str_ptr, 'frictionValue', istart)
         call tree_remove_child_by_name(str_ptr, 'frictionType', istart)
         call tree_remove_child_by_name(str_ptr, 'height', istart)
         call tree_remove_child_by_name(str_ptr, 'width', istart)

         if (.not. success) then
            ! Some error during reading: decrement counter to ignore this long culvert.
            nlongculverts = nlongculverts - 1
         end if
      end do

      call convert1D2DLongCulverts(all_x_coordinates, all_y_coordinates, all_z_coordinates, all_coordinates_size)
      call replaceCoordinatesInStructures(all_x_coordinates, all_y_coordinates, strs_ptr)

      longculvertindex = nlongculverts0
      ! Loop all structures once again, and for long culverts: add the newly created branchids.
      do i = 1, nstr
         str_ptr => strs_ptr%child_nodes(i)%node_ptr

         success = .true.

         if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
            ! Only read [Structure] blocks, skip any other (e.g., [General]).
            cycle
         end if
         typestr = ' '
         call prop_get(str_ptr, '', 'type', typestr, success)
         if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
            cycle
         end if

         call prop_get(str_ptr, '', 'id', st_id, success)

         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
            call err_flush()
         else
            longculvertindex = longculvertindex + 1
            if (size(longculverts(longculvertindex)%netlinks) > 2) then
               call prop_set(str_ptr, '', 'branchId', longculverts(longculvertindex)%branchId)
               call add_longculvert_branch(network, longculverts(longculvertindex))
            else
               call prop_set(str_ptr, '', 'contactId', longculverts(longculvertindex)%contactId)
            end if
         end if
      end do

      ! Write converted cross sections file.
      if (write_converted_files_) then
         call newfil(mout, crsdef_output)
         if (mout == 0) then
            call SetMessage(LEVEL_ERROR, 'Failed to open file '''//trim(crsdef_output)//''' for writing.')
         else
            call prop_write_inifile(mout, prop_ptr, ierr)
            close (mout)
         end if
      end if

      call parseCrossSectionDefinitionFile(prop_ptr, network)
      call tree_destroy(prop_ptr)
      call fill_hashtable(network%CSDefinitions)

      ! Write converted structures file.
      if (write_converted_files_) then
         call newfil(mout, structures_output)
         if (mout == 0) then
            call SetMessage(LEVEL_ERROR, 'Failed to open file '''//trim(structures_output)//''' for writing.')
         else
            call prop_write_inifile(mout, strs_ptr, ierr)
         end if
      end if
      call tree_destroy(strs_ptr)

   end subroutine convertLongCulvertsAsNetwork

   subroutine replaceCoordinatesInStructures(xcoords, ycoords, structures)
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_num_nodes, tree_remove_child_by_name
      use properties, only: prop_get, prop_set
      use messagehandling, only: msgbuf, err_flush, IDLEN
      use string_module, only: strcmpi
      use m_longculverts_data, only: longculverts
      implicit none

      real(kind=dp), intent(in) :: xcoords(:)
      real(kind=dp), intent(in) :: ycoords(:)
      type(tree_data), pointer, intent(inout) :: structures

      type(tree_data), pointer :: current
      character(len=IDLEN) :: typestr
      integer :: i, j, coordindex, longculvertindex, ncoords, ierror
      logical :: success

      coordindex = 1
      do i = 1, nlongculverts !< save adjusted all_x_coordinates and all_y_coordinates to new structure file
         longculvertindex = 0
         do j = 1, tree_num_nodes(structures) !> check all structure file blocks
            current => structures%CHILD_NODES(j)%node_ptr
            call prop_get(current, '', 'type', typestr, success)
            if (success .and. strcmpi(typestr, 'longCulvert')) then
               longculvertindex = longculvertindex + 1
               if (longculvertindex == i) then
                  ncoords = size(longculverts(i)%xcoords)
                  call tree_remove_child_by_name(current, 'xCoordinates', ierror)
                  if (ierror /= 0) then
                     write (msgbuf, '(A,I0)') 'Error Removing xCoordinates from structure #', j
                     call err_flush()
                  end if
                  call tree_remove_child_by_name(current, 'yCoordinates', ierror)
                  if (ierror /= 0) then
                     write (msgbuf, '(A,I0)') 'Error Removing xCoordinates from structure #', j
                     call err_flush()
                  end if
                  call prop_set(current, '', 'xCoordinates', xcoords(coordindex:coordindex + ncoords - 1), '')
                  call prop_set(current, '', 'yCoordinates', ycoords(coordindex:coordindex + ncoords - 1), '')
                  longculverts(i)%xcoords = xcoords(coordindex:coordindex + ncoords - 1)
                  longculverts(i)%ycoords = ycoords(coordindex:coordindex + ncoords - 1)
                  coordindex = coordindex + ncoords + 1
                  exit
               end if
            end if
         end do
      end do
   end subroutine

   !> Loads the long culverts from a structures.ini file and
   !! creates extra netnodes+links for them.
   subroutine loadLongCulvertsAsNetwork(structurefile, jaKeepExisting, ierr)
      use dfm_error
      use string_module, only: strcmpi
      use m_Roughness
      use m_readstructures
      use m_network
      use messageHandling
      use properties
      use unstruc_channel_flow
      use system_utils, only: split_filename
      use m_alloc, only: realloc
      use m_missing, only: dmiss
      use m_read_location_info, only: read_polyline_coordinates
      use m_physcoef, only: IFRCTYPUNI, FRCUNI1D

      implicit none

      character(len=*), intent(in) :: structurefile !< File name of the structure.ini file.
      integer, intent(in) :: jaKeepExisting !< Whether or not (1/0) to keep the existing already read long culverts.
      integer, intent(out) :: ierr !< Result status, DFM_NOERR in case of success.

      type(tree_data), pointer :: strs_ptr
      type(tree_data), pointer :: str_ptr
      character(len=IdLen) :: typestr
      character(len=IdLen) :: st_id
      character(len=IdLen) :: txt
      character(len=IdLen) :: base_dir, fnam
      integer :: readerr, nstr, i, numcoords
      integer, allocatable, dimension(:) :: links
      logical :: success
      integer :: istart
      integer :: nlongculverts0, iref
      integer :: num_columns

      real(kind=dp), allocatable :: x_coordinates(:), y_coordinates(:), z_coordinates(:)

      ! Local concatenated polyline arrays for make1D2DLongCulverts (dmiss-separated segments).
      real(kind=dp), allocatable :: all_x_coordinates(:), all_y_coordinates(:), all_z_coordinates(:)
      integer :: all_coordinates_size

      ierr = DFM_NOERR

      call split_filename(structurefile, base_dir, fnam) ! Needed to resolve locationFile= paths.

      nlongculverts0 = nlongculverts ! Remember any old longculvert count

      if (jaKeepExisting == 0) then
         nlongculverts = 0
         if (allocated(longculverts)) then
            deallocate (longculverts)
         end if
      end if

      all_coordinates_size = 0

      ! Temporarily put structures.ini file into a property tree
      call tree_create(trim(structurefile), strs_ptr)
      call prop_inifile(structurefile, strs_ptr, readerr)
      ! check if file was successfully opened
      if (readerr /= 0) then
         ierr = DFM_WRONGINPUT
         call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
      end if

      nstr = tree_num_nodes(strs_ptr)
      call realloc(longculverts, nlongculverts + nstr)
      newculverts = .false.
      do i = 1, nstr
         str_ptr => strs_ptr%child_nodes(i)%node_ptr

         success = .true.

         if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
            ! Only read [Structure] blocks, skip any other (e.g., [General]).
            cycle
         end if

         typestr = ' '
         call prop_get(str_ptr, '', 'type', typestr, success)
         if (.not. success .or. .not. strcmpi(typestr, 'longCulvert')) then
            cycle
         end if

         nlongculverts = nlongculverts + 1

         call prop_get(str_ptr, '', 'id', st_id, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ' from '''//trim(structurefile)//''', id is missing.'
            call err_flush()
         end if

         if (success) then
            call read_polyline_coordinates(str_ptr, trim(st_id), structurefile, base_dir, 'longCulvert', &
                                           x_coordinates, y_coordinates, z_coordinates, num_columns, success)
         end if

         if (success) then
            numcoords = size(x_coordinates)
            longculverts(nlongculverts)%id = st_id
            longculverts(nlongculverts)%xcoords = x_coordinates
            longculverts(nlongculverts)%ycoords = y_coordinates

            allocate (longculverts(nlongculverts)%bl(numcoords))
            longculverts(nlongculverts)%bl = z_coordinates

            ! Append this culvert's coordinates (plus dmiss separator) to the local concatenated arrays.
            call realloc(all_x_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            call realloc(all_y_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            call realloc(all_z_coordinates, all_coordinates_size + numcoords + 1, keepExisting=.true., fill=dmiss)
            all_x_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = x_coordinates
            all_y_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = y_coordinates
            all_z_coordinates(all_coordinates_size + 1:all_coordinates_size + numcoords) = z_coordinates
            ! all_x_coordinates(all_coordinates_size + numcoords + 1) stays dmiss: the segment separator.
            all_coordinates_size = all_coordinates_size + numcoords + 1

            call get_value_or_addto_forcinglist(str_ptr, 'valveRelativeOpening', longculverts(nlongculverts)%valve_relative_opening, st_id, &
                                                ST_LONGCULVERT, network%forcinglist, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'valveRelativeOpening not found for long culvert: '//trim(st_id))
            end if

            txt = 'both'
            call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
            longculverts(nlongculverts)%allowed_flowdir = allowedFlowDirToInt(txt)

            call prop_get(str_ptr, '', 'branchId', longculverts(nlongculverts)%branchId, success)
            if (.not. success) then
               call prop_get(str_ptr, '', 'contactId', longculverts(nlongculverts)%contactID, success)
            end if
            if (success) then
               call prop_get(str_ptr, '', 'csDefId', longculverts(nlongculverts)%csDefId, success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'csDefId not found for long culvert: '//trim(st_id))
               end if

               newculverts = .true.
            end if

            longculverts(nlongculverts)%numlinks = numcoords - 1
            allocate (longculverts(nlongculverts)%netlinks(numcoords - 1))
            allocate (longculverts(nlongculverts)%flowlinks(numcoords - 1))
            longculverts(nlongculverts)%flowlinks = -999
            longculverts(nlongculverts)%netlinks = -999

            if (newculverts) then
               call addlongculvertcrosssections(network, longculverts(nlongculverts)%branchid, longculverts(nlongculverts)%csDefId, longculverts(nlongculverts)%bl, iref)
               if (iref > 0) then
                  ! Use top (#2) of tabulated cross section definition to derive width and height
                  longculverts(nlongculverts)%width = network%CSDefinitions%Cs(iref)%totalwidth(2)
                  longculverts(nlongculverts)%height = network%CSDefinitions%Cs(iref)%height(2)
                  longculverts(nlongculverts)%friction_type = network%CSDefinitions%Cs(iref)%frictiontype(1)
                  longculverts(nlongculverts)%friction_value = network%CSDefinitions%Cs(iref)%frictionvalue(1)
               end if
            else !these values are no longer in the structures.ini after conversion
               txt = 'both'
               call prop_get(str_ptr, '', 'allowedFlowdir', txt, success)
               longculverts(nlongculverts)%allowed_flowdir = allowedFlowDirToInt(txt)

               call prop_get(str_ptr, '', 'width', longculverts(nlongculverts)%width, success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'width not found for long culvert: '//st_id)
               end if
               call prop_get(str_ptr, '', 'height', longculverts(nlongculverts)%height, success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'height not found for long culvert: '//st_id)
               end if
               call prop_get(str_ptr, '', 'frictionType', typestr, success)
               if (.not. success) then
                  longculverts(nlongculverts)%friction_type = ifrctypuni
               else
                  call frictionTypeStringToInteger(typestr, longculverts(nlongculverts)%friction_type)
               end if
               call prop_get(str_ptr, '', 'frictionValue', longculverts(nlongculverts)%friction_value, success)
               if (.not. success) then
                  longculverts(nlongculverts)%friction_value = frcuni1D
               end if
            end if
         end if

         if (.not. success) then
            ! Some error during reading: decrement counter to ignore this long culvert.
            nlongculverts = nlongculverts - 1
         end if

      end do

      if (.not. newculverts) then
         allocate (links(all_coordinates_size))
         call make1D2DLongCulverts(all_x_coordinates, all_y_coordinates, all_z_coordinates, all_coordinates_size, links)
         istart = 1
         do i = nlongculverts0 + 1, nlongculverts
            longculverts(i)%netlinks = links(istart:istart + longculverts(i)%numlinks - 1)
            istart = istart + longculverts(i)%numlinks + 2
         end do
      end if

      call tree_destroy(strs_ptr)

   end subroutine loadLongCulvertsAsNetwork

   !> Finalizes some necessary network administration after all long culverts have been read.
   !! Actual reading is done in other subroutine loadLongCulvertsAsNetwork().
   subroutine finalizeLongCulvertsInNetwork()
      use network_data
      use gridoperations
      integer :: Lnet, i, ilongc

      ! NOTE: IF setnodadm() is again called after this subroutine has completed, with more netlink permutations,
      !! Then the longculvert()%netlinks array is incorrect. This can be fixed if we change our approach
      !! to always using closeto1dnetlink() calls in the longCulvertsToProfs() subroutine, instead. For now, we are safe, though.

      ! Netlink numbers have probably been permuted by setnodadm, so also update netlinks.
      do ilongc = 1, nlongculverts
         do i = 1, longculverts(ilongc)%numlinks
            ! Netlink numbers have probably been permuted after the initial long culvert reading, so also update netlinks.
            Lnet = Lperminv(longculverts(ilongc)%netlinks(i))
            longculverts(ilongc)%netlinks(i) = Lnet
         end do
      end do
   end subroutine finalizeLongCulvertsInNetwork

   !> Reallocates a given longculvert array to larger size.
   !! Any existing longculvert data is copied into the new array.
   subroutine reallocLongCulverts(lcs, newsize)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_longculvert), allocatable, intent(inout) :: lcs(:) !< The existing longculvert array.
      integer, intent(in) :: newsize !< The desired new size.

      ! Local variables
      type(t_longculvert), allocatable :: oldlcs(:)
      integer :: oldsize, i

      ! Program code

      if (allocated(lcs)) then
         oldsize = size(lcs)
      else
         oldsize = 0
      end if

      if (newsize > oldsize) then
         allocate (oldlcs(oldsize))
         do i = 1, oldsize
            oldlcs(i) = lcs(i)
         end do

         if (allocated(lcs)) then
            deallocate (lcs)
         end if
         allocate (lcs(newsize))
         do i = 1, oldsize
            lcs(i) = oldlcs(i)
         end do
      end if

   end subroutine reallocLongCulverts

   !> Sets up a 1D2D entry or exit link for a long culvert: assigns the cross-section,
   !! width, prof1D and bob arrays for the given flow link.
   subroutine setup_longculvert_1D2D_link(ilongc, Lf, bl_up, bl_dn)
      use m_flowgeom, only: wu, bob
      use unstruc_channel_flow, only: network
      use m_network, only: t_network

      integer, intent(in) :: ilongc !< Long culvert index
      integer, intent(in) :: Lf !< Flow link number
      real(kind=dp), intent(in) :: bl_up !< Bed level at upstream side of this link
      real(kind=dp), intent(in) :: bl_dn !< Bed level at downstream side of this link

      character(len=idLen) :: link_id

      if (longculverts(ilongc)%is_2D2D()) then
         link_id = longculverts(ilongc)%contactId
      else
         link_id = longculverts(ilongc)%branchId
      end if
      call add_longculvert_crosssection(network, Lf, link_id, longculverts(ilongc)%csdefId)

      wu(Lf) = longculverts(ilongc)%width
      bob(1, Lf) = bl_up
      bob(2, Lf) = bl_dn

   end subroutine setup_longculvert_1D2D_link

   !> Initializes the cross section administration for long culverts in prof1d and other relevant flow geometry arrays.
   !! * Sets netlink numbers and flowlink numbers.
   !> * Fills for the corresponding flow links the bedlevels, bobs and prof1d data.
   subroutine longculvertsToProfs(skiplinks)
      use network_data
      use m_flowgeom

      logical, intent(in) :: skiplinks !< Skip determining the flow links or not

      integer :: Lnet, Lf, i, ilongc, k1, k2

      !
      ! If we have retrieved the flowlinks and so on via the cache file,
      ! skip this loop
      !
      if (.not. skiplinks) then
         do ilongc = 1, nlongculverts
            do i = 1, longculverts(ilongc)%numlinks
               if (longculverts(ilongc)%flowlinks(i) < 0) then
                  ! Flow links have not yet been initialized, this is the first call.
                  ! Netlink numbers have been set correctly in finalizeLongCulvertsInNetwork() already.
                  Lnet = longculverts(ilongc)%netlinks(i)
                  longculverts(ilongc)%flowlinks(i) = lne2ln(Lnet)
               end if
            end do

            if (longculverts(ilongc)%numlinks <= 0) then
               ! Skip this long culvert when it is not active on this grid
               cycle
            end if

            ! Set upstream flow node
            Lf = abs(longculverts(ilongc)%flowlinks(1))

            if (longculverts(ilongc)%numlinks == 1) then
               longculverts(ilongc)%flownode_up = ln(1, Lf)
               longculverts(ilongc)%flownode_dn = ln(2, Lf)
            else
               if (ln(1, Lf) <= ndx2d) then
                  longculverts(ilongc)%flownode_up = ln(1, Lf)
               else
                  longculverts(ilongc)%flownode_up = ln(2, Lf)
               end if
               ! Set downstream flow node
               Lf = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks))
               if (ln(2, Lf) <= ndx2d) then
                  longculverts(ilongc)%flownode_dn = ln(2, Lf)
               else
                  longculverts(ilongc)%flownode_dn = ln(1, Lf)
               end if
            end if
         end do
      end if

      ! The cache and partition paths can retain flow links while changing their
      ! local orientation. Therefore this mapping must also be refreshed when
      ! skiplinks is true.
      do ilongc = 1, nlongculverts
         call setLongCulvertFlowDirectionSign(ilongc)
      end do

      if (newculverts) then
         do ilongc = 1, nlongculverts
            do i = 2, longculverts(ilongc)%numlinks - 1
               Lf = abs(longculverts(ilongc)%flowlinks(i))
               if (Lf > 0) then
                  k1 = ln(1, Lf)
                  k2 = ln(2, Lf)
                  bob(1, Lf) = longculverts(ilongc)%bl(i - 1)
                  bob(2, Lf) = longculverts(ilongc)%bl(i)
                  if (k1 > ndx2d) then !if 1d link
                     bl(k1) = bob(1, Lf)
                  else
                     bl(k1) = min(bl(k1), bob(1, Lf))
                  end if
                  if (k2 > ndx2d) then
                     bl(k2) = bob(2, Lf)
                  else
                     bl(k2) = min(bl(k2), bob(2, Lf))
                  end if
               end if
            end do
            ! Entry 1D2D link
            Lf = abs(longculverts(ilongc)%flowlinks(1))
            if (Lf > 0) then
               !> take max of 1D and 2D bedlevels, as culvert cannot lie below the 2D bedlevel
               call setup_longculvert_1D2D_link(ilongc, Lf, longculverts(ilongc)%bl(2), &
                                                max(longculverts(ilongc)%bl(1), bl(ln(2, Lf))))
            end if

            ! Exit 1D2D link
            if (longculverts(ilongc)%numlinks > 1) then
               Lf = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks))
               if (Lf > 0) then
                  call setup_longculvert_1D2D_link(ilongc, Lf, longculverts(ilongc)%bl(longculverts(ilongc)%numlinks), &
                                                   max(longculverts(ilongc)%bl(longculverts(ilongc)%numlinks + 1), bl(ln(2, Lf))))
               end if
            end if
         end do
      else !voor nu houden we de oude implementatie intact
         do ilongc = 1, nlongculverts
            do i = 1, longculverts(ilongc)%numlinks
               Lf = abs(longculverts(ilongc)%flowlinks(i))
               !if (kcu(lf) == LINK_1D) then ! TODO: UNST-5433: change when 1d2d links are *extra* in addition to culvert polyline
               k1 = ln(1, Lf)
               k2 = ln(2, Lf)

               bob(1, Lf) = longculverts(ilongc)%bl(i)
               bob(2, Lf) = longculverts(ilongc)%bl(i + 1)
               if (k1 > ndx2d) then
                  bl(k1) = bob(1, Lf)
               else ! k1 = 2d point
                  bl(k1) = min(bl(k1), bob(1, Lf))
               end if

               if (k2 > ndx2d) then
                  bl(k2) = bob(2, Lf)
               else
                  bl(k2) = min(bl(k2), bob(2, Lf))
               end if

               wu(Lf) = longculverts(ilongc)%width
               prof1D(1, Lf) = wu(Lf)
               prof1D(2, Lf) = longculverts(ilongc)%height
               prof1D(3, Lf) = -2 ! for now, simple rectan
            end do
         end do
      end if

   end subroutine longculvertsToProfs

   !> Reconstruct the flow link orientation based on input polyline
   subroutine setLongCulvertFlowDirectionSign(ilongc)
      use network_data, only: kn, xk, yk
      use precision_basics, only: equal

      integer, intent(in) :: ilongc
      integer :: Lnet, k1, k2
      logical :: endpoint_is_first_node, endpoint_is_second_node

      longculverts(ilongc)%flow_dir = 1

      if (longculverts(ilongc)%numlinks <= 0) then
         return
      end if
      if (.not. allocated(longculverts(ilongc)%flowlinks) .or. .not. allocated(longculverts(ilongc)%netlinks) .or. &
          .not. allocated(longculverts(ilongc)%xcoords) .or. .not. allocated(longculverts(ilongc)%ycoords)) then
         return
      end if
      if (size(longculverts(ilongc)%flowlinks) < 1 .or. size(longculverts(ilongc)%netlinks) < 1 .or. &
          size(longculverts(ilongc)%xcoords) < 2 .or. size(longculverts(ilongc)%ycoords) < 2) then
         return
      end if

      Lnet = longculverts(ilongc)%netlinks(1)
      if (Lnet <= 0 .or. Lnet > size(kn, dim=2)) then
         return
      end if

      k1 = kn(1, Lnet)
      k2 = kn(2, Lnet)
      if (k1 <= 0 .or. k2 <= 0) then
         return
      end if

      endpoint_is_first_node = equal(xk(k1), longculverts(ilongc)%xcoords(2)) .and. equal(yk(k1), longculverts(ilongc)%ycoords(2))
      endpoint_is_second_node = equal(xk(k2), longculverts(ilongc)%xcoords(2)) .and. equal(yk(k2), longculverts(ilongc)%ycoords(2))

      if (endpoint_is_first_node .and. .not. endpoint_is_second_node) then
         longculverts(ilongc)%flow_dir = -1
      else if (.not. endpoint_is_second_node) then
         call mess(LEVEL_WARN, 'Cannot match the second coordinate of long culvert '//trim(longculverts(ilongc)%id)// &
                   ' to either endpoint of its first flow link;')
      end if
   end subroutine setLongCulvertFlowDirectionSign

   !> Fill frcu and icrctyp for the corresponding flow link numbers of the long culverts
   subroutine setFrictionForLongculverts()
      use m_flow
      implicit none

      integer :: LL, ilongc, Lf

      do ilongc = 1, nlongculverts
         do LL = 1, longculverts(ilongc)%numlinks
            Lf = abs(longculverts(ilongc)%flowlinks(LL))
            if (Lf > 0) then
               if (longculverts(ilongc)%friction_type > 0) then
                  ifrcutp(Lf) = longculverts(ilongc)%friction_type
                  if (longculverts(ilongc)%friction_value > 0) then
                     frcu(Lf) = longculverts(ilongc)%friction_value
                  end if
               end if
            end if
         end do
      end do

   end subroutine setFrictionForLongculverts

   !> In case  the valve_relative_area < 1 the flow area
   !! at the first link is reduced by valve_relative_area, or set to 0 by allowed_flowdir
   subroutine reduceFlowAreaAtLongculverts()
      use m_flow
      use m_1d_structures, only: FLOWDIR_POSITIVE, FLOWDIR_NONE, FLOWDIR_NEGATIVE
      implicit none

      integer i, L, L_dir, allowed_flowdir
      real(dp) :: valve_relative_opening

      do i = 1, nlongculverts
         if (longculverts(i)%numlinks > 0) then
            L = abs(longculverts(i)%flowlinks(1))
            if (L > 0) then
               valve_relative_opening = min(longculverts(i)%valve_relative_opening, 1.0_dp)
               valve_relative_opening = max(valve_relative_opening, 0.0_dp)
               au(L) = valve_relative_opening * au(L)
               call getflowdir(L, L_dir)
               L_dir = longculverts(i)%flow_dir * L_dir
               allowed_flowdir = longculverts(i)%allowed_flowdir
               if (allowed_flowdir == FLOWDIR_NONE &
                   .or. L_dir < 0 .and. allowed_flowdir == FLOWDIR_POSITIVE &
                   .or. L_dir > 0 .and. allowed_flowdir == FLOWDIR_NEGATIVE) then
                  hu(L) = 0.0_dp
                  au(L) = 0.0_dp
               end if
            end if
         end if
      end do

   end subroutine reduceFlowAreaAtLongculverts

   !> Gets the pointer of the valve opening height for a given culvert structure.
   !! If the type of the given structure is not culvert, then it gets a null pointer
   !! This pointer points directly to the %culvert%valveOpening.
   type(c_ptr) function get_valve_relative_opening_c_loc(lculv)
      type(t_longculvert), intent(in), target :: lculv

      get_valve_relative_opening_c_loc = c_loc(lculv%valve_relative_opening)

   end function get_valve_relative_opening_c_loc

   !> Generates 1D netlinks and 1D2D connections for a (multiple) new long culvert(s).
   !! The new net links get added to the active network_data.
   !! The culvert(s) must be specified by a polyline with x/y/z coordinates.
   !! In case of multiple culverts, the coordinate arrays must have missing value
   !! (dmiss) separators between each polyline.
   subroutine make1D2DLongCulverts(xplCulv, yplCulv, zplCulv, nplCulv, linksCulv)
      use precision, only: dp
      use m_missing
      use geometry_module
      use m_alloc
      use network_data
      use m_cell_geometry
      use m_samples
      use gridoperations
      implicit none

      real(kind=dp), intent(in) :: xplCulv(:) !< x-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(in) :: yplCulv(:) !< y-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(in) :: zplCulv(:) !< z-coordinates of the polyline of one or more culverts.
      integer, intent(in) :: nplCulv !< Number of points in the culvert polyline.
      integer, intent(out) :: linksCulv(:) !< Resulting netlink numbers of one or more culverts.

      integer :: j, jpoint, jstart, jend, k1, k2, ipoly
      real(kind=dp) :: x1, y1, z1, x2, y2, z2

      ipoly = 0
      jpoint = 1
      do while (jpoint < nplCulv)

         ! Find next start and end point in pli set:
         call get_startend(nplCulv - jpoint + 1, xplCulv(jpoint:nplCulv), yplCulv(jpoint:nplCulv), jstart, jend, dmiss)
         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1

         if (jstart >= jend) then
            call mess(LEVEL_WARN, 'generateLongCulverts: No valid start+end point found in polyline.')
            !goto 888
         end if

         ipoly = ipoly + 1

         ! Starting point:
         x1 = xplCulv(jstart)
         y1 = yplCulv(jstart)
         z1 = zplCulv(jstart)
         call setnewpoint(x1, y1, z1, k1)
         zk(k1) = z1

         do j = jstart + 1, jend
            x2 = xplCulv(j)
            y2 = yplCulv(j)
            z2 = zplCulv(j)
            call setnewpoint(x2, y2, z2, k2)
            zk(k2) = z2

            if (j == jstart + 1 .or. j == jend) then
               kn3typ = 5 ! 1D2D netlink type for entry-side and exit-side.
            else
               kn3typ = 1 ! purely 1D netlink type for inner pipe pieces (if any).
            end if
            call connectdbn(k1, k2, linksCulv(j - 1))
            k1 = k2
         end do

         !           advance pointer
         jpoint = jend + 2
      end do

      ! NOTE: here we do not explicitly check whether end points lie inside
      ! 2D grid cells, for performance reasons.

      ! TODO: UNST-4334: Detect whether link is already there
      !xc = 0.5d0*(x1+x2)
      !yc = 0.5d0*(y1+y2)
      !CALL CLOSETO1Dnetlink(Xc,Yc,LS,XLS,YLS,dum, 0)
      !if (Ls > 0) then

      ! Successful exit
      return

888   continue
      ! Something went wrong.

   end subroutine make1D2DLongCulverts
   !> Generates 1D netlinks and 1D2D connections for a (multiple) new long culvert(s).
   !! The new net links get added to the active network_data.
   !! The culvert(s) must be specified by a polyline with x/y/z coordinates.
   !! In case of multiple culverts, the coordinate arrays must have missing value
   !! (dmiss) separators between each polyline.
   subroutine convert1D2DLongCulverts(xplCulv, yplCulv, zplCulv, nplCulv)
      use precision, only: dp
      use m_missing
      use geometry_module
      use m_alloc
      use network_data
      use precision_basics, only: comparereal
      use m_samples
      use m_save_ugrid_state
      use gridoperations

      implicit none

      real(kind=dp), intent(inout) :: xplCulv(:) !< x-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(inout) :: yplCulv(:) !< y-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(in) :: zplCulv(:) !< z-coordinates of the polyline of one or more culverts.
      integer, intent(in) :: nplCulv !< Number of points in the culvert polyline.

      integer :: jpoint, jstart, jend, ipoly, inode

      if (meshgeom1d%numnode == -1 .and. meshgeom1d%nnodes == -1) then
         ! This is to allow more than one call to loadNetwork/unc_read_net_ugrid. Remove any previously read network state.
         call default_save_ugrid_state()
         meshgeom1d%nbranches = 0
         meshgeom1d%ngeometry = 0
         meshgeom1d%nnodes = 0
         meshgeom1d%numedge = 0
         meshgeom1d%numnode = 0
      end if

      if (.not. associated(meshgeom1d%nodeidx) .and. meshgeom1d%numnode > 0) then
         !if an existing network is read but nodeidx and nodeidx_inverse are not allocated, fill them here.
         allocate (meshgeom1d%nodeidx(meshgeom1d%numnode))
         meshgeom1d%nodeidx = [(inode, inode=1, meshgeom1d%numnode)]
         if (associated(meshgeom1d%nodeidx_inverse)) then
            deallocate (meshgeom1d%nodeidx_inverse)
         end if
         allocate (meshgeom1d%nodeidx_inverse(meshgeom1d%numnode))
         do inode = 1, meshgeom1d%numnode
            meshgeom1d%nodeidx_inverse(meshgeom1d%nodeidx(inode)) = inode
         end do
      end if

      ipoly = 0
      jpoint = 1
      do while (jpoint < nplCulv)
         ! Find next start and end point in pli set:
         call get_startend(nplCulv - jpoint + 1, xplCulv(jpoint:nplCulv), yplCulv(jpoint:nplCulv), jstart, jend, dmiss)
         ipoly = ipoly + 1
         jstart = jstart + jpoint - 1
         jend = jend + jpoint - 1
         call process_single_longculvert(xplCulv(jstart:jend), yplCulv(jstart:jend), zplCulv(jstart:jend), ipoly)
         ! advance pointer
         jpoint = jend + 2
      end do

   end subroutine convert1D2DLongCulverts

   !> Process a single long culvert defined by its polyline points.
   subroutine process_single_longculvert(xplCulv, yplCulv, zplCulv, i_longculvert)
      use precision, only: dp
      use m_missing
      use geometry_module
      use m_alloc
      use network_data
      use precision_basics, only: comparereal
      use m_samples
      use m_save_ugrid_state
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations

      implicit none

      real(kind=dp), intent(inout) :: xplCulv(:) !< x-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(inout) :: yplCulv(:) !< y-coordinates of the polyline of one or more culverts.
      real(kind=dp), intent(in) :: zplCulv(:) !< z-coordinates of the polyline of one or more culverts.
      integer, intent(in) :: i_longculvert !< long culvert index

      integer :: j, k1, k2, numculvertpoints, currentbranchindex, newnodeindex, newedgeindex, newgeomindex, newnetnodeindex
      real(kind=dp) :: x2, y2, z2, pathlength, pathdiff
      character(len=5) :: ipolychar, nodechar
      character(:), allocatable :: longculvert_name
      integer :: poly_point_count, L

      poly_point_count = size(xplCulv)

      !remember current indices before reallocation
      newedgeindex = meshgeom1d%numedge + 1
      newnodeindex = meshgeom1d%numnode + 1
      newnetnodeindex = meshgeom1d%nnodes + 1
      newgeomindex = meshgeom1d%ngeometry + 1
      currentbranchindex = meshgeom1d%nbranches + 1
      write (ipolychar, '(I0)') i_longculvert
      longculvert_name = 'longCulvert_'//trim(ipolychar)

      if (poly_point_count <= 3) then
         !> 2-point and 3-point culverts: only 1D2D links, no branch/meshgeom1d administration.
         !! All links get kn3typ=5. These culverts are identified by contactId.
         longculverts(i_longculvert)%contactId = longculvert_name

         call longculvert_create_endpoint(xplCulv(1), yplCulv(1), zplCulv(1), k1)
         xplCulv(1) = xk(k1)
         yplCulv(1) = yk(k1)

         kn3typ = 5
         do j = 2, poly_point_count - 1
            x2 = xplCulv(j)
            y2 = yplCulv(j)
            z2 = zplCulv(j)
            call setnewpoint(x2, y2, z2, k2)
            call connectdbn(k1, k2, L)
            if (allocated(dxe)) then
               dxe(L) = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
            end if
            longculverts(i_longculvert)%netlinks(j - 1) = L
            k1 = k2
         end do

         call longculvert_create_endpoint(xplCulv(poly_point_count), yplCulv(poly_point_count), zplCulv(poly_point_count), k2)
         xplCulv(poly_point_count) = xk(k2)
         yplCulv(poly_point_count) = yk(k2)

         call connectdbn(k1, k2, L)
         if (allocated(dxe)) then
            dxe(L) = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
         end if
         longculverts(i_longculvert)%netlinks(poly_point_count - 1) = L

      else ! Multi-point culvert

         longculverts(i_longculvert)%branchId = longculvert_name

         !> only multi point culverts get meshgeom1d entries
         call reallocate_meshgeom1d_arrays(poly_point_count)

         nbranchids(currentbranchindex) = longculverts(i_longculvert)%branchId
         numculvertpoints = poly_point_count - 2

         ! Setup network nodes using interior points (skip polyline endpoints)
         meshgeom1d%nnodex(newnetnodeindex:newnetnodeindex + 1) = [xplCulv(2), xplCulv(poly_point_count - 1)]
         meshgeom1d%nnodey(newnetnodeindex:newnetnodeindex + 1) = [yplCulv(2), yplCulv(poly_point_count - 1)]

         ! Create start endpoint
         call longculvert_create_endpoint(xplCulv(1), yplCulv(1), zplCulv(1), k1)
         xplCulv(1) = xk(k1)
         yplCulv(1) = yk(k1)

         pathlength = 0.0_dp
         pathdiff = 0.0_dp

         do j = 2, poly_point_count - 1
            x2 = xplCulv(j)
            y2 = yplCulv(j)
            z2 = zplCulv(j)
            call setnewpoint(x2, y2, z2, k2)

            if (j == 2) then
               kn3typ = 5 ! 1D2D netlink type for entry-side
            else
               pathdiff = dbdistance(x2, y2, xplCulv(j - 1), yplCulv(j - 1), jsferic, jasfer3D, dmiss)
               kn3typ = 1 ! purely 1D netlink type for inner pipe pieces
               meshgeom1d%edgebranchidx(newedgeindex) = currentbranchindex
               meshgeom1d%edgeoffsets(newedgeindex) = pathlength + pathdiff / 2
               newedgeindex = newedgeindex + 1
            end if

            ! Node
            meshgeom1d%nodebranchidx(newnodeindex) = currentbranchindex
            meshgeom1d%nodeidx(newnodeindex) = k2
            meshgeom1d%nodeidx_inverse(k2) = newnodeindex
            pathlength = pathlength + pathdiff
            meshgeom1d%nodeoffsets(newnodeindex) = pathlength
            newnodeindex = newnodeindex + 1

            call connectdbn(k1, k2, L)
            if (allocated(dxe)) then
               dxe(L) = dbdistance(xplCulv(j - 1), yplCulv(j - 1), x2, y2, jsferic, jasfer3D, dmiss)
            end if

            longculverts(i_longculvert)%netlinks(j - 1) = L
            k1 = k2
         end do

         ! End point
         meshgeom1d%nbranchlengths(currentbranchindex) = pathlength
         kn3typ = 5
         call longculvert_create_endpoint(xplCulv(poly_point_count), yplCulv(poly_point_count), zplCulv(poly_point_count), k2)
         xplCulv(poly_point_count) = xk(k2)
         yplCulv(poly_point_count) = yk(k2)

         call connectdbn(k1, k2, L)
         longculverts(i_longculvert)%netlinks(poly_point_count - 1) = L

         ! Common finalization for multi-point culverts
         meshgeom1d%nodex(newnetnodeindex:newnetnodeindex + 1) = meshgeom1d%nnodex(newnetnodeindex:newnetnodeindex + 1)
         meshgeom1d%nodey(newnetnodeindex:newnetnodeindex + 1) = meshgeom1d%nnodey(newnetnodeindex:newnetnodeindex + 1)
         meshgeom1d%nedge_nodes(1:2, currentbranchindex) = [newnetnodeindex, newnetnodeindex + 1]

         write (nodechar, '(I0)') newnetnodeindex
         nnodeids(newnetnodeindex) = 'BR_longCulvert_'//trim(ipolychar)//'_node_'//trim(nodechar)
         write (nodechar, '(I0)') newnetnodeindex + 1
         nnodeids(newnetnodeindex + 1) = 'BR_longCulvert_'//trim(ipolychar)//'_node_'//trim(nodechar)
         meshgeom1d%nbranchgeometrynodes(currentbranchindex) = numculvertpoints
         meshgeom1d%ngeopointx(newgeomindex:newgeomindex + numculvertpoints - 1) = xplCulv(2:poly_point_count - 1)
         meshgeom1d%ngeopointy(newgeomindex:newgeomindex + numculvertpoints - 1) = yplCulv(2:poly_point_count - 1)
      end if

   end subroutine process_single_longculvert

   subroutine reallocate_meshgeom1d_arrays(poly_point_count)
      use m_save_ugrid_state
      use m_alloc
      use network_data, only: kc

      integer, intent(in) :: poly_point_count

      meshgeom1d%nbranches = meshgeom1d%nbranches + 1
      meshgeom1d%nnodes = meshgeom1d%nnodes + 2 ! only 2 network nodes per branch

      meshgeom1d%numnode = meshgeom1d%numnode + poly_point_count - 2
      meshgeom1d%numedge = meshgeom1d%numedge + poly_point_count - 3
      meshgeom1d%ngeometry = meshgeom1d%ngeometry + poly_point_count - 2

      call reallocP(meshgeom1d%nbranchorder, meshgeom1d%nbranches, keepexisting=.true., fill=-999)
      call reallocP(meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranches, keepexisting=.true., fill=-999)
      call reallocP(meshgeom1d%nedge_nodes, [2, meshgeom1d%nbranches], keepexisting=.true.)
      call reallocP(meshgeom1d%nbranchlengths, meshgeom1d%nbranches, keepexisting=.true., fill=-999.0_dp)
      call realloc(nbranchids, meshgeom1d%nbranches, keepexisting=.true., fill='')

      call reallocP(meshgeom1d%nnodex, meshgeom1d%nnodes, keepexisting=.true., fill=-999.0_dp)
      call reallocP(meshgeom1d%nnodey, meshgeom1d%nnodes, keepexisting=.true., fill=-999.0_dp)
      call reallocP(meshgeom1d%nodex, meshgeom1d%numnode, keepexisting=.true., fill=-999.0_dp)
      call reallocP(meshgeom1d%nodey, meshgeom1d%numnode, keepexisting=.true., fill=-999.0_dp)
      call realloc(nnodeids, meshgeom1d%nnodes, keepexisting=.true.)

      call reallocP(meshgeom1d%nodeidx_inverse, size(kc), keepexisting=.true., fill=-999)

      call reallocP(meshgeom1d%nodeidx, meshgeom1d%numnode, keepexisting=.true., fill=-999)
      call reallocP(meshgeom1d%nodebranchidx, meshgeom1d%numnode, keepexisting=.true., fill=-999)
      call reallocP(meshgeom1d%nodeoffsets, meshgeom1d%numnode, keepexisting=.true., fill=-999.0_dp)

      call reallocP(meshgeom1d%edgebranchidx, meshgeom1d%numedge, keepexisting=.true., fill=-999)
      call reallocP(meshgeom1d%edgeoffsets, meshgeom1d%numedge, keepexisting=.true., fill=-999.0_dp)

      call reallocP(meshgeom1d%ngeopointx, meshgeom1d%ngeometry, keepexisting=.true., fill=-999.0_dp)
      call reallocP(meshgeom1d%ngeopointy, meshgeom1d%ngeometry, keepexisting=.true., fill=-999.0_dp)

   end subroutine reallocate_meshgeom1d_arrays

   !> Add new cross section locations on a particular branch in the network.
   !! The cross section definition (defining the long culvert's shape)
   !! must already have been read from file.
   subroutine addlongculvertcrosssections(network, branchId, csdefId, all_z_coordinates, iref)
      use precision, only: dp
      use m_hash_search
      use m_readCrossSections
      use m_network
      type(t_network), intent(inout) :: network !< Network structure
      character(len=IdLen), intent(in) :: branchId !< Branch id on which to place the cross section
      character(len=IdLen), intent(in) :: csdefId !< Id of cross section definition
      real(kind=dp), allocatable, intent(in) :: all_z_coordinates(:) !< (numlinks+1) Bed level on the long culvert support points
      integer, intent(out) :: iref !< Index of reference cross section definition (if csdefId was found)

      integer :: k
      integer :: inext
      integer :: indx
      type(t_CrossSection), pointer :: pCrs
      character(len=5) :: kchar

      indx = hashsearch(network%brs%hashlist, branchId)
      iref = hashsearch(network%CSDefinitions%hashlist, csdefId)
      if (indx > 0 .and. iref > 0) then
         ! This code assumes 1 gridpoint per culvert coordinate,
         ! which means the culvert network branches cannot be modified after converting
         do k = 1, network%brs%branch(indx)%gridpointscount

            if (network%crs%count + 1 > network%crs%size) then
               call realloc(network%crs)
            end if
            inext = network%crs%count + 1
            pCrs => network%crs%cross(inext)
            write (kchar, '(I0)') k
            pCrs%csid = trim(branchId)//'_'//trim(kchar)
            pCrs%branchid = indx
            pCrs%bedLevel = 0.0_dp
            pCrs%shift = all_z_coordinates(k) !number of gridpoints in branch should match all_z_coordinates+2!!
            pCrs%chainage = network%brs%branch(indx)%gridpointschainages(k)
            call finalizeCrs(network, pCrs, iref, inext)
         end do
      end if

   end subroutine addlongculvertcrosssections

   !> add special 1D2D crossection for the longculvert and add it to the line2cross array
   subroutine add_longculvert_crosssection(network, flowlink, link_id, cs_def_id)
      use precision, only: dp
      use m_hash_search, only: hashsearch
      use m_CrossSections, only: realloc
      use m_readCrossSections, only: finalizeCrs
      use m_network, only: t_network
      use m_GlobalParameters, only: t_chainage2cross

      type(t_network), intent(inout) :: network !< Network structure
      integer, intent(in) :: flowlink !< Flowlink number on which to place the cross section. Should be 1D2D link belonging to the long culvert
      character(len=IdLen), intent(in) :: link_id !< Branch id or contact id on which to place the cross section
      character(len=IdLen), intent(in) :: cs_def_id !< Id of cross section definition

      integer :: idef, icrs
      character(len=16) :: kchar

      ! Find cross section definition indices by ID.
      idef = hashsearch(network%CSDefinitions%hashlist, cs_def_id)
      if (idef <= 0) then
         call SetMessage(LEVEL_ERROR, 'Cross-section definition not found: '//trim(cs_def_id))
         return
      end if

      ! Create new cross section instance of cross section definition `idef`.
      if (network%crs%count + 1 > network%crs%size) then
         call realloc(network%crs)
      end if
      icrs = network%crs%count + 1

      associate (cross_section => network%crs%cross(icrs))
         write (kchar, '(I0)') flowlink
         cross_section%csid = trim(link_id)//"_"//trim(kchar)
         call finalizeCrs(network, cross_section, idef, icrs)
      end associate

      ! Assign new cross section along flowlink.
      network%adm%line2cross(flowlink, :) = t_chainage2cross(c1=icrs, c2=icrs, f=1.0_dp, distance=0.0_dp)
   end subroutine add_longculvert_crosssection

   !> Add new branch information to the network. Only add necessary information for long culverts (incomplete!)
   subroutine add_longculvert_branch(network, longculvert)
      use precision, only: dp
      use m_hash_search
      use m_readCrossSections
      use m_network
      type(t_network), intent(inout) :: network !< Network structure
      type(t_longculvert), intent(in) :: longculvert !< Branch id on which to place the cross section

      integer :: inext

      inext = network%brs%count + 1
      if (inext > network%brs%size) then
         call realloc(network%brs)
      end if

      network%brs%branch(inext)%Id = longculvert%branchId

   end subroutine add_longculvert_branch

   !> Fills in flowlink numbers for a given longculvert.
   !! Note 1: This long culvert is considered invalid if its starting node, or ending node, is outside the global network.
   !! Note 2: In a parallel simulation, the flowlink number gets 0 if the flowlink is not on the current subdomain.
   !! Note 3: In a parallel simulation, it can happen that the starting (ending) node of the polylin of the long culvert is
   !! not on the current subdomain. In this case, the starting (ending) node ON the current subdomain is
   !! found firstly, and then search flowlinks for the interior polyline points.
   !! TODO (UNST-6073): currently it does not support the situation when, in a parallel simulation, the polyline enters
   !! one subdomain, then leaves, and then enters again.
   subroutine find1d2dculvertlinks(network, longculvert, numcoords)

      use m_cell_geometry, only: xz, yz
      use m_network
      use m_flowgeom
      use network_data, only: lne
      use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
      use precision_basics, only: comparereal
      use m_flowparameters, only: EPS10
      use m_partitioninfo, only: jampi, reduce_int_max
      use m_find_flownode, only: find_nearest_flownodes_kdtree
      use m_hash_search
      use m_find_flownode, only: find_nearest_flownodes_kdtree
      use kdtree2Factory, only: treeglob
      use m_save_ugrid_state, only: contact_cell_idx, contactnetlinks, hashlist_contactids
      use network_data, only: LINK_1D, LINK_1D2D_STREETINLET

      implicit none

      type(t_network), intent(inout) :: network !< Network structure
      integer, intent(in) :: numcoords !< number of polyline coordinates
      type(t_longculvert), intent(inout) :: longculvert !< A givin long culvert
      integer :: i, j, branch_idx, contact_idx, othernode, nodenum, linknum, linkabs, is, ie, jafounds, jafounde, L_net
      integer, allocatable :: inode(:), inodeGlob(:), jnode(:)

      integer :: ierror

      longculvert%flowlinks = 0
      jafounds = 0 ! Found the starting node or not
      jafounde = 0 ! Found the ending node or not
      is = 1 ! the starting node of the polyline
      ie = numcoords ! the ending node of the polyline

      ! Find the flownode numbers for the starting and ending points of the long culvert polyline
      call realloc(inode, 2, keepExisting=.false., fill=0)
      call realloc(inodeGlob, 2, keepExisting=.false., fill=0)

      branch_idx = hashsearch(network%brs%hashlist, longculvert%branchId)
      contact_idx = hashsearch(hashlist_contactids, longculvert%contactId)

      if (jampi == 0 .and. branch_idx <= 0 .and. contact_idx <= 0) then
         call mess(LEVEL_WARN, 'find1d2dculvertlinks: cannot find the branch or contact corresponding to the long culvert '//trim(longculvert%branchId)//' '//trim(longculvert%contactId))
         return
      end if
      !Find the last 1D node of the branch
      if (branch_idx > 0 .and. network%BRS%size >= i) then
         inode(1) = network%BRS%Branch(branch_idx)%FROMNODE%GRIDNUMBER
         inode(2) = network%BRS%Branch(branch_idx)%TONODE%GRIDNUMBER
      else if (contact_idx > 0) then ! 2D2D contact, read long culvert info directly from contacts array
         L_net = contactnetlinks(contact_idx)
         inode(1) = abs(lne(2, L_net)) !> reverse direction for 2D2D contact
         inode(2) = abs(lne(1, L_net))
      end if

      inodeGlob(1:2) = inode(1:2)
      if (jampi > 0) then
         ! Communicate inode in parallel run to get inodeGlob
         call reduce_int_max(2, inodeGlob)
      end if

      if (inodeGlob(1) <= 0 .or. inodeGlob(2) <= 0) then
         ! The long culvert is not valid if its starting or ending node is outside the global network
         longculvert%numlinks = 0
         call mess(LEVEL_WARN, 'find1d2dculvertlinks: a long culvert is not valid if its starting or ending node is outside the global network.')
         return
      else ! This long culvert is valid on the current domain
         ! check the starting node
         if (inode(1) > 0) then ! The starting node is inside the current domain
            nodenum = inode(1)
            do i = 1, nd(nodenum)%lnx
               linkabs = abs(nd(nodenum)%ln(i))
               if (kcu(abs(linkabs)) == 5) then
                  longculvert%flownode_up = ln(1, linkabs) + ln(2, linkabs) - nodenum
                  ! For the later search
                  jafounds = 1
               end if
            end do
         else
            ! Find the first known flow node in the current partition (if 2D flow node was not found outside of the loop already)
            call realloc(jnode, 1, keepExisting=.false., fill=0)
            do j = is + 1, ie - 1
               call find_nearest_flownodes_kdtree(treeglob, 1, longculvert%xcoords(j), longculvert%ycoords(j), jnode, 1, INDTP_1D, ierror)
               if (ierror == 0 .and. jnode(1) > 0) then
                  nodenum = jnode(1) ! For the later search
                  is = j ! this will be the starting node of the long culvert in current domain
                  jafounds = 1
                  exit
               end if
            end do
         end if

         ! check the ending node
         if (inode(2) > 0) then ! The ending node is inside the current domain
            do i = 1, nd(inode(2))%lnx
               linkabs = abs(nd(inode(2))%ln(i))
               if (kcu(abs(linkabs)) == 5) then
                  longculvert%flownode_dn = ln(1, linkabs) + ln(2, linkabs) - inode(2)
                  ! For the later search
                  jafounde = 1
               end if
            end do
         else
            ! Find the last known flow node in the current partition (if 2D flow ndoe was not found outside of the loop already)
            call realloc(jnode, 1, keepExisting=.false., fill=0)
            do j = ie - 1, is + 1, -1
               call find_nearest_flownodes_kdtree(treeglob, 1, longculvert%xcoords(j), longculvert%ycoords(j), jnode, 1, INDTP_1D, ierror)
               if (ierror == 0 .and. jnode(1) > 0) then
                  ie = j ! this will be the ending node of the long culvert in current domain
                  jafounde = 1
                  exit
               end if
            end do
         end if
      end if

      if (jafounds == 1 .and. jafounde == 1) then
         if (contact_idx > 0) then
            longculvert%flowlinks(1) = contactnetlinks(contact_idx)
         else
            do i = 1, nd(nodenum)%lnx
               linknum = nd(nodenum)%ln(i)
               if (kcu(abs(linknum)) == 5) then
                  longculvert%flowlinks(1) = -1 * linknum
                  is = is + 1
                  exit
               end if
            end do
            ! For the interior polyline points
            do j = is, ie - 1 ! j is link index, or , right node index
               if (j > is) then !> don't traverse 1D2D links
                  nodenum = othernode
               end if
               if (nodenum > 0) then
                  do i = 1, nd(nodenum)%lnx
                     linknum = nd(nodenum)%ln(i)
                     linkabs = abs(linknum)
                     othernode = ln(1, linkabs) + ln(2, linkabs) - nodenum

                     if (j <= ie) then
                        if ((kcu(linkabs) == LINK_1D .or. kcu(linkabs) == LINK_1D2D_STREETINLET) .and. (comparereal(xz(othernode), longculvert%xcoords(j + 1), EPS10) == 0 .and. comparereal(yz(othernode), longculvert%ycoords(j + 1), EPS10) == 0)) then
                           longculvert%flowlinks(j) = -1 * linknum
                           exit
                        end if
                     end if
                  end do
               end if
            end do
         end if
      end if
   end subroutine

   !> Find 2D netcell the longculvert endpoint is located in, add a new node and return its node number
   subroutine longculvert_create_endpoint(x, y, z, k)
      use precision, only: dp
      use network_data, only: xzw, yzw, zk
      use gridoperations, only: setnewpoint, incells

      real(kind=dp), intent(in) :: x, y, z !< coordinates of long culvert endpoint as read from polyline
      integer, intent(out) :: k !< new node index

      integer :: node1d2d
      real(kind=dp) :: x2, y2 !> coordinates of new "snapped" long culvert endpoint

      call incells(x, y, node1d2d)
      if (node1d2d == 0) then
         write (msgbuf, '(a,g0.4,a,g0.4,a)') 'No 2D cell found for long culvert endpoint at (x,y) = (', x, ', ', y, '). Please check the netFile and structureFile.'
         call err_flush()
      end if
      x2 = xzw(node1d2d)
      y2 = yzw(node1d2d)
      call setnewpoint(x2, y2, z, k)
   end subroutine longculvert_create_endpoint

   !> Counts the number of long culverts in the structure file, and determine the input type (with crsdef/brid or only polyline)
   subroutine count_long_culverts_in_structure_file(structurefiles)
      use dfm_error
      use string_module, only: strcmpi, strsplit
      use m_missing
      use m_Roughness
      use m_readstructures
      use m_network
      use messageHandling
      use properties
      use unstruc_channel_flow
      use m_unstruc_model_data, only: md_convertlongculverts

      character(len=*), intent(in) :: structurefiles !< File name of the structure.ini file.

      type(tree_data), pointer :: strs_ptr

      integer :: readerr, nstr, i
      integer :: num_longculverts, num_newculverts
      integer :: ifil, ierr
      character(len=256), dimension(:), allocatable :: structurefiles_array
      character(:), allocatable :: structurefile
      integer, dimension(:), allocatable :: longculvert_indices
      type(tree_data), dimension(:), allocatable :: nodes
      num_longculverts = 0
      num_newculverts = 0

      call strsplit(structurefiles, 1, structurefiles_array, 1)
      do ifil = 1, size(structurefiles_array)
         structurefile = structurefiles_array(ifil)

         ! Temporarily put structures.ini file into a property tree
         call tree_create(trim(structurefile), strs_ptr)
         call prop_inifile(structurefile, strs_ptr, readerr)
         ! check if file was successfully opened
         if (readerr /= 0) then
            ierr = DFM_WRONGINPUT
            call mess(LEVEL_ERROR, 'Error opening file ''', trim(structurefile), ''' for loading the long culverts.')
         end if
         nstr = tree_num_nodes(strs_ptr)
         allocate (nodes(nstr))
         do i = 1, nstr
            nodes(i) = strs_ptr%child_nodes(i)%node_ptr
         end do

         longculvert_indices = pack([(i, i=1, nstr)], strcmpi(tree_get_name(nodes), 'Structure') .and. node_has_key(nodes, 'type', 'longCulvert'))
         nodes = nodes(longculvert_indices)

         num_longculverts = num_longculverts + size(nodes)
         num_newculverts = num_newculverts + count(node_has_key(nodes, 'branchId')) + count(node_has_key(nodes, 'contactId'))
         deallocate (nodes)
      end do
      if (num_longculverts > 0) then
         nlongculverts = num_longculverts
         if (num_newculverts > 0) then
            newculverts = .true.
            md_convertlongculverts = 0
            if (num_newculverts /= num_longculverts) then
               call mess(LEVEL_ERROR, 'Error loading long culverts, only one input type is supported!')
            end if
         end if
      end if

   end subroutine count_long_culverts_in_structure_file

   subroutine initialize_long_culverts(md_1dfiles, md_convertlongculverts, write_converted_files)
      use m_set_nod_adm, only: setnodadm
      use m_globalparameters, only: t_filenames
      use network_data, only: numl1d

      type(t_filenames), intent(inout) :: md_1dfiles
      integer, intent(in) :: md_convertlongculverts !< Flag to indicate whether to convert old-style long culverts on-the-fly.
      logical, optional, intent(in) :: write_converted_files !< Whether or not to write the converted structures and cross-sections files. (default = .false.)
      character(:), allocatable :: structure_files
      logical :: write_converted_files_
      integer :: ilongc, num_longculvert_links

      write_converted_files_ = .false.
      if (present(write_converted_files)) then
         write_converted_files_ = write_converted_files
      end if

      structure_files = md_1dfiles%structures
      call count_long_culverts_in_structure_file(structure_files)
      if (nlongculverts > 0) then
         nlongculverts = 0 ! makelongculverts breaks if culverts are counted but not created
         if (newculverts) then
            call initialize_existing_long_culverts(structure_files)
         else !> old style long culvert input
            if (md_convertlongculverts > 0) then !> convert on-the-fly
               call makelongculverts_commandline(md_1dfiles, write_converted_files=write_converted_files_)
               newculverts = .true.
            else ! initialize old-style (Herman) long culverts
               call initialize_existing_long_culverts(structure_files)
               call setnodadm(0)
               call finalizeLongCulvertsInNetwork()
            end if
         end if
      end if

      ! Determine whether all 1D netlinks belong to long culverts.
      num_longculvert_links = 0
      do ilongc = 1, nlongculverts
         num_longculvert_links = num_longculvert_links + longculverts(ilongc)%numlinks
      end do

      only_longculvert_1D = (num_longculvert_links == numl1d)

   end subroutine initialize_long_culverts

   subroutine initialize_existing_long_culverts(structure_files)
      use dfm_error
      use string_module, only: strsplit
      implicit none
      character(len=*), intent(in) :: structure_files !< File name of the structure.ini file.
      integer :: ierr
      integer :: ifil
      character(len=256), dimension(:), allocatable :: structure_files_array

      ierr = 0
      call strsplit(structure_files, 1, structure_files_array, 1)
      call loadLongCulvertsAsNetwork(structure_files_array(1), 0, ierr)
      do ifil = 2, size(structure_files_array)
         call loadLongCulvertsAsNetwork(structure_files_array(ifil), 1, ierr)
      end do
   end subroutine initialize_existing_long_culverts

   subroutine makelongculverts_commandline(md_1dfiles, write_converted_files)
      use m_readstructures
      use string_module, only: strsplit
      use unstruc_netcdf, only: unc_write_net, UNC_CONV_UGRID
      use system_utils
      use m_set_nod_adm
      use messagehandling, only: IDLEN
      use gridoperations, only: findcells
      use m_globalparameters, only: t_filenames
      use unstruc_channel_flow, only: network
      use m_network, only: admin_network
      use m_1d_networkreader, only: construct_network_from_meshgeom
      use m_save_ugrid_state
      use m_node, only: dealloc
      use m_branch, only: dealloc
      use m_unstruc_model_data, only: md_partugrid

      type(t_filenames), intent(inout) :: md_1dfiles
      logical, optional, intent(in) :: write_converted_files !< Whether or not to write the converted structures and cross-sections files. (default = .false.)
      character(len=:), allocatable :: md_culvertprefix
      character(len=:), allocatable :: converted_fnamesstring
      character(len=:), allocatable :: converted_crsdefsstring
      character(len=:), allocatable :: tempstring_crsdef
      character(len=:), allocatable :: tempstring_fnames
      character(len=200), dimension(:), allocatable :: fnames

      logical :: write_converted_files_
      integer :: istat, ifil, ierr, i

      write_converted_files_ = .false.
      if (present(write_converted_files)) then
         write_converted_files_ = write_converted_files
      end if

      call findcells(0)
      md_culvertprefix = 'converted_'
      if (len_trim(md_1dfiles%structures) > 0) then

         call strsplit(md_1dfiles%structures, 1, fnames, 1)

         if (len_trim(md_1dfiles%cross_section_definitions) > 0) then
            call convertLongCulvertsAsNetwork(fnames(1), 0, md_culvertprefix, converted_fnamesstring, converted_crsdefsstring, istat, md_1dfiles%cross_section_definitions, write_converted_files=write_converted_files_)
         else
            call convertLongCulvertsAsNetwork(fnames(1), 0, md_culvertprefix, converted_fnamesstring, converted_crsdefsstring, istat, write_converted_files=write_converted_files_)
         end if
         do ifil = 2, size(fnames)
            call convertLongCulvertsAsNetwork(fnames(ifil), 1, md_culvertprefix, tempstring_fnames, tempstring_crsdef, istat, write_converted_files=write_converted_files_)
            converted_crsdefsstring = trim(trim(converted_crsdefsstring)//', ')//tempstring_crsdef
            converted_fnamesstring = trim(trim(converted_fnamesstring)//', ')//tempstring_fnames
         end do
         deallocate (fnames)
         call setnodadm(0)
         call finalizeLongCulvertsInNetwork()

         if (meshgeom1d%numedge > 0) then !> only create a network if actual 1D links exist (4+ point long culverts)
            nbranchlongnames = nbranchids
            nnodelongnames = nnodeids
            call realloc(nodeids, meshgeom1d%numnode)
            call realloc(nodelongnames, meshgeom1d%numnode)
            network%numl = meshgeom1d%numedge
            call dealloc(network%nds)
            call dealloc(network%brs)

            ierr = construct_network_from_meshgeom(network, meshgeom1d, nbranchids, nbranchlongnames, nnodeids, &
                                                   nnodelongnames, nodeids, nodelongnames, network1dname, mesh1dname, 0, 0, 0)
         else
            md_partugrid = 1 ! set ugrid partitioning flag explicitly to true, so that the 2D-2D long culvert contact will be written to the partitioned ugrid file
         end if
         do i = 1, nlongculverts
            call addlongculvertcrosssections(network, longculverts(i)%branchid, longculverts(i)%csDefId, longculverts(i)%bl, ierr)
         end do
         i = 0
         call admin_network(network, i)

         md_1dfiles%structures = converted_fnamesstring
         md_1dfiles%cross_section_definitions = converted_crsdefsstring
      end if

   end subroutine makelongculverts_commandline

   elemental function node_has_key(node, keystr, valuestr) result(has_key)
      use string_module, only: strcmpi
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_get_name, tree_get_data
      type(tree_data), intent(in) :: node
      character(len=*), intent(in) :: keystr
      character(len=*), intent(in), optional :: valuestr
      logical :: has_key
      integer :: i

      has_key = .false.
      if (associated(node%child_nodes)) then
         do i = 1, size(node%child_nodes)
            if (strcmpi(tree_get_name(node%child_nodes(i)%node_ptr), keystr)) then
               if (present(valuestr)) then
                  if (strcmpi(tree_get_data(node%child_nodes(i)%node_ptr), trim(valuestr))) then
                     has_key = .true.
                     return
                  end if
               else
                  has_key = .true.
                  return
               end if
            end if
         end do
      end if
   end function node_has_key

end module m_longculverts
