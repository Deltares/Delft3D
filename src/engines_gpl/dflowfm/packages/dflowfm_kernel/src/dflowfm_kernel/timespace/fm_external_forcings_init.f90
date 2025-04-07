!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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
submodule(fm_external_forcings) fm_external_forcings_init
   use precision_basics, only: dp
   implicit none

   integer, parameter :: INI_VALUE_LEN = 256
   integer, parameter :: INI_KEY_LEN = 32

contains

   !> reads new external forcings file and makes required initialisations. Only to be called once as part of fm_initexternalforcings.
   module subroutine init_new(external_force_file_name, iresult)
      use properties, only: get_version_number, prop_file
      use tree_structures, only: tree_data, tree_create, tree_destroy, tree_num_nodes, tree_count_nodes_byname, tree_get_name
      use messageHandling, only: warn_flush, err_flush, msgbuf, LEVEL_FATAL
      use fm_external_forcings_data, only: nbndz, itpenz, nbndu, itpenu, thrtt, set_lateral_count_in_external_forcings_file
      use m_flowgeom, only: ba
      use m_laterals, only: balat, qplat, lat_ids, n1latsg, n2latsg, kclat, numlatsg, nnlat
      use string_module, only: str_tolower
      use system_utils, only: split_filename
      use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
      use m_ec_parameters, only: provFile_uniform
      use m_partitioninfo, only: jampi, reduce_sum, is_ghost_node
      use m_flow, only: kmx
      use m_deprecation, only: check_file_tree_for_deprecated_keywords
      use fm_deprecated_keywords, only: deprecated_ext_keywords
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use m_alloc, only: realloc
      use unstruc_messages, only: threshold_abort
      use m_reallocsrc, only: reallocsrc

      character(len=*), intent(in) :: external_force_file_name !< file name for new external forcing boundary blocks
      integer, intent(inout) :: iresult !< integer error code. Intent(inout) to preserve earlier errors.

      integer :: initial_threshold_abort
      logical :: res
      logical :: is_successful
      type(tree_data), pointer :: bnd_ptr !< tree of extForceBnd-file's [boundary] blocks
      type(tree_data), pointer :: node_ptr
      integer :: istat
      character(len=:), allocatable :: group_name
      integer :: i
      integer :: num_items_in_file
      character(len=INI_VALUE_LEN) :: fnam, base_dir
      integer :: k, n, k1
      integer :: ib, ibqh, ibt
      integer :: maxlatsg, max_num_src
      integer :: major, minor
      character(len=:), allocatable :: file_name
      integer, allocatable :: itpenzr(:), itpenur(:)

      iresult = DFM_NOERR
      file_name = trim(external_force_file_name)
      if (len_trim(file_name) <= 0) then
         ! empty line in MDU is allowed: exit without error
         return
      end if

      res = .true.

      call tree_create(file_name, bnd_ptr)
      call prop_file('ini', file_name, bnd_ptr, istat)
      if (istat /= 0) then
         write (msgbuf, '(a,a,a)') 'External forcing file ''', trim(file_name), ''' could not be read'
         call err_flush()
         iresult = DFM_WRONGINPUT
         return
      end if

      ! check FileVersion
      major = 1
      minor = 0
      call get_version_number(bnd_ptr, major=major, minor=minor, success=is_successful)
      if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in ''' &
            //file_name//''': v', major, minor, '. Current format: v', ExtfileNewMajorVersion, ExtfileNewMinorVersion, &
            '. Ignoring this file.'
         call err_flush()
         iresult = DFM_WRONGINPUT
         return
      end if

      call init_registered_items()

      call split_filename(file_name, base_dir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

      num_items_in_file = tree_num_nodes(bnd_ptr)

      ! Build temporary reverse lookup table that maps boundary block # in file -> boundary condition nr in openbndsect (separate for u and z).
      allocate (itpenzr(num_items_in_file))
      allocate (itpenur(num_items_in_file))
      itpenzr(:) = 0
      itpenur(:) = 0
      do ibt = 1, nbndz
         ib = itpenz(ibt)
         if (ib > 0 .and. ib <= num_items_in_file) then
            itpenzr(ib) = ibt
         end if
      end do
      do ibt = 1, nbndu
         ib = itpenu(ibt)
         if (ib > 0 .and. ib <= num_items_in_file) then
            itpenur(ib) = ibt
         end if
      end do

      ! Allocate lateral provider array now, just once, because otherwise realloc's in the loop would destroy target arrays in ecInstance.
      maxlatsg = tree_count_nodes_byname(bnd_ptr, 'lateral')
      if (maxlatsg > 0) then
         call realloc(balat, maxlatsg, keepExisting=.false., fill=0d0)
         call realloc(qplat, (/max(1, kmx), maxlatsg/), keepExisting=.false., fill=0d0)
         call realloc(lat_ids, maxlatsg, keepExisting=.false.)
         call realloc(n1latsg, maxlatsg, keepExisting=.false., fill=0)
         call realloc(n2latsg, maxlatsg, keepExisting=.false., fill=0)
      end if

      ! Allocate source-sink related arrays now, just once, because otherwise realloc's in the loop would destroy target arrays in ecInstance.
      max_num_src = tree_count_nodes_byname(bnd_ptr, 'sourcesink')
      if (max_num_src > 0) then
         call reallocsrc(max_num_src, 0)
      end if

      ib = 0
      ibqh = 0
      initial_threshold_abort = threshold_abort
      threshold_abort = LEVEL_FATAL
      do i = 1, num_items_in_file
         node_ptr => bnd_ptr%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(node_ptr))

         select case (str_tolower(group_name))
         case ('general')
            ! General block, was already read.

         case ('boundary')
            res = res .and. init_boundary_forcings(node_ptr, base_dir, file_name, group_name, itpenzr, itpenur, ib, ibqh)

         case ('lateral')
            res = res .and. init_lateral_forcings(node_ptr, base_dir, i, major)

         case ('meteo')
            res = res .and. init_meteo_forcings(node_ptr, base_dir, file_name, group_name)

         case ('sourcesink')
            res = res .and. init_sourcesink_forcings(node_ptr, base_dir, file_name, group_name)

         case default ! Unrecognized item in an ext block
            ! res remains unchanged: Not an error (support commented/disabled blocks in ext file)
            write (msgbuf, '(5a)') 'Unrecognized block in file ''', file_name, ''': [', group_name, ']. Ignoring this block.'
            call warn_flush()
         end select
      end do
      threshold_abort = initial_threshold_abort

      if (allocated(itpenzr)) deallocate (itpenzr)
      if (allocated(itpenur)) deallocate (itpenur)
      if (numlatsg > 0) then
         do n = 1, numlatsg
            balat(n) = 0d0
            do k1 = n1latsg(n), n2latsg(n)
               k = nnlat(k1)
               if (k > 0) then
                  if (.not. is_ghost_node(k)) then
                     balat(n) = balat(n) + ba(k)
                  end if
               end if
            end do
         end do
         if (jampi > 0) then
            call reduce_sum(numlatsg, balat)
         end if
         if (allocated(kclat)) then
            deallocate (kclat)
         end if
      end if

      call check_file_tree_for_deprecated_keywords(bnd_ptr, deprecated_ext_keywords, istat, prefix='While reading '''//trim(file_name)//'''')

      call set_lateral_count_in_external_forcings_file(numlatsg) !save number of laterals to module variable

      call tree_destroy(bnd_ptr)
      if (allocated(thrtt)) then
         call init_threttimes()
      end if

      if (res) then
         iresult = DFM_NOERR
      else
         iresult = DFM_WRONGINPUT
      end if
   end subroutine init_new

   !> reads boundary blocks from new external forcings file and makes required initialisations
   function init_boundary_forcings(node_ptr, base_dir, file_name, group_name, itpenzr, itpenur, ib, ibqh) result(res)
      use tree_data_types, only: tree_data
      use fm_external_forcings_data, only: filetype, qhpliname
      use timespace_parameters, only: NODE_ID
      use timespace_data, only: WEIGHTFACTORS, POLY_TIM, SPACEANDTIME, getmeteoerror
      use tree_structures, only: tree_get_name, tree_get_data_string
      use messageHandling, only: mess, LEVEL_ERROR, err_flush, warn_flush, msgbuf
      use string_module, only: strcmpi
      use properties, only: prop_get
      use unstruc_files, only: resolvePath

      type(tree_data), pointer, intent(in) :: node_ptr !< The tree node of the boundary block
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file.
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in warning messages, actual data is read from node_ptr.
      character(len=*), intent(in) :: group_name !< Name of the block, only used in warning messages.
      integer, dimension(:), intent(in) :: itpenzr !< boundary condition nr in openbndsect for z
      integer, dimension(:), intent(in) :: itpenur !< boundary condition nr in openbndsect for u
      integer, intent(inout) :: ib !< block counter for boundaries
      integer, intent(inout) :: ibqh !< block counter for qh boundaries
      logical :: res 

      integer, dimension(1) :: target_index
      character(len=INI_VALUE_LEN) :: location_file, quantity, forcing_file, property_name, property_value
      type(tree_data), pointer :: block_ptr
      character(len=300) :: error_message
      character(len=1) :: oper
      logical :: is_successful
      integer :: method, num_items_in_block, j

      res = .true.
      
      ! First check for required input:
      call prop_get(node_ptr, '', 'quantity', quantity, is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
         call err_flush()
         return
      end if
      ib = ib + 1

      call prop_get(node_ptr, '', 'nodeId', location_file, is_successful)
      if (is_successful) then
         filetype = NODE_ID
         method = SPACEANDTIME
      else
         filetype = POLY_TIM
         method = WEIGHTFACTORS
         call prop_get(node_ptr, '', 'locationFile', location_file, is_successful)
      end if

      if (is_successful) then
         call resolvePath(location_file, base_dir)
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''locationFile'' is missing.'
         call err_flush()
         return
      end if

      call prop_get(node_ptr, '', 'forcingFile ', forcing_file, is_successful)
      if (is_successful) then
         call resolvePath(forcing_file, base_dir)
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
         call err_flush()
         return
      end if

      oper = '-'
      call prop_get(node_ptr, '', 'operand ', oper, is_successful)

      num_items_in_block = 0
      if (associated(node_ptr%child_nodes)) then
         num_items_in_block = size(node_ptr%child_nodes)
      end if

      ! Perform dummy-reads of supported keywords to prevent them from being reported as unused input.
      ! The keywords below were already read in read_location_files_from_boundary_blocks().
      call prop_get(node_ptr, '', 'returnTime', property_value)
      call prop_get(node_ptr, '', 'return_time', property_value)
      call prop_get(node_ptr, '', 'openBoundaryTolerance', property_value)
      call prop_get(node_ptr, '', 'nodeId', property_value)
      call prop_get(node_ptr, '', 'bndWidth1D', property_value)
      call prop_get(node_ptr, '', 'bndBlDepth', property_value)

      ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingFile=...
      do j = 1, num_items_in_block
         block_ptr => node_ptr%child_nodes(j)%node_ptr
         ! todo: read multiple quantities
         property_name = trim(tree_get_name(block_ptr))
         call tree_get_data_string(block_ptr, property_value, is_successful)
         if (is_successful) then
            if (strcmpi(property_name, 'forcingFile')) then
               forcing_file = property_value
               call resolvePath(forcing_file, base_dir)
               if (oper /= 'O' .and. oper /= '+') then
                  oper = 'O'
                  if (quantity_pli_combination_is_registered(quantity, location_file)) then
                     oper = '+'
                  end if
               end if
               call register_quantity_pli_combination(quantity, location_file)
               if (filetype == NODE_ID .or. quantity == 'qhbnd') then
                  select case (quantity)
                  case ('waterlevelbnd')
                     target_index = itpenzr(ib)

                  case ('qhbnd')
                     ibqh = ibqh + 1
                     target_index = (/ibqh/)
                     if (filetype /= NODE_ID) then
                        location_file = qhpliname(ibqh)
                     end if

                  case ('dischargebnd')
                     target_index = itpenur(ib)

                  case default
                     target_index = (/-1/)
                  end select

                  if (target_index(1) <= 0) then
                     ! This boundary has been skipped in an earlier phase (findexternalboundarypoints),
                     ! so, also do *not* connect it as a spacetimerelation here.
                     is_successful = .true. ! No failure: boundaries are allowed to remain disconnected.
                  else
                     is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=NODE_ID, method=method, &
                                                                     operand=oper, forcing_file=forcing_file, targetindex=target_index(1))
                  end if
               else
                  is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=method, &
                                                                  operand=oper, forcing_file=forcing_file)
               end if
               res = res .and. is_successful ! Remember any previous errors.
               oper = '-'
            end if
         end if
      end do
      if (.not. is_successful) then ! This addtimespace was not successful
         error_message = getmeteoerror()
         if (len_trim(error_message) > 0) then
            call mess(LEVEL_ERROR, trim(error_message))
         end if
         call mess(LEVEL_ERROR, 'initboundaryblockforcings: Error while initializing quantity '''//trim(quantity)// &
                   '''. Check preceding log lines for details.')
      end if

   end function init_boundary_forcings

   !> Read the discharge specification by the current [Lateral] block from new external forcings file.
   !! File version 1 only allowed for a locationFile, file version 2.01 allowed for nodeId, branchId + chainage, numCoordinates + xCoordinates + yCoordinates.
   !! File version 2.02 allows for everything: locationFile, nodeId, branchId + chainage, numCoordinates + xCoordinates + yCoordinates.
   subroutine read_lateral_discharge_definition(node_ptr, loc_id, base_dir, ilattype, loc_spec_type, node_id, branch_id, chainage, num_coordinates, x_coordinates, y_coordinates, location_file, is_success)
      use messageHandling, only: mess, err, LEVEL_ERROR
      use precision, only: dp
      use m_missing, only: imiss, dmiss
      use properties, only: has_key, prop_get
      use tree_data_types, only: tree_data
      use timespace_parameters, only: LOCTP_NODEID, LOCTP_BRANCHID_CHAINAGE, LOCTP_POLYGON_XY, LOCTP_POLYGON_FILE
      use m_laterals, only: ILATTP_1D
      use unstruc_files, only: resolvePath

      type(tree_data), pointer, intent(in) :: node_ptr !< The tree node of the lateral block
      character(len=*), intent(in) :: loc_id !< The id of the lateral
      character(len=*), intent(in) :: base_dir !< The base directory of the lateral
      integer, intent(inout) :: ilattype !< The type of lateral (1D, 2D, or both)
      integer, intent(out) :: loc_spec_type !< Specify how lateral discharge is defined
      character(len=*), intent(out) :: node_id !< The node id of the lateral, only set if loc_spec_type = LOCTP_NODEID
      character(len=*), intent(out) :: branch_id !< The branch id of the lateral, only set if loc_spec_type = LOCTP_BRANCHID_CHAINAGE
      real(kind=dp), intent(out) :: chainage !< The chainage of the lateral, only set if loc_spec_type = LOCTP_BRANCHID_CHAINAGE
      integer, intent(out) :: num_coordinates !< The number of coordinates of the lateral, only set if loc_spec_type = LOCTP_POLYGON_XY
      real(kind=dp), allocatable, intent(out) :: x_coordinates(:), y_coordinates(:) !< The x and y coordinates of the lateral, only set if loc_spec_type = LOCTP_POLYGON_XY
      character(len=*), intent(out) :: location_file !< The location file of the lateral, only set if loc_spec_type = LOCTP_POLYGON_FILE
      logical, intent(out) :: is_success !< Flag indicating if the reading was successful

      logical :: has_node_id, has_branch_id, has_chainage, has_num_coordinates, has_location_file, has_x_coordinates, has_y_coordinates
      integer :: number_of_discharge_specifications, ierr
      integer, parameter :: maximum_number_of_discharge_specifications = 4

      loc_spec_type = imiss
      node_id = ''
      branch_id = ''
      chainage = dmiss
      num_coordinates = imiss
      location_file = ''
      is_success = .false.

      has_node_id = has_key(node_ptr, 'Lateral', 'nodeId')
      has_branch_id = has_key(node_ptr, 'Lateral', 'branchId')
      has_chainage = has_key(node_ptr, 'Lateral', 'chainage')
      has_num_coordinates = has_key(node_ptr, 'Lateral', 'numCoordinates')
      has_x_coordinates = has_key(node_ptr, 'Lateral', 'xCoordinates')
      has_y_coordinates = has_key(node_ptr, 'Lateral', 'yCoordinates')
      has_location_file = has_key(node_ptr, 'Lateral', 'locationFile')

      ! Test if multiple discharge methods were set
      number_of_discharge_specifications = sum([(1, integer :: i=1, maximum_number_of_discharge_specifications)], [has_node_id, has_branch_id .or. has_chainage, has_num_coordinates .or. has_x_coordinates .or. has_y_coordinates, has_location_file])

      if (number_of_discharge_specifications < 1) then
         call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': No discharge specifications found. Use nodeId, branchId + chainage, numCoordinates + xCoordinates + yCoordinates, or locationFile.')
         return
      else if (number_of_discharge_specifications > 1) then
         call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': Multiple discharge specifications found. Use nodeId, branchId + chainage, numCoordinates + xCoordinates + yCoordinates, or locationFile.')
         return
      end if

      ! nodeId                  => location_specifier = LOCTP_NODEID
      ! branchId+chainage       => location_specifier = LOCTP_BRANCH_CHAINAGE
      ! numcoor+xcoors+ycoors   => location_specifier = LOCTP_XY_POLYGON
      ! locationFile = test.pol => location_specifier = LOCTP_POLYGON_FILE
      if (has_node_id) then
         call prop_get(node_ptr, 'Lateral', 'nodeId', node_id)
         loc_spec_type = LOCTP_NODEID
         ilattype = ILATTP_1D
         is_success = .true.
         return
      end if

      if (has_branch_id .or. has_chainage) then
         if (.not. (has_branch_id .and. has_chainage)) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': branchId and chainage must be set together.')
            return
         end if

         call prop_get(node_ptr, 'Lateral', 'branchId', branch_id)
         call prop_get(node_ptr, 'Lateral', 'chainage', chainage)
         if (len_trim(branch_id) > 0 .and. chainage /= dmiss .and. chainage >= 0.0d0) then
            loc_spec_type = LOCTP_BRANCHID_CHAINAGE
            ilattype = ILATTP_1D
            is_success = .true.
            return
         else
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': values of branchId and chainage are invalid.')
            return
         end if
      end if

      if (has_num_coordinates .or. has_x_coordinates .or. has_y_coordinates) then
         if (.not. (has_num_coordinates .and. has_x_coordinates .and. has_y_coordinates)) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': numCoordinates, xCoordinates and yCoordinates must be set together.')
            return
         end if
         call prop_get(node_ptr, 'Lateral', 'numCoordinates', num_coordinates)
         if (num_coordinates <= 0) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': numCoordinates must be greater than 0.')
            return
         end if
         allocate (x_coordinates(num_coordinates), stat=ierr)
         allocate (y_coordinates(num_coordinates), stat=ierr)
         call prop_get(node_ptr, 'Lateral', 'xCoordinates', x_coordinates, num_coordinates)
         call prop_get(node_ptr, 'Lateral', 'yCoordinates', y_coordinates, num_coordinates)
         loc_spec_type = LOCTP_POLYGON_XY
         is_success = .true.
         return
      end if

      if (has_location_file) then
         location_file = ''
         call prop_get(node_ptr, 'Lateral', 'locationFile', location_file)
         if (len_trim(location_file) == 0) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': locationFile is empty.')
            return
         end if
         call resolvePath(location_file, base_dir)
         loc_spec_type = LOCTP_POLYGON_FILE
         is_success = .true.
         return
      end if
      call err('Programming error, please report: read_lateral_discharge_definition failed to read lateral '''//trim(loc_id)//'''')
   end subroutine read_lateral_discharge_definition

   !> Read lateral blocks from new external forcings file and makes required initialisations
   function init_lateral_forcings(node_ptr, base_dir, block_number, major) result(is_successful)
      use messageHandling, only: err_flush, msgbuf, mess, LEVEL_ERROR, LEVEL_INFO
      use string_module, only: str_tolower
      use tree_data_types, only: tree_data
      use m_laterals, only: qplat, lat_ids, n1latsg, n2latsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, numlatsg, nnlat, nlatnd, apply_transport
      use m_flowgeom, only: ndxi, xz, yz
      use m_alloc, only: realloc, reserve_sufficient_space
      use fm_external_forcings_data, only: kx, qid
      use m_wind, only: jaqin
      use properties, only: prop_get
      use unstruc_files, only: resolvePath
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use timespace, only: selectelset_internal_nodes

      type(tree_data), pointer, intent(in) :: node_ptr !< Tree structure containing the lateral block.
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file.
      integer, intent(in) :: block_number !< Number of the block, only used in error message.
      integer, intent(in) :: major !< Major version number of ext-file

      character(len=INI_VALUE_LEN) :: loc_id
      integer :: loc_spec_type, num_coordinates
      character(len=INI_VALUE_LEN) :: node_id, branch_id, location_file, item_type
      real(kind=dp) :: chainage
      real(kind=dp), allocatable :: x_coordinates(:), y_coordinates(:)
      logical :: is_successful, is_read
      character(len=300) :: rec
      integer :: ilattype, nlat, ierr

      is_successful = .false.

      loc_id = ' '
      call prop_get(node_ptr, 'Lateral', 'id', loc_id, is_read)
      if (.not. is_read .or. len_trim(loc_id) == 0) then
         write (msgbuf, '(a,i0,a)') 'Required field ''id'' missing in lateral (block #', block_number, ').'
         call err_flush()
         return
      end if

      ! locationType = optional for lateral
      ! locationType = 1d | 2d | all/1d2d
      item_type = ' '
      if (major >= 2) then
         call prop_get(node_ptr, 'Lateral', 'locationType', item_type, is_read)
      else
         call prop_get(node_ptr, 'Lateral', 'type', item_type, is_read)
      end if
      select case (str_tolower(trim(item_type)))
      case ('1d')
         ilattype = ILATTP_1D
      case ('2d')
         ilattype = ILATTP_2D
      case ('1d2d', 'all')
         ilattype = ILATTP_ALL
      case default
         ilattype = ILATTP_ALL
      end select

      call reserve_sufficient_space(apply_transport, numlatsg + 1, 0)
      call prop_get(node_ptr, 'Lateral', 'applyTransport', apply_transport(numlatsg + 1), is_read)

      call read_lateral_discharge_definition(node_ptr, loc_id, base_dir, ilattype, loc_spec_type, node_id, branch_id, chainage, num_coordinates, x_coordinates, y_coordinates, location_file, is_successful)
      if (.not. is_successful) then
         return
      end if

      call ini_alloc_laterals()

      call prepare_lateral_mask(kclat, ilattype)

      numlatsg = numlatsg + 1
      call realloc(nnlat, max(2 * ndxi, nlatnd + ndxi), keepExisting=.true., fill=0)
      call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd + 1:), nlat, &
                                      loc_spec_type, location_file, num_coordinates, x_coordinates, y_coordinates, branch_id, chainage, node_id)

      n1latsg(numlatsg) = nlatnd + 1
      n2latsg(numlatsg) = nlatnd + nlat

      nlatnd = nlatnd + nlat

      if (allocated(x_coordinates)) deallocate (x_coordinates, stat=ierr)
      if (allocated(y_coordinates)) deallocate (y_coordinates, stat=ierr)

      ! [lateral]
      ! Flow = 1.23 | test.tim | REALTIME
      kx = 1
      rec = ' '
      call prop_get(node_ptr, 'Lateral', 'discharge', rec, is_read)
      if (.not. is_read .and. major <= 1) then ! Old pre-2.00 keyword 'flow'
         call prop_get(node_ptr, 'Lateral', 'flow', rec, is_read)
      end if
      if (len_trim(rec) > 0) then
         call resolvePath(rec, base_dir)
      else
         write (msgbuf, '(a,a,a)') 'Required field ''discharge'' missing in lateral ''', trim(loc_id), '''.'
         call err_flush()
         return
      end if

      qid = 'lateral_discharge' ! New quantity name in .bc files
      is_read = adduniformtimerelation_objects(qid, '', 'lateral', trim(loc_id), 'discharge', trim(rec), numlatsg, &
                                               kx, qplat(1, :))
      if (is_read) then
         jaqin = 1
         lat_ids(numlatsg) = loc_id
         call mess(LEVEL_INFO, 'Succesfully added lateral '//trim(loc_id)//'.')
      else
         is_successful = .false.
         call mess(LEVEL_ERROR, 'Lateral discharge information at '//trim(loc_id)//' could not be read from '//trim(rec)//'.')
         return
      end if

      is_successful = .true.

   end function init_lateral_forcings

   !> Read the current [Meteo] block from new external forcings file
      !! and do required initialisation for that quantity.
   function init_meteo_forcings(node_ptr, base_dir, file_name, group_name) result(res)
      use string_module, only: strcmpi, str_tolower
      use messageHandling, only: err_flush, msgbuf, LEVEL_INFO, mess
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL
      use m_missing, only: dmiss
      use tree_data_types, only: tree_data
      use timespace, only: convert_method_string_to_integer, get_default_method_for_file_type, &
                           update_method_with_weightfactor_fallback, update_method_in_case_extrapolation, &
                           convert_file_type_string_to_integer
      use fm_external_forcings_data, only: filetype, transformcoef, kx, tair_available, dewpoint_available
      use fm_external_forcings, only: allocatewindarrays
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U
      use m_wind, only: airdensity, jawindstressgiven, jaspacevarcharn, ja_airdensity, japatm, jawind, jarain, &
                        jaqin, jaqext, jatair, jaclou, jarhum, solrad_available, longwave_available, ec_pwxwy_x, ec_pwxwy_y, ec_pwxwy_c, &
                        ec_charnock, wcharnock, rain, qext
      use m_flowgeom, only: ndx, lnx, xz, yz
      use m_flowparameters, only: btempforcingtypA, btempforcingtypC, btempforcingtypH, btempforcingtypL, btempforcingtypS, &
                                  itempforcingtyp
      use timespace, only: timespaceinitialfield
      use m_meteo, only: ec_addtimespacerelation
      use dfm_error, only: DFM_NOERR
      use properties, only: prop_get
      use unstruc_files, only: resolvePath
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use m_alloc, only: aerr

      type(tree_data), pointer, intent(in) :: node_ptr !< Tree structure containing the meteo block.
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file.
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in warning messages, actual data is read from node_ptr.
      character(len=*), intent(in) :: group_name !< Name of the block, only used in warning messages.

      logical :: res

      integer, allocatable :: mask(:)
      logical :: invert_mask
      logical :: is_variable_name_available
      logical :: is_extrapolation_allowed
      character(len=INI_VALUE_LEN) :: variable_name
      character(len=INI_VALUE_LEN) :: interpolation_method, forcing_file, forcing_file_type, item_type, quantity, target_mask_file
      character(len=1) :: oper
      real(dp) :: max_search_radius
      ! generalized properties+pointers to target element grid:
      integer :: target_location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      integer :: target_num_points !< Number of points in target element set.
      real(dp), dimension(:), pointer :: target_x !< Pointer to x-coordinates array of target element set.
      real(dp), dimension(:), pointer :: target_y !< Pointer to y-coordinates array of target element set.
      integer :: ierr, method, ilattype
      logical :: is_successful

      res = .false.

      call prop_get(node_ptr, '', 'quantity ', quantity, is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
         call err_flush()
         return
      end if

      call prop_get(node_ptr, '', 'forcingFileType ', forcing_file_type, is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, &
            ']. Field ''forcingFileType'' is missing.'
         call err_flush()
         return
      end if

      call prop_get(node_ptr, '', 'forcingFile ', forcing_file, is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', forcing_file, ''': [', group_name, &
            ']. Field ''forcingFile'' is missing.'
         call err_flush()
         return
      else
         call resolvePath(forcing_file, base_dir)
      end if

      target_mask_file = ''
      call prop_get(node_ptr, '', 'targetMaskFile ', target_mask_file)

      invert_mask = .false.
      call prop_get(node_ptr, '', 'targetMaskInvert ', invert_mask, is_successful)

      is_variable_name_available = .false.
      variable_name = ' '
      call prop_get(node_ptr, '', 'forcingVariableName ', variable_name, is_variable_name_available)

      call prop_get(node_ptr, '', 'interpolationMethod ', interpolation_method, is_successful)
      if (is_successful) then
         method = convert_method_string_to_integer(interpolation_method)
         call update_method_with_weightfactor_fallback(forcing_file_type, method)
      else
         method = get_default_method_for_file_type(forcing_file_type)
      end if
      if (method == -1) then
         if (is_successful) then
            write (msgbuf, '(7a)') 'There is no method associated with ''interpolationMethod'' ', trim(interpolation_method), &
               ' in block in file ''', file_name, ''': [', group_name, '].'
         else
            write (msgbuf, '(7a)') 'Block contains no ''interpolationMethod'' in file ''', file_name, ''': [', group_name, &
               '] nor an internal value associated with given ''forcingFileType'':', trim(forcing_file_type), '.'
         end if
         call err_flush()
         return
      end if

      is_extrapolation_allowed = .false.
      call prop_get(node_ptr, '', 'extrapolationAllowed ', is_extrapolation_allowed, is_successful)
      call update_method_in_case_extrapolation(method, is_extrapolation_allowed)

      max_search_radius = -1
      call prop_get(node_ptr, '', 'extrapolationSearchRadius ', max_search_radius, is_successful)

      oper = 'O'
      call prop_get(node_ptr, '', 'operand ', oper, is_successful)

      transformcoef = DMISS
      call prop_get(node_ptr, '', 'averagingType ', transformcoef(4), is_successful)
      call prop_get(node_ptr, '', 'averagingRelSize ', transformcoef(5), is_successful)
      call prop_get(node_ptr, '', 'averagingNumMin ', transformcoef(8), is_successful)
      call prop_get(node_ptr, '', 'averagingPercentile ', transformcoef(7), is_successful)

      filetype = convert_file_type_string_to_integer(forcing_file_type)

      ! Default location type: s-points. Only cases below that need u-points or different, will override.
      target_location_type = UNC_LOC_S

      is_successful = scan_for_heat_quantities(quantity, kx)
      if (.not. is_successful) then
         select case (quantity)
         case ('airdensity')
            kx = 1
            if (.not. allocated(airdensity)) then
               allocate (airdensity(ndx), stat=ierr, source=0d0)
               call aerr('airdensity(ndx)', ierr, ndx)
            end if
         case ('airpressure', 'atmosphericpressure')
            kx = 1
            ierr = allocate_patm(0._dp)

         case ('airpressure_windx_windy', 'airpressure_stressx_stressy', 'airpressure_windx_windy_charnock')
            kx = 1
            call allocatewindarrays()

            jawindstressgiven = merge(1, 0, quantity == 'airpressure_stressx_stressy')
            jaspacevarcharn = merge(1, 0, quantity == 'airpressure_windx_windy_charnock')

            ierr = allocate_patm(100000._dp)

            if (.not. allocated(ec_pwxwy_x)) then
               allocate (ec_pwxwy_x(ndx), ec_pwxwy_y(ndx), stat=ierr, source=0d0)
               call aerr('ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)', ierr, 2 * ndx)
            end if

            if (jaspacevarcharn == 1) then
               if (.not. allocated(ec_pwxwy_c)) then
                  allocate (ec_pwxwy_c(ndx), wcharnock(lnx), stat=ierr, source=0d0)
                  call aerr('ec_pwxwy_c(ndx), wcharnock(lnx)', ierr, ndx + lnx)
               end if
            end if

         case ('charnock')
            kx = 1
            if (.not. allocated(ec_charnock)) then
               allocate (ec_charnock(ndx), stat=ierr, source=0d0)
               call aerr('ec_charnock(ndx)', ierr, ndx)
            end if
            if (.not. allocated(wcharnock)) then
               allocate (wcharnock(lnx), stat=ierr)
               call aerr('wcharnock(lnx)', ierr, lnx)
            end if

         case ('windx', 'windy', 'windxy', 'stressxy', 'stressx', 'stressy')
            kx = 1
            target_location_type = UNC_LOC_U
            call allocatewindarrays()

            jawindstressgiven = merge(1, 0, quantity(1:6) == 'stress')

         case ('rainfall', 'rainfall_rate') ! case is zeer waarschijnlijk overbodig
            kx = 1
            if (.not. allocated(rain)) then
               allocate (rain(ndx), stat=ierr, source=0d0)
               call aerr('rain(ndx)', ierr, ndx)
            end if

         case ('qext')
            ! Only time-independent sample file supported for now: sets Qext initially and this remains constant in time.
            if (jaQext == 0) then
               write (msgbuf, '(a)') 'quantity '''//trim(quantity)//' in file ''', file_name, ''': [', group_name, &
                  '] is missing QExt=1 in MDU.'
               call err_flush()
               return
            end if
            if (.not. strcmpi(forcing_file_type, 'sample')) then
               write (msgbuf, '(a)') 'Unknown forcingFileType '''//trim(forcing_file_type)//' in file ''', file_name, &
                  ''': [', group_name, '], quantity=', trim(quantity), '.'
               call err_flush()
               return
            end if
            method = get_default_method_for_file_type(forcing_file_type)
            call prop_get(node_ptr, '', 'locationType', item_type, is_successful)
            select case (str_tolower(trim(item_type)))
            case ('1d')
               ilattype = ILATTP_1D
            case ('2d')
               ilattype = ILATTP_2D
            case ('1d2d', 'all')
               ilattype = ILATTP_ALL
            case default
               ilattype = ILATTP_ALL
            end select

            mask(:) = 0
            call prepare_lateral_mask(mask, ilattype)

            res = timespaceinitialfield(xz, yz, qext, ndx, forcing_file, filetype, method, oper, transformcoef, UNC_LOC_S, mask)
            return ! This was a special case, don't continue with timespace processing below.
         case default
            write (msgbuf, '(a)') 'Unknown quantity '''//trim(quantity)//' in file ''', file_name, ''': [', group_name, &
               '].'
            call err_flush()
            return
         end select
      end if

      ! Derive target element set properties from the quantity's topological location type
      call get_location_target_properties(target_location_type, target_num_points, target_x, target_y, ierr)
      if (ierr /= DFM_NOERR) then
         write (msgbuf, '(7a)') 'Invalid data in file ''', file_name, ''': [', group_name, &
            ']. Line ''quantity = ', trim(quantity), ''' has no known target grid properties.'
         call err_flush()
         return
      end if

      !> Prepare target mask for the quantity's target element set.
      call construct_target_mask(mask, target_num_points, target_mask_file, target_location_type, invert_mask, ierr)
      if (ierr /= DFM_NOERR) then
         write (msgbuf, '(7a)') 'Unsupported data in file ''', file_name, ''': [', group_name, &
            ']. Line ''quantity = ', trim(quantity), ''' cannot be combined with targetMaskFile.'
         call err_flush()
         return
      end if

      select case (trim(str_tolower(forcing_file_type)))
      case ('bcascii')
         ! NOTE: Currently, we only support name=global meteo in.bc files, later maybe station time series as well.
         is_successful = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, 'global', filetype, &
                                                 method, oper, forcingfile=forcing_file)
      case default
         if (is_variable_name_available) then
            is_successful = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, forcing_file, filetype, &
                                                    method, oper, varname=variable_name)
         else
            is_successful = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, forcing_file, filetype, &
                                                    method, oper)
         end if
      end select

      if (is_successful) then
         select case (quantity)
         case ('airdensity')
            call mess(LEVEL_INFO, 'Enabled variable airdensity for windstress while reading external forcings.')
            ja_airdensity = 1

         case ('airpressure', 'atmosphericpressure')
            japatm = 1

         case ('airpressure_windx_windy', 'airpressure_stressx_stressy', 'airpressure_windx_windy_charnock')
            jawind = 1
            japatm = 1

         case ('charnock')
            jaspacevarcharn = 1

         case ('rainfall', 'rainfall_rate')
            jarain = 1
            jaqin = 1

         case ('windx', 'windy', 'windxy', 'stressxy', 'stressx', 'stressy')
            jawind = 1
         case ('airtemperature')
            jatair = 1
            btempforcingtypA = .true.
            tair_available = .true.
         case ('cloudiness')
            jaclou = 1
            btempforcingtypC = .true.
         case ('humidity')
            jarhum = 1
            btempforcingtypH = .true.
         case ('dewpoint') ! Relative humidity array used to store dewpoints
            itempforcingtyp = 5
            dewpoint_available = .true.
         case ('solarradiation')
            btempforcingtypS = .true.
            solrad_available = .true.
         case ('longwaveradiation')
            btempforcingtypL = .true.
            longwave_available = .true.
         case ('humidity_airtemperature_cloudiness')
            itempforcingtyp = 1
         case ('dewpoint_airtemperature_cloudiness')
            itempforcingtyp = 3
            dewpoint_available = .true.
            tair_available = .true.
         case ('humidity_airtemperature_cloudiness_solarradiation')
            itempforcingtyp = 2
            tair_available = .true.
            solrad_available = .true.
         case ('dewpoint_airtemperature_cloudiness_solarradiation')
            itempforcingtyp = 4
            dewpoint_available = .true.
            tair_available = .true.
            solrad_available = .true.
         end select

         res = .true.

      end if

   end function init_meteo_forcings

   !> Read sourcesink blocks from new external forcings file.
   function init_sourcesink_forcings(node_ptr, base_dir, file_name, group_name) result(is_successful)
      use messageHandling, only: err_flush, msgbuf
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use unstruc_files, only: resolvePath
      use m_transport, only: NAMLEN, NUMCONST, const_names, ISALT, ITEMP, ISED1, ISEDN, ISPIR, ITRA1, ITRAN
      use netcdf_utils, only: ncu_sanitize_name
      use m_missing, only: dmiss
      use m_addsorsin, only: addsorsin, addsorsin_from_polyline_file
      use fm_external_forcings_data, only: numsrc, qstss
      use dfm_error, only: DFM_NOERR

      type(tree_data), pointer, intent(in) :: node_ptr !< Tree structure containing the sourcesink block.
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file.
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in error messages, actual data is read from node_ptr.
      character(len=*), intent(in) :: group_name !< Name of the block, only used in error messages.

      character(len=INI_VALUE_LEN) :: sourcesink_id
      character(len=INI_VALUE_LEN) :: sourcesink_name
      character(len=INI_VALUE_LEN) :: location_file
      character(len=INI_VALUE_LEN) :: discharge_input
      character(len=INI_VALUE_LEN), dimension(:), allocatable :: constituent_delta_file
      character(len=NAMLEN) :: const_name
      character(len=INI_VALUE_LEN) :: quantity_id

      integer :: num_coordinates
      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      ! only constant profiles (1 value) or linear profiles (2 values) are allowed
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink
      real(kind=dp) :: area
      integer :: i_const
      integer :: ierr
      logical :: is_successful
      logical :: is_read
      logical :: have_location_file, have_location_coordinates

      is_successful = .false.

      sourcesink_id = ' '
      call prop_get(node_ptr, '', 'id', sourcesink_id, is_read)
      if (.not. is_read .or. len_trim(sourcesink_id) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''id'' is missing.'
         call err_flush()
         return
      end if
      call prop_get(node_ptr, '', 'name', sourcesink_name, is_read)

      call prop_get(node_ptr, '', 'locationFile', location_file, have_location_file)
      if (have_location_file) then
         call resolvePath(location_file, base_dir)
      else
         call prop_get(node_ptr, '', 'numCoordinates', num_coordinates, is_read)
         if (is_read) then
            if (num_coordinates <= 0) then
               write (msgbuf, '(3a)') 'SourceSink '''//trim(sourcesink_id)//''': numCoordinates must be greater than 0.'
               call err_flush()
               return
            end if
            allocate (x_coordinates(num_coordinates), stat=ierr)
            call prop_get(node_ptr, '', 'xCoordinates', x_coordinates, num_coordinates, is_read)
            if (is_read) then
               allocate (y_coordinates(num_coordinates), stat=ierr)
               call prop_get(node_ptr, '', 'yCoordinates', y_coordinates, num_coordinates, is_read)
            end if
         end if
         have_location_coordinates = is_read
      end if
      if (.not. have_location_file .and. .not. have_location_coordinates) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(file_name), ''': [', trim(group_name), ']. Location information is incomplete or missing.'
         call err_flush()
         return
      end if

      ! read optional vertical profiles.
      z_range_source(:) = dmiss
      z_range_sink(:) = dmiss
      call prop_get(node_ptr, '', 'zSource', z_range_source, num_range_points, is_read)
      call prop_get(node_ptr, '', 'zSink', z_range_sink, num_range_points, is_read)

      call prop_get(node_ptr, '', 'discharge', discharge_input, is_read)
      if (.not. is_read) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(file_name), ''': [', trim(group_name), ']. Key "discharge" is missing.'
         call err_flush()
         return
      end if

      ! read optional value 'area' to compute the momentum released
      area = 0.0_dp
      call prop_get(node_ptr, '', 'area', area, is_read)

      if (have_location_file) then
         call addsorsin_from_polyline_file(location_file, sourcesink_id, z_range_source, z_range_sink, area, ierr)
      else
         call addsorsin(sourcesink_id, x_coordinates, y_coordinates, &
                     z_range_source, z_range_sink, area, ierr)
      end if
      
      if (ierr /= DFM_NOERR) then
         write (msgbuf, '(5a)') 'Error while processing ''', trim(file_name), ''': [', trim(group_name), ']. ' &
            // 'Source sink with id='//trim(sourcesink_id)//'. could not be added.'
         call err_flush()
         return
      end if

      quantity_id = 'sourcesink_discharge' ! New quantity name in .bc files
      !call resolvePath(filename, basedir) ! TODO!
      is_successful = adduniformtimerelation_objects(quantity_id, '', 'source sink', trim(sourcesink_id), 'discharge', trim(discharge_input), (numconst + 1)*(numsrc-1) + 1, &
                                               1, qstss)

      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Error while processing ''', trim(file_name), ''': [', trim(group_name), ']. ' &
            // 'Could not initialize discharge data in ''', trim(discharge_input), ''' for source sink with id='//trim(sourcesink_id)//'.'
         call err_flush()
         return
      end if

      ! Constituents (salinity, temperature, sediments, tracers) may have a timeseries file
      ! specifying the difference in concentration added by the source/sink.
      ! All these files are optional, so no check on 'is_read' can be present below.
      if (NUMCONST > 0) then
         allocate (constituent_delta_file(NUMCONST), stat=ierr)
         do i_const = 1, NUMCONST
            is_read = .false.
            const_name = const_names(i_const)
            if (i_const == ISALT) then
               const_name = 'salinity'
               call prop_get(node_ptr, '', 'salinityDelta', constituent_delta_file(i_const), is_read)
            else if (i_const == ITEMP) then
               call prop_get(node_ptr, '', 'temperatureDelta', constituent_delta_file(i_const), is_read)
            else if (i_const == ISPIR) then
               cycle
            else
               ! tracers and sediments: remove special characters from const_name before constructing the property to read.
               call ncu_sanitize_name(const_name)
               if (i_const >= ISED1 .and. i_const <= ISEDN) then
                  call prop_get(node_ptr, '', 'sedFrac'//trim(const_name)//'Delta', constituent_delta_file(i_const), is_read)
               else if (i_const >= ITRA1 .and. i_const <= ITRAN) then
                  call prop_get(node_ptr, '', 'tracer'//trim(const_name)//'Delta', constituent_delta_file(i_const), is_read)
               end if
            end if
            
            if (is_read) then
               quantity_id = 'sourcesink_' // trim(const_name) // 'Delta'  ! New quantity name in .bc files
               !call resolvePath(filename, basedir) ! TODO!
               is_successful = adduniformtimerelation_objects(quantity_id, '', 'source sink', trim(sourcesink_id), TRIM(const_name)//'Delta', trim(constituent_delta_file(i_const)), (numconst + 1)*(numsrc-1) + 1 + i_const, &
                                                        1, qstss)
               continue
            end if
         end do
      end if

      is_successful = .true.

   end function init_sourcesink_forcings

   !> Get several target grid properties for a given location type.
   !!
   !! Properties include: coordinates and location count,
   !! typically used in setting up the time-space relations for
   !! external forcings quantities.
   subroutine get_location_target_properties(target_location_type, target_num_points, target_x, target_y, ierr)
      use fm_location_types
      use m_flowgeom, only: ndx, lnx, xz, yz, xu, yu
      use precision_basics, only: dp
      use dfm_error, only: DFM_NOERR, DFM_NOTIMPLEMENTED
      integer, intent(in) :: target_location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      integer, intent(out) :: target_num_points !< Number of points in target element set.
      real(dp), dimension(:), pointer, intent(out) :: target_x !< Pointer to x-coordinates array of target element set.
      real(dp), dimension(:), pointer, intent(out) :: target_y !< Pointer to y-coordinates array of target element set.
      integer, intent(out) :: ierr !< Result status (DFM_NOERR if succesful, or different if unknown quantity location was given).

      ierr = DFM_NOERR

      select case (target_location_type)
      case (UNC_LOC_S)
         target_num_points = ndx
         target_x => xz(1:target_num_points)
         target_y => yz(1:target_num_points)
      case (UNC_LOC_U)
         target_num_points = lnx
         target_x => xu(1:target_num_points)
         target_y => yu(1:target_num_points)
      case default
         ierr = DFM_NOTIMPLEMENTED
      end select
   end subroutine get_location_target_properties

   !> Construct target mask array for later ec_addtimespacerelation/timespaceinitialfield calls.
   subroutine construct_target_mask(mask, target_num_points, target_mask_file, target_location_type, invert_mask, ierr)
      use fm_location_types
      use m_flowgeom, only: ndx, lnx, xz, yz, kcs
      use timespace_parameters, only: LOCTP_POLYGON_FILE
      use timespace, only: selectelset_internal_nodes, selectelset_internal_links
      use dfm_error, only: DFM_NOTIMPLEMENTED, DFM_NOERR

      integer, dimension(:), allocatable, intent(out) :: mask !< Mask array for the target element set.
      integer, intent(in) :: target_num_points !< Number of points in target element set. Will be used to allocate the mask array.
      character(len=*), intent(in) :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      integer, intent(in) :: target_location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      logical, intent(in) :: invert_mask !< Flag to invert the mask (1s to 0s and vice versa).
      integer, intent(out) :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      integer, dimension(:), allocatable :: selected_points !< Array of selected points based on the target mask file.
      integer :: number_of_selected_points, point

      ierr = DFM_NOERR

      allocate (mask(target_num_points), source=0)

      if (len_trim(target_mask_file) > 0) then
         ! Mask flow nodes/links/etc. based on inside polygon(s), or outside.
         allocate (selected_points(target_num_points), source=0)
         select case (target_location_type)
         case (UNC_LOC_S)
            ! in: kcs, all allowed flow nodes, out: mask: all masked flow nodes.
            call selectelset_internal_nodes(xz, yz, kcs, ndx, selected_points, number_of_selected_points, LOCTP_POLYGON_FILE, &
                                            target_mask_file)
         case (UNC_LOC_U)
            ! in: no link pre-mask, all flow links, out: mask: all masked flow links.
            call selectelset_internal_links(lnx, selected_points, number_of_selected_points, LOCTP_POLYGON_FILE, &
                                            target_mask_file)
         case default
            ierr = DFM_NOTIMPLEMENTED
            return
         end select

         do point = 1, number_of_selected_points
            mask(selected_points(point)) = 1
         end do
         if (invert_mask) then
            mask = ieor(mask, 1)
         end if
      else
         if (target_location_type == UNC_LOC_S) then
            ! 100% masking: accept all flow locations that were already active in their own mask array.
            where (kcs /= 0) mask = 1
         else
            mask = 1
         end if
      end if
   end subroutine construct_target_mask

   !> Scan the quantity name for heat relatede quantities.
   function scan_for_heat_quantities(quantity, kx) result(success)
      use m_wind, only: tair, clou, rhum, qrad, longwave
      use m_flowgeom, only: ndx
      use m_alloc, only: aerr, realloc

      character(len=*), intent(in) :: quantity !< Name of the data set.
      integer, intent(out) :: kx !< Number of individual quantities in the data set
      logical :: success !< Return value, indicates whether the quantity is supported in this subroutine.

      integer :: ierr

      kx = 1
      success = .true.

      select case (quantity)

      case ('airtemperature')
         call realloc(tair, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('tair(ndx)', ierr, ndx)
      case ('cloudiness')
         call realloc(clou, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('clou(ndx)', ierr, ndx)
      case ('humidity')
         call realloc(rhum, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('rhum(ndx)', ierr, ndx)
      case ('dewpoint') ! Relative humidity array used to store dewpoints
         call realloc(rhum, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('rhum(ndx)', ierr, ndx)
      case ('solarradiation')
         call realloc(qrad, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('qrad(ndx)', ierr, ndx)
      case ('longwaveradiation')
         call realloc(longwave, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('longwave(ndx)', ierr, ndx)
      case ('humidity_airtemperature_cloudiness')
         kx = 3
      case ('dewpoint_airtemperature_cloudiness')
         kx = 3
      case ('humidity_airtemperature_cloudiness_solarradiation')
         kx = 4
      case ('dewpoint_airtemperature_cloudiness_solarradiation')
         kx = 4
      case default
         success = .false.
      end select
   end function scan_for_heat_quantities

end submodule fm_external_forcings_init
