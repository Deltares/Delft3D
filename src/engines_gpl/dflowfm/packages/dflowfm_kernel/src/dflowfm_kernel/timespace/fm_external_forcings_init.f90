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
submodule(fm_external_forcings) fm_external_forcings_init
   use precision_basics, only: dp
   use m_missing, only: dmiss => dmiss_neg

   implicit none(type, external)

   integer, parameter :: INI_VALUE_LEN = 256

contains

   !> reads new external forcings file and makes required initialisations. Only to be called once as part of fm_initexternalforcings.
   module subroutine init_new(external_force_file_name, iresult)
      use properties, only: get_version_number, prop_file
      use tree_structures, only: tree_data, tree_create, tree_destroy, tree_num_nodes, tree_count_nodes_byname, tree_get_name
      use messageHandling, only: warn_flush, err_flush, msgbuf, LEVEL_FATAL
      use fm_external_forcings_data, only: nbndz, itpenz, nbndu, itpenu, set_lateral_count_in_external_forcings_file
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
      use m_source_sink, only: source_sinks

      character(len=*), intent(in) :: external_force_file_name !< file name for new external forcing boundary blocks
      integer, intent(inout) :: iresult !< integer error code. Intent(inout) to preserve earlier errors.

      integer :: initial_threshold_abort
      logical :: res
      logical :: is_successful
      type(tree_data), pointer :: bnd_ptr !< tree of extForceBnd-file's [boundary] blocks
      type(tree_data), pointer :: block_ptr
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

      if (file_name(len_trim(file_name) - 3:) == '.ini') then
         write (msgbuf, '(a)') 'The inifieldfile is deprecated. Consider moving the content of '//trim(file_name)//' to the external forcings file.'
         call warn_flush()
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
      major = 0
      minor = 0
      call get_version_number(bnd_ptr, major=major, minor=minor, success=is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(a,a,a)') 'File version number not found in external forcing file ''', trim(file_name), '''.'
         call warn_flush()
      else if (major > ExtfileNewMajorVersion .or. (major == ExtfileNewMajorVersion .and. minor > ExtfileNewMinorVersion)) then
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
         call realloc(balat, maxlatsg, keepExisting=.false., fill=0.0_dp)
         call realloc(qplat, [max(1, kmx), maxlatsg], keepExisting=.false., fill=0.0_dp)
         call realloc(lat_ids, maxlatsg, keepExisting=.false.)
         call realloc(n1latsg, maxlatsg, keepExisting=.false., fill=0)
         call realloc(n2latsg, maxlatsg, keepExisting=.false., fill=0)
      end if

      ! Allocate source-sink related arrays now, just once, because otherwise realloc's in the loop would destroy target arrays in ecInstance.
      call initialize_bubblescreens(bnd_ptr, base_dir, file_name, max_num_src)

      max_num_src = max_num_src + tree_count_nodes_byname(bnd_ptr, 'sourcesink')

      if (max_num_src > 0) then
         call source_sinks%initialize(max_num_src)
      end if

      res = res .and. add_bubblescreen_source_sinks()

      ib = 0
      ibqh = 0
      initial_threshold_abort = threshold_abort
      threshold_abort = LEVEL_FATAL
      do i = 1, num_items_in_file
         block_ptr => bnd_ptr%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(block_ptr))

         select case (str_tolower(group_name))
         case ('general')
            ! General block, was already read.

         case ('boundary')
            res = res .and. init_boundary_forcings(block_ptr, base_dir, file_name, group_name, itpenzr, itpenur, ib, ibqh)

         case ('lateral')
            res = res .and. init_lateral_forcings(block_ptr, base_dir, i, major)

         case ('spatial', 'meteo', 'parameter', 'initial')
            res = res .and. init_spatial_fields(block_ptr, base_dir, file_name, group_name)

         case ('sourcesink')
            res = res .and. init_sourcesink_forcings(block_ptr, base_dir, file_name, group_name)

         case default ! Unrecognized item in an ext block
            ! res remains unchanged: Not an error (support commented/disabled blocks in ext file)
            write (msgbuf, '(5a)') 'Unrecognized block in file ''', file_name, ''': [', group_name, ']. Ignoring this block.'
            call warn_flush()
         end select
      end do

      threshold_abort = initial_threshold_abort

      if (allocated(itpenzr)) then
         deallocate (itpenzr)
      end if
      if (allocated(itpenur)) then
         deallocate (itpenur)
      end if
      if (numlatsg > 0) then
         do n = 1, numlatsg
            balat(n) = 0.0_dp
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

      if (res) then
         iresult = DFM_NOERR
      else
         iresult = DFM_WRONGINPUT
      end if
   end subroutine init_new

   !> reads boundary blocks from new external forcings file and makes required initialisations
   function init_boundary_forcings(block_ptr, base_dir, file_name, group_name, itpenzr, itpenur, ib, ibqh) result(res)
      use tree_data_types, only: tree_data
      use fm_external_forcings_data, only: filetype, qhpliname,kbndu
      use timespace_parameters, only: NODE_ID, OPERAND_OVERRIDE, OPERAND_ADD, OPERAND_UNKNOWN, convert_legacy_operand_string_to_integer
      use timespace_data, only: WEIGHTFACTORS, POLY_TIM, SPACEANDTIME, getmeteoerror
      use tree_structures, only: tree_get_name, tree_get_data_string
      use messageHandling, only: mess, LEVEL_ERROR, err_flush, warn_flush, msgbuf
      use string_module, only: strcmpi
      use properties, only: prop_get
      use unstruc_files, only: resolvePath

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to boundary block in extforce file; child node of the extforce file tree
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in warning messages, actual data is read from block_ptr
      character(len=*), intent(in) :: group_name !< Name of the block, only used in warning messages
      integer, dimension(:), intent(in) :: itpenzr !< boundary condition nr in openbndsect for z
      integer, dimension(:), intent(in) :: itpenur !< boundary condition nr in openbndsect for u
      integer, intent(inout) :: ib !< block counter for boundaries
      integer, intent(inout) :: ibqh !< block counter for qh boundaries
      logical :: res

      integer, dimension(1) :: target_index
      character(len=INI_VALUE_LEN) :: location_file, quantity, forcing_file, property_name, property_value
      type(tree_data), pointer :: key_value_ptr
      character(len=300) :: error_message
      integer       :: operand, ibndu,facdis
      

      logical       :: is_successful
      integer       :: method, num_items_in_block, j

      res = .true.

      ! First check for required input:
      call prop_get(block_ptr, '', 'quantity', quantity, is_successful)
      if (.not. is_successful) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
         call err_flush()
         return
      end if
      ib = ib + 1

      call prop_get(block_ptr, '', 'nodeId', location_file, is_successful)
      if (is_successful) then
         filetype = NODE_ID
         method = SPACEANDTIME
      else
         filetype = POLY_TIM
         method = WEIGHTFACTORS
         call prop_get(block_ptr, '', 'locationFile', location_file, is_successful)
      end if

      if (is_successful) then
         call resolvePath(location_file, base_dir)
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''locationFile'' is missing.'
         call err_flush()
         return
      end if

      call prop_get(block_ptr, '', 'forcingFile ', forcing_file, is_successful)
      if (is_successful) then
         call resolvePath(forcing_file, base_dir)
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
         call err_flush()
         return
      end if

      operand = OPERAND_UNKNOWN
      call prop_get(block_ptr, '', 'operand ', property_value, is_successful)
      if (is_successful) then
         operand = convert_legacy_operand_string_to_integer(property_value)
      end if

      num_items_in_block = 0
      if (associated(block_ptr%child_nodes)) then
         num_items_in_block = size(block_ptr%child_nodes)
      end if

      ! Perform dummy-reads of supported keywords to prevent them from being reported as unused input.
      ! The keywords below were already read in read_location_files_from_boundary_blocks().
      call prop_get(block_ptr, '', 'returnTime', property_value)
      call prop_get(block_ptr, '', 'return_time', property_value)
      call prop_get(block_ptr, '', 'openBoundaryTolerance', property_value)
      call prop_get(block_ptr, '', 'nodeId', property_value)
      call prop_get(block_ptr, '', 'bndWidth1D', property_value)
      call prop_get(block_ptr, '', 'bndBlDepth', property_value)

      ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingFile=...
      do j = 1, num_items_in_block
         key_value_ptr => block_ptr%child_nodes(j)%node_ptr
         ! todo: read multiple quantities
         property_name = trim(tree_get_name(key_value_ptr))
         call tree_get_data_string(key_value_ptr, property_value, is_successful)
         if (is_successful) then
            if (strcmpi(property_name, 'forcingFile')) then
               forcing_file = property_value
               call resolvePath(forcing_file, base_dir)
               if (operand /= OPERAND_OVERRIDE .and. operand /= OPERAND_ADD) then
                  operand = OPERAND_OVERRIDE
                  if (quantity_pli_combination_is_registered(quantity, location_file)) then
                     operand = OPERAND_ADD
                  end if
               end if
               call register_quantity_pli_combination(quantity, location_file)
               if (filetype == NODE_ID .or. quantity == 'qhbnd') then
                  select case (quantity)
                  case ('waterlevelbnd')
                     target_index = itpenzr(ib)

                  case ('qhbnd')
                     ibqh = ibqh + 1
                     target_index = [ibqh]
                     if (filetype /= NODE_ID) then
                        location_file = qhpliname(ibqh)
                     end if

                  case ('dischargebnd')
                     target_index = itpenur(ib)

                  case default
                     target_index = [-1]
                  end select

                  if (target_index(1) <= 0) then
                     ! This boundary has been skipped in an earlier phase (findexternalboundarypoints),
                     ! so, also do *not* connect it as a spacetimerelation here.
                     is_successful = .true. ! No failure: boundaries are allowed to remain disconnected.
                  else
                     is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=NODE_ID, method=method, &
                                                                     operand=operand, forcing_file=forcing_file, targetindex=target_index(1))
                  end if
               else
                  is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=method, &
                                                                  operand=operand, forcing_file=forcing_file)
                  ! TK_temp: Determine sign needed for discharge boundaries from history file
                  if (quantity == 'dischargebnd') then
                     facdis = 1
                     if (index(trim(forcing_file)//'|', '_his.nc|') > 0 ) then
                        call det_sign_discharge(location_file,facdis)
                     end if
                     ! fill kbndu(7,:) with multiplification discharge boundaries
                     do ibndu = 1, size(kbndu,2)
                        if (kbndu(5,ibndu) == ib) then
                            kbndu(7,ibndu) = facdis
                        end if
                     end do
                 end if
               end if
               res = res .and. is_successful ! Remember any previous errors.
               operand = OPERAND_UNKNOWN
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
   subroutine read_lateral_discharge_definition(block_ptr, loc_id, base_dir, ilattype, loc_spec_type, node_id, branch_id, chainage, num_coordinates, x_coordinates, y_coordinates, location_file, is_success)
      use messageHandling, only: mess, err, LEVEL_ERROR
      use precision, only: dp
      use m_missing, only: imiss, dmiss
      use properties, only: has_key, prop_get
      use tree_data_types, only: tree_data
      use timespace_parameters, only: LOCTP_NODEID, LOCTP_BRANCHID_CHAINAGE, LOCTP_POLYGON_XY, LOCTP_POLYGON_FILE
      use fm_location_types, only: SPATIAL_LOCATION_1D
      use unstruc_files, only: resolvePath

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to lateral block in extforce file; child node of the extforce file tree
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

      has_node_id = has_key(block_ptr, 'Lateral', 'nodeId')
      has_branch_id = has_key(block_ptr, 'Lateral', 'branchId')
      has_chainage = has_key(block_ptr, 'Lateral', 'chainage')
      has_num_coordinates = has_key(block_ptr, 'Lateral', 'numCoordinates')
      has_x_coordinates = has_key(block_ptr, 'Lateral', 'xCoordinates')
      has_y_coordinates = has_key(block_ptr, 'Lateral', 'yCoordinates')
      has_location_file = has_key(block_ptr, 'Lateral', 'locationFile')

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
         call prop_get(block_ptr, 'Lateral', 'nodeId', node_id)
         loc_spec_type = LOCTP_NODEID
         ilattype = SPATIAL_LOCATION_1D
         is_success = .true.
         return
      end if

      if (has_branch_id .or. has_chainage) then
         if (.not. (has_branch_id .and. has_chainage)) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': branchId and chainage must be set together.')
            return
         end if

         call prop_get(block_ptr, 'Lateral', 'branchId', branch_id)
         call prop_get(block_ptr, 'Lateral', 'chainage', chainage)
         if (len_trim(branch_id) > 0 .and. chainage /= dmiss .and. chainage >= 0.0_dp) then
            loc_spec_type = LOCTP_BRANCHID_CHAINAGE
            ilattype = SPATIAL_LOCATION_1D
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
         call prop_get(block_ptr, 'Lateral', 'numCoordinates', num_coordinates)
         if (num_coordinates <= 0) then
            call mess(LEVEL_ERROR, 'Lateral '''//trim(loc_id)//''': numCoordinates must be greater than 0.')
            return
         end if
         allocate (x_coordinates(num_coordinates), stat=ierr)
         allocate (y_coordinates(num_coordinates), stat=ierr)
         call prop_get(block_ptr, 'Lateral', 'xCoordinates', x_coordinates, num_coordinates)
         call prop_get(block_ptr, 'Lateral', 'yCoordinates', y_coordinates, num_coordinates)
         loc_spec_type = LOCTP_POLYGON_XY
         is_success = .true.
         return
      end if

      if (has_location_file) then
         location_file = ''
         call prop_get(block_ptr, 'Lateral', 'locationFile', location_file)
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
   function init_lateral_forcings(block_ptr, base_dir, block_number, major) result(is_successful)
      use messageHandling, only: err_flush, msgbuf, mess, LEVEL_ERROR, LEVEL_INFO
      use string_module, only: str_tolower
      use tree_data_types, only: tree_data
      use m_laterals, only: qplat, lat_ids, n1latsg, n2latsg, kclat, numlatsg, nnlat, nlatnd, apply_transport
      use m_flowgeom, only: ndxi, xz, yz
      use m_alloc, only: realloc, reserve_sufficient_space
      use fm_external_forcings_data, only: kx, qid
      use fm_location_types, only: parse_spatial_location_type, SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL
      use m_wind, only: jaqin
      use properties, only: prop_get
      use unstruc_files, only: resolvePath
      use m_flowgeom_mask, only: construct_mask
      use fm_location_types, only: UNC_LOC_S
      use timespace, only: selectelset_internal_nodes

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to lateral block in extforce file; child node of the extforce file tree
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file
      integer, intent(in) :: block_number !< Number of the block, only used in error message
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
      call prop_get(block_ptr, 'Lateral', 'id', loc_id, is_read)
      if (.not. is_read .or. len_trim(loc_id) == 0) then
         write (msgbuf, '(a,i0,a)') 'Required field ''id'' missing in lateral (block #', block_number, ').'
         call err_flush()
         return
      end if

      ! locationType = optional for lateral
      ! locationType = 1d | 2d | all/1d2d
      item_type = 'all'
      if (major >= 2) then
         call prop_get(block_ptr, 'Lateral', 'locationType', item_type, is_read)
      else
         call prop_get(block_ptr, 'Lateral', 'type', item_type, is_read)
      end if
      ilattype = parse_spatial_location_type(trim(item_type))

      call reserve_sufficient_space(apply_transport, numlatsg + 1, 0)
      call prop_get(block_ptr, 'Lateral', 'applyTransport', apply_transport(numlatsg + 1), is_read)

      call read_lateral_discharge_definition(block_ptr, loc_id, base_dir, ilattype, loc_spec_type, node_id, branch_id, chainage, num_coordinates, x_coordinates, y_coordinates, location_file, is_successful)
      if (.not. is_successful) then
         return
      end if

      call ini_alloc_laterals()

      call construct_mask(kclat, UNC_LOC_S, ilattype)

      numlatsg = numlatsg + 1
      call realloc(nnlat, max(2 * ndxi, nlatnd + ndxi), keepExisting=.true., fill=0)
      call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd + 1:), nlat, &
                                      loc_spec_type, location_file, num_coordinates, x_coordinates, y_coordinates, branch_id, chainage, node_id)

      n1latsg(numlatsg) = nlatnd + 1
      n2latsg(numlatsg) = nlatnd + nlat

      nlatnd = nlatnd + nlat

      if (allocated(x_coordinates)) then
         deallocate (x_coordinates, stat=ierr)
      end if
      if (allocated(y_coordinates)) then
         deallocate (y_coordinates, stat=ierr)
      end if

      ! [lateral]
      ! Flow = 1.23 | test.tim | REALTIME
      kx = 1
      rec = ' '
      call prop_get(block_ptr, 'Lateral', 'discharge', rec, is_read)
      if (.not. is_read .and. major <= 1) then ! Old pre-2.00 keyword 'flow'
         call prop_get(block_ptr, 'Lateral', 'flow', rec, is_read)
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

   !> Resolve the target array and location type for a meteo/spatial quantity.
   !! Handles allocation and flag setup for all meteo quantities.
   !! target_array remains null for most quantities (EC module writes directly
   !! into named arrays); only set for quantities that need an explicit target pointer (e.g. qext).
   function resolve_meteo_target(quantity, file_name, target_location_type, target_array) result(success)
      use messageHandling
      use m_alloc, only: realloc
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U
      use m_wind, only: air_density, jawindstressgiven, jaspacevarcharn, &
                        ec_pwxwy_x, ec_pwxwy_y, ec_pwxwy_c, ec_charnock, wcharnock, &
                        rain, air_pressure, pseudo_air_pressure, water_level_correction, &
                        qext, jaqin
      use m_flowgeom, only: ndx, lnx
      use string_module, only: str_tolower

      implicit none

      character(len=*), intent(in) :: quantity !< Name of the quantity.
      character(len=*), intent(in) :: file_name !< Name of the file, used for warning messages.
      integer, intent(out) :: target_location_type !< Location type (UNC_LOC_S or UNC_LOC_U).
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< Pointer to model array. Null for most meteo quantities.
      logical :: success

      real(dp), parameter :: DEFAULT_AIR_PRESSURE = 100000.0_dp
      associate (dummy => file_name)
      end associate
      target_array => null()
      target_location_type = UNC_LOC_S ! default for all meteo quantities except wind
      success = .true.
      select case (str_tolower(quantity))
      case ('airdensity')
         call realloc(air_density, ndx, fill=0.0_dp, keepexisting=.true.)

      case ('airpressure', 'atmosphericpressure')
         call realloc(air_pressure, ndx, keepExisting=.true., fill=0.0_dp)

      case ('pseudoairpressure')
         call realloc(pseudo_air_pressure, ndx, keepExisting=.true., fill=0.0_dp)

      case ('waterlevelcorrection')
         call realloc(water_level_correction, ndx, keepExisting=.true., fill=0.0_dp)

      case ('airpressure_windx_windy', 'airpressure_stressx_stressy', 'airpressure_windx_windy_charnock')
         call allocatewindarrays()
         call realloc(air_pressure, ndx, keepexisting=.true., fill=DEFAULT_AIR_PRESSURE)
         call realloc(ec_pwxwy_x, ndx, keepexisting=.true., fill=0.0_dp)
         call realloc(ec_pwxwy_y, ndx, keepexisting=.true., fill=0.0_dp)
         jawindstressgiven = merge(1, 0, str_tolower(quantity) == 'airpressure_stressx_stressy')
         jaspacevarcharn = merge(1, 0, str_tolower(quantity) == 'airpressure_windx_windy_charnock')
         if (jaspacevarcharn == 1) then
            call realloc(ec_pwxwy_c, ndx, keepexisting=.true., fill=0.0_dp)
            call realloc(wcharnock, lnx, keepexisting=.true., fill=0.0_dp)
         end if

      case ('charnock')
         call realloc(ec_charnock, ndx, keepexisting=.true., fill=0.0_dp)
         call realloc(wcharnock, lnx, keepexisting=.true., fill=0.0_dp)

      case ('windx', 'windy', 'windxy', 'stressxy', 'stressx', 'stressy')
         target_location_type = UNC_LOC_U
         jawindstressgiven = merge(1, 0, str_tolower(quantity(1:6)) == 'stress')
         call allocatewindarrays()

      case ('rainfall', 'rainfall_rate')
         call realloc(rain, ndx, keepexisting=.true., fill=0.0_dp)

      case ('qext')
         call realloc(qext, ndx, keepExisting=.true., fill=0.0_dp)
         target_array => qext
         jaqin = 1
      case default
         success = .false.
      end select

   end function resolve_meteo_target

!> Read a 3D initial field using EC with sigma coordinates (WEIGHTFACTORS method).
!! Encapsulates all sigma-coordinate globals (zcs, kbot, ktop) and time reference globals.
   function read_3d_sigma_field(quantity, target_x, target_y, mask, kx, forcing_file, &
                                filetype, method, oper, variable_name, ec_item, target_data) result(res)
      use m_setzcs, only: setzcs
      use m_flow, only: zcs, kbot, ktop, ndkx
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user
      use m_meteo, only: ec_addtimespacerelation, ec_gettimespacevalue_by_itemID, ecInstancePtr
      use m_alloc, only: reallocP
      use m_missing, only: dmiss

      character(len=*), intent(in) :: quantity, forcing_file, variable_name
      real(dp), intent(in) :: target_x(:), target_y(:)
      integer, intent(in) :: mask(:), kx, filetype, method, oper
      integer, intent(inout) :: ec_item
      real(dp), pointer, intent(out) :: target_data(:)
      logical :: res

      integer, pointer :: pkbot(:), pktop(:)

      call reallocP(target_data, ndkx, fill=dmiss, keepExisting=.false.)
      call setzcs()
      pkbot => kbot
      pktop => ktop

      res = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, forcing_file, &
                                    filetype, method, oper, z=zcs, pkbot=pkbot, pktop=pktop, &
                                    varname=variable_name, tgt_item1=ec_item)
      res = res .and. ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, irefdate, tzone, &
                                                     tunit, tstart_user, target_data)
   end function read_3d_sigma_field

   !> Handle a [Spatial]/[Initial]/[Parameter] block whose forcingFileType is 1dField.
   function init_field1d_block(quantity, forcing_file, file_name) result(res)
      use stdlib_kinds, only: c_bool
      use timespace_parameters, only: FIELD1D
      use unstruc_inifields, only: init1dField, fm_quantity_name_to_source_quantity_name, set_global_values, set_global_water_values, finish_initialization, &
                                   specified_water_1dfield, specified_friction_1dfield, water_global_value_1dfield, friction_global_value_1dfield, water_global_quantity_1dfield
      use m_flowgeom, only: ndx2D, ndxi, lnx1d
      use dfm_error, only: DFM_NOERR
      use messageHandling, only: err_flush, msgbuf
      use string_module, only: str_tolower
      use m_alloc, only: realloc

      character(len=*), intent(in) :: quantity !< Quantity name as it appears in the ext block.
      character(len=*), intent(in) :: forcing_file !< Path to the 1dField .ini file.
      character(len=*), intent(in) :: file_name !< Path to the ext file, used in error messages.

      logical :: res

      character(len=256) :: source_quantity_name
      logical(kind=c_bool), allocatable :: specified_indices(:)
      real(dp) :: global_value
      logical :: global_value_provided
      integer :: ierr

      ! Map the FM quantity name (e.g. 'initialWaterLevel') to the name used inside
      ! the 1dField file (e.g. 'waterlevel').
      call fm_quantity_name_to_source_quantity_name(quantity, FIELD1D, source_quantity_name)
      if (source_quantity_name == '') then
         write (msgbuf, '(a)') 'Quantity '''//trim(quantity)//''' is not supported with forcingFileType=1dField'// &
            ' in '''//trim(file_name)//'''. Supported quantities: initialWaterLevel, initialWaterDepth,'// &
            ' initialVelocity, frictionCoefficient.'
         call err_flush()
         res = .false.
         return
      end if

      ierr = init1dField(forcing_file, file_name, source_quantity_name, specified_indices, global_value, global_value_provided)
      res = (ierr == DFM_NOERR)
      if (.not. res) return

      !> accumulate specified indices in global mask arrays, will be applied after all processing is done.
      select case (str_tolower(source_quantity_name))
      case ('waterlevel', 'waterdepth')
         call realloc(specified_water_1dfield, ndxi - ndx2D, fill=.false._c_bool, keepExisting=.true.)
         specified_water_1dfield = specified_water_1dfield .or. specified_indices
         if (global_value_provided) then
            water_global_value_1dfield = global_value
            water_global_quantity_1dfield = quantity
         end if

      case ('frictioncoefficient')
         call realloc(specified_friction_1dfield, lnx1d, fill=.false._c_bool, keepExisting=.true.)
         specified_friction_1dfield = specified_friction_1dfield .or. specified_indices
         if (global_value_provided) then
            friction_global_value_1dfield = global_value
         end if
      end select

      ! Shared post-processing: s1 = bl + hs for waterdepth, friction-type stamp, etc.
      call finish_initialization(quantity)

   end function init_field1d_block

   module function init_spatial_fields(block_ptr, base_dir, file_name, group_name) result(res)
      use m_ec_spatial_extrapolation, only: init_spatial_extrapolation
      use m_sferic, only: jsferic
      use string_module, only: str_tolower, strcmpi
      use messageHandling, only: err_flush, msgbuf
      use tree_data_types, only: tree_data
      use fm_location_types, only: parse_spatial_location_type, UNC_LOC_S, UNC_LOC_U, UNC_LOC_3DV, UNC_LOC_S3D, SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL
      use m_meteo, only: ec_addtimespacerelation, ec_gettimespacevalue_by_itemID, ecInstancePtr
      use m_flowtimes, only: tzone, tunit
      use m_ec_parameters, only: ec_undef_int
      use timespace_parameters, only: WEIGHTFACTORS, FIELD1D
      use properties, only: prop_get
      use m_alloc, only: realloc, reallocP
      use m_spatial_field, only: t_spatial_field_input, read_spatial_field_block, validate_spatial_field_input, &
                                 t_averaging_input, read_averaging_input, averaging_params_to_transformcoef                                 
      use unstruc_inifields, only: resolve_parameter_target, resolve_initial_target, process_hydrological_quantities, set_friction_type_values_explicit, resolve_initial_3D_target, resolve_integer_target, initialfield2Dto3D_dbl_indx
      use fm_external_forcings_data, only: NTRANSFORMCOEF
      use timespace, only: timespaceinitialfield, timespaceinitialfield_int
      use m_setinitialverticalprofile, only: setinitialverticalprofile
      use processes_input, only: painp
      use m_flowparameters, only: ja_friction_coefficient_time_dependent
      use m_heatfluxes, only: secchi_depth_is_time_varying
      use timespace_parameters, only: OPERAND_OVERRIDE
      use m_flowgeom_mask, only: construct_mask

      type(tree_data), pointer, intent(in) :: block_ptr
      character(len=*), intent(in) :: base_dir
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: group_name

      logical :: res

      integer, allocatable :: mask(:)
      integer :: target_location_type
      integer :: target_num_points
      real(dp), dimension(:), pointer :: target_x
      real(dp), dimension(:), pointer :: target_y
      integer :: ierr
      integer :: kx, first_index
      integer :: ec_item
      type(t_spatial_field_input) :: input
      real(dp), parameter :: DEFAULT_AIR_PRESSURE = 100000.0_dp

      real(dp), dimension(:), pointer :: target_data
      integer, dimension(:), pointer :: target_data_integer
      real(kind=dp), dimension(:, :), pointer :: target_array_3d
      integer :: oper_backup

      res = .false.
      ec_item = ec_undef_int
      target_data => null()
      target_data_integer => null()
      target_array_3d => null()

      input = read_spatial_field_block(block_ptr)
      res = validate_spatial_field_input(input, file_name, group_name, base_dir)
      if (.not. res) return

      associate (quantity => input%quantity, &
                 forcing_file => input%forcing_file, &
                 forcing_file_type => input%forcing_file_type, &
                 target_mask_file => input%target_mask_file, &
                 filetype => input%filetype, &
                 invert_mask => input%invert_mask, &
                 oper => input%oper, &
                 method => input%method, &
                 variable_name => input%variable_name, &
                 is_static_field => input%is_static_field)

         if (filetype == FIELD1D) then ! field1d is special, since it is not yet part of EC-module data reading+interpolation. TODO: refactor.
            res = init_field1d_block(quantity, forcing_file, file_name)
            return
         end if

         kx = 1
         ec_item = ec_undef_int
         target_data => null()

         res = scan_for_heat_quantities(quantity, target_location_type, kx)
         if (.not. res) then
            res = process_hydrological_quantities(quantity, file_name, target_location_type, target_data)
         end if
         if (.not. res) then
            res = resolve_parameter_target(quantity, file_name, target_location_type, target_data, kx)
         end if
         if (.not. res) then
            res = resolve_initial_target(quantity, file_name, target_location_type, target_data)
         end if
         if (.not. res) then
            res = resolve_meteo_target(quantity, file_name, target_location_type, target_data)
         end if
         if (.not. res) then
            res = resolve_initial_3D_target(quantity, target_location_type, target_array_3d, first_index)
         end if
         if (.not. res) then
            res = resolve_integer_target(quantity, target_location_type, target_data_integer)
         end if
         if (.not. res) then
            if (str_tolower(quantity) == 'bedlevel') then
               ! Ugly special case, bedlevel must not throw an error as it is read elsewhere.
               res = .true.
               return
            end if
         end if
         if (.not. res) then
            write (msgbuf, '(a)') 'Unknown quantity '''//trim(quantity)//' in file '''//file_name//''': ['//group_name//'].'
            call err_flush()
            return
         end if

         call get_location_target_properties(target_location_type, target_num_points, target_x, target_y, is_static_field, ierr)

         call construct_mask(mask, target_location_type, parse_spatial_location_type(trim(input%location_type)), target_mask_file, invert_mask, ierr)

         call init_spatial_extrapolation(input%max_search_radius, jsferic)

         if (is_static_field) then
            if (target_location_type == UNC_LOC_3DV) then ! vertical profiles are special
               call setinitialverticalprofile(target_data, size(target_data), forcing_file)
               res = .true.
            else ! normal spatial field
               block
                  real(dp) :: transformcoef(NTRANSFORMCOEF)
                  transformcoef = -999.0_dp
                  call averaging_params_to_transformcoef(input%averaging_input, transformcoef)
                  ! ugly extra reading for value, tracerfallvelocity and tracerdecaytime. Can be moved to t_spatial_field_input once transformcoef is eliminated
                  call prop_get(block_ptr, '', 'value', transformcoef(1))
                  call prop_get(block_ptr, '', 'tracerFallVelocity', transformcoef(2))
                  call prop_get(block_ptr, '', 'tracerDecayTime', transformcoef(6))

                  if (associated(target_array_3d)) then ! allocate temporary buffer for 3D
                     call reallocP(target_data, target_num_points, fill=dmiss, keepExisting=.false.)
                     oper_backup = oper
                     oper = OPERAND_OVERRIDE ! first call must always override, actual operand to be applied in initialfield2Dto3D_dbl_indx
                  end if

                  if (associated(target_data)) then
                     res = timespaceinitialfield(target_x, target_y, target_data, target_num_points, &
                                                 forcing_file, filetype, method, oper, transformcoef, target_location_type, mask)
                  else if (associated(target_data_integer)) then
                     res = timespaceinitialfield_int(target_x, target_y, target_data_integer, target_num_points, forcing_file, filetype, oper, transformcoef)
                  else if (associated(target_array_3d) .and. method == WEIGHTFACTORS) then !> special case
                     res = read_3d_sigma_field(quantity, target_x, target_y, mask, kx, forcing_file, filetype, method, oper, variable_name, ec_item, target_data)
                  end if

                  if (associated(target_array_3d)) then !> 3D postprocessing
                     oper = oper_backup
                     call initialfield2Dto3D_dbl_indx(target_data, target_array_3d, first_index, transformcoef(13), transformcoef(14), oper)
                     ! WAQ sp cast: waqparameter/waqsegmentnumber filled into dp buffer, cast back to painp.
                     if (str_tolower(quantity(1:12)) == 'waqparameter' .or. str_tolower(quantity(1:15)) == 'waqsegmentnumber') then
                        painp(first_index, 1:target_num_points) = target_data(1:target_num_points)
                        deallocate (target_array_3D)
                     end if
                     deallocate (target_data)
                  end if
               end block
            end if
         else
            select case (trim(str_tolower(forcing_file_type)))
            case ('bcascii')
               res = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, 'global', filetype, &
                                             method, oper, forcingfile=forcing_file, tgt_item1=ec_item, tgt_data1=target_data)
            case default
               if (len_trim(variable_name) > 0) then
                  res = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, forcing_file, filetype, &
                                                method, oper, varname=variable_name, tgt_item1=ec_item, tgt_data1=target_data)
               else if (target_location_type == UNC_LOC_S3D) then
                  res = read_3d_sigma_field(quantity, target_x, target_y, mask, kx, forcing_file, filetype, method, oper, variable_name, ec_item, target_data)
               else
                  res = ec_addtimespacerelation(quantity, target_x, target_y, mask, kx, forcing_file, filetype, &
                                                method, oper, tgt_item1=ec_item, tgt_data1=target_data)
               end if
            end select
         end if

         !  explicitly set time_dependent flags, not done in enable_quantity as is_static_field is not available.
         !  TODO: remove them by handling time-dependence generically.
         if (.not. is_static_field) then
            select case (str_tolower(quantity))
            case ('frictioncoefficient')
               ja_friction_coefficient_time_dependent = 1
            case ('secchidepth')
               secchi_depth_is_time_varying = .true.
            end select
         end if

         if (res) then
            res = enable_quantity(quantity)
            if (.not. res) then ! Friction coefficient is a special case, requires additional reading
               if (strcmpi(quantity, 'frictioncoefficient')) then
                  res = set_friction_type_values_explicit(block_ptr, input%oper)
               end if
            end if
            res = .true. ! For now if ec connection succeeded we don't care about enable_quantity.
         else
            write (msgbuf, '(a)') 'Failed to initialize quantity '''//trim(quantity)//''' from file '''//file_name// &
               ''': ['//group_name//']. Check previous log lines for details.'
            call err_flush()
         end if
      end associate

   end function init_spatial_fields

   !> Activate the model flags corresponding to a successfully loaded meteo quantity.
   !! Called after a successful ec_addtimespacerelation in init_spatial_fields.
   !! Returns .false. on a conflict (e.g. solarradiation + netsolarradiation).
   function enable_quantity(quantity) result(success)
      use m_wind, only: jaspacevarcharn, ja_airdensity, air_pressure_available, jawind, jarain, &
                        jaqin, solar_radiation_available, net_solar_radiation_available, long_wave_radiation_available, &
                        sensible_heat_flux_available, latent_heat_flux_available, pseudo_air_pressure_available, water_level_correction_available
      use m_flowparameters, only: itempforcingtyp
      use stdlib_kinds, only: c_bool
      use tree_data_types
      use tree_structures
      use m_missing, only: dmiss
      use m_alloc, only: realloc
      use messageHandling

      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use unstruc_files, only: resolvePath
      use system_utils, only: split_filename

      use timespace_parameters, only: FIELD1D
      use timespace, only: timespaceinitialfield, timespaceinitialfield_int
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U

      use m_flow, only: s1, hs, h_unsat
      use m_flowparameters, only: janudge
      use m_flowgeom, only: ndxi, ndx, bl
      use m_wind, only: jaevap, evap

      use m_hydrology_data, only: infiltcap, DFM_HYD_INFILT_CONST, &
                                  DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, ActEvap
      use m_grw, only: jaintercept2D
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read

      use m_heatfluxes, only: secchi_depth_is_spatially_varying, spatial_secchi_depth
      use m_physcoef, only: secchi_depth
      use m_meteo, only: ec_addtimespacerelation
      !use m_vegetation, only: stemheight, stemheightstd
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U
      use m_subsidence, only: jasubsupl
      use string_module, only: str_tolower
      use m_find_name, only: find_name

      use fm_external_forcings_utils, only: split_qid

      character(len=*), intent(in) :: quantity !< The quantity name as read from the [Meteo] block.
      character(len=idlen) :: qid_base, qid_specific

      integer n
      logical :: success

      success = .true.
      call split_qid(quantity, qid_base, qid_specific)

      select case (trim(quantity))
      case ('airdensity')
         ja_airdensity = 1

      case ('airpressure', 'atmosphericpressure')
         air_pressure_available = .true.

      case ('pseudoAirPressure')
         pseudo_air_pressure_available = .true.

      case ('waterLevelCorrection')
         water_level_correction_available = .true.

      case ('airpressure_windx_windy', 'airpressure_stressx_stressy', 'airpressure_windx_windy_charnock')
         jawind = 1
         air_pressure_available = .true.

      case ('charnock')
         jaspacevarcharn = 1

      case ('rainfall', 'rainfall_rate')
         jarain = 1
         jaqin = 1

      case ('windx', 'windy', 'windxy', 'stressxy', 'stressx', 'stressy')
         jawind = 1

      case ('dewpoint')
         itempforcingtyp = 5

      case ('solarradiation')
         if (net_solar_radiation_available) then
            write (msgbuf, '(3a)') 'quantity = ', trim(quantity), ' cannot be combined with netsolarradiation.'
            call err_flush()
            success = .false.
            return
         end if
         solar_radiation_available = .true.

      case ('netsolarradiation')
         if (solar_radiation_available) then
            write (msgbuf, '(3a)') 'quantity = ', trim(quantity), ' cannot be combined with solarradiation.'
            call err_flush()
            success = .false.
            return
         end if
         net_solar_radiation_available = .true.

      case ('longwaveradiation')
         long_wave_radiation_available = .true.

      case ('sensibleheatflux')
         sensible_heat_flux_available = .true.

      case ('latentheatflux')
         latent_heat_flux_available = .true.

      case ('humidity_airtemperature_cloudiness')
         itempforcingtyp = 1

      case ('dewpoint_airtemperature_cloudiness')
         itempforcingtyp = 3

      case ('humidity_airtemperature_cloudiness_solarradiation')
         itempforcingtyp = 2
         solar_radiation_available = .true.

      case ('dewpoint_airtemperature_cloudiness_solarradiation')
         itempforcingtyp = 4
         solar_radiation_available = .true.
      case default
         success = .false.
      end select

      if (success == .false.) then
         success = .true.
         select case (str_tolower(qid_base))
         case ('initialwaterdepth', 'waterdepth')
            s1(1:ndxi) = bl(1:ndxi) + hs(1:ndxi)
         case ('bedrock_surface_elevation')
            jasubsupl = 1
         case ('infiltrationcapacity')
            where (infiltcap /= dmiss)
               infiltcap = infiltcap * 1e-3_dp / (24.0_dp * 3600.0_dp) ! mm/day => m/s
            end where
         case ('potentialevaporation')
            where (PotEvap /= dmiss)
               PotEvap = PotEvap * 1e-3_dp / (3600.0_dp) ! mm/hr => m/s
            end where
            jaevap = 1
            if (.not. allocated(evap)) then
               call realloc(evap, ndx, keepExisting=.false., fill=0.0_dp)
            end if
            evap = -PotEvap ! evap and PotEvap are now still doubling

            if (.not. allocated(ActEvap)) then
               call realloc(ActEvap, ndx, keepExisting=.false., fill=0.0_dp)
            end if
            jadhyd = 1
         case ('initialunsaturedzonethickness', 'interceptionlayerthickness')
            where (h_unsat == -999.0_dp)
               h_unsat = 0.0_dp
            end where
            if (quantity == 'interceptionlayerthickness') then
               jaintercept2D = 1
            end if
         case ('sea_ice_area_fraction')
            ja_ice_area_fraction_read = 1
         case ('sea_ice_thickness')
            ja_ice_thickness_read = 1
         case ('secchidepth')
            secchi_depth_is_spatially_varying = .true.
            do n = 1, ndx
               if (spatial_secchi_depth(n) == dmiss) then
                  spatial_secchi_depth(n) = secchi_depth(1)
               end if
            end do
            ! TODO: fix this buggy legacy mess
            !case ('stemheight')
            !   if (stemheightstd > 0.0_dp) then
            !      stemheight = stemheight * (1.0_dp + stemheightstd * (ran0(idum) - 0.5_dp))
            !   end if
         case ('nudgesalinitytemperature')
            janudge = 1
         case default
            success = .false.
         end select
      end if

   end function enable_quantity

   !> Parse source/sink coordinates, either from the ext file, a polyline file specified in the ext file, or a combination of both
   module function sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink) result(is_successful)
      use messageHandling, only: err_flush, msgbuf
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use m_missing, only: dmiss
      use m_polygon, only: dzL, npl
      use m_read_location_info, only: read_polyline_coordinates
      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to sourcesink block in extforce file; child node of the extforce file tree
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in error messages, actual data is read from block_ptr
      character(len=*), intent(in) :: group_name !< Name of the block, only used in error messages

      real(kind=dp), dimension(:), allocatable, intent(out) :: x_coordinates
      real(kind=dp), dimension(:), allocatable, intent(out) :: y_coordinates
      integer, parameter :: num_range_points = 2 ! only constant profiles (1 value) or linear profiles (2 values) are allowed
      real(kind=dp), dimension(num_range_points), intent(out) :: z_range_source
      real(kind=dp), dimension(num_range_points), intent(out) :: z_range_sink

      character(len=INI_VALUE_LEN) :: sourcesink_id
      real(kind=dp), dimension(:), allocatable :: z_coordinates
      integer :: num_columns
      logical :: is_successful
      logical :: is_read
      logical :: source_z_in_ext_file, sink_z_in_ext_file
      logical :: have_location_file
      integer :: npts

      is_successful = .false.
      z_range_source(:) = dmiss
      z_range_sink(:) = dmiss

      ! Read source/sink z range information from ext file, load it from the polyline file later on as a fallback.
      source_z_in_ext_file = .false.
      sink_z_in_ext_file = .false.
      call prop_get(block_ptr, '', 'zSource', z_range_source, num_range_points, source_z_in_ext_file)
      call prop_get(block_ptr, '', 'zSink', z_range_sink, num_range_points, sink_z_in_ext_file)

      ! Read the id for error messages
      sourcesink_id = ' '
      call prop_get(block_ptr, '', 'id', sourcesink_id, is_read)

      ! Use generic polyline reader
      call read_polyline_coordinates(block_ptr, trim(sourcesink_id), file_name, base_dir, group_name, &
                                     x_coordinates, y_coordinates, z_coordinates, num_columns, is_successful)
      if (.not. is_successful) return

      ! Source/sink-specific: interpret z columns as z_range_source / z_range_sink
      have_location_file = (num_columns > 0)
      if (have_location_file) then
         npts = size(z_coordinates)

         ! Avoid having two places specifying the same (and potentially conflicting) z data.
         if (num_columns > 2 .and. (source_z_in_ext_file .or. sink_z_in_ext_file)) then
            write (msgbuf, '(a)') 'Error in source sink initialization, source/sink z information cannot be specified both ' &
               //'in the ext file and in the polyline file. Make sure the polyline file only contains x and y columns'
            call err_flush()
            is_successful = .false.
            return
         end if

         if (.not. source_z_in_ext_file) then
            z_range_source(1) = z_coordinates(npts)
            if (num_columns > 3) then
               ! 4th column (dzL) needs to be read from the polygon module directly,
               ! since read_polyline_coordinates only returns the 3rd column (zpl).
               z_range_source(2) = dzL(npl)
            end if
         end if

         if (.not. sink_z_in_ext_file) then
            z_range_sink(1) = z_coordinates(1)
            if (num_columns > 3) then
               z_range_sink(2) = dzL(1)
            end if
         end if
      end if

      if (allocated(z_coordinates)) deallocate (z_coordinates)
      is_successful = .true.

   end function

   !> Read sourcesink blocks from new external forcings file.
   function init_sourcesink_forcings(block_ptr, base_dir, file_name, group_name) result(is_successful)
      use messageHandling, only: err_flush, msgbuf
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use unstruc_files, only: resolvePath
      use m_transport, only: NAMLEN, NUMCONST, const_names, ISALT, ITEMP, ISED1, ISEDN, ISPIR, ITRA1, ITRAN
      use netcdf_utils, only: ncu_sanitize_name
      use m_missing, only: dmiss
      use m_source_sink, only: addsorsin, source_sinks, source_sink_all_discharges
      use dfm_error, only: DFM_NOERR
      use m_filez, only: oldfil
      use m_polygon, only: xpl, ypl, zpl, dzL
      use m_reapol, only: reapol

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to sourcesink block in extforce file; child node of the extforce file tree
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in error messages, actual data is read from block_ptr
      character(len=*), intent(in) :: group_name !< Name of the block, only used in error messages

      character(len=INI_VALUE_LEN) :: sourcesink_id
      character(len=INI_VALUE_LEN) :: sourcesink_name
      character(len=INI_VALUE_LEN) :: discharge_input
      character(len=INI_VALUE_LEN), dimension(:), allocatable :: constituent_delta_file
      character(len=NAMLEN) :: const_name_with_prefix
      character(len=INI_VALUE_LEN) :: quantity_id, property_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2 ! only constant profiles (1 value) or linear profiles (2 values) are allowed
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink
      real(kind=dp) :: area
      integer :: i_const
      integer :: ierr
      logical :: is_successful
      logical :: is_read

      is_successful = .false.
      z_range_source(:) = dmiss
      z_range_sink(:) = dmiss

      sourcesink_id = ' '
      call prop_get(block_ptr, '', 'id', sourcesink_id, is_read)
      if (.not. is_read .or. len_trim(sourcesink_id) == 0) then
         write (msgbuf, '(a)') 'Incomplete block in file '''//trim(file_name)//''': ['//trim(group_name)//']. Field ''id'' is missing.'
         call err_flush()
         return
      end if
      call prop_get(block_ptr, '', 'name', sourcesink_name, is_read)

      is_successful = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. is_successful) then
         return ! Error message already printed in sourcesink_parse_coordinates
      end if

      call prop_get(block_ptr, '', 'discharge', discharge_input, is_read)
      if (.not. is_read) then
         write (msgbuf, '(a)') 'Incomplete block in file '''//trim(file_name)//''': ['//trim(group_name)//']. Key "discharge" is missing.'
         call err_flush()
         return
      end if

      ! read optional value 'area' to compute the momentum released
      area = 0.0_dp
      call prop_get(block_ptr, '', 'area', area, is_read)

      ! Create the actual source/sink based on the parsed data
      call addsorsin(sourcesink_id, x_coordinates, y_coordinates, z_range_source, z_range_sink, area, ierr)
      if (ierr /= DFM_NOERR) then
         write (msgbuf, '(a)') 'Error while processing '''//trim(file_name)//''': ['//trim(group_name), ']. ' &
            //'Source sink with id='//trim(sourcesink_id)//'. could not be added.'
         call err_flush()
         return
      end if

      quantity_id = 'sourcesink_discharge' ! New quantity name in .bc files
      !call resolvePath(filename, basedir) ! TODO!
      is_successful = adduniformtimerelation_objects(quantity_id, '', 'source sink', trim(sourcesink_id), 'discharge', trim(discharge_input), source_sinks%num_total, &
                                                     1, source_sink_all_discharges(1, :))

      if (.not. is_successful) then
         write (msgbuf, '(a)') 'Error while processing '''//trim(file_name)//''': ['//trim(group_name)//']. ' &
            //'Could not initialize discharge data in '''//trim(discharge_input)//''' for source sink with id='//trim(sourcesink_id)//'.'
         call err_flush()
         return
      end if

      ! Constituents (salinity, temperature, sediments, tracers) may have a timeseries file
      ! specifying the difference in concentration added by the source/sink.
      ! All these files are optional, so no check on 'is_read' can be present below.
      if (numconst > 0) then
         allocate (constituent_delta_file(numconst), stat=ierr)
         do i_const = 1, numconst
            is_read = .false.
            const_name_with_prefix = const_names(i_const)
            if (i_const == isalt) then
               ! Rename 'salt' constituent to 'salinity' for source-sink input.
               const_name_with_prefix = 'salinity'
            else if (i_const == itemp) then
               ! temperature name is correct already
               continue
            else if (i_const == ispir) then
               ! Spiral flow intensity "constituent" not relevant for source-sinks.
               cycle
            else
               ! Tracers and sediments: remove special characters from constituent name before constructing the property to read.
               call ncu_sanitize_name(const_name_with_prefix)

               ! Add correct "group" prefix to constituent name.
               if (i_const >= ised1 .and. i_const <= isedn) then
                  const_name_with_prefix = 'sedFrac'//trim(const_name_with_prefix)
               else if (i_const >= itra1 .and. i_const <= itran) then
                  const_name_with_prefix = 'tracer'//trim(const_name_with_prefix)
               end if
            end if

            property_name = trim(const_name_with_prefix)//'Delta'
            call prop_get(block_ptr, '', property_name, constituent_delta_file(i_const), is_read)

            if (is_read) then
               quantity_id = 'sourcesink_'//trim(property_name) ! New quantity name in .bc files
               !call resolvePath(filename, basedir) ! TODO!
               is_successful = adduniformtimerelation_objects(quantity_id, '', 'source sink', trim(sourcesink_id), trim(property_name), trim(constituent_delta_file(i_const)), source_sinks%num_total, &
                                                              1, source_sink_all_discharges(1 + i_const, :))
               continue
            end if
         end do
      end if

      is_successful = .true.

   end function init_sourcesink_forcings

   !> Read bubblescreen blocs from the extfile, read its polyline (file or inline coordinates), find flowcells crossed by the polyline and calculate the resulting bubblescreen area.
   subroutine initialize_bubblescreens(bnd_ptr, base_dir, file_name, num_bubblescreen_source_sinks)
      use fm_external_forcings_data, only: t_Bubblescreen, bubblescreens
      use m_source_sink, only: source_sinks
      use fm_external_forcings_utils, only: read_bubblescreen_forcing_attributes
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_data, tree_num_nodes, tree_count_nodes_byname, tree_get_name
      use string_module, only: strcmpi, str_tolower
      use network_data
      use m_flow
      use m_cellmask_from_polygon_set, only: find_cells_crossed_by_polyline, init_cell_geom_as_polylines, cleanup_cell_geom_polylines
      use m_alloc, only: realloc
      use m_find_flownode, only: find_nearest_flownodes
      use m_GlobalParameters, only: INDTP_2D
      use messageHandling, only: err_flush, msgbuf, warn_flush
      use m_bubblescreen, only: compute_bubblescreen_area
      use m_structures, only: nNodesBubbleScreen, nodeCountBubbleScreen
      use m_partitioninfo, only: jampi, reduce_cells
      use m_flowgeom, only: ndx
      use m_read_location_info, only: read_polyline_coordinates

      ! Parameters
      type(tree_data), pointer, intent(in) :: bnd_ptr !< tree of extForceBnd-file's [boundary] blocks
      character(len=*), intent(in) :: base_dir !< Base directory of the ext file
      character(len=*), intent(in) :: file_name !< Name of the ext file, only used in error messages, actual data is read from block_ptr
      integer, intent(out) :: num_bubblescreen_source_sinks !< Number of source/sinks needed for all bubblescreens, used for preallocation in EC module

      ! Local variables
      logical :: is_successful
      integer :: i !< Loop index
      integer :: i_bubblescreen !< Loop index for bubblescreens within the .ext file
      integer :: num_bubblescreens
      integer :: num_items_in_file
      integer :: num_columns
      real(kind=dp), dimension(:), allocatable :: polygon_x_coordinates !< x-coordinates of bubblescreen
      real(kind=dp), dimension(:), allocatable :: polygon_y_coordinates !< y-coordinates of bubblescreen
      real(kind=dp), dimension(:), allocatable :: polygon_z_coordinates !< z-coordinates of bubblescreen (unused, required by generic reader)
      character(len=:), allocatable :: group_name !< Name of the block, only used in error messages
      character(len=:), allocatable :: id !< Bubblescreen id
      character, dimension(:), allocatable :: error

      type(tree_data), pointer :: block_ptr
      type(t_Bubblescreen) :: bubblescreen
      integer :: n_cells
      integer, dimension(:), allocatable :: bubblescreen_cells

      ! Initialization
      i_bubblescreen = 0
      num_bubblescreen_source_sinks = 0
      num_items_in_file = tree_num_nodes(bnd_ptr)

      if (allocated(bubblescreens)) then
         deallocate (bubblescreens)
      end if
      if (allocated(bubblescreen_air_discharge)) then
         deallocate (bubblescreen_air_discharge)
      end if
      ! Count the number of [bubblescreen] blocks and allocate the bubblescreens and bubblescreen_air_discharge arrays
      num_bubblescreens = tree_count_nodes_byname(bnd_ptr, 'bubblescreen')
      allocate (bubblescreens(num_bubblescreens))
      allocate (bubblescreen_air_discharge(num_bubblescreens))

      ! Initialize cache
      call init_cell_geom_as_polylines()

      ! Cycle through all [blocks] in the .ext file tree and find the [bubblescreen] blocks
      do i = 1, num_items_in_file
         block_ptr => bnd_ptr%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(block_ptr))

         if (str_tolower(group_name) == 'bubblescreen') then
            i_bubblescreen = i_bubblescreen + 1
            is_successful = read_bubblescreen_forcing_attributes(block_ptr, base_dir, file_name, group_name, bubblescreen%id, &
                                                                 polygon_x_coordinates, polygon_y_coordinates, polygon_z_coordinates, num_columns, bubblescreen%z_level, bubblescreen%discharge_input)
            if (is_successful) then
               if (num_columns > 2 .and. allocated(polygon_z_coordinates)) then
                  if (any(polygon_z_coordinates /= dmiss)) then
                     write (msgbuf, '(a)') 'Bubblescreen '''//trim(id)//''': z-coordinates were read from polygon input (pliz), but they are ignored. '// &
                        'use zLevel to specify Bubblescreen location.'
                     call warn_flush()
                  end if
               end if
               ! Find cells crossed by the polyline and pre-init the bubblescreen data structure
               call find_cells_crossed_by_polyline(polygon_x_coordinates, polygon_y_coordinates, bubblescreen%flowcell_indices, error)
               bubblescreen%num_flowcells = size(bubblescreen%flowcell_indices)
               n_cells = bubblescreen%num_flowcells
               ! we need the global number of bubblescreen cells, otherswise when doing addSourceSink the vectors will be re-allocated
               ! and then EC-module will be left with dangling pointers.
               bubblescreen_cells = bubblescreen%flowcell_indices
               if (jampi == 1) then
                  bubblescreen_cells = reduce_cells(bubblescreen%flowcell_indices, ndx)
                  n_cells = size(bubblescreen_cells)
               end if

               num_bubblescreen_source_sinks = num_bubblescreen_source_sinks + n_cells
               bubblescreen%total_area = compute_bubblescreen_area(bubblescreen)
               call realloc(bubblescreen%is_active, bubblescreen%num_flowcells, fill=.true.)
            end if

            ! Add the pre-initialized bubblescreen to bubblescreens
            bubblescreens(i_bubblescreen) = bubblescreen
         end if
      end do

      call cleanup_cell_geom_polylines()
      ! initialize global geometry
      call realloc(nodeCountBubbleScreen, size(bubblescreens), fill=0)
      nNodesBubbleScreen = 0

   end subroutine initialize_bubblescreens

   !> Create bubblescreen source-sinks and set up the EC module connection. In parallel models the bubblescreen input is reduced, as
   !! Source-sinks need to be added globally.
   function add_bubblescreen_source_sinks() result(is_successful)
      use fm_external_forcings_utils, only: read_bubblescreen_forcing_attributes
      use m_filez, only: oldfil
      use m_reapol, only: reapol
      use messageHandling, only: err_flush, msgbuf, msg_flush
      use tree_data_types, only: tree_data
      use m_polygon, only: xpl, ypl, zpl, npl
      use m_cellmask_from_polygon_set, only: find_cells_crossed_by_polyline
      use network_data
      use m_flow
      use fm_external_forcings_data
      use m_source_sink, only: addsorsin, addsorsin_from_polyline_file, setsorsin, source_sinks
      use m_missing, only: dmiss
      use m_partitioninfo, only: jampi, reduce_cells, reduce_double_array_max, my_rank
      use m_alloc, only: realloc
      use m_flowgeom, only: ndx
      use m_structures, only: nNodesBubbleScreen, nodeCountBubbleScreen, geomXBubbleScreen, geomYBubbleScreen

      ! Local variables
      logical :: is_successful !< Success flag
      integer :: cidx !< Index for crossed cells
      integer :: i, j !< Loop indices
      integer :: ierr !< Error code
      integer :: bubblescreen_source_sink_count
      integer :: n_cells
      integer, dimension(:), allocatable :: bubblescreen_cells
      integer :: local_count
      integer :: tm_global_count

      real(kind=dp), dimension(:), allocatable :: x_flowcell !< x-coordinate of flow cell
      real(kind=dp), dimension(:), allocatable :: y_flowcell !< y-coordinate of flow cell
      real(kind=dp), dimension(2) :: z_flowcell_source !< z-coordinate of flow cell source
      real(kind=dp), dimension(2) :: z_flowcell_sink !< z-coordinate of flow cell sink

      character(len=305) :: srcid !< Source id (length 255 + 50 for bubblescreen index)

      ! Initialization
      is_successful = .false.

      ! Find the bubblescreen with matching id
      do i = 1, size(bubblescreens)

         bubblescreen_source_sink_count = 0
         local_count = 0
         associate (bubblescreen => bubblescreens(i))

            n_cells = bubblescreen%num_flowcells
            bubblescreen_cells = bubblescreen%flowcell_indices
            ! we need the global number of bubblescreen cells, addsorsin must be called on every partition
            if (jampi == 1) then
               bubblescreen_cells = reduce_cells(bubblescreen%flowcell_indices, ndx)
               n_cells = size(bubblescreen_cells)
            end if
            call realloc(x_flowcell, n_cells, fill=-huge(1.0_dp), keepexisting=.false.)
            call realloc(y_flowcell, n_cells, fill=-huge(1.0_dp), keepexisting=.false.)
            do j = 1, n_cells
               if (.not. bubblescreen_cells(j) == -1) then
                  x_flowcell(j) = xzw(bubblescreen_cells(j))
                  y_flowcell(j) = yzw(bubblescreen_cells(j))
               end if
            end do
            if (jampi == 1) then
               call reduce_double_array_max(n_cells, x_flowcell)
               call reduce_double_array_max(n_cells, y_flowcell)
            end if
            tm_global_count = nNodesBubbleScreen
            nNodesBubbleScreen = nNodesBubbleScreen + n_cells
            call realloc(geomXBubbleScreen, nNodesBubbleScreen, keepExisting=.true.)
            call realloc(geomYBubbleScreen, nNodesBubbleScreen, keepExisting=.true.)
            geomXBubbleScreen(tm_global_count + 1:nNodesBubbleScreen) = x_flowcell
            geomYBubbleScreen(tm_global_count + 1:nNodesBubbleScreen) = y_flowcell
            nodeCountBubbleScreen(i) = n_cells

            z_flowcell_source = 0.0_dp ! Dummy value, will be set properly later
            z_flowcell_sink = bubblescreen%z_level
            call realloc(bubblescreen%source_sink_indices, bubblescreen%num_flowcells, fill=-1, keepexisting=.false.)
            ! Cycle through all bubblescreen flow cells and create source/sink objects for each of them
            do cidx = 1, n_cells
               ! Create the source/sink name
               bubblescreen_source_sink_count = bubblescreen_source_sink_count + 1
               write (srcid, '(A,I0)') trim(bubblescreen%id), bubblescreen_source_sink_count

               ! Create a linked source/sink in the flow cell
               call addsorsin(srcid, [x_flowcell(cidx), x_flowcell(cidx)], [y_flowcell(cidx), y_flowcell(cidx)], z_flowcell_source, z_flowcell_sink, 0.0_dp, ierr)
               if (bubblescreen_cells(cidx) /= -1) then
                  local_count = local_count + 1
                  bubblescreen%flowcell_indices(local_count) = bubblescreen_cells(cidx) !> the order bubblescreen_cells and flowcell_indices is not the same, so overwrite this
                  bubblescreen%source_sink_indices(local_count) = source_sinks%num_total !> global counter which has just been incremented by addsorsin
               end if
            end do

            is_successful = adduniformtimerelation_objects('bubblescreen_discharge', '', 'source sink', trim(bubblescreen%id), 'discharge', &
                                                           trim(bubblescreen%discharge_input), i, 1, bubblescreen_air_discharge)

            if (.not. is_successful) then
               write (msgbuf, '(5a)') 'Could not initialize discharge data in ''', trim(bubblescreen%discharge_input), ''' for bubble screen with id='//trim(bubblescreen%id)//'.'
               call err_flush()
               return
            end if
         end associate
      end do

      is_successful = .true.

   end function add_bubblescreen_source_sinks

   !> Get several target grid properties for a given location type.
   !!
   !! Properties include: coordinates and location count,
   !! typically used in setting up the time-space relations for
   !! external forcings quantities.
   subroutine get_location_target_properties(target_location_type, target_num_points, target_x, target_y, exclude_boundary_nodes, ierr)
      use fm_location_types
      use m_flowgeom, only: ndx, ndxi, lnx, xz, yz, xu, yu
      use network_data, only: xk, yk, numk
      use precision_basics, only: dp
      use dfm_error, only: DFM_NOERR, DFM_NOTIMPLEMENTED

      integer, intent(in) :: target_location_type
      integer, intent(out) :: target_num_points
      real(dp), dimension(:), pointer, intent(out) :: target_x
      real(dp), dimension(:), pointer, intent(out) :: target_y
      logical, intent(in) :: exclude_boundary_nodes !> equals is_static_field, boundary nodes are only included for time-varying
      integer, intent(out) :: ierr

      ierr = DFM_NOERR

      select case (target_location_type)
      case (UNC_LOC_S, UNC_LOC_S3D)
         if (exclude_boundary_nodes) then
            target_num_points = ndxi
         else
            target_num_points = ndx
         end if
         target_x => xz(1:target_num_points)
         target_y => yz(1:target_num_points)
      case (UNC_LOC_U)
         target_num_points = lnx
         target_x => xu(1:target_num_points)
         target_y => yu(1:target_num_points)
      case (UNC_LOC_CN)
         target_num_points = numk
         target_x => xk(1:target_num_points)
         target_y => yk(1:target_num_points)
      case (UNC_LOC_GLOBAL)
         target_num_points = 0
         target_x => null()
         target_y => null()
      case default
         ierr = DFM_NOTIMPLEMENTED
      end select
   end subroutine get_location_target_properties

   !> Scan the quantity name for heat relatede quantities.
   function scan_for_heat_quantities(quantity, target_location_type, kx) result(success)
      use m_wind, only: air_temperature, cloudiness, dew_point_temperature, relative_humidity, solar_radiation, long_wave_radiation, sensible_heat_flux, latent_heat_flux
      use m_flowgeom, only: ndx
      use m_alloc, only: aerr, realloc
      use fm_location_types, only: UNC_LOC_S
      character(len=*), intent(in) :: quantity !< Name of the data set.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U. For heat quantities this is always UNC_LOC_S
      integer, intent(out) :: kx !< Number of individual quantities in the data set
      logical :: success !< Return value, indicates whether the quantity is supported in this subroutine.

      integer :: ierr

      kx = 1
      success = .true.
      target_location_type = UNC_LOC_S
      select case (quantity)

      case ('airtemperature')
         call realloc(air_temperature, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('air_temperature(ndx)', ierr, ndx)
      case ('cloudiness')
         call realloc(cloudiness, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('cloudiness(ndx)', ierr, ndx)
      case ('humidity')
         call realloc(relative_humidity, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('relative_humidity(ndx)', ierr, ndx)
      case ('dewpoint')
         call realloc(dew_point_temperature, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('dew_point_temperature(ndx)', ierr, ndx)
      case ('solarradiation', 'netsolarradiation')
         call realloc(solar_radiation, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('solar_radiation(ndx)', ierr, ndx)
      case ('longwaveradiation')
         call realloc(long_wave_radiation, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('long_wave_radiation(ndx)', ierr, ndx)
      case ('sensibleheatflux')
         call realloc(sensible_heat_flux, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('sensible_heat_flux(ndx)', ierr, ndx)
      case ('latentheatflux')
         call realloc(latent_heat_flux, ndx, stat=ierr, fill=0.0_dp, keepexisting=.false.)
         call aerr('latent_heat_flux(ndx)', ierr, ndx)
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
