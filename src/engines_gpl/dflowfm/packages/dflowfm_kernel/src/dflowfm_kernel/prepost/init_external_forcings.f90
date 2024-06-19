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
submodule(m_init_ext_forcings) initialisation

   implicit none

contains

!> reads new external forcings file and makes required initialisations
   module function init_external_forcings(external_force_file_name) result(res)
      use properties
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_flowexternalforcings
      use m_flowgeom
      use timespace_data, only: weightfactors, poly_tim, uniform, spaceandtime, getmeteoerror
      use m_lateral, only: balat, qplat, lat_ids, n1latsg, n2latsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, numlatsg, nnlat, nlatnd
      use m_meteo, only: ec_addtimespacerelation
      use timespace
      use string_module, only: str_tolower, strcmpi
      use system_utils
      use unstruc_files, only: resolvePath
      use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
      use m_missing
      use m_ec_parameters, only: provFile_uniform
      use m_partitioninfo, only: jampi, reduce_sum, is_ghost_node
      use m_lateral, only: apply_transport
      use m_flow, only: kmx
      use m_deprecation, only: check_file_tree_for_deprecated_keywords
      use fm_deprecated_keywords, only: deprecated_ext_keywords
      
      character(len=*), intent(in)  :: external_force_file_name   !< file name for new external forcing boundary blocks
      logical                       :: res
      
      logical                       :: is_successful
      type(tree_data), pointer      :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
      type(tree_data), pointer      :: node_ptr            !
      integer                       :: istat               !
      integer, parameter            :: INI_KEY_LEN = 32    !
      integer, parameter            :: INI_VALUE_LEN = 256 !
      character(len=:), allocatable :: group_name          !
      character(len=INI_VALUE_LEN)  :: property_name
      character(len=INI_VALUE_LEN)  :: property_value
      character(len=INI_VALUE_LEN)  :: quantity
      character(len=INI_VALUE_LEN)  :: location_file       !
      character(len=INI_VALUE_LEN)  :: forcing_file        !
      character(len=INI_VALUE_LEN)  :: forcing_file_type   !
      character(len=INI_VALUE_LEN)  :: target_mask_file    !
      integer                       :: i, j                 !
      integer                       :: method
      integer                       :: num_items_in_file   !
      integer                       :: num_items_in_block
      character(len=1)              :: oper                !
      character(len=300)            :: rec
      character(len=INI_VALUE_LEN)  :: nodeid
      character(len=INI_VALUE_LEN)  :: branchid
      character(len=INI_VALUE_LEN)  :: loc_id
      character(len=INI_VALUE_LEN)  :: item_type
      character(len=INI_VALUE_LEN)  :: fnam
      character(len=INI_VALUE_LEN)  :: base_dir
      double precision              :: chainage
      integer                       :: ierr               ! error number from allocate function
      integer                       :: ilattype, nlat
      integer                       :: k, n, k1, nini
      integer, dimension(1)         :: target_index
      integer                       :: ib, ibqh, ibt
      integer                       :: maxlatsg
      integer                       :: major, minor
      integer                       :: loc_spec_type
      integer                       :: numcoordinates
      double precision, allocatable :: xcoordinates(:), ycoordinates(:)
      character(len=:), allocatable :: file_name
      integer, allocatable          :: itpenzr(:), itpenur(:)

      file_name = trim(external_force_file_name)

      res = .true.

      call tree_create(file_name, bnd_ptr)
      call prop_file('ini', file_name, bnd_ptr, istat)

      ! check FileVersion
      major = 1
      minor = 0
      call prop_get_version_number(bnd_ptr, major=major, minor=minor, success=is_successful)
      if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
          write (msgbuf,'(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''&
              //file_name//''': v', major, minor, '. Current format: v', ExtfileNewMajorVersion, ExtfileNewMinorVersion, &
              '. Ignoring this file.'
         call err_flush()
         res = .false.
         return
      end if

      call init_registered_items()

      call split_filename(file_name, base_dir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

      num_items_in_file = tree_num_nodes(bnd_ptr)

      ! Build temporary reverse lookup table that maps boundary block # in file -> boundary condition nr in openbndsect (separate for u and z).
      allocate (itpenzr(num_items_in_file), itpenur(num_items_in_file))
      itpenzr(:) = 0
      itpenur(:) = 0
      do ibt = 1, nbndz
         ib = itpenz(ibt)
         if (ib > 0) then
            itpenzr(ib) = ibt
         end if
      end do
      do ibt = 1, nbndu
         ib = itpenu(ibt)
         if (ib > 0) then
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

      ib = 0
      ibqh = 0
      do i = 1, num_items_in_file
         node_ptr => bnd_ptr%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(node_ptr))

         select case (str_tolower(group_name))
         case ('general')
            ! General block, was already read.

         case ('boundary')
            res = init_boundary_forcings()

         case ('lateral')
            res = init_lateral_forcings()

         case ('meteo')
            res = init_meteo_forcings()

         case default       ! Unrecognized item in a ext block
            ! res remains unchanged: Not an error (support commented/disabled blocks in ext file)
            write (msgbuf, '(5a)') 'Unrecognized block in file ''', file_name, ''': [', group_name, ']. Ignoring this block.'
            call warn_flush()

         end select
      end do

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

      call tree_destroy(bnd_ptr)
      if (allocated(thrtt)) then
         call init_threttimes()
      end if

   contains

!> reads boundary blocks from new external forcings file and makes required initialisations
      function init_boundary_forcings() result(res)

         logical :: res

         type(tree_data), pointer     :: block_ptr           !

         res = .false.
         ! First check for required input:
         call prop_get(node_ptr, '', 'quantity', quantity, is_successful)
         if (.not. is_successful) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
            call warn_flush()
            return
         end if
         ib = ib + 1

         call prop_get(node_ptr, '', 'nodeId', location_file, is_successful)
         if (is_successful) then
            filetype = node_id
            method = spaceandtime
         else
            filetype = poly_tim
            method = weightfactors
            call prop_get(node_ptr, '', 'locationfile', location_file, is_successful)
         end if

         if (is_successful) then
            call resolvePath(location_file, base_dir)
         else
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''locationfile'' is missing.'
            call warn_flush()
            return
         end if

         call prop_get(node_ptr, '', 'forcingFile ', forcing_file, is_successful)
         if (is_successful) then
            call resolvePath(forcing_file, base_dir)
         else
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
            call warn_flush()
            return
         end if

         oper = '-'
         call prop_get(node_ptr, '', 'operand ', oper, is_successful)

         num_items_in_block = 0
         if (associated(node_ptr%child_nodes)) then
            num_items_in_block = size(node_ptr%child_nodes)
         end if

         ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingfile=...
         do j = 1, num_items_in_block
            block_ptr => node_ptr%child_nodes(j)%node_ptr
            ! todo: read multiple quantities
            property_name = trim(tree_get_name(block_ptr))
            call tree_get_data_string(block_ptr, property_value, is_successful)
            if (is_successful) then
               if (property_name == 'quantity') then
                  quantity = property_value ! We already knew this
               else if (property_name == 'locationfile') then
                  location_file = property_value ! We already knew this
                  call resolvePath(location_file, base_dir)
               else if (property_name == 'forcingfile') then
                  forcing_file = property_value
                  call resolvePath(forcing_file, base_dir)
                  if (oper /= 'O' .and. oper /= '+') then
                     oper = 'O'
                     if (quantity_pli_combination_is_registered(quantity, location_file)) then
                        oper = '+'
                     end if
                  end if
                  call register_quantity_pli_combination(quantity, location_file)
                  if (filetype == node_id .or. quantity == 'qhbnd') then
                     select case (quantity)
                     case ('waterlevelbnd')
                        target_index = itpenzr(ib)

                     case ('qhbnd')
                        ibqh = ibqh + 1
                        target_index = (/ibqh/)
                        if (filetype /= node_id) then
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
                     else if (forcing_file == '-') then
                        is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=node_id, method=method, &
                                                                  operand=oper, targetindex=target_index(1))
                     else
                        is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=node_id, method=method, &
                                                                operand=oper, forcingfile=forcing_file, targetindex=target_index(1))
                     end if
                  else
                     if (forcing_file == '-') then
                        is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=method, &
                                                                  operand=oper)
                     else
                        is_successful = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=method, &
                                                                  operand=oper, forcingfile=forcing_file)
                     end if
                  end if
                  res = res .and. is_successful ! Remember any previous errors.
                  oper = '-'
               else if (property_name == 'operand') then
                  continue
               else if (property_name == 'returntime' .or. property_name == 'return_time') then
                  continue                   ! used elsewhere to set Thatcher-Harleman delay
               else if (property_name == 'openboundarytolerance') then
                  continue                   ! used in findexternalboundarypoints/readlocationfiles... to set search distance. Not relevant here.
               else if (property_name == 'nodeid') then
                  continue
               else if (property_name == 'bndwidth1d') then
                  continue
               else if (property_name == 'bndbldepth') then
                  continue
               else
                  ! res remains unchanged: support ignored lines in ext file.
                     write (msgbuf, '(9a)') 'Unrecognized line in file ''', file_name, ''' for block [', group_name, ']: ', &
                         trim(property_name), ' = ', trim(property_value), '. Ignoring this line.'
                  call warn_flush()
                  cycle
               end if
            end if
         end do
         if (.not. is_successful) then ! This addtimespace was not successful
            rec = getmeteoerror()
            if (len_trim(rec) > 0) then
               call mess(LEVEL_WARN, trim(rec))
            end if
            call mess(LEVEL_WARN, 'initboundaryblockforcings: Error while initializing quantity '''//trim(quantity)//&
                '''. Check preceding log lines for details.')
         end if

         res = .true.

      end function init_boundary_forcings

      !> reads lateral blocks from new external forcings file and makes required initialisations
      function init_lateral_forcings() result(res)

         logical  :: res

         res = .false.

         loc_id = ' '
         call prop_get(node_ptr, '', 'Id', loc_id, is_successful)
         if (.not. is_successful .or. len_trim(loc_id) == 0) then
            write (msgbuf, '(a,i0,a)') 'Required field ''Id'' missing in lateral (block #', i, ').'
            call warn_flush()
            return
         end if

         ! locationType = optional for lateral
         ! fileVersion >= 2: locationType = 1d | 2d | all
         ! fileVersion <= 1: Type         = 1d | 2d | 1d2d
         item_type = ' '
         if (major >= 2) then
            call prop_get(node_ptr, '', 'locationType', item_type, is_successful)
         else
            call prop_get(node_ptr, '', 'Type', item_type, is_successful)
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
         call prop_get(node_ptr, '', 'applyTransport', apply_transport(numlatsg + 1), is_successful)

         ! [lateral]
         ! fileVersion >= 2: nodeId                  => location_specifier = LOCTP_NODEID
         !                   branchId+chainage       => location_specifier = LOCTP_BRANCH_CHAINAGE
         !                   numcoor+xcoors+ycoors   => location_specifier = LOCTP_XY_POLYGON
         ! fileVersion <= 1: LocationFile = test.pol => location_specifier = LOCTP_POLYGON_FILE
         loc_spec_type = imiss
         nodeId = ' '
         branchid = ' '
         chainage = dmiss
         numcoordinates = imiss
         !
         if (major >= 2) then
            call prop_get(node_ptr, '', 'nodeId', nodeId, is_successful)
            if (is_successful) then
               loc_spec_type = LOCTP_NODEID
               ilattype = ILATTP_1D
            else
               call prop_get(node_ptr, '', 'branchId', branchid, is_successful)
               if (is_successful) then
                  call prop_get(node_ptr, '', 'chainage', chainage, is_successful)
               end if
               if (is_successful) then
                  if (len_trim(branchid) > 0 .and. chainage /= dmiss .and. chainage >= 0.0d0) then
                     loc_spec_type = LOCTP_BRANCHID_CHAINAGE
                     ilattype = ILATTP_1D
                  end if
               else
                  call prop_get(node_ptr, '', 'numCoordinates', numcoordinates, is_successful)
                  if (is_successful .and. numcoordinates > 0) then
                     allocate (xcoordinates(numcoordinates), stat=ierr)
                     allocate (ycoordinates(numcoordinates), stat=ierr)
                     call prop_get_doubles(node_ptr, '', 'xCoordinates', xcoordinates, numcoordinates, is_successful)
                     call prop_get_doubles(node_ptr, '', 'yCoordinates', ycoordinates, numcoordinates, is_successful)
                     if (is_successful) then
                        loc_spec_type = LOCTP_POLYGON_XY
                     end if
                  end if
               end if
            end if
         else ! fileVersion <= 1
            loc_spec_type = LOCTP_POLYGON_FILE
            !
            location_file = ''
            call prop_get(node_ptr, '', 'LocationFile', location_file, is_successful)
            if (.not. is_successful .or. len_trim(location_file) == 0) then
               write (msgbuf, '(a,a,a)') 'Required field ''LocationFile'' missing in lateral ''', trim(loc_id), '''.'
               call warn_flush()
               return
            else
               call resolvePath(location_file, base_dir)
            end if
         end if
         if (loc_spec_type == imiss) then
            write (msgbuf, '(a,a,a)') 'Unrecognized location specification in lateral ''', trim(loc_id), '''.'
            call warn_flush()
            return
         end if

         call ini_alloc_laterals()

         call prepare_lateral_mask(kclat, ilattype)

         numlatsg = numlatsg + 1
         call realloc(nnlat, max(2*ndxi, nlatnd + ndxi), keepExisting=.true., fill=0)
         call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd + 1:), nlat, &
                               loc_spec_type, location_file, numcoordinates, xcoordinates, ycoordinates, branchid, chainage, nodeId)

         n1latsg(numlatsg) = nlatnd + 1
         n2latsg(numlatsg) = nlatnd + nlat

         nlatnd = nlatnd + nlat

         if (allocated(xcoordinates)) deallocate (xcoordinates, stat=ierr)
         if (allocated(ycoordinates)) deallocate (ycoordinates, stat=ierr)

         ! [lateral]
         ! Flow = 1.23 | test.tim | REALTIME
         kx = 1
         rec = ' '
         call prop_get(node_ptr, '', 'discharge', rec, is_successful)
         if (.not. is_successful .and. major <= 1) then ! Old pre-2.00 keyword 'flow'
            call prop_get(node_ptr, '', 'flow', rec, is_successful)
         end if
         if (len_trim(rec) > 0) then
            call resolvePath(rec, base_dir)
         else
            write (msgbuf, '(a,a,a)') 'Required field ''discharge'' missing in lateral ''', trim(loc_id), '''.'
            call warn_flush()
            return
         end if

         qid = 'lateral_discharge' ! New quantity name in .bc files
         is_successful = adduniformtimerelation_objects(qid, '', 'lateral', trim(loc_id), 'discharge', trim(rec), numlatsg, kx, qplat(1, :))
         if (is_successful) then
            jaqin = 1
            lat_ids(numlatsg) = loc_id
         end if

         res = .true.

      end function init_lateral_forcings

      !> reads meteo blocks from new external forcings file and makes required initialisations
      function init_meteo_forcings() result(res)
         use timespace_parameters

         logical :: res

         integer, allocatable           :: k_mask(:)
         logical                        :: invert_mask
         logical                        :: is_variable_name_available
         logical                        :: is_target_mask_available
         character(len=INI_KEY_LEN)     :: variable_name
         character(len=INI_VALUE_LEN)   :: interpolation_method
         real(kind=hp)                  :: max_search_radius

         res = .false.

         call prop_get(node_ptr, '', 'quantity ', quantity, is_successful)
         if (.not. is_successful) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
            call warn_flush()
            return
         end if

         call prop_get(node_ptr, '', 'forcingFileType ', forcing_file_type, is_successful)
         if (.not. is_successful) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, &
                ']. Field ''forcingFileType'' is missing.'
            call warn_flush()
            return
         end if

         call prop_get(node_ptr, '', 'forcingFile ', forcing_file, is_successful)
         if (.not. is_successful) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', forcing_file, ''': [', group_name, &
                ']. Field ''forcingFile'' is missing.'
            call warn_flush()
            return
         else
            call resolvePath(forcing_file, base_dir)
         end if

         target_mask_file = ''
         call prop_get(node_ptr, '', 'targetMaskFile ', target_mask_file, is_target_mask_available)

         invert_mask = .false.
         call prop_get(node_ptr, '', 'targetMaskInvert ', invert_mask, is_successful)

         call realloc(kcsini, ndx, keepExisting=.false., fill=0)
         if (len_trim(target_mask_file) > 0) then
            ! Mask flow nodes based on inside polygon(s), or outside.
            ! in: kcs, all flow nodes, out: kcsini: all masked flow nodes.
            call realloc(k_mask, ndx, keepExisting=.false., fill=0)
            call selectelset_internal_nodes(xz, yz, kcs, ndx, k_mask, nini, LOCTP_POLYGON_FILE, target_mask_file)
            do n = 1, nini
               kcsini(k_mask(n)) = 1
            end do
            if (invert_mask) then
               kcsini = ieor(kcsini, 1)
            end if

         else
            ! 100% masking: accept all flow nodes that were already active in kcs.
            where (kcs /= 0) kcsini = 1
         end if

         is_variable_name_available = .false.
         variable_name = ' '
         call prop_get(node_ptr, '', 'varname ', variable_name, is_variable_name_available)

         call prop_get(node_ptr, '', 'interpolationMethod ', interpolation_method, is_successful)
         if (is_successful) then
            method = convert_method_string_to_integer(interpolation_method)
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
            call warn_flush()
            return
         end if

         max_search_radius = -1
         call prop_get(node_ptr, '', 'extrapolationSearchRadius ', max_search_radius, is_successful)

         oper = 'O'
         call prop_get(node_ptr, '', 'operand ', oper, is_successful)

         filetype = convert_file_type_string_to_integer(forcing_file_type)

         select case (quantity)
         case ('airpressure', 'atmosphericpressure')
            ierr = allocate_patm()
            kx = 1

         case ('rainfall', 'rainfall_rate') ! case is zeer waarschijnlijk overbodig
            if (.not. allocated(rain)) then
               allocate (rain(ndx), stat=ierr, source=0d0)
               call aerr('rain(ndx)', ierr, ndx)
            end if
            kx = 1

         case ('qext')
            ! Only time-independent sample file supported for now: sets Qext initially and this remains constant in time.
            if (jaQext == 0) then
               write (msgbuf, '(a)') 'quantity '''//trim(quantity)//' in file ''', file_name, ''': [', group_name, &
                  '] is missing QExt=1 in MDU. Ignoring this block.'
               call warn_flush()
               return
            end if
            if (.not. strcmpi(forcing_file_type, 'sample')) then
               write (msgbuf, '(a)') 'Unknown forcingFileType '''//trim(forcing_file_type)//' in file ''', file_name, &
                  ''': [', group_name, '], quantity=', trim(quantity), '. Ignoring this block.'
               call warn_flush()
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

            call realloc(kcsini, ndx, keepExisting=.false., fill=0)
            call prepare_lateral_mask(kcsini, ilattype)

            res = timespaceinitialfield(xz, yz, qext, ndx, forcing_file, filetype, method, oper, transformcoef, 2, kcsini)
            return ! This was a special case, don't continue with timespace processing below.
         case default
            write (msgbuf, '(a)') 'Unknown quantity '''//trim(quantity)//' in file ''', file_name, ''': [', group_name, &
               ']. Ignoring this block.'
            call warn_flush()
            return
         end select

         select case (trim(str_tolower(forcing_file_type)))
         case ('bcascii')
            ! NOTE: Currently, we only support name=global meteo in.bc files, later maybe station time series as well.
            is_successful = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, 'global', filetype, &
                                              method, oper, forcingfile=forcing_file)
         case default
            if (is_variable_name_available) then
               is_successful = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcing_file, filetype, method, oper, &
                                                 varname=variable_name)
            else
               is_successful = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcing_file, filetype, method, oper)
            end if
         end select

         if (is_successful) then
            select case (quantity)
            case ('airpressure', 'atmosphericpressure')
               japatm = 1
            case ('rainfall', 'rainfall_rate')
               jarain = 1
               jaqin = 1
            case ('windxy')
               jawind = 1
            end select
         end if
         res = .true.

      end function init_meteo_forcings

   end function init_external_forcings

!> Allocate and initialized atmosperic pressure variable(s)
   module function allocate_patm() result(status)
      use m_wind, only: patm
      use m_cell_geometry, only: ndx
      use m_alloc, only: aerr

      integer                    :: status

      status = 0
      if (.not. allocated(patm)) then
         allocate (patm(ndx), stat=status, source=0.d0)
         call aerr('patm(ndx)', status, ndx)
      end if

   end function allocate_patm

end submodule initialisation
