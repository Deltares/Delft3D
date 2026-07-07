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

!> Reading + initializing of initial and parameter fields.
!! The IniFieldFile from the MDU is the successor of the old
!! *.ext file for quantities such as initialwaterlevel,
!! frictioncoefficient, etc.
module unstruc_inifields

   use m_setinitialverticalprofile, only: setinitialverticalprofile
   use m_add_tracer, only: add_tracer
   use m_setzcs, only: setzcs
   use messagehandling, only: msgbuf, warn_flush, err_flush
   use properties
   use string_module, only: str_lower, strcmpi
   use precision_basics, only: dp, sp
   use stdlib_kinds, only: c_bool

   use precision, only: dp
   implicit none(type, external)
   private

   public :: init1dField, spaceInit1dField, &
             set_friction_type_values, initialfield2Dto3D_dbl_indx, initialfield2Dto3D, resolve_initial_target, resolve_parameter_target, process_hydrological_quantities, &
             set_friction_type_values_explicit, finish_initialization, resolve_initial_3d_target, resolve_integer_target, &
             set_global_water_values, set_global_values, fm_quantity_name_to_source_quantity_name, finalize_1dfield_global_values, averagingTypeStringToInteger

   !> The file version number of the IniFieldFile format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.

   ! IniFieldFile current version: 2.02
   integer, parameter :: IniFieldMajorVersion = 2
   integer, parameter :: IniFieldMinorVersion = 2

   ! History IniFieldVersion:
   ! 2.02: Quantities 'waterlevel' and 'waterdepth' have been renamed to 'initialWaterLevel' and 'initialWaterDepth'.
   ! 2.01: Added field 'frictionType'
   ! 2.00: extrapolationMethod changed from integer to logical.
   ! 1.01: initial implemented version

   ! Module-level state for deferred assignment of 1dField file [Global] values.
   logical(kind=c_bool), allocatable, public :: specified_water_1dfield(:)
   logical(kind=c_bool), allocatable, public :: specified_friction_1dfield(:)
   real(dp), public :: water_global_value_1dfield = -999.0_dp
   real(dp), public :: friction_global_value_1dfield = -999.0_dp
   character(len=256), public :: water_global_quantity_1dfield = ''

contains

   !> Set all the unset values of the array with the global value.
   !> The negative_mask is false for those elements of array that have not been set yet.
   subroutine set_global_values(array, negative_mask, value)
      use stdlib_kinds, only: c_bool
      real(kind=dp), intent(inout) :: array(:) !< Array to be changed
      logical(kind=c_bool), intent(in) :: negative_mask(:) !< True when value is not to be overwritten anymore
      real(kind=dp), intent(in) :: value !< Global value

      integer :: i

      do i = 1, size(negative_mask)
         if (.not. negative_mask(i)) then
            array(i) = value
         end if
      end do
   end subroutine set_global_values

   !> Set the unset values of the water levels/water depths with the specified global value.
   !> The global_quantity specifies which of the two quantities (depth or level) is represented by the global value.
   subroutine set_global_water_values(bed_levels, water_depths, water_levels, negative_mask, &
                                      global_quantity, global_value, ini_file_name)
      use stdlib_kinds, only: c_bool
      use messagehandling
      real(kind=dp), intent(in) :: bed_levels(:) !< Bed levels
      real(kind=dp), intent(inout) :: water_depths(:) !< Water depths
      real(kind=dp), intent(inout) :: water_levels(:) !< Water levels
      logical(kind=c_bool), intent(in) :: negative_mask(:) !< True when specified already
      character(len=*), intent(in) :: global_quantity !< Quantity specified by global_value
      real(kind=dp), intent(in) :: global_value !< Global value
      character(len=*), intent(in) :: ini_file_name !< Name of ini file, used for error messages

      integer, parameter :: enum_water_level = 0
      integer, parameter :: enum_water_depth = 1
      integer :: water_specifier
      logical(kind=c_bool), allocatable :: mask(:)

      if (strcmpi(global_quantity, 'waterlevel') .or. strcmpi(global_quantity, 'initialWaterLevel')) then
         water_specifier = enum_water_level
      else if (strcmpi(global_quantity, 'waterdepth') .or. strcmpi(global_quantity, 'initialWaterDepth')) then
         water_specifier = enum_water_depth
      else
         write (msgbuf, '(a)') 'File '''//trim(ini_file_name)// &
            ''': error while setting initial field values of quantities ''initialWaterLevel'' and ''initialWaterDepth'';'// &
            ' Provided quantity name '''//trim(global_quantity)//''' is invalid.'
         call err_flush()
      end if

      select case (water_specifier)
      case (enum_water_level)
         call set_global_values(water_levels, negative_mask, global_value)
      case (enum_water_depth)
         call set_global_values(water_depths, negative_mask, global_value)
      end select

      mask = .not. negative_mask
      select case (water_specifier)
      case (enum_water_level)
         call set_water_depth_from_level(bed_levels, water_depths, water_levels, mask)
      case (enum_water_depth)
         call set_water_level_from_depth(bed_levels, water_depths, water_levels, mask)
      end select
   end subroutine set_global_water_values

   !> Apply 1dField global values for water and friction, only for those points that have not been set already. Call once after all init_new calls.
   subroutine finalize_1dfield_global_values()
      use m_flow, only: s1, hs, frcu
      use m_flowgeom, only: bl, ndx2D, ndxi, lnx1d
      use m_missing, only: dmiss

      if (allocated(specified_water_1dfield)) then
         if (len_trim(water_global_quantity_1dfield) > 0 .and. .not. all(specified_water_1dfield)) then
            call set_global_water_values(bl(ndx2D + 1:ndxi), hs(ndx2D + 1:ndxi), s1(ndx2D + 1:ndxi), &
                                         specified_water_1dfield, water_global_quantity_1dfield, &
                                         water_global_value_1dfield, '1dField global')
         end if
         deallocate (specified_water_1dfield)
         water_global_quantity_1dfield = ''
      end if

      if (allocated(specified_friction_1dfield)) then
         if (friction_global_value_1dfield /= dmiss .and. .not. all(specified_friction_1dfield)) then
            call set_global_values(frcu(1:lnx1d), specified_friction_1dfield, friction_global_value_1dfield)
         end if
         deallocate (specified_friction_1dfield)
         friction_global_value_1dfield = dmiss
      end if
   end subroutine finalize_1dfield_global_values

   !> Converts a quantity name from a D-Flow FM initial fields file to its corresponding source name in the dataFile.
   !! This source name typically depends on the data file type and may be used in subsequent calls to init1DField(),
   !! and possibly in the future also timespaceinitialfield().
   subroutine fm_quantity_name_to_source_quantity_name(quantity_name, file_type, source_quantity_name)
      use string_module, only: str_tolower
      use timespace_parameters, only: FIELD1D
      character(len=*), intent(in) :: quantity_name !< Input quantity name (as it appears in the IniFieldFile).
      integer, intent(in) :: file_type !< Data file type (one from the enum integers in timespace_parameters).
      character(len=*), intent(out) :: source_quantity_name !< Source name how the quantity is referred to in the data file. Empty string if combination is not supported.

      source_quantity_name = ''

      if (file_type == FIELD1D) then
         select case (str_tolower(trim(quantity_name)))
         case ('initialwaterlevel', 'waterlevel')
            source_quantity_name = 'waterlevel'
         case ('initialwaterdepth', 'waterdepth')
            source_quantity_name = 'waterdepth'
         case ('initialvelocity')
            source_quantity_name = 'velocity'
         case ('frictioncoefficient')
            source_quantity_name = 'frictioncoefficient'
         end select
      end if
   end subroutine fm_quantity_name_to_source_quantity_name

   !> Read the global section of the 1dField file
   subroutine init_1d_field_read_global(field_ptr, ini_field_file_name, ini_file_name, intended_quantity, value, value_provided, &
                                        num_errors)
      use tree_data_types, only: tree_data

      type(tree_data), pointer, intent(in) :: field_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      character(len=*), intent(in) :: ini_field_file_name !< file name for iniField file
      character(len=*), intent(in) :: ini_file_name !< file name for 1dField file
      character(len=*), intent(in) :: intended_quantity !< quantity that is specified in iniField file
      real(kind=dp), intent(out) :: value !< The global value to be read
      logical, intent(out) :: value_provided !< Indicates if global value was provided
      integer, intent(inout) :: num_errors !< Incremented with the number of encountered warnings/errors

      integer, parameter :: string_length = 256
      character(len=string_length) :: unit
      character(len=string_length) :: quantity
      integer :: global_section_count
      logical :: success

      global_section_count = tree_count_nodes_byname(field_ptr, 'Global')
      value_provided = .false.

      if (global_section_count == 0) then
         write (msgbuf, '(3a)') 'File ''', trim(ini_file_name), ''': [Global] block is missing.'
         call warn_flush()
         return
      else if (global_section_count > 1) then
         write (msgbuf, '(3a)') 'In file ''', trim(ini_file_name), &
            ''': Only the first [Global] block is read, other [Global] blocks are ignored.'
         call warn_flush()
      end if

      call prop_get(field_ptr, 'Global', 'quantity', quantity, success)
      if (.not. success) then
         num_errors = num_errors + 1
         write (msgbuf, '(3a)') 'Incomplete block in file ''', trim(ini_file_name), &
            ''': [Global]. Field ''quantity'' is missing.'
         call err_flush()
         return
      end if
      if (.not. strcmpi(quantity, intended_quantity) &
          .and. .not. (strcmpi(quantity, 'initialvelocity') .and. strcmpi(intended_quantity, 'velocity'))) then ! Silly exception, because in earlier D-HYDRO Suite 1D2D releases, this was already called 'initialvelocity'. Will phase out in file format 3.00 later.
         num_errors = num_errors + 1
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(ini_file_name), &
            ''': [Global]. Field ''quantity'' does not match the "quantity" which is specified in iniField file ''', &
            trim(ini_field_file_name), '''.'
         call err_flush()
         return
      end if
      if ((.not. strcmpi(quantity, 'bedlevel')) .and. (.not. strcmpi(quantity, 'waterlevel')) .and. &
          (.not. strcmpi(quantity, 'waterdepth')) .and. (.not. strcmpi(quantity, 'frictioncoefficient')) .and. &
          (.not. strcmpi(quantity, 'velocity')) .and. &
          (.not. strcmpi(quantity, 'initialvelocity')) & ! Silly exception, because in earlier D-HYDRO Suite 1D2D releases, this was already called 'initialvelocity'. Will phase out in file format 3.00 later.
          ) then
         num_errors = num_errors + 1
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(ini_file_name), ''': [Global]. Quantity ''', trim(quantity), &
            ''' is unknown.'
         call err_flush()
         return
      end if
      ! read unit
      call prop_get(field_ptr, 'Global', 'unit', unit, success)
      if (.not. success) then
         write (msgbuf, '(3a)') 'Incomplete block in file ''', trim(ini_file_name), ''': [Global]. Field ''unit'' is missing.'
         call warn_flush()
      end if

      call prop_get(field_ptr, 'Global', 'value', value, success)
      if (.not. success) then
         write (msgbuf, '(3a)') 'Incomplete block in file ''', trim(ini_file_name), ''': [Global]. Field ''value'' is missing.'
         call warn_flush()
      end if
      value_provided = success
   end subroutine init_1d_field_read_global

   !> Set the water levels for the indices where mask is true
   subroutine set_water_level_from_depth(bed_levels, water_depths, water_levels, mask)
      use stdlib_kinds, only: c_bool
      real(kind=dp), intent(in) :: bed_levels(:) !< Bed levels
      real(kind=dp), intent(in) :: water_depths(:) !< Water depths
      real(kind=dp), intent(inout) :: water_levels(:) !< Water levels
      logical(kind=c_bool), intent(in) :: mask(:) !< True when water level should be set

      integer :: i
      do i = 1, size(mask)
         if (mask(i)) then
            water_levels(i) = water_depths(i) + bed_levels(i)
         end if
      end do
   end subroutine set_water_level_from_depth

   !> Set the water depths for the indices where mask is true
   subroutine set_water_depth_from_level(bed_levels, water_depths, water_levels, mask)
      use stdlib_kinds, only: c_bool
      real(kind=dp), intent(in) :: bed_levels(:) !< Bed levels
      real(kind=dp), intent(inout) :: water_depths(:) !< Water depths
      real(kind=dp), intent(in) :: water_levels(:) !< Water levels
      logical(kind=c_bool), intent(in) :: mask(:) !< True when water level should be set

      integer :: i
      do i = 1, size(mask)
         if (mask(i)) then
            water_depths(i) = water_levels(i) - bed_levels(i)
         end if
      end do
   end subroutine set_water_depth_from_level

   !> Reads and initializes a 1d Field file (*.ini).
   function init1dField(filename, inifieldfilename, quant, specified_indices, global_value, global_value_provided) result(ierr)
      use stdlib_kinds, only: c_bool
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_alloc
      use m_flow
      use m_flowgeom
      use dfm_error
      use m_array_predicates, only: is_monotonically_increasing
      use fm_deprecated_keywords, only: deprecated_ext_keywords
      use m_deprecation, only: check_file_tree_for_deprecated_keywords

      implicit none

      character(len=*), intent(in) :: filename !< file name for 1dField file
      character(len=*), intent(in) :: inifieldfilename !< file name of iniField file (only for messages)
      character(len=*), intent(in) :: quant !< quantity that is specified in iniField file
      logical(kind=c_bool), allocatable, intent(out) :: specified_indices(:) !< Mask indicating the indices where values have been specified
      real(kind=dp), intent(out) :: global_value !< Provides global value to be applied to unset values
      logical, intent(out) :: global_value_provided !< Indicates whether a global value was provided
      integer :: ierr !< Result status (DFM_NOERR on success)

      type(tree_data), pointer :: field_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      type(tree_data), pointer :: node_ptr !
      integer :: istat !
      integer, parameter :: ini_key_len = 32 !
      integer, parameter :: ini_value_len = 256 !
      character(len=ini_key_len) :: groupname !
      character(len=ini_value_len) :: branchId !
      real(kind=dp), allocatable :: values(:) !
      integer :: numLocations !
      real(kind=dp), allocatable :: chainage(:) !
      integer :: num_items_in_file !
      logical :: retVal !
      integer :: ib, i, numerr !

      ierr = DFM_NOERR
      global_value_provided = .false.

      call tree_create(trim(filename), field_ptr)
      call prop_file('ini', trim(filename), field_ptr, istat)

      if (istat /= 0) then
         write (msgbuf, '(3a)') 'Error opening 1D field file ''', trim(filename), '''. Is the file path correct?'
         call warn_flush()
         goto 888
      end if

      num_items_in_file = 0
      if (associated(field_ptr%child_nodes)) then
         num_items_in_file = size(field_ptr%child_nodes)
      end if

      ib = 0
      numLocations = 0
      numerr = 0

      call init_1d_field_read_global(field_ptr, inifieldfilename, filename, quant, global_value, global_value_provided, numerr)

      ! TODO: future inclusion of init1dField and timespaceinitialfield into EC-module should make the location_type (UNC_LOC_S, etc.) a dummy argument.
      if (strcmpi(quant, 'waterlevel') .or. strcmpi(quant, 'waterdepth') .or. strcmpi(quant, 'bedlevel') .or. &
          strcmpi(quant, 'velocity')) then
         call realloc(specified_indices, ndxi - ndx2D, fill=.false._c_bool, keepExisting=.false.)
      else if (strcmpi(quant, 'frictioncoefficient')) then
         call realloc(specified_indices, lnx1d, fill=.false._c_bool, keepExisting=.false.)
      else
         numerr = numerr + 1
         write (msgbuf, '(5a)') 'Unsupported quantity in file ''', trim(inifieldfilename), ''': ''', trim(quant), '''.'
         call err_flush()
         goto 888
      end if

      ! loop on each block
      do i = 1, num_items_in_file

         node_ptr => field_ptr%child_nodes(i)%node_ptr
         groupname = tree_get_name(node_ptr)

         ! Step 1: read the block
         if (strcmpi(groupname, 'General') .or. strcmpi(groupname, 'Global')) then
            cycle
         else if (strcmpi(groupname, 'Branch')) then
            call prop_get(node_ptr, '', 'branchId', branchId, retVal)
            if (.not. retVal) then
               numerr = numerr + 1
               write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), &
                  ']. Field ''branchId'' is missing.'
               call warn_flush()
               cycle
            end if

            call prop_get(node_ptr, '', 'numLocations', numLocations, retVal)
            if (.not. retVal) then
               numLocations = 0
            end if

            call realloc(chainage, numLocations, keepExisting=.false.)
            if (numLocations > 0) then
               call prop_get(node_ptr, '', 'chainage', chainage, numLocations, retVal)
               if (.not. retVal) then
                  numerr = numerr + 1
                  write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), &
                     ']. Field ''chainage'' could not be read.'
                  call warn_flush()
                  cycle
               end if

               if (.not. is_monotonically_increasing(chainage)) then
                  numerr = numerr + 1
                  write (msgbuf, '(3a)') 'Invalid data in file ''', trim(filename), &
                     ''': the locations are not sorted by increasing chainage.'
                  call err_flush()
                  cycle
               end if

               call realloc(values, numLocations, keepExisting=.false.)
               call prop_get(node_ptr, '', 'values', values, numLocations, retVal)
               if (.not. retVal) then
                  numerr = numerr + 1
                  write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), &
                     ']. Field ''values'' could not be read.'
                  call err_flush()
                  cycle
               end if
            else
               call realloc(values, 1, keepExisting=.false.)
               call prop_get(node_ptr, '', 'values', values(1), retVal)
               if (.not. retVal) then
                  numerr = numerr + 1
                  write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), &
                     ']. Field ''values'' could not be read.'
                  call err_flush()
                  cycle
               end if
            end if
            ib = ib + 1
         else
            write (msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), &
               ']. Ignoring this block.'
            call warn_flush()
            cycle
         end if

         ! Step 2: operations
         if (strcmpi(quant, 'waterlevel')) then
            call spaceInit1dfield(branchId, chainage, values, 2, s1(ndx2D + 1:ndxi), specified_indices)
            call set_water_depth_from_level(bl(ndx2D + 1:ndxi), hs(ndx2D + 1:ndxi), s1(ndx2D + 1:ndxi), specified_indices)
         else if (strcmpi(quant, 'waterdepth')) then
            call spaceInit1dfield(branchId, chainage, values, 2, hs(ndx2D + 1:ndxi), specified_indices)
            call set_water_level_from_depth(bl(ndx2D + 1:ndxi), hs(ndx2D + 1:ndxi), s1(ndx2D + 1:ndxi), specified_indices)
         else if (strcmpi(quant, 'frictioncoefficient')) then
            call spaceInit1dfield(branchId, chainage, values, 1, frcu(1:lnx1d), specified_indices)
         else if (strcmpi(quant, 'velocity')) then
            call spaceInit1dfield(branchId, chainage, values, 1, u1(1:lnx1d), specified_indices)
         else if (strcmpi(quant, 'bedlevel')) then
            numerr = numerr + 1
            write (msgbuf, '(5a)') 'Unsupported block in file ''', trim(filename), ''': [', trim(groupname), &
               ']. Reading bedlevel from 1dField file type is not yet supported.'
            call err_flush()
            cycle
         end if
      end do

      if (numerr > 0) then
         goto 888
      end if

      call check_file_tree_for_deprecated_keywords(field_ptr, deprecated_ext_keywords, istat, &
                                                   prefix='While reading '''//trim(filename)//'''')
      ! No errors
      write (msgbuf, '(a, i10,a)') 'Finish initializing 1dField file '''//trim(filename)//''':', ib, &
         ' [Branch] blocks have been read and handled.'
      call msg_flush()
      return

888   continue
      ! There were errors
      ierr = DFM_WRONGINPUT
      return

   end function init1dField

   !> Converts averaging type string to an integer value.
   !! Returns -1 when an invalid type string is given.
   subroutine averagingTypeStringToInteger(sAveragingType, iAveragingType)
      use m_ec_interpolationsettings
      use string_module, only: str_tolower
      implicit none
      character(len=*), intent(in) :: sAveragingType ! averaging type string
      integer, intent(out) :: iAveragingType ! averaging type integer

      select case (trim(str_tolower(sAveragingType)))
      case ('mean')
         iAveragingType = AVGTP_MEAN
      case ('nearestnb')
         iAveragingType = AVGTP_NEARESTNB
      case ('max')
         iAveragingType = AVGTP_MAX
      case ('min')
         iAveragingType = AVGTP_MIN
      case ('invdist')
         iAveragingType = AVGTP_INVDIST
      case ('minabs')
         iAveragingType = AVGTP_MINABS
      case ('median')
         iAveragingType = AVGTP_MEDIAN
      case default
         iAveragingType = -1
      end select

      return

   end subroutine averagingTypeStringToInteger

   !> Interpolate 1D spatial initial fields, from input samples to flow state arrays.
   !! The method is:
   !! 1) When one sample value is given:
   !!    if it is from a [Global] block, then this value will be set on all branches.
   !!    if it is from a [Branch] block, then this value will be set on a this branch.
   !! 2) if more than one sample values are given, then on this branch:
   !!          *
   !!         / \
   !!        /   *----
   !!   ----*
   !! between two samples use linear interpolation,
   !! on the left side of the most left sample, use constant value of this sample,
   !! on the right side of the most right sample, use constant value of this sample.
   subroutine spaceInit1dField(sBranchId, sChainages, sValues, ipos, res, modified_elements)
      use stdlib_kinds, only: c_bool
      use m_alloc
      use m_network
      use m_inquire_flowgeom
      use unstruc_channel_flow
      use m_flowgeom, only: ndx2d
      use m_flowparameters, only: EPS10
      use precision_basics
      use m_hash_search
      use dfm_error

      implicit none
      character(len=*), intent(in) :: sBranchId !< Sample branchId
      real(kind=dp), intent(in) :: sChainages(:) !< Sample chainages
      real(kind=dp), intent(in) :: sValues(:) !< Sample values
      integer, intent(in) :: ipos !< position: 1= u point location, 2= 1d flownode(netnode) location
      real(kind=dp), intent(inout) :: res(:) !< Flow state array into which the interpolated values will be stored.
                                                !!Should be only the 1D slice (especially in the case of ipos==2, flow nodes).
      logical(kind=c_bool), intent(inout) :: modified_elements(:) !< true for every index for which res was set

      integer :: nbrstart, ibr, k, j, i, ipre, ns, ncount
      integer :: is, ip1, ip2, ipe
      type(t_branch), pointer :: pbr
      real(kind=dp) :: chai, sChaiPrev, sChai, sValPrev, sVal, minsChai, maxsChai

      if (size(sValues) == 1) then
         ! assign sValues(1) on a certain branch
         nbrstart = hashsearch(network%brs%hashlist, sBranchId)
         pbr => network%brs%branch(nbrstart)
         do is = 1, pbr%gridpointsseqcount
            ip1 = pbr%k1gridpointsseq(is)
            ip2 = pbr%k2gridpointsseq(is)
            if (ipos == 1) then
               ipe = ip2 - 1 ! upoints loop
            else if (ipos == 2) then
               ipe = ip2 ! grid points loop
            end if

            do i = ip1, ipe
               if (ipos == 1) then
                  k = pbr%lin(i)
               else if (ipos == 2) then
                  k = pbr%grd(i) - ndx2d
               end if

               res(k) = sValues(1)
               modified_elements(k) = .true.
            end do
         end do

      else
         ![Branch] block with numLocations > 1, and needs interpolations
         ns = size(sChainages)
         minsChai = sChainages(1)
         maxsChai = sChainages(ns)

         ibr = hashsearch(network%brs%hashlist, sBranchId)
         pbr => network%brs%branch(ibr)

         if (ipos == 1) then
            ncount = pbr%uPointsCount
         else if (ipos == 2) then
            ncount = pbr%gridPointsCount
         end if

         ipre = 2
         do j = 1, ncount
            if (ipos == 1) then
               chai = pbr%uPointsChainages(j)
               k = pbr%lin(j)
            else if (ipos == 2) then
               chai = pbr%gridPointsChainages(j)
               k = pbr%grd(j) - ndx2d
            end if
            ! Constant value before the first data segment and after the last data segment.
            if (comparereal(chai, minsChai, EPS10) <= 0) then
               res(k) = sValues(1)
               modified_elements(k) = .true.
               cycle
            else if (comparereal(chai, maxsChai, EPS10) >= 0) then
               res(k) = sValues(ns)
               modified_elements(k) = .true.
               cycle
            end if

            ! Linear interpolation, find the data segment in which the current position k lies.
            do i = ipre, ns
               sChaiPrev = sChainages(i - 1)
               sChai = sChainages(i)
               sValPrev = sValues(i - 1)
               sVal = sValues(i)

               if (comparereal(chai, sChaiPrev, EPS10) >= 0 .and. comparereal(chai, sChai, EPS10) < 0) then
                  if (comparereal(sChai, sChaiPrev, EPS10) /= 0) then
                     res(k) = sValPrev + (sVal - sValPrev) / (sChai - sChaiPrev) * (chai - sChaiPrev)
                     modified_elements(k) = .true.
                  else
                     res(k) = (sVal + sValPrev) / 2
                     modified_elements(k) = .true.
                  end if
                  ipre = i
                  exit
               end if
            end do
         end do
      end if
   end subroutine spaceInit1dField

   !> set  friction type (ifrcutp) values
   subroutine set_friction_type_values()

      use precision_basics, only: dp
      use fm_external_forcings_data, only: operand, transformcoef
      use m_flow, only: ifrctypuni, ifrcutp, frcu
      use m_flowgeom, only: lnx
      use m_missing, only: dmiss
      use timespace_parameters, only: OPERAND_OVERRIDE

      implicit none

      integer :: link

      if (transformcoef(3) /= -999.0_dp .and. int(transformcoef(3)) /= ifrctypuni .and. operand == OPERAND_OVERRIDE) then
         do link = 1, lnx
            if (frcu(link) /= dmiss) then
               ! type array only must be used if different from uni
               ifrcutp(link) = int(transformcoef(3))
            end if
         end do
      end if

   end subroutine set_friction_type_values

!> Set friction type (ifrcutp) values from explicit arguments.
   !! Used by the new init_spatial_fields path.
   function set_friction_type_values_explicit(block_ptr, operand) result(res)
      use m_flow, only: ifrctypuni, ifrcutp, frcu
      use m_flowgeom, only: lnx
      use m_missing, only: dmiss
      use timespace_parameters, only: OPERAND_OVERRIDE
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use m_roughness, only: frictionTypeStringToInteger
      use m_physcoef, only: ifrctypuni
      use timespace_parameters, only: OPERAND_OVERRIDE
      implicit none

      type(tree_data), pointer, intent(in) :: block_ptr
      integer, intent(in) :: operand !< Operand for the friction type assignment.
      logical :: res
      integer :: link

      character(len=256) :: friction_type_str
      integer :: friction_type_int

      friction_type_str = ''
      call prop_get(block_ptr, '', 'frictionType', friction_type_str, res)
      call frictionTypeStringToInteger(friction_type_str, friction_type_int)
      if (res .and. friction_type_int /= ifrctypuni .and. operand == OPERAND_OVERRIDE) then
         do link = 1, lnx
            if (frcu(link) /= dmiss) then
               ifrcutp(link) = friction_type_int
            end if
         end do
      end if

   end function set_friction_type_values_explicit

   !> Subroutine to initialize the subsupl array based on the ibedlevtyp value.
   subroutine initialize_subsupl()
      use m_subsidence, only: sdu_blp, subsupl_t0, subsupl, subsout, subsupl_tp
      use m_flowparameters, only: ibedlevtyp
      use m_meteo, only: ec_addtimespacerelation
      ! use m_flow, only:
      use network_data, only: numk
      use m_flowgeom, only: lnx, ndx
      use m_alloc, only: aerr

      implicit none

      integer, allocatable :: mask(:)
      integer :: kx, ierr
      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6

      kx = 1
      if (allocated(subsupl)) then
         deallocate (subsupl)
      end if
      if (allocated(subsupl_t0)) then
         deallocate (subsupl_t0)
      end if
      if (allocated(subsupl_tp)) then
         deallocate (subsupl_tp)
      end if
      if (allocated(subsout)) then
         deallocate (subsout)
      end if
      if (allocated(sdu_blp)) then
         deallocate (sdu_blp)
      end if

      select case (ibedlevtyp)
      case (enum_field1D) ! Cell centers
         allocate (subsupl(ndx), stat=ierr)
         call aerr('subsupl(ndx)', ierr, ndx)
         subsupl = 0.0_dp
         allocate (subsupl_t0(ndx), stat=ierr)
         call aerr('subsupl_t0(ndx)', ierr, ndx)
         subsupl_t0 = 0.0_dp
         allocate (subsupl_tp(ndx), stat=ierr)
         call aerr('subsupl_tp(ndx)', ierr, ndx)
         subsupl_tp = 0.0_dp
         allocate (subsout(ndx), stat=ierr)
         call aerr('subsout(ndx)', ierr, ndx)
         subsout = 0.0_dp

      case (enum_field2D) ! u-points
         if (allocated(mask)) then
            deallocate (mask)
         end if
         allocate (mask(lnx), source=1, stat=ierr)
         call aerr('mask(lnx)', ierr, lnx)
         allocate (subsupl(lnx), stat=ierr)
         call aerr('subsupl(lnx)', ierr, lnx)
         subsupl = 0.0_dp
         allocate (subsupl_t0(lnx), stat=ierr)
         call aerr('subsupl_t0(lnx)', ierr, lnx)
         subsupl_t0 = 0.0_dp
         allocate (subsupl_tp(lnx), stat=ierr)
         call aerr('subsupl_tp(lnx)', ierr, lnx)
         subsupl_tp = 0.0_dp
         allocate (subsout(lnx), stat=ierr)
         call aerr('subsout(lnx)', ierr, lnx)
         subsout = 0.0_dp

      case (enum_field3D, enum_field4D, enum_field5D, enum_field6D) ! Cell corners / net nodes
         if (allocated(mask)) then
            deallocate (mask)
         end if
         allocate (mask(numk), source=1, stat=ierr)
         call aerr('mask(numk)', ierr, numk)
         allocate (subsupl(numk), stat=ierr)
         call aerr('subsupl(numk)', ierr, numk)
         subsupl = 0.0_dp
         allocate (subsupl_t0(numk), stat=ierr)
         call aerr('subsupl_t0(numk)', ierr, numk)
         subsupl_t0 = 0.0_dp
         allocate (subsupl_tp(numk), stat=ierr)
         call aerr('subsupl_tp(numk)', ierr, numk)
         subsupl_tp = 0.0_dp
         allocate (subsout(numk), stat=ierr)
         call aerr('subsout(numk)', ierr, numk)
         subsout = 0.0_dp
      end select

      allocate (sdu_blp(ndx), stat=ierr)
      call aerr('sdu_blp(ndx)', ierr, ndx)
      sdu_blp = 0.0_dp

   end subroutine initialize_subsupl

   !> Resolve the target array and location type for quantities that are of integer type.
   !! Returns .true. if the quantity was recognized and target_array is associated.
   function resolve_integer_target(qid, target_location_type, target_array) result(success)
      use fm_location_types, only: UNC_LOC_U
      use m_flowgeom, only: iadv, ibot
      use string_module, only: str_tolower

      character(len=*), intent(in) :: qid
      integer, intent(out) :: target_location_type
      integer, dimension(:), pointer, intent(out) :: target_array
      logical :: success

      target_array => null()
      target_location_type = 0
      success = .true.

      select case (str_tolower(qid))
      case ('advectiontype')
         target_location_type = UNC_LOC_U
         target_array => iadv
      case ('ibedlevtype')
         target_location_type = UNC_LOC_U
         target_array => ibot
      case default
         success = .false.
      end select
   end function resolve_integer_target

!> Resolve the target array and location type for quantities that need to be stored in a 3D array.
!! Returns .true. if the quantity was recognized and target_array is associated.
   function resolve_initial_3d_target(quantity, target_location_type, target_array_3d, first_index) result(success)
      use string_module, only: str_tolower
      use messagehandling, only: mess, LEVEL_WARN
      use m_flow, only: sa1
      use m_flowparameters, only: jasal
      use m_transport, only: const_names, ISED1
      use m_transportdata, only: itrac2const, constituents
      use m_sediment, only: stm_included, sed, jased, sedh
      use m_fm_wq_processes, only: wqbotnames, wqbot
      use m_flowgeom, only: ndx
      use m_missing, only: dmiss
      use m_alloc, only: realloc
      use fm_external_forcings_data, only: trnames, NAMTRACLEN
      use fm_external_forcings_utils, only: split_qid, get_tracername
      use m_find_name, only: find_name
      use m_add_bndtracer, only: add_bndtracer
      use m_add_tracer, only: add_tracer
      use fm_location_types, only: UNC_LOC_S
      use processes_input, only: paname, painp, num_spatial_parameters

      character(len=*), intent(in) :: quantity !< Name of the quantity
      integer, intent(out) :: target_location_type !< Location type (UNC_LOC_S, UNC_LOC_U or UNC_LOC_3DV).
      real(kind=dp), dimension(:, :), pointer, intent(out) :: target_array_3d !< Output to the target 3D array.
      integer, intent(out) :: first_index !< First index in the target array, for quantities that have multiple instances (e.g. sediment fractions, tracers, etc.).
      logical :: success !< true if the quantity was recognized and target_array_3d is associated.

      character(len=256) :: qid_base, qid_specific
      character(len=NAMTRACLEN) :: tracnam, qidnam
      character(len=20) :: tracunit
      integer :: iconst, itrac, isednum, iwqbot, janew, iostat

      target_array_3d => null()
      first_index = 1
      target_location_type = UNC_LOC_S
      success = .true.

      call split_qid(quantity, qid_base, qid_specific)

      select case (str_tolower(qid_base))
      case ('initialsalinity')
         if (jasal <= 0) then
            success = .false.
            return
         end if
         target_array_3d(1:1, 1:size(sa1)) => sa1
         first_index = 1

      case ('initialsedfrac')
         if (.not. stm_included) then
            success = .false.
            return
         end if
         iconst = find_name(const_names, qid_specific)
         if (iconst <= 0) then
            call mess(LEVEL_WARN, 'resolve_initial_3d_target: unknown sediment fraction '''//trim(qid_specific)//'''.')
            success = .false.
            return
         end if
         first_index = iconst - ISED1 + 1
         target_array_3d => sed

      case ('initialsediment')
         if (jased <= 0) then
            success = .false.
            return
         end if
         call realloc(sedh, ndx, keepExisting=.false., fill=dmiss)
         read (qid_specific(1:1), '(i1)', iostat=iostat) isednum
         if (iostat /= 0) isednum = 1
         first_index = isednum
         target_array_3d => sed

      case ('initialtracer')
         call get_tracername(quantity, tracnam, qidnam)
         tracunit = " "
         call add_bndtracer(tracnam, tracunit, itrac, janew)
         call add_tracer(qid_specific, iconst)
         itrac = find_name(trnames, qid_specific)
         if (itrac == 0) then
            call mess(LEVEL_WARN, 'resolve_initial_3d_target: tracer '''//trim(qid_specific)//''' not found.')
            success = .false.
            return
         end if
         first_index = itrac2const(itrac)
         target_array_3d => constituents

      case ('initialwaqbot')
         iwqbot = find_name(wqbotnames, qid_specific)
         if (iwqbot == 0) then
            call mess(LEVEL_WARN, 'resolve_initial_3d_target: WAQ bottom variable '''//trim(qid_specific)//''' not found.')
            success = .false.
            return
         end if
         first_index = iwqbot
         target_array_3d => wqbot

      case ('waqparameter', 'waqsegmentnumber')
         target_location_type = UNC_LOC_S
         call find_or_add_waq_input(qid_specific, paname, num_spatial_parameters, .true., &
                                    waq_values=painp, index_waq_input=first_index)
         allocate (target_array_3d(first_index:first_index, size(painp, 2)))

      case default
         success = .false.
      end select
   end function resolve_initial_3d_target

   !> Resolve the target array and location type for an [Initial] quantity.
   !! Handles all quantities that map to a plain real(dp) 1D array.
   function resolve_initial_target(qid, inifilename, target_location_type, target_array) result(success)
      use messageHandling
      use m_alloc, only: realloc
      use m_missing, only: dmiss
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_3DV
      use fm_external_forcings_data, only: uxini, uyini, inivelx, inively
      use m_flow, only: s1, hs, sa1, satop, sabot, tem1, h_unsat, kmx
      use m_flowgeom, only: ndx, lnx
      use m_flowparameters, only: jasal, inisal2D, uniformsalinityabovez, uniformsalinitybelowz, &
                                  temperature_model, TEMPERATURE_MODEL_NONE, initem2D, inivel
      use unstruc_model, only: md_extfile
      use string_module, only: str_tolower

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the ini file, used for warning messages.
      integer, intent(out) :: target_location_type !< Location type (UNC_LOC_S, UNC_LOC_U or UNC_LOC_3DV).
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< Pointer to the model array. Null if not handled here.
      logical :: success !< true if the quantity was recognized.

      target_array => null()
      target_location_type = 0
      success = .true.
      select case (str_tolower(qid))
      case ('waterlevel', 'initialwaterlevel')
         if (str_tolower(qid) == 'waterlevel') then
            call mess(LEVEL_WARN, 'Initial field quantity '''//trim(qid)//''' found in file '''//trim(inifilename) &
                      //''' is deprecated, use ''initialWaterLevel'' instead. Please update your input file.')
         end if
         target_location_type = UNC_LOC_S
         target_array => s1

      case ('waterdepth', 'initialwaterdepth')
         if (str_tolower(qid) == 'waterdepth') then
            call mess(LEVEL_WARN, 'Initial field quantity '''//trim(qid)//''' found in file '''//trim(inifilename) &
                      //''' is deprecated, use ''initialWaterDepth'' instead. Please update your input file.')
         end if
         target_location_type = UNC_LOC_S
         target_array => hs

      case ('initialunsaturedzonethickness')
         call realloc(h_unsat, ndx, keepExisting=.true., fill=dmiss)
         target_location_type = UNC_LOC_S
         target_array => h_unsat

      case ('initialsalinitytop')
         if (jasal > 0) then
            call realloc(satop, ndx, keepExisting=.true., fill=dmiss)
            if (inisal2D /= 0 .and. inisal2D /= 2) then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)// &
                         ''', initialSalinityTop and initialSalinityBot found. Only one of them can be used.')
            end if
            inisal2D = 2
            uniformsalinityabovez = dmiss
            target_location_type = UNC_LOC_S
            target_array => satop
         end if

      case ('initialsalinitybot')
         if (jasal > 0) then
            call realloc(sabot, ndx, keepExisting=.true., fill=dmiss)
            if (inisal2D /= 0 .and. inisal2D /= 3) then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)// &
                         ''', initialSalinityTop and initialSalinityBot found. Only one of them can be used.')
            end if
            inisal2D = 3
            uniformsalinitybelowz = dmiss
            target_location_type = UNC_LOC_S
            target_array => sabot
         end if

      case ('initialtemperature')
         if (temperature_model /= TEMPERATURE_MODEL_NONE) then
            target_location_type = UNC_LOC_S
            target_array => tem1
            initem2D = 1
         end if

      case ('initialvelocityx')
         call realloc(uxini, lnx, fill=dmiss)
         target_location_type = UNC_LOC_U
         target_array => uxini
         inivelx = 1
         if (inively == 1) inivel = 1

      case ('initialvelocityy')
         call realloc(uyini, lnx, fill=dmiss)
         target_location_type = UNC_LOC_U
         target_array => uyini
         inively = 1
         if (inivelx == 1) inivel = 1

      case ('initialverticaltemperatureprofile')
         if (temperature_model /= TEMPERATURE_MODEL_NONE .and. kmx > 0) then
            target_location_type = UNC_LOC_3DV
            target_array => tem1
         end if

      case ('initialverticalsalinityprofile')
         if (jasal > 0 .and. kmx > 0) then
            target_location_type = UNC_LOC_3DV
            target_array => sa1
         end if

      case default
         success = .false.
      end select

   end function resolve_initial_target

   !> Resolve the target array and location type for a [Parameter] quantity.
   !! Handles all quantities that map to a plain real(dp) 1D array.
   function resolve_parameter_target(qid, inifilename, target_location_type, target_array, kx) result(success)
      use messageHandling
      use m_alloc, only: realloc, aerr
      use m_missing, only: dmiss
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN, UNC_LOC_S3D, UNC_LOC_GLOBAL
      use m_flow, only: frcu, cftrtfac, viusp, diusp, frcInternalTides2D, DissInternalTidesPerArea, frculin, Cdwusp, jacftrtfac
      use m_flowgeom, only: ndx, lnx, grounlay, jagrounlay
      use m_flowparameters, only: jatrt, javiusp, jadiusp, jafrculin, jaCdwusp, jafrcInternalTides2D, ibedlevtyp, jawave, waveforcing
      use m_heatfluxes, only: spatial_secchi_depth
      use m_wind, only: wind_drag_type, CD_TYPE_CONST
      use m_vegetation, only: stemdiam, stemdens, stemheight
      use m_nudge, only: nudge_time, nudge_rate
      use m_physcoef, only: constant_dicoww, dicoww
      use m_array_or_scalar, only: assign_pointer_to_t_array, realloc
      use unstruc_model, only: md_ptr
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
      use m_waveconst, only: WAVE_NC_OFFLINE, WAVEFORCING_DISSIPATION_3D, WAVEFORCING_RADIATION_STRESS, WAVEFORCING_DISSIPATION_TOTAL
      use processes_input, only: sfunname, sfuninp, num_spatial_time_fuctions
      use fm_external_forcings_utils, only: split_qid
      use string_module, only: str_tolower
      use processes_input, only: funame, funinp, num_time_functions

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity (may include a specific suffix, e.g. waqsegmentfunction:myFunc).
      character(len=*), intent(in) :: inifilename !< Name of the ini file, used for warning messages.
      integer, intent(out) :: target_location_type !< Location type (UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN or UNC_LOC_S3D).
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< Pointer to the model array. Null for EC-driven quantities.
      integer, intent(out) :: kx !< Number of values per location; set to 2 for nudgesalinitytemperature, 1 otherwise.
      logical :: success
      integer :: ierr
      character(len=idlen) :: qid_base, qid_specific
      integer :: index_waq_input

      call split_qid(qid, qid_base, qid_specific)

      target_array => null()
      target_location_type = 0
      kx = 1
      success = .true.

      select case (str_tolower(qid_base))
      case ('frictioncoefficient')
         target_location_type = UNC_LOC_U
         target_array => frcu

      case ('groundlayerthickness')
         target_location_type = UNC_LOC_U
         target_array => grounlay
         jagrounlay = 1

      case ('frictiontrtfactor')
         if (jatrt /= 1) then
            call mess(LEVEL_WARN, 'Reading '''//trim(inifilename)//''', quantity '//trim(qid)// &
                      ' requires [trachytopes] to be switched on in MDU. Ignoring this block.')
            success = .false.
            return
         end if
         if (.not. allocated(cftrtfac)) then
            allocate (cftrtfac(lnx), stat=ierr)
            call aerr('cftrtfac(lnx)', ierr, lnx)
            cftrtfac = 1.0_dp
         end if
         target_location_type = UNC_LOC_U
         target_array => cftrtfac
         jacftrtfac = 1

      case ('horizontaleddyviscositycoefficient')
         if (javiusp == 0) then
            if (allocated(viusp)) deallocate (viusp)
            allocate (viusp(lnx), stat=ierr)
            call aerr('viusp(lnx)', ierr, lnx)
            viusp = dmiss
            javiusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => viusp

      case ('horizontaleddydiffusivitycoefficient')
         if (jadiusp == 0) then
            if (allocated(diusp)) deallocate (diusp)
            allocate (diusp(lnx), stat=ierr)
            call aerr('diusp(lnx)', ierr, lnx)
            diusp = dmiss
            jadiusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => diusp

      case ('internaltidesfrictioncoefficient')
         if (jaFrcInternalTides2D /= 1) then
            if (allocated(frcInternalTides2D)) deallocate (frcInternalTides2D)
            allocate (frcInternalTides2D(Ndx), stat=ierr)
            call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
            frcInternalTides2D = dmiss
            if (allocated(DissInternalTidesPerArea)) deallocate (DissInternalTidesPerArea)
            allocate (DissInternalTidesPerArea(Ndx), stat=ierr)
            call aerr('DissInternalTidesPerArea(Ndx)', ierr, Ndx)
            DissInternalTidesPerArea = 0.0_dp
            jaFrcInternalTides2D = 1
         end if
         target_location_type = UNC_LOC_S
         target_array => frcInternalTides2D

      case ('linearfrictioncoefficient')
         target_location_type = UNC_LOC_U
         target_array => frculin
         jafrculin = 1

      case ('secchidepth')
         call realloc(spatial_secchi_depth, ndx, keepExisting=.true., fill=dmiss, stat=ierr)
         target_location_type = UNC_LOC_S
         target_array => spatial_secchi_depth

      case ('backgroundverticaleddydiffusivitycoefficient')
         target_location_type = UNC_LOC_S
         call realloc(dicoww, ndx, keepExisting=.true., fill=constant_dicoww, stat=ierr)
         call assign_pointer_to_t_array(dicoww, target_array, ierr)

      case ('stemdiameter')
         if (.not. allocated(stemdiam)) then
            allocate (stemdiam(ndx), stat=ierr)
            call aerr('stemdiam(ndx)', ierr, ndx)
            stemdiam = dmiss
         end if
         target_location_type = UNC_LOC_S
         target_array => stemdiam

      case ('stemdensity')
         if (.not. allocated(stemdens)) then
            allocate (stemdens(ndx), stat=ierr)
            call aerr('stemdens(ndx)', ierr, ndx)
            stemdens = dmiss
         end if
         target_location_type = UNC_LOC_S
         target_array => stemdens

      case ('stemheight')
         if (.not. allocated(stemheight)) then
            allocate (stemheight(ndx), stat=ierr)
            call aerr('stemheight(ndx)', ierr, ndx)
            stemheight = dmiss
         end if
         target_location_type = UNC_LOC_S
         target_array => stemheight

      case ('windstresscoefficient')
         if (jaCdwusp == 0) then
            if (allocated(Cdwusp)) deallocate (Cdwusp)
            allocate (Cdwusp(lnx), stat=ierr)
            call aerr('Cdwusp(lnx)', ierr, lnx)
            Cdwusp = dmiss
            jaCdwusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => Cdwusp
         wind_drag_type = CD_TYPE_CONST

      case ('nudgerate')
         call alloc_nudging()
         target_location_type = UNC_LOC_S
         target_array => nudge_rate

      case ('nudgetime')
         call alloc_nudging()
         target_location_type = UNC_LOC_S
         target_array => nudge_time

         ! --- Time-dependent EC-only quantities (target_array remains null; EC writes directly) ---

      case ('sea_ice_area_fraction', 'sea_ice_thickness')
         if (ja_ice_area_fraction_read == 0 .and. ja_ice_thickness_read == 0) then
            call fm_ice_activate_by_ext_forces(ndx, md_ptr)
         end if
         target_location_type = UNC_LOC_S

      case ('wavesignificantheight', 'waveperiod', 'wavedirection')
         if (jawave /= WAVE_NC_OFFLINE) then
            write (msgbuf, '(a,i0,a)') 'Reading '''//trim(inifilename)//''', quantity "'//trim(qid)// &
               '" requires WaveModelNr=', WAVE_NC_OFFLINE, '.'
            call warn_flush()
            success = .false.
            return
         end if
         target_location_type = UNC_LOC_S

      case ('wavebreakerdissipation', 'whitecappingdissipation')
         if (.not. (jawave == WAVE_NC_OFFLINE .and. waveforcing == WAVEFORCING_DISSIPATION_3D)) then
            write (msgbuf, '(a,i0,a,i0,a)') 'Reading '''//trim(inifilename)//''', quantity "'//trim(qid)// &
               '" requires WaveModelNr=', WAVE_NC_OFFLINE, ' and WaveForcing=', WAVEFORCING_DISSIPATION_3D, '.'
            call warn_flush()
            success = .false.
            return
         end if
         target_location_type = UNC_LOC_S

      case ('xwaveforce', 'ywaveforce')
         if (.not. (jawave == WAVE_NC_OFFLINE .and. &
                    (waveforcing == WAVEFORCING_RADIATION_STRESS .or. waveforcing == WAVEFORCING_DISSIPATION_3D))) then
            write (msgbuf, '(a,i0,a,i0,a,i0,a)') 'Reading '''//trim(inifilename)//''', quantity "'//trim(qid)// &
               '" requires WaveModelNr=', WAVE_NC_OFFLINE, ' and WaveForcing=', WAVEFORCING_RADIATION_STRESS, &
               ' or ', WAVEFORCING_DISSIPATION_3D, '.'
            call warn_flush()
            success = .false.
            return
         end if
         target_location_type = UNC_LOC_S

      case ('totalwaveenergydissipation')
         if (.not. (jawave == WAVE_NC_OFFLINE .and. waveforcing == WAVEFORCING_DISSIPATION_TOTAL)) then
            write (msgbuf, '(a,i0,a,i0,a)') 'Reading '''//trim(inifilename)//''', quantity "'//trim(qid)// &
               '" requires WaveModelNr=', WAVE_NC_OFFLINE, ' and WaveForcing=', WAVEFORCING_DISSIPATION_TOTAL, '.'
            call warn_flush()
            success = .false.
            return
         end if
         target_location_type = UNC_LOC_S

      case ('bedrock_surface_elevation')
         call initialize_subsupl()
         select case (ibedlevtyp)
         case (1) ! cell centres
            target_location_type = UNC_LOC_S
         case (2) ! u-points
            target_location_type = UNC_LOC_U
         case (3, 4, 5, 6) ! cell corners / net nodes
            target_location_type = UNC_LOC_CN
         end select

      case ('waqsegmentfunction')
         target_location_type = UNC_LOC_S
         call find_or_add_waq_input(qid_specific, sfunname, num_spatial_time_fuctions, .true., &
                                    waq_values_ptr=sfuninp, index_waq_input=index_waq_input)
      case ('waqfunction')
         target_location_type = UNC_LOC_GLOBAL
         call find_or_add_waq_input(qid_specific, funame, num_time_functions, .false., &
                                    waq_values_ptr=funinp, index_waq_input=index_waq_input)
      case ('nudgesalinitytemperature')
         target_location_type = UNC_LOC_S3D
         kx = 2
         call alloc_nudging()

      case default
         success = .false.
      end select

   end function resolve_parameter_target

   !> Allocate nudging arrays.
   subroutine alloc_nudging()
      use m_alloc, only: realloc
      use m_cell_geometry, only: ndx
      use m_flow, only: ndkx
      use m_missing, only: dmiss
      use m_nudge, only: nudge_salinity, nudge_temperature, nudge_time, nudge_rate

      call realloc(nudge_temperature, ndkx, fill=dmiss)
      call realloc(nudge_salinity, ndkx, fill=dmiss)
      call realloc(nudge_time, ndx, fill=dmiss)
      call realloc(nudge_rate, ndx, fill=dmiss)
   end subroutine alloc_nudging

   !> Search a particular water quality input name in a list of names,
   !! and if not found, add it to the list, also increasing the associated value array.
   subroutine find_or_add_waq_input(waq_input_name, waq_names, waq_input_count, is_spatial, waq_values, waq_values_ptr, index_waq_input)
      use m_find_name, only: find_name
      use m_waq_precision, only: real_wp
      use m_flow, only: ndkx
      use m_alloc, only: realloc, reallocP

      character(len=*), intent(in) :: waq_input_name !< Name of the water quality input that is searched for.
      character(len=*), allocatable, dimension(:), intent(inout) :: waq_names !< (input index) List of water quality input names to be searched in.
      integer, intent(inout) :: waq_input_count !< Current count of the water quality inputs. Will be incremented if a new input name is added.
      logical, intent(in) :: is_spatial !< Whether or not this input is a spatial parameter (as opposed to a temporal function). Determines the length of the second dimension in the waq_values array (space-independent has length 1 there).
      real(kind=real_wp), allocatable, dimension(:, :), optional, intent(inout) :: waq_values !< (input index, location index) Allocatable array of water quality input values, will be increased if a new input name is added. Use either this one or the _pointer argument.
      real(kind=dp), pointer, dimension(:, :), optional, intent(inout) :: waq_values_ptr !< (input index, location index) Pointer array List of water quality input values, will be increased if a new input name is added. Use either this one or the previous non-_pointer argument.
      integer, intent(out) :: index_waq_input !< Index of the found or added water quality input (in the search set, as well as parameter set).

      integer :: waq_location_count

      index_waq_input = find_name(waq_names, waq_input_name)

      if (index_waq_input == 0) then
         waq_input_count = waq_input_count + 1
         index_waq_input = waq_input_count

         if (is_spatial) then
            waq_location_count = Ndkx
         else
            waq_location_count = 1 ! Temporal functions are not spatial, so only one value per function.
         end if
         call realloc(waq_names, waq_input_count, keepExisting=.true., fill=waq_input_name)
         if (present(waq_values)) then
            call realloc(waq_values, [waq_input_count, waq_location_count], keepExisting=.true., fill=0.0_real_wp)
         end if
         if (present(waq_values_ptr)) then
            call reallocP(waq_values_ptr, [waq_input_count, waq_location_count], keepExisting=.true., fill=0.0_dp)
         end if
      end if
   end subroutine find_or_add_waq_input

   !> Helper routine to process several hydrological quantities that could either be in a [Parameter]
   !! or [Initial] block (this latter for backwards compatibility).
   !! This is a temporary solution until the frontend supports [Parameter].
   !!
   !! TODO: Probably this code fragment can be moved back to process_parameter_block() again once FM1D2D-2932
   !! is done.
   function process_hydrological_quantities(qid, inifilename, target_location_type, target_array) result(success)
      use messageHandling
      use m_alloc, only: realloc, aerr
      use fm_location_types, only: UNC_LOC_S
      use m_flow, only: h_unsat
      use m_flowgeom, only: ndx
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, &
                                  horton_infiltration_config, &
                                  InterceptThickness, interceptionmodel, DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, InterceptHs, &
                                  infiltcap, infiltrationmodel
      use string_module, only: str_tolower

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the ini file.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U.
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (real(kind=dp)).
      logical :: success

      success = .true.
      select case (str_tolower(qid))
      case ('hortonmininfcap')
         target_location_type = UNC_LOC_S
         target_array => horton_infiltration_config%min_inf_cap
      case ('hortonmaxinfcap')
         target_location_type = UNC_LOC_S
         target_array => horton_infiltration_config%max_inf_cap
      case ('hortondecreaserate')
         target_location_type = UNC_LOC_S
         target_array => horton_infiltration_config%decrease_rate
      case ('hortonrecoveryrate')
         target_location_type = UNC_LOC_S
         target_array => horton_infiltration_config%recovery_rate
      case ('interceptionlayerthickness')
         target_location_type = UNC_LOC_S
         call realloc(InterceptHs, ndx, keepExisting=.true., fill=0.0_dp)
         call realloc(h_unsat, ndx, keepExisting=.true., fill=0.0_dp)
         call realloc(InterceptThickness, ndx, keepExisting=.false.)
         target_array => InterceptThickness
         interceptionmodel = DFM_HYD_INTERCEPT_LAYER
         jadhyd = 1
      case ('infiltrationcapacity')
         if (infiltrationmodel /= DFM_HYD_INFILT_CONST) then
            write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(qid) &
               //'''. This requires ''InfiltrationModel=', DFM_HYD_INFILT_CONST, ''' in the MDU file (constant).'
            call warn_flush()
            success = .false.
            return
         end if
         target_location_type = UNC_LOC_S
         target_array => infiltcap
      case ('potentialevaporation')
         target_location_type = UNC_LOC_S
         call realloc(potEvap, ndx, keepExisting=.true., fill=0.0_dp)
         target_array => PotEvap
      case default
         success = .false.
         return
      end select

   end function process_hydrological_quantities

   !> Perform finalization after reading the input file.
   subroutine finish_initialization(qid)
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
      use m_vegetation, only: stemheight, stemheightstd
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U
      use m_subsidence, only: jasubsupl
      use string_module, only: str_tolower
      use m_find_name, only: find_name

      use fm_external_forcings_utils, only: split_qid
      implicit none

      character(len=*), intent(in) :: qid !< Quantity identifier.

      integer :: idum
      integer :: n
      real(kind=dp), external :: ran0
      character(len=idlen) :: qid_base, qid_specific

      call split_qid(qid, qid_base, qid_specific)

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
      case ('frictioncoefficient')
         call set_friction_type_values()
      case ('initialunsaturedzonethickness', 'interceptionlayerthickness')
         where (h_unsat == -999.0_dp)
            h_unsat = 0.0_dp
         end where
         if (qid == 'interceptionlayerthickness') then
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
      case ('stemheight')
         if (stemheightstd > 0.0_dp) then
            stemheight = stemheight * (1.0_dp + stemheightstd * (ran0(idum) - 0.5_dp))
         end if
      case ('nudgesalinitytemperature')
         janudge = 1
      end select

   end subroutine finish_initialization

   !ONLY USED IN INIT_OLD, REMOVE ONCE INIT_OLD IS GONE
   subroutine initialfield2Dto3D(input_array_2d, output_array_3d, vertical_range_min, vertical_range_max, operand)
      use m_missing

      implicit none

      real(kind=dp), dimension(:), intent(inout), target :: input_array_2d !< The input array on 2d grid cells (1:ndx).
      real(kind=dp), dimension(:), intent(inout), target :: output_array_3d !< The output array on 3d grid cells (1:ndkx).
      real(kind=dp), intent(in) :: vertical_range_min !< Lower limit for the optional vertical range. Use dmiss for no custom range.
      real(kind=dp), intent(in) :: vertical_range_max !< Upper limit for the optional vertical range. Use dmiss for no custom range.
      integer, intent(in) :: operand !< The operand to be used for combining the input field values with any previously set values.

      real(kind=dp), dimension(:, :), pointer :: output_array_3d_tmp

      output_array_3d_tmp(1:1, 1:size(output_array_3d)) => output_array_3d

      call initialfield2Dto3D_dbl_indx(input_array_2d, output_array_3d_tmp, 1, vertical_range_min, vertical_range_max, operand)

   end subroutine initialfield2Dto3D

   !> The values from the input array on 2D grid cells are copied to the 3D locations in the output array.
   !! Optionally, a vertical range can be specified, which then only updates the 3D output array elements if their vertical
   !! position lies within that range. Without this range, all 3D cells in a single  vertical column get the same 2D input value.
   subroutine initialfield2Dto3D_dbl_indx(input_array_2d, output_array_3d, first_index, vertical_range_min, vertical_range_max, operand)
      use precision_basics
      use m_flow, only: kmx, kbot, ktop, zws
      use m_missing
      use timespace, only: operate

      implicit none

      real(kind=dp), dimension(:), intent(inout), target :: input_array_2d !< The input array on 2d grid cells (1:ndx).
      real(kind=dp), dimension(:, :), intent(inout) :: output_array_3d !< The output array on 3d grid cells.
      !< First dimension is the "constituent" dimension, e.g., to set individual tracers or sediment fractions.
      !< The second dimension is the 3D grid cell dimension (1:ndkx)
      integer, intent(in) :: first_index !< The value for the first "constituent" index of the output array.
      real(kind=dp), intent(in) :: vertical_range_min !< Lower limit for the optional vertical range. Use dmiss for no custom range.
      real(kind=dp), intent(in) :: vertical_range_max !< Upper limit for the optional vertical range. Use dmiss for no custom range.
      integer, intent(in) :: operand !< The operand to be used for combining the input field values with any previously set values.

      real(kind=dp) :: lower_limit, upper_limit, level_at_pressure_point
      integer :: n, k, kb, kt

      lower_limit = -huge(1.0_dp)
      upper_limit = huge(1.0_dp)
      if (vertical_range_min /= dmiss) then
         lower_limit = vertical_range_min
      end if
      if (vertical_range_max /= dmiss) then
         upper_limit = vertical_range_max
      end if
      do n = 1, size(input_array_2d)
         if (input_array_2d(n) /= dmiss) then
            if (kmx == 0) then
               call operate(output_array_3d(first_index, n), input_array_2d(n), operand)
            else
               kb = kbot(n)
               kt = ktop(n)
               call operate(output_array_3d(first_index, n), input_array_2d(n), operand)
               do k = kb, kt
                  level_at_pressure_point = 0.5_dp * (zws(k) + zws(k - 1))
                  if (level_at_pressure_point > lower_limit .and. level_at_pressure_point < upper_limit) then
                     call operate(output_array_3d(first_index, k), input_array_2d(n), operand)
                  end if
               end do
            end if
         end if
      end do
   end subroutine initialfield2Dto3D_dbl_indx
end module unstruc_inifields
