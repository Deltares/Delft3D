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

   use precision, only: dp
   implicit none
   private

   public :: init1dField, initialize_initial_fields, spaceInit1dField, readIniFieldProvider, checkIniFieldFileVersion, &
             set_friction_type_values, initialfield2Dto3D_dbl_indx, initialfield2Dto3D

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
contains

   function checkIniFieldFileVersion(inifilename, inifield_ptr) result(ierr)
      use dfm_error
      character(len=*), intent(in) :: inifilename !< name of initial field file, should already be opened in inifield_ptr.
      type(tree_data), pointer :: inifield_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      integer :: ierr !< Result status (DFM_NOERR on success)

      integer :: major, minor
      logical :: success

      ierr = DFM_NOERR

      major = 0
      minor = 0
      call get_version_number(inifield_ptr, major=major, minor=minor, success=success)
      if (.not. success .or. major < IniFieldMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') &
            'Unsupported IniFieldFile format detected in '''//trim(inifilename)//''': v', major, minor, &
            '. Current format: v', IniFieldMajorVersion, IniFieldMinorVersion, '. Ignoring this file.'
         call warn_flush()
         ierr = DFM_EFILEFORMAT
      end if
   end function checkIniFieldFileVersion

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

   !> Reads and initializes an initial field file.
   !! The IniFieldFile can contain multiple [Initial] and [Parameter] blocks
   !! that specify the data provider details for initial conditions and
   !! model parameters/coefficients.
   function initialize_initial_fields(inifilename) result(ierr)
      use stdlib_kinds, only: c_bool
      use tree_data_types
      use tree_structures
      use m_alloc, only: reallocP
      use m_ec_parameters, only: ec_undef_int
      use m_cell_geometry, only: xz, yz

      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use m_missing, only: dmiss
      use messageHandling
      use unstruc_files, only: resolvePath
      use system_utils, only: split_filename

      use timespace_parameters, only: FIELD1D
      use timespace, only: timespaceinitialfield, timespaceinitialfield_int

      use m_flow, only: s1, hs, frcu, ndkx, kbot, ktop, ndkx, zcs
      use m_flowgeom, only: ndx2d, ndxi, ndx, bl
      use m_flowtimes, only: irefdate, tzone, tunit, tstart_user

      use fm_external_forcings_data, only: qid, operand, transformcoef, success, trnames

      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, &
                                  DFM_HYD_INTERCEPT_LAYER
      use m_transportdata, only: itrac2const, constituents
      use m_fm_icecover, only: fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation, ec_gettimespacevalue_by_itemID, ecInstancePtr, quantity_name_config_file_to_internal_name
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_S3D, UNC_LOC_3DV
      use fm_external_forcings_utils, only: get_tracername

      use fm_deprecated_keywords, only: deprecated_ext_keywords
      use m_deprecation, only: check_file_tree_for_deprecated_keywords
      use m_timespaceinitialfield_mpi
      use m_find_name, only: find_name
      use m_get_kbot_ktop, only: getkbotktop
      use timespace_parameters, only: WEIGHTFACTORS

      implicit none
      character(len=*), intent(in) :: inifilename !< name of initial field file
      integer :: ierr !< Result status (DFM_NOERR on success)

      type(tree_data), pointer :: inifield_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      type(tree_data), pointer :: node_ptr
      integer, parameter :: ini_key_len = 32
      integer, parameter :: ini_value_len = 256
      character(len=ini_key_len) :: groupname
      character(len=ini_value_len) :: varname
      character(len=ini_value_len) :: tracnam, qidnam, source_quantity_name
      character(len=ini_value_len) :: global_water_level_quantity
      integer :: num_items_in_file
      character(len=255) :: fnam, filename
      character(len=255) :: basedir
      integer :: istat
      integer :: i, ib, ja, iconst, itrac, k
      integer :: target_location_type, first_index
      integer :: method, iloctype, filetype, ierr_loc
      integer :: target_location_count
      logical(kind=c_bool), allocatable :: specified_water_levels(:) !< indices where waterlevels are specified with non-global values
      logical(kind=c_bool), allocatable :: specified_indices(:)
      real(kind=dp) :: global_value, water_level_global_value
      logical :: global_value_provided, water_level_global_value_provided
      logical :: time_dependent_array
      integer, allocatable :: kcsini(:) ! node code during initialization
      integer :: ec_item
      integer :: quantity_value_count

      real(kind=dp), pointer, dimension(:) :: target_array, x_loc, y_loc
      real(kind=dp), pointer, dimension(:, :) :: target_array_3d
      real(kind=sp), pointer, dimension(:, :) :: target_array_3d_sp
      integer, pointer, dimension(:) :: target_array_integer
      integer, dimension(:), allocatable :: mask
      real(kind=dp) :: factor
      integer, pointer, dimension(:) :: pktop, pkbot

      ierr = DFM_NOERR
      success = .true.
      allocate (specified_water_levels(ndxi - ndx2D))
      specified_water_levels = .false.
      global_water_level_quantity = ''
      water_level_global_value_provided = .false.

      call mess(LEVEL_INFO, 'Reading initial field file '''//trim(inifilename)//'''.')

      call tree_create(trim(inifilename), inifield_ptr)
      call prop_file('ini', trim(inifilename), inifield_ptr, istat)

      call split_filename(inifilename, basedir, fnam)
      ierr = checkIniFieldFileVersion(inifilename, inifield_ptr)
      if (ierr /= DFM_NOERR) then
         goto 888
      end if

      num_items_in_file = 0
      if (associated(inifield_ptr%child_nodes)) then
         num_items_in_file = size(inifield_ptr%child_nodes)
      end if

      ib = 0
      !! Now loop on each block
      do i = 1, num_items_in_file

         node_ptr => inifield_ptr%child_nodes(i)%node_ptr
         !! Step 1: Read each block
         call readIniFieldProvider(inifilename, node_ptr, groupname, qid, filename, filetype, method, &
                                   iloctype, operand, transformcoef, ja, varname)
         ! convert quantity name used in configuration file to a consistent internal name
         qid = quantity_name_config_file_to_internal_name(qid)

         if (ja == 1) then
            call resolvePath(filename, basedir)
            ib = ib + 1
         else
         end if
         if ((.not. strcmpi(groupname, 'Initial')) .and. (.not. strcmpi(groupname, 'Parameter'))) then
            cycle
         end if

         !! Step 2: operation for each block
         if (filetype == field1D) then
            call fm_quantity_name_to_source_quantity_name(qid, filetype, source_quantity_name)
            if (source_quantity_name == '') then
               ierr = DFM_WRONGINPUT
               write (msgbuf, '(a)') 'File '''//trim(inifilename)//''': quantity '''//trim(qid)//''' is not supported.'// &
                  ' Please use the correct quantity name in the IniFieldFile.'
               call err_flush()
               return
            end if
            ierr_loc = init1dField(filename, inifilename, source_quantity_name, specified_indices, global_value, global_value_provided)
            if (ierr_loc /= DFM_NOERR) then
               success = .false.
               exit ! Or, consider cycle instead, to try all remaining blocks and return with an error only at the very end.
            end if
            if (strcmpi(qid, 'initialwaterlevel') .or. strcmpi(qid, 'initialwaterdepth') & ! Official names
                .or. strcmpi(qid, 'waterlevel') .or. strcmpi(qid, 'waterdepth') & ! Backwards compatible names
                .or. strcmpi(qid, 'initialvelocity')) then
               specified_water_levels = specified_water_levels .or. specified_indices
               if (global_value_provided) then
                  water_level_global_value_provided = .true.
                  water_level_global_value = global_value
                  global_water_level_quantity = qid
               end if
            else if (strcmpi(qid, 'frictioncoefficient')) then
               if (.not. all(specified_indices) .and. global_value_provided) then
                  call set_global_values(frcu, specified_indices, global_value)
                  ! Otherwise, use the default values
               end if
            else
               ierr = DFM_WRONGINPUT
               write (msgbuf, '(a)') 'File '''//trim(inifilename)//''': quantity '''//trim(qid)//''' is not supported.'
               call err_flush()
               return
            end if
         else
            if (strcmpi(groupname, 'Initial')) then
               call process_initial_block(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                          target_array_3d, first_index, method)
            else
               call process_parameter_block(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                            target_array_integer, target_array_3d, target_array_3d_sp, first_index, quantity_value_count, &
                                            filetype)
            end if

            ! This part of the code might be moved or changed. (See UNST-8247)
            ! 'initialtracer' with method unequal to WEIGHTFACTORS will be handled by calling fill_field_values below
            if (qid(1:13) == 'initialtracer' .and. method == WEIGHTFACTORS) then
               call reallocP(target_array, ndkx, keepExisting=.false., fill=dmiss)
               ! Get iconst via qid, tracnam, itrac, itrac2const
               call get_tracername(qid, tracnam, qidnam)
               call add_tracer(tracnam, iconst) ! or just gets constituents number if tracer already exists
               itrac = find_name(trnames, tracnam)
               if (itrac == 0) then
                  call mess(LEVEL_WARN, 'flow_initexternalforcings: tracer '//trim(tracnam)//' not found')
                  success = .false.
                  return
               end if
               iconst = itrac2const(itrac)
               !
               quantity_value_count = 1
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)
               ec_item = ec_undef_int
               call setzcs()
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, quantity_value_count, filename, &
                                                 filetype, method, operand, z=zcs, pkbot=kbot, pktop=ktop, &
                                                 varname=varname, tgt_item1=ec_item)
               success = success .and. ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, irefdate, tzone, &
                                                                      tunit, tstart_user, target_array)
               if (.not. success) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: error reading '//trim(qid)//'from '//trim(filename))
               end if
               factor = merge(transformcoef(2), 1.0_dp, transformcoef(2) /= -999.0_dp)
               do k = 1, Ndkx
                  if (target_array(k) /= dmiss) then
                     constituents(iconst, k) = target_array(k) * factor
                  end if
               end do
            end if

            if (.not. success) then
               ierr = DFM_WRONGINPUT
               cycle
            end if

            if (time_dependent_array) then
               call set_coordinates_for_location_type(target_location_type, x_loc, y_loc, target_location_count, iloctype, kcsini)
               if (target_location_type == UNC_LOC_S3D) then
                  pkbot => kbot
                  pktop => ktop
                  call setzcs()
                  success = ec_addtimespacerelation(qid, x_loc, y_loc, kcsini, quantity_value_count, filename, filetype, method, operand, &
                                                    varname=varname, z=zcs, pkbot=pkbot, pktop=pktop)
               else
                  success = ec_addtimespacerelation(qid, x_loc, y_loc, kcsini, quantity_value_count, filename, filetype, method, operand, &
                                                    varname=varname)
               end if
            else
               if (.not. associated(target_array) .and. .not. associated(target_array_3d)) then
                  cycle
               end if

               call fill_field_values(target_array, target_array_3d, target_location_type, first_index, filename, &
                                      filetype, method, operand, transformcoef, iloctype, kcsini, success)
            end if

            if (success) then
               call finish_initialization(qid)
            end if

            if (.not. success) then
               ierr = DFM_WRONGINPUT
            end if
         end if

      end do

      if (.not. all(specified_water_levels) .and. water_level_global_value_provided) then
         call set_global_water_values(bl(ndx2D + 1:ndxi), hs(ndx2D + 1:ndxi), s1(ndx2D + 1:ndxi), specified_water_levels, &
                                      global_water_level_quantity, water_level_global_value, inifilename)
         ! Otherwise, use the default values
      end if

      write (msgbuf, '(a,i8,a)') 'Finish initializing the initial field file '''//trim(inifilename)//''':', ib, &
         ' blocks have been read and handled.'
      call msg_flush()

      if (.not. success) then
         ierr = DFM_WRONGINPUT
      end if

888   continue
      ! Return with whichever ierr status was set before.

      call check_file_tree_for_deprecated_keywords(inifield_ptr, deprecated_ext_keywords, istat, prefix='While reading ''' &
                                                   //trim(inifilename)//'''')

   end function initialize_initial_fields

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

   !> Reads all key values for a data provider from an IniFieldFile block.
   !! All returned values will typically be used for a call to timespaceinitialfield().
   subroutine readIniFieldProvider(inifilename, node_ptr, groupname, quantity, filename, filetype, method, &
                                   iloctype, operand, transformcoef, ja, varname)
      use timespace_parameters
      use fm_external_forcings_utils, only: read_tracer_properties
      use m_ec_interpolationsettings, only: RCEL_DEFAULT
      use m_ec_parameters, only: interpolate_time, interpolate_spacetimeSaveWeightFactors
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL
      use m_grw
      use m_Roughness, only: frictionTypeStringToInteger

      character(len=*), intent(in) :: inifilename !< Name of the ini file, only used in warning messages, actual data is read from node_ptr.
      type(tree_data), pointer :: node_ptr !< The tree structure containing a single ini-file chapter/block.
      character(len=*), intent(out) :: groupname !< Identifier of the read chapter (e.g., 'Initial')
      character(len=*), intent(out) :: quantity !< Identifier of current quantity (e.g., 'initialWaterLevel')
      character(len=*), intent(out) :: filename !< Name of data file for current quantity.
      integer, intent(out) :: filetype !< File type of current quantity.
      integer, intent(out) :: method !< Time-interpolation method for current quantity.
      integer, intent(out) :: iloctype !< The spatial type of the target locations: 1D, 2D or all.
      character(len=1), intent(out) :: operand !< Operand w.r.t. previous data ('O'verride or '+'Append)
      real(kind=dp), intent(out) :: transformcoef(:) !< Transformation coefficients
      integer, intent(out) :: ja !< Whether a block was successfully read or not.
      character(len=*), intent(out) :: varname !< Variable name within filename; only in case of NetCDF. Will be empty string if not specified in input.

      integer, parameter :: ini_key_len = 32
      integer, parameter :: ini_value_len = 256
      character(len=ini_value_len) :: dataFileType
      character(len=ini_value_len) :: interpolationMethod
      character(len=ini_value_len) :: averagingType
      character(len=ini_value_len) :: locationType
      character(len=ini_value_len) :: friction_type
      integer :: iav, averagingNumMin, int_friction_type
      character(len=ini_value_len) :: extrapolation
      logical :: retVal
      ja = 0
      groupname = tree_get_name(node_ptr)

      if (strcmpi(groupname, 'General')) then
         ja = 1
         return
      end if

      transformcoef = -999.0_dp

      if ((.not. strcmpi(groupname, 'Initial')) .and. (.not. (strcmpi(groupname, 'Parameter')))) then
         write (msgbuf, '(5a)') 'Unrecognized block in file ''', trim(inifilename), ''': [', trim(groupname), &
            ']. Ignoring this block.'
         call warn_flush()
         return
      end if

      ! read quantity
      call prop_get(node_ptr, '', 'quantity', quantity, retVal)
      if (.not. retVal) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            ']. Field ''quantity'' is missing. Ignoring this block.'
         call warn_flush()
         return
      end if

      ! read datafile
      call prop_get(node_ptr, '', 'dataFile', filename, retVal)
      if (retVal) then
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            '] for quantity='//trim(quantity)//'. Field ''dataFile'' is missing. Ignoring this block.'
         call warn_flush()
         return
      end if

      ! read dataFileType
      call prop_get(node_ptr, '', 'dataFileType ', dataFileType, retVal)
      if (.not. retVal) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            '] for quantity='//trim(quantity)//'. Field ''dataFileType'' is missing. Ignoring this block.'
         call warn_flush()
         return
      end if
      filetype = convert_file_type_string_to_integer(dataFileType)
      if (filetype == FILE_TYPE_UNKNOWN) then
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
            //trim(quantity)//'. Field ''dataFileType'' has invalid value '''//trim(dataFileType)//'''. Ignoring this block.'
         call warn_flush()
         return
      end if

      varname = ''
      call prop_get(node_ptr, '', 'dataVariableName ', varname)

      ! if dataFileType is 1dField, then it is not necessary to read interpolationMethod, operand, averagingType,
      ! averagingRelSize, averagingNumMin, averagingPercentile, locationType, extrapolationMethod, value
      if (filetype /= field1D) then
         ! read interpolationMethod
         call prop_get(node_ptr, '', 'interpolationMethod ', interpolationMethod, retVal)
         if (retVal) then
            method = convert_method_string_to_integer(interpolationMethod)
            call update_method_with_weightfactor_fallback(dataFileType, method)

            if (method == METHOD_UNKNOWN .or. (method == interpolate_time .and. filetype /= inside_polygon)) then
               write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
                  //trim(quantity)//'. Field ''interpolationMethod'' has invalid value '''//trim(interpolationMethod)// &
                  '''. Ignoring this block.'
               call warn_flush()
               return
            end if
         else
            method = get_default_method_for_file_type(dataFileType)

            if (method == METHOD_UNKNOWN) then
               write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
                  //trim(quantity)//'. Field ''interpolationMethod'' is missing. Ignoring this block.'
               call warn_flush()
               return
            end if
         end if

         if (method == interpolate_spacetimeSaveWeightFactors) then ! 'averaging'
            ! read averagingType
            call prop_get(node_ptr, '', 'averagingType ', averagingType, retVal)
            if (.not. retVal) then
               averagingType = 'mean'
            end if
            call averagingTypeStringToInteger(averagingType, iav)
            if (iav >= 0) then
               transformcoef(4) = real(iav, kind=dp)
            else
               write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='// &
                  trim(quantity)//'. Field ''averagingType'' has invalid value '''//trim(averagingType)//'''. Ignoring this block.'
               call warn_flush()
               return
            end if

            ! read averagingRelSize
            call prop_get(node_ptr, '', 'averagingRelSize', transformcoef(5), retVal)
            if (.not. retVal) then
               transformcoef(5) = RCEL_DEFAULT
            else
               if (transformcoef(5) <= 0.0_dp) then
                  write (msgbuf, '(5a,f10.3,a,f10.3,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                     '] for quantity='//trim(quantity)//'. Field ''averagingRelSize'' has invalid value ', transformcoef(5), &
                     '. Setting to default: ', RCEL_DEFAULT, '.'
                  call warn_flush()
                  transformcoef(5) = RCEL_DEFAULT
               end if
            end if

            ! read averagingNumMin
            call prop_get(node_ptr, '', 'averagingNumMin', averagingNumMin, retVal)
            if (.not. retVal) then
               transformcoef(8) = 1.0_dp
            else
               if (averagingNumMin <= 0) then
                  write (msgbuf, '(5a,i0,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                     '] for quantity='//trim(quantity)//'. Field ''averagingNumMin'' has invalid value ', averagingNumMin, &
                     '. Setting to default: 1.'
                  call warn_flush()
                  transformcoef(8) = 1.0_dp
               else
                  transformcoef(8) = real(averagingNumMin, kind=dp)
               end if
            end if

            ! read averagingPercentile
            call prop_get(node_ptr, '', 'averagingPercentile', transformcoef(7), retVal)
            if (.not. retVal) then
               transformcoef(7) = 0.0_dp
            else
               if (transformcoef(7) < 0.0_dp) then
                  write (msgbuf, '(5a,f10.3,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                     '] for quantity='//trim(quantity)//'. Field ''averagingPercentile'' has invalid value ', &
                     transformcoef(7), '. Setting to default: 0.0.'
                  call warn_flush()
                  transformcoef(7) = 0.0_dp
               end if
            end if
         end if

         call prop_get(node_ptr, '', 'locationType ', locationType, retVal)
         if (.not. retVal) then
            ilocType = ILATTP_ALL
         else
            select case (trim(str_tolower(locationType)))
            case ('1d')
               ilocType = ILATTP_1D
            case ('2d')
               ilocType = ILATTP_2D
            case ('1d2d')
               ilocType = ILATTP_ALL
            case default
               ilocType = ILATTP_ALL
            end select
         end if

         ! if the infiltrationmodel is not horton, but a horton quantity is detected, then send a error message
         if (infiltrationmodel /= DFM_HYD_INFILT_HORTON .and. &
             strcmpi(quantity, 'Horton', 6)) then
            write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(quantity) &
               //'''. This requires ''InfiltrationModel=', DFM_HYD_INFILT_HORTON, ''' in the MDU file (Horton).'
            call warn_flush()
         end if

         ! read extrapolationMethod
         call prop_get(node_ptr, '', 'extrapolationMethod', extrapolation, retVal)
         if (retVal .and. strcmpi(trim(extrapolation), 'yes')) then
            ! TODO: implement extrapolation method (see UNST-8626) and then remove this warning
            write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
               //trim(quantity)//'. Field ''extrapolationMethod'' is not (yet) supported. Continuing without extrapolation.'
            call warn_flush()
         end if

         ! read value
         if (filetype == inside_polygon .and. method == METHOD_CONSTANT) then
            call prop_get(node_ptr, '', 'value', transformcoef(1), retVal)
            if (.not. retVal) then
               write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                  '] for quantity='//trim(quantity)//'. Field ''value'' is missing. Ignore this block.'
               call warn_flush()
               return
            end if
         end if
      end if ! .not. strcmpi(dataFileType, '1dField'))

      ! read operand, for any filetype
      call prop_get(node_ptr, '', 'operand ', operand, retVal)
      if (.not. retVal) then
         operand = 'O'
      else
         if ((.not. strcmpi(operand, 'O')) .and. (.not. strcmpi(operand, 'A')) .and. (.not. strcmpi(operand, '+')) .and. &
             (.not. strcmpi(operand, '*')) .and. (.not. strcmpi(operand, 'X')) .and. (.not. strcmpi(operand, 'N')) .and. &
             (.not. strcmpi(operand, 'V'))) then
            write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
               //trim(quantity)//'. Field ''operand'' has invalid value '''//trim(operand)//'''. Ignoring this block.'
            call warn_flush()
            return
         end if
      end if

      if (strcmpi(quantity, 'frictioncoefficient')) then
         friction_type = ''
         call prop_get(node_ptr, '', 'frictionType', friction_type)
         call frictionTypeStringToInteger(friction_type, int_friction_type)
         if (int_friction_type > 0) then
            transformcoef(3) = real(int_friction_type, dp)
         end if
      end if

      if (strcmpi(quantity(1:13), 'initialtracer')) then
         call read_tracer_properties(node_ptr, transformcoef)
      end if

      ! We've made it to here, success!
      ja = 1
      return

   end subroutine readIniFieldProvider

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
      use m_flowparameters, only: eps10
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
            if (comparereal(chai, minsChai, eps10) <= 0) then
               res(k) = sValues(1)
               modified_elements(k) = .true.
               cycle
            else if (comparereal(chai, maxsChai, eps10) >= 0) then
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

               if (comparereal(chai, sChaiPrev, eps10) >= 0 .and. comparereal(chai, sChai, eps10) < 0) then
                  if (comparereal(sChai, sChaiPrev, eps10) /= 0) then
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

      implicit none

      integer :: link

      if (transformcoef(3) /= -999.0_dp .and. int(transformcoef(3)) /= ifrctypuni .and. (operand == 'O' .or. operand == 'V')) then
         do link = 1, lnx
            if (frcu(link) /= dmiss) then
               ! type array only must be used if different from uni
               ifrcutp(link) = int(transformcoef(3))
            end if
         end do
      end if

   end subroutine set_friction_type_values

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

   !> Set the control parameters for the actual reading of either the [Initial] type items from the input file or
   !! connecting the input to the EC-module.
   subroutine process_initial_block(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                    target_array_3d, indx, method)
      use stdlib_kinds, only: c_bool
      use system_utils, only: split_filename
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_alloc, only: realloc, aerr, reallocP
      use m_missing, only: dmiss
      use m_ec_parameters, only: ec_undef_int

      use m_meteo, only: ec_addtimespacerelation
      use unstruc_files, only: resolvePath
      use unstruc_model, only: md_extfile
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, DFM_HYD_INTERCEPT_LAYER
      use string_module, only: str_tolower
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN, UNC_LOC_S3D, UNC_LOC_3DV

      use fm_external_forcings_data, only: success, transformcoef, trnames, uxini, uyini, inivelx, &
                                           inively, NAMTRACLEN
      use fm_external_forcings_utils, only: split_qid, get_tracername !, copy_3d_arrays_double_indexed_to_single_indexed

      use m_flow, only: s1, hs, sabot, satop, sa1, ndkx, tem1, h_unsat, kmx
      use m_flowgeom, only: ndx, lnx
      use m_flowparameters, only: jasal, inisal2D, uniformsalinityabovez, uniformsalinitybelowz, temperature_model, &
                                  TEMPERATURE_MODEL_NONE, initem2D, inivel

      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, DFM_HYD_INTERCEPT_LAYER
      use m_fm_icecover, only: fm_ice_activate_by_ext_forces
      use m_sediment, only: stm_included, sed, jased, sedh
      use m_transportdata, only: ISED1, const_names, itrac2const, constituents
      use m_fm_wq_processes, only: wqbotnames, wqbot
      use m_find_name, only: find_name
      use m_add_bndtracer, only: add_bndtracer
      use timespace_parameters, only: WEIGHTFACTORS

      ! use network_data
      ! use dfm_error

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the quantity.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U.
      logical, intent(out) :: time_dependent_array !< Logical indicating, whether the quantity is time dependent or not.
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (real(kind=dp)).
      real(kind=dp), dimension(:, :), pointer, intent(out) :: target_array_3d !< pointer to the array that corresponds to the quantity (real(kind=dp)).
      integer, intent(out) :: indx !< Index of the quantity.
      integer, intent(in) :: method !< interpolation type for the space related data.

      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6
      integer :: iostat
      integer :: iconst, isednum, itrac, iwqbot
      character(len=idlen) :: qid_base, qid_specific
      character(len=NAMTRACLEN) :: tracnam, qidnam
      character(len=20) :: tracunit
      integer :: janew

      integer :: layer

      target_array => null()
      indx = 1
      time_dependent_array = .false.
      target_location_type = 0

      call split_qid(qid, qid_base, qid_specific)

      ! UNST-8840: temporarily support hydrological quanties either as [Parameter] or [Initial] blocks.
      call process_hydrological_quantities(qid_base, inifilename, target_location_type, target_array)
      if (associated(target_array)) then
         ! Hydrological quantity found, no continuation by the select case below needed.
         call mess(LEVEL_WARN, 'Initial field quantity '''//trim(qid)//''' found in file '''//trim(inifilename) &
                   //''' as an [Initial] block should be a [Parameter] block in future releases. Please update your input file.')
         return
      end if

      select case (str_tolower(qid_base))
      case ('waterlevel', 'initialwaterlevel')
         if (strcmpi(qid_base, 'waterlevel')) then
            call mess(LEVEL_WARN, 'Initial field quantity '''//trim(qid)//''' found in file '''//trim(inifilename) &
                      //''' is deprecated, use ''initialWaterLevel'' instead. Please update your input file.')
         end if
         target_location_type = UNC_LOC_S
         target_array => s1
      case ('waterdepth', 'initialwaterdepth')
         if (strcmpi(qid_base, 'waterdepth')) then
            call mess(LEVEL_WARN, 'Initial field quantity '''//trim(qid)//''' found in file '''//trim(inifilename) &
                      //''' is deprecated, use ''initialWaterDepth'' instead. Please update your input file.')
         end if
         target_location_type = UNC_LOC_S
         target_array => hs
      case ('bedlevel')
         ! Bed level was earlier set in setbedlevelfromextfile()
      case ('initialunsaturedzonethickness')
         call realloc(h_unsat, ndx, keepExisting=.true., fill=dmiss)
         target_location_type = UNC_LOC_S
         target_array => h_unsat

      case ('initialsalinity')

         if (jasal > 0) then
            target_location_type = UNC_LOC_S3D
            target_array_3d(1:1, 1:size(sa1)) => sa1
         end if

      case ('initialsalinitytop')
         if (jasal > 0) then
            call realloc(satop, ndx, keepexisting=.true., fill=dmiss)
            target_location_type = UNC_LOC_S
            target_array => satop
            if (inisal2D /= 0 .and. inisal2D /= 2) then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', initialSalinityTop and initialSalinityBot found. Only one of them can be used.')
               success = .false.
            end if
            inisal2D = 2
            uniformsalinityabovez = transformcoef(3)
         end if

      case ('initialsalinitybot')

         if (jasal > 0) then
            call realloc(sabot, ndx, keepexisting=.true., fill=dmiss)
            target_location_type = UNC_LOC_S
            target_array => sabot
            if (inisal2D /= 0 .and. inisal2D /= 3) then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', initialSalinityTop and initialSalinityBot found. Only one of them can be used.')
               success = .false.
            end if
            inisal2D = 3
            uniformsalinitybelowz = transformcoef(4)
         end if

      case ('initialsedfrac')
         if (stm_included) then
            iconst = 0
            if (ISED1 > 0 .and. trim(qid_specific) /= '') then
               iconst = find_name(const_names, qid_specific)
            end if
            if (iconst > 0) then
               target_location_type = UNC_LOC_S3D
               indx = iconst - ised1 + 1
               target_array_3d => sed
            else
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(qid_specific)//''' from QUANTITY '''//trim(qid)//'''.')
               success = .false.
            end if
         end if

      case ('initialsediment')

         if (jased > 0) then
            call realloc(sedh, ndx, keepExisting=.false., fill=dmiss)
            read (qid_specific(1:1), '(i1)', iostat=iostat) isednum
            if (iostat /= 0) then
               isednum = 1
            end if
            target_location_type = UNC_LOC_S3D
            indx = isednum
            target_array_3d => sed
         end if

      case ('initialtemperature')
         if (temperature_model /= TEMPERATURE_MODEL_NONE) then
            target_location_type = UNC_LOC_S
            target_array => tem1
            initem2D = 1
         end if
      case ('initialtracer')
         if (method == WEIGHTFACTORS) then
            ! handled elsewhere
            return
         end if

         call get_tracername(qid, tracnam, qidnam)
         tracunit = " "
         call add_bndtracer(tracnam, tracunit, itrac, janew)

         call add_tracer(qid_specific, iconst) ! or just gets constituents number if tracer already exists
         itrac = find_name(trnames, qid_specific)

         if (itrac == 0) then
            call mess(LEVEL_WARN, 'flow_init initial fields: tracer '//trim(qid_specific)//' not found')
            success = .false.
            return
         end if
         iconst = itrac2const(itrac)
         target_location_type = UNC_LOC_S3D
         indx = iconst
         target_array_3d => constituents
      case ('initialvelocity')
         call SetMessage(LEVEL_WARN, 'initialvelocity is not supported in the inifields file. Use initialvelocityx'// &
                         ' and initialvelocityy instead.')
         success = .false.

      case ('initialvelocityx')
         call realloc(uxini, lnx, fill=dmiss)
         target_location_type = UNC_LOC_U
         target_array => uxini
         inivelx = 1
         if (inively == 1) then
            inivel = 1
         end if

      case ('initialvelocityy')

         call realloc(uyini, lnx, fill=dmiss)
         target_location_type = UNC_LOC_U
         target_array => uyini
         inively = 1
         if (inivelx == 1) then
            inivel = 1
         end if

      case ('initialverticaltemperatureprofile')
         if (temperature_model /= TEMPERATURE_MODEL_NONE .and. kmx > 0) then
            target_location_type = UNC_LOC_3DV
            target_array => tem1
         end if

      case ('initialverticalsalinityprofile')
         if (jasal > 0. .and. kmx > 0) then
            target_location_type = UNC_LOC_3DV
            target_array => sa1
         end if
      case ('initialverticalsedfracprofile')
         if (stm_included .and. kmx > 0) then
            !call get_sedfracname(qid, qid_specific, qidnam)
            iconst = 0
            if (ISED1 > 0 .and. trim(qid_specific) /= '') then
               iconst = find_name(const_names, qid_specific)
            end if
            if (iconst > 0) then
               target_array = dmiss
               target_location_type = UNC_LOC_3DV
               target_array_3d => sed
               indx = iconst - ISED1 + 1
            else
               call mess(LEVEL_WARN, 'Error in initial fields: initialverticalsedfracprofile, sedimentfraction '''// &
                         trim(qid_specific)//''' not found.')
               success = .false.
               return
            end if
         end if

      case ('initialverticalsigmasedfracprofile')
         if (stm_included .and. kmx > 0) then
            iconst = 0
            if (ISED1 > 0 .and. trim(qid_specific) /= '') then
               iconst = find_name(const_names, qid_specific)
            end if
            if (iconst > 0) then
               allocate (target_array(1:ndkx))
               target_array = dmiss
               target_location_type = UNC_LOC_3DV
               target_array_3d => sed
               indx = iconst - ISED1 + 1
            end if
         end if

      case ('initialwaqbot')
         iwqbot = find_name(wqbotnames, qid_specific)

         if (iwqbot == 0) then
            call mess(LEVEL_ERROR, 'flow_initexternalforcings: water quality bottom variable '//trim(qid_specific)//' not found')
            success = .false.
            return
         end if

         if (transformcoef(3) == DMISS) then
            layer = -1
         else
            layer = nint(transformcoef(3))
            if (layer > max(kmx, 1)) then
               call mess(LEVEL_ERROR, 'Specified layer for '''//trim(qid)//''' is higher than kmx: ', layer, kmx)
               success = .false.
               return
            end if
         end if
         target_array_3d => wqbot
         indx = iwqbot
         target_location_type = UNC_LOC_S3D

      case default
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), &
            ' Field ''quantity'' (='//trim(qid)//') does not match (refer to User Manual). Ignoring this block.'
         call warn_flush()
         success = .false.
      end select

   end subroutine process_initial_block

   !> Set the control parameters for the actual reading of the items from the input file or
   !! connecting the input to the EC-module.
   subroutine process_parameter_block(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                      target_array_integer, target_array_3d, target_array_3d_sp, target_quantity_index, quantity_value_count, filetype)
      use stdlib_kinds, only: c_bool
      use system_utils, only: split_filename
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_alloc, only: realloc, aerr
      use unstruc_files, only: resolvePath
      use timespace_parameters, only: NCGRID
      use m_missing, only: dmiss
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN, UNC_LOC_GLOBAL, UNC_LOC_S3D
      use m_flowparameters, only: jatrt, javiusp, jafrcInternalTides2D, jadiusp, jafrculin, jaCdwusp, ibedlevtyp, jawave, waveforcing
      use m_flowparameters, only: ja_friction_coefficient_time_dependent
      use m_flow, only: frcu
      use m_flow, only: jacftrtfac, cftrtfac, viusp, diusp, DissInternalTidesPerArea, frcInternalTides2D, frculin, Cdwusp
      use m_flowgeom, only: ndx, lnx, grounlay, iadv, jagrounlay, ibot
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use fm_external_forcings_data, only: success
      use fm_external_forcings_utils, only: split_qid
      use m_heatfluxes, only: secchisp
      use m_wind, only: wind_drag_type, CD_TYPE_CONST
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation
      use m_vegetation, only: stemdiam, stemdens, stemheight
      use unstruc_model, only: md_extfile, md_ptr
      use m_nudge, only: nudge_time, nudge_rate
      use string_module, only: str_tolower
      use m_waveconst, only: WAVE_NC_OFFLINE, WAVEFORCING_DISSIPATION_3D, WAVEFORCING_RADIATION_STRESS, WAVEFORCING_DISSIPATION_TOTAL
      use processes_input, only: paname, painp, num_spatial_parameters, &
                                 funame, funinp, num_time_functions, &
                                 sfunname, sfuninp, num_spatial_time_fuctions
      use m_physcoef, only: constant_dicoww, dicoww
      use m_array_or_scalar, only: assign_pointer_to_t_array, realloc

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the ini file.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U.
      logical, intent(out) :: time_dependent_array !< Logical indicating, whether the quantity is time dependent or not.
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (real(kind=dp)).
      integer, dimension(:), pointer, intent(out) :: target_array_integer !< pointer to the array that corresponds to the quantity (integer).
      real(kind=dp), dimension(:, :), pointer, intent(out) :: target_array_3d !< pointer to the array that corresponds to the quantity (real(kind=dp)), if it has an extra dimension.
      real(kind=sp), dimension(:, :), pointer, intent(out) :: target_array_3d_sp !< pointer to the array that corresponds to the quantity (real(kind=sp)), if it has an extra dimension.
      integer, intent(out) :: target_quantity_index !< Index of the quantity in the first dimension of target_array_3d, if applicable.
      integer, intent(out) :: quantity_value_count !< The number of values for this quantity on a single location. E.g. 1 for scalar fields, 2 for vector fields.
      integer, intent(in) :: filetype !< Type of the file being read (NCGRID, etc).

      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6
      character(len=idlen) :: qid_base, qid_specific
      integer :: ierr

      target_array => null()
      target_array_integer => null()
      target_array_3d => null()
      target_array_3d_sp => null()
      time_dependent_array = .false.
      target_quantity_index = 1
      quantity_value_count = 1

      call split_qid(qid, qid_base, qid_specific)

      ! UNST-8840: temporarily support hydrological quanties either as [Parameter] or [Initial] blocks.
      call process_hydrological_quantities(qid, inifilename, target_location_type, target_array)
      if (associated(target_array)) then
         ! Hydrological quantity found, no continuation by the select case below needed.
         return
      end if

      select case (str_tolower(qid_base))
      case ('frictioncoefficient')
         target_location_type = UNC_LOC_U
         target_array => frcu
         if (filetype == NCGRID) then
            time_dependent_array = .true.
            ja_friction_coefficient_time_dependent = 1
         end if
      case ('advectiontype')
         target_location_type = UNC_LOC_U
         target_array_integer => iadv
      case ('groundlayerthickness')
         target_location_type = UNC_LOC_U
         target_array => grounlay
         jagrounlay = 1
      case ('bedrock_surface_elevation')
         call initialize_subsupl()
         time_dependent_array = .true.
         select case (ibedlevtyp)
         case (enum_field1D)
            target_location_type = UNC_LOC_S
         case (enum_field2D)
            target_location_type = UNC_LOC_U
         case (enum_field3D, enum_field4D, enum_field5D, enum_field6D)
            target_location_type = UNC_LOC_CN
         end select
         ! Note: target_array not needed, handled via quantity in ec_addtimespacerelation()
      case ('frictiontrtfactor')
         if (jatrt /= 1) then
            call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting QUANTITY '//trim(qid)// &
                      ', but [trachytopes] is not switched on in MDU file. Ignoring this block.')
            success = .false.
         else
            if (.not. allocated(cftrtfac)) then
               allocate (cftrtfac(lnx), stat=ierr)
               call aerr('cftrtfac(lnx)', ierr, lnx)
               cftrtfac = 1.0_dp
            end if
            target_location_type = UNC_LOC_U
            target_array => cftrtfac
            jacftrtfac = 1
         end if
      case ('horizontaleddyviscositycoefficient')
         if (javiusp == 0) then
            if (allocated(viusp)) then
               deallocate (viusp)
            end if
            allocate (viusp(lnx), stat=ierr)
            call aerr('viusp(lnx)', ierr, lnx)
            viusp = dmiss
            javiusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => viusp
      case ('horizontaleddydiffusivitycoefficient')
         if (jadiusp == 0) then
            if (allocated(diusp)) then
               deallocate (diusp)
            end if
            allocate (diusp(lnx), stat=ierr)
            call aerr('diusp(lnx)', ierr, lnx)
            diusp = dmiss
            jadiusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => diusp
      case ('ibedlevtype')
         target_location_type = UNC_LOC_U
         target_array_integer => ibot
      case ('internaltidesfrictioncoefficient')
         if (jaFrcInternalTides2D /= 1) then ! not added yet
            if (allocated(frcInternalTides2D)) then
               deallocate (frcInternalTides2D)
            end if
            allocate (frcInternalTides2D(Ndx), stat=ierr)
            call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
            frcInternalTides2D = DMISS

            if (allocated(DissInternalTidesPerArea)) then
               deallocate (DissInternalTidesPerArea)
            end if
            allocate (DissInternalTidesPerArea(Ndx), stat=ierr)
            call aerr(' DissInternalTidesPerArea(Ndx)', ierr, Ndx)
            DissInternalTidesPerArea = 0.0_dp
            jaFrcInternalTides2D = 1
         end if
         target_location_type = UNC_LOC_S
         target_array => frcInternalTides2D
      case ('linearfrictioncoefficient')
         target_location_type = UNC_LOC_U
         target_array => frculin
         jafrculin = 1
      case ('sea_ice_area_fraction', 'sea_ice_thickness')
         if (ja_ice_area_fraction_read == 0 .and. ja_ice_thickness_read == 0) then
            call fm_ice_activate_by_ext_forces(ndx, md_ptr)
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .true.
      case ('secchidepth')
         call realloc(secchisp, ndx, keepExisting=.true., fill=dmiss, stat=ierr)
         target_location_type = UNC_LOC_S
         target_array => secchisp
      case ('backgroundverticaleddydiffusivitycoefficient')
         target_location_type = UNC_LOC_S
         call realloc(dicoww, ndx, constant_dicoww)
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
            if (allocated(Cdwusp)) then
               deallocate (Cdwusp)
            end if
            allocate (Cdwusp(lnx), stat=ierr)
            call aerr('Cdwusp(lnx)', ierr, lnx)
            Cdwusp = dmiss
            jaCdwusp = 1
         end if
         target_location_type = UNC_LOC_U
         target_array => Cdwusp
         wind_drag_type = CD_TYPE_CONST
      case ('wavesignificantheight', 'waveperiod', 'wavedirection')
         if (jawave == WAVE_NC_OFFLINE) then
            target_location_type = UNC_LOC_S
            time_dependent_array = .true.
         else
            write (msgbuf, '(a,i0,a)') 'Reading *.ext forcings file '''//trim(md_extfile)// &
               ''', QUANTITY "'//trim(qid)//'" found but "WaveModelNr" is not ', WAVE_NC_OFFLINE, '.'
            call warn_flush()
            success = .false.
         end if
      case ('wavebreakerdissipation', 'whitecappingdissipation')
         if (jawave == WAVE_NC_OFFLINE .and. waveforcing == WAVEFORCING_DISSIPATION_3D) then
            target_location_type = UNC_LOC_S
            time_dependent_array = .true.
         else
            write (msgbuf, '(a,i0,a,i0,a)') 'Reading *.ext forcings file '''//trim(md_extfile)// &
               ''', quantity "'//trim(qid)//'" found but "WaveModelNr" is not ', WAVE_NC_OFFLINE, ', '// &
               'or "WaveForcing" is not ', WAVEFORCING_DISSIPATION_3D, '.'
            call warn_flush()
            success = .false.
         end if
      case ('xwaveforce', 'ywaveforce')
         if (jawave == WAVE_NC_OFFLINE .and. (waveforcing == WAVEFORCING_RADIATION_STRESS .or. waveforcing == WAVEFORCING_DISSIPATION_3D)) then
            target_location_type = UNC_LOC_S
            time_dependent_array = .true.
         else
            write (msgbuf, '(a,i0,a,i0,a,i0,a)') 'Reading *.ext forcings file '''//trim(md_extfile)// &
               ''', quantity "'//trim(qid)//'" found but "WaveModelNr" is not ', WAVE_NC_OFFLINE, ', '// &
               'or "WaveForcing" is not ', WAVEFORCING_RADIATION_STRESS, ' or ', WAVEFORCING_DISSIPATION_3D, '.'
            call warn_flush()
            success = .false.
         end if
      case ('totalwaveenergydissipation')
         if (jawave == WAVE_NC_OFFLINE .and. waveforcing == WAVEFORCING_DISSIPATION_TOTAL) then
            target_location_type = UNC_LOC_S
            time_dependent_array = .true.
         else
            write (msgbuf, '(a,i0,a,i0,a)') 'Reading *.ext forcings file '''//trim(md_extfile)// &
               ''', quantity "'//trim(qid)//'" found but "WaveModelNr" is not ', WAVE_NC_OFFLINE, ', '// &
               'or "WaveForcing" is not ', WAVEFORCING_DISSIPATION_TOTAL, '.'
            call warn_flush()
            success = .false.
         end if
      case ('waqparameter')
         target_location_type = UNC_LOC_S
         call find_or_add_waq_input(qid_specific, paname, num_spatial_parameters, .true., waq_values=painp, index_waq_input=target_quantity_index)
         target_array_3d_sp => painp
         ! TODO: UNST-9008: discuss with Michelle whether this case is in fact equal to waqsegmentnumber.
         ! TODO: UNST-9008: discuss with Michelle generalized 2D/3D handling that is repeated in old code.
      case ('waqsegmentnumber')
         target_location_type = UNC_LOC_S
         call find_or_add_waq_input(qid_specific, paname, num_spatial_parameters, .true., waq_values=painp, index_waq_input=target_quantity_index)
         target_array_3d_sp => painp
         ! TODO: UNST-9008: discuss with Michelle generalized 2D/3D handling that is repeated in old code.
      case ('waqfunction')
         target_location_type = UNC_LOC_GLOBAL
         time_dependent_array = .true.
         call find_or_add_waq_input(qid_specific, funame, num_time_functions, .false., waq_values_ptr=funinp, index_waq_input=target_quantity_index)
      case ('waqsegmentfunction')
         target_location_type = UNC_LOC_S
         time_dependent_array = .true.
         call find_or_add_waq_input(qid_specific, sfunname, num_spatial_time_fuctions, .true., waq_values_ptr=sfuninp, index_waq_input=target_quantity_index)
      case ('nudgesalinitytemperature')
         target_location_type = UNC_LOC_S3D
         time_dependent_array = .true.
         quantity_value_count = 2
         call alloc_nudging()
      case ('nudgerate')
         target_location_type = UNC_LOC_S
         call alloc_nudging()
         target_array => nudge_rate
      case ('nudgetime')
         target_location_type = UNC_LOC_S
         call alloc_nudging()
         target_array => nudge_time
      case default
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), &
            ' Field '''//trim(qid)//''' is not a recognized ''[Parameter]'' quantity (refer to User Manual). Ignoring this block.'
         call warn_flush()
         success = .false.
      end select

   end subroutine process_parameter_block

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
   subroutine process_hydrological_quantities(qid, inifilename, target_location_type, target_array)
      use messageHandling
      use m_alloc, only: realloc, aerr
      use fm_location_types, only: UNC_LOC_S
      use m_flow, only: h_unsat
      use m_flowgeom, only: ndx
      use fm_external_forcings_data, only: success
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, &
                                  HortonMinInfCap, HortonMaxInfCap, HortonDecreaseRate, HortonRecoveryRate, &
                                  InterceptThickness, interceptionmodel, DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, InterceptHs, &
                                  infiltcap, infiltrationmodel
      use string_module, only: str_tolower

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the ini file.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U.
      real(kind=dp), dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (real(kind=dp)).

      select case (str_tolower(qid))
      case ('hortonmininfcap')
         target_location_type = UNC_LOC_S
         target_array => HortonMinInfCap
      case ('hortonmaxinfcap')
         target_location_type = UNC_LOC_S
         target_array => HortonMaxInfCap
      case ('hortondecreaserate')
         target_location_type = UNC_LOC_S
         target_array => HortonDecreaseRate
      case ('hortonrecoveryrate')
         target_location_type = UNC_LOC_S
         target_array => HortonRecoveryRate
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
      end select
   end subroutine process_hydrological_quantities

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

      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use m_hydrology_data, only: infiltcap, DFM_HYD_INFILT_CONST, &
                                  DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, ActEvap
      use m_grw, only: jaintercept2D
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read

      use m_heatfluxes, only: jasecchisp, secchisp
      use m_physcoef, only: secchidepth
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
         jaSecchisp = 1
         do n = 1, ndx
            if (secchisp(n) == dmiss) then
               secchisp(n) = secchidepth
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

   subroutine fill_field_values(target_array, target_array_3d, target_location_type, first_index, filename, filetype, method, operand, &
                                transformcoef, iloctype, kcsini, success)

      use m_alloc, only: reallocP
      use timespace, only: timespaceinitialfield
      use m_missing, only: dmiss
      use m_flow, only: ndkx
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_S3D, UNC_LOC_3DV

      real(kind=dp), dimension(:), pointer, intent(inout) :: target_array !< The array to be filled with values. (in case of a 2d array)
      real(kind=dp), dimension(:, :), pointer, intent(inout) :: target_array_3d !< The array to be filled with values. (in case of a 3d array)
      integer, intent(in) :: target_location_type !< The location type of the target array.
      integer, intent(in) :: first_index !< The first index of the target array (3D).
      character(len=*), intent(in) :: filename !< The name of the file containing the field values.
      integer, intent(in) :: filetype !< The type of the file containing the field values.
      integer, intent(in) :: method !< The method to be used for filling the field values.
      character(len=*), intent(in) :: operand !< The operand to be used for filling the field values.
      real(kind=dp), dimension(:), intent(in) :: transformcoef !< The transformation coefficients.
      integer, intent(in) :: iloctype !< The spatial type of the target locations: 1D, 2D or all.
      integer, dimension(:), allocatable, intent(inout) :: kcsini !< Mask array.
      logical, intent(inout) :: success !< The success of the filling of the field values.
      integer :: loc_type

      integer :: num_items !< The number of target locations.
      real(kind=dp), dimension(:), pointer :: x_loc, y_loc !< The x and y coordinates of the target locations.
      character(len=1) :: used_operand !< The operand to be used for filling the field values.

      if (target_location_type == UNC_LOC_3DV) then
         call setinitialverticalprofile(target_array, ndkx, filename)
         success = .true.
      else
         loc_type = target_location_type
         call set_coordinates_for_location_type(target_location_type, x_loc, y_loc, num_items, iloctype, kcsini)

         if (target_location_type == UNC_LOC_S3D) then
            used_operand = 'O'
            call reallocP(target_array, num_items, fill=dmiss, keepExisting=.false.)
            loc_type = UNC_LOC_S ! timespaceinitialfield expects UNC_LOC_S in stead of UNC_LOC_S3D
         else
            used_operand = operand
         end if

         success = timespaceinitialfield(x_loc, y_loc, target_array, num_items, filename, filetype, method, used_operand, &
                                         transformcoef, loc_type)

         if (associated(target_array_3d)) then
            call initialfield2Dto3D_dbl_indx(target_array, target_array_3d, first_index, transformcoef(13), transformcoef(14), &
                                             operand)
            deallocate (target_array)
            target_array => null()
         end if
      end if

   end subroutine fill_field_values

   subroutine set_coordinates_for_location_type(target_location_type, x_loc, y_loc, num_items, iloctype, kcsini)

      use m_alloc, only: realloc
      use m_cell_geometry, only: xz, yz
      use network_data, only: xk, yk, numk
      use m_flowgeom, only: ndx, lnx, xu, yu
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_S3D, UNC_LOC_CN
      use m_lateral_helper_fuctions, only: prepare_lateral_mask

      integer, intent(in) :: target_location_type !< The spatial type of the target locations: 1D, 2D or all.
      real(kind=dp), pointer, dimension(:), intent(out) :: x_loc, y_loc !< The x and y coordinates of the target locations.
      integer, intent(out) :: num_items !< The number of target locations.
      integer, intent(in) :: iloctype !< The spatial type of the target locations: 1D, 2D or all. Used for filling the kcsini mask array. Valid values: ILATTP_1D, ILATTP_2D, ILATTP_ALL.
      integer, dimension(:), allocatable, intent(inout) :: kcsini !< Mask array.

      select case (target_location_type)
      case (UNC_LOC_S, UNC_LOC_S3D)
         call realloc(kcsini, ndx)
         call prepare_lateral_mask(kcsini, iloctype)
         x_loc => xz(1:ndx)
         y_loc => yz(1:ndx)
         num_items = ndx
      case (UNC_LOC_U)
         call realloc(kcsini, lnx, keepExisting=.false.)
         kcsini = 1
         x_loc => xu(1:lnx)
         y_loc => yu(1:lnx)
         num_items = lnx
      case (UNC_LOC_CN)
         call realloc(kcsini, numk, keepExisting=.false.)
         kcsini = 1
         x_loc => xk(1:numk)
         y_loc => yk(1:numk)
         num_items = numk
      case default
         x_loc => null()
         y_loc => null()
      end select
   end subroutine set_coordinates_for_location_type

   !> The values from the input array on 2D grid cells are copied to the 3D locations in the output array.
   !! Optionally, a vertical range can be specified, which then only updates the 3D output array elements if their vertical
   !! position lies within that range. Without this range, all 3D cells in a single vertical column get the same 2D input value.

   !> The values from the input array are transferred to the 3d locations in the output array. A bamdwith can be specified,
   !! by using the bandwith_lower_limit and bandwith_upper_limit. If the bandwith is not specified, the values are transferred to all
   !! 3d grid cells
   subroutine initialfield2Dto3D(input_array_2d, output_array_3d, vertical_range_min, vertical_range_max, operand)
      use m_missing

      implicit none

      real(kind=dp), dimension(:), intent(inout), target :: input_array_2d !< The input array on 2d grid cells (1:ndx).
      real(kind=dp), dimension(:), intent(inout), target :: output_array_3d !< The output array on 3d grid cells (1:ndkx).
      real(kind=dp), intent(in) :: vertical_range_min !< Lower limit for the optional vertical range. Use dmiss for no custom range.
      real(kind=dp), intent(in) :: vertical_range_max !< Upper limit for the optional vertical range. Use dmiss for no custom range.
      character(len=*), intent(in) :: operand !< The operand to be used for combining the input field values with any previously set values.

      real(kind=dp), dimension(:, :), pointer :: output_array_3d_tmp

      output_array_3d_tmp(1:1, 1:size(output_array_3d)) => output_array_3d

      call initialfield2Dto3D_dbl_indx(input_array_2d, output_array_3d_tmp, 1, vertical_range_min, vertical_range_max, operand)

   end subroutine initialfield2Dto3D

   !> The values from the input array on 2D grid cells are copied to the 3D locations in the output array.
   !! Optionally, a vertical range can be specified, which then only updates the 3D output array elements if their vertical
   !! position lies within that range. Without this range, all 3D cells in a single  vertical column get the same 2D input value.
   subroutine initialfield2Dto3D_dbl_indx(input_array_2d, output_array_3d, first_index, vertical_range_min, vertical_range_max, operand)
      use m_flowgeom, only: ndx
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
      character(len=*), intent(in) :: operand !< The operand to be used for combining the input field values with any previously set values.

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
      do n = 1, ndx
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
