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
!

!> Reading + initializing of initial and parameter fields.
!! The IniFieldFile from the MDU is the successor of the old
!! *.ext file for quantities such as initialwaterlevel,
!! frictioncoefficient, etc.
module unstruc_inifields

   use unstruc_messages
   use properties
   use string_module, only: str_lower, strcmpi

   implicit none
   private ! Prevent used modules from being exported

   public :: init1dField, initialize_initial_fields, spaceInit1dField, readIniFieldProvider, checkIniFieldFileVersion, &
             set_friction_type_values

!> The file version number of the IniFieldFile format: d.dd, [config_major].[config_minor], e.g., 1.03
!!
!! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
!! Convention for format version changes:
!! * if a new format is backwards compatible with old files, only
!!   the minor version number is incremented.
!! * if a new format is not backwards compatible (i.e., old files
!!   need to be converted/updated by user), then the major version number
!!   is incremented.

! IniFieldFile current version: 2.00
   integer, parameter :: IniFieldMajorVersion = 2
   integer, parameter :: IniFieldMinorVersion = 0

! History IniFieldFile versions:

! 2.00 (2019-06-18): Added LocationType and changed ExtrapolationMethod to yes/no value.
! 1.01 (2019-03-12): First version of *.ini type initial fields and parameters file.

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
      call prop_get_version_number(inifield_ptr, major=major, minor=minor, success=success)
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
      double precision, intent(inout) :: array(:) !< Array to be changed
      logical(kind=c_bool), intent(in) :: negative_mask(:) !< True when value is not to be overwritten anymore
      double precision, intent(in) :: value !< Global value

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
      double precision, intent(in) :: bed_levels(:) !< Bed levels
      double precision, intent(inout) :: water_depths(:) !< Water depths
      double precision, intent(inout) :: water_levels(:) !< Water levels
      logical(kind=c_bool), intent(in) :: negative_mask(:) !< True when specified already
      character(len=*), intent(in) :: global_quantity !< Quantity specified by global_value
      double precision, intent(in) :: global_value !< Global value
      character(len=*), intent(in) :: ini_file_name !< Name of ini file, used for error messages

      integer, parameter :: enum_water_level = 0
      integer, parameter :: enum_water_depth = 1
      integer :: water_specifier
      logical(kind=c_bool), allocatable :: mask(:)

      if (strcmpi(global_quantity, 'waterlevel')) then
         water_specifier = enum_water_level
      else if (strcmpi(global_quantity, 'waterdepth')) then
         water_specifier = enum_water_depth
      else
         write (msgbuf, '(a)') 'File '''//trim(ini_file_name)// &
            ''': error while setting initial field values of quantities ''waterlevel'' and ''waterdepth''; Provided quantity name '''// &
            trim(global_quantity)//''' is invalid.'
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
      use m_alloc, only: realloc
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use messageHandling
      use unstruc_files, only: resolvePath
      use system_utils, only: split_filename
      use m_flow, only: s1, hs, frcu
      use m_flowgeom, only: ndx2d, ndxi, xz, yz, ndx, lnx, xu, yu, kcs, bl, iadv
      use timespace_parameters, only: FIELD1D
      use timespace, only: timespaceinitialfield, timespaceinitialfield_int
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use fm_external_forcings_data, only: qid, operand, transformcoef, success
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, &
                                  DFM_HYD_INTERCEPT_LAYER
      use unstruc_model, only: md_extfile
      use m_fm_icecover, only: fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U
      use fm_deprecated_keywords, only: deprecated_ext_keywords
      use m_deprecation, only: check_file_tree_for_deprecated_keywords

      implicit none
      character(len=*), intent(in) :: inifilename !< name of initial field file
      integer :: ierr !< Result status (DFM_NOERR on success)

      type(tree_data), pointer :: inifield_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      type(tree_data), pointer :: node_ptr
      integer, parameter :: ini_key_len = 32
      integer, parameter :: ini_value_len = 256
      character(len=ini_key_len) :: groupname
      character(len=ini_value_len) :: varname
      character(len=ini_value_len) :: global_water_level_quantity
      integer :: num_items_in_file
      character(len=255) :: fnam, filename
      character(len=255) :: basedir
      integer :: istat
      integer :: i, ib, ja, kx
      integer :: num_items
      integer :: target_location_type
      integer :: method, iloctype, filetype, ierr_loc
      logical(kind=c_bool), allocatable :: specified_water_levels(:) !< indices where waterlevels are specified with non-global values
      logical(kind=c_bool), allocatable :: specified_indices(:)
      double precision :: global_value, water_level_global_value
      logical :: global_value_provided, water_level_global_value_provided
      logical :: time_dependent_array
      integer, allocatable :: kcsini(:) ! node code during initialization

      logical, external :: timespaceinitialfield_mpi
      double precision, pointer, dimension(:) :: target_array, x_loc, y_loc
      integer, pointer, dimension(:) :: target_array_integer

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
         if (ja == 1) then
            call resolvePath(filename, basedir)
            ib = ib + 1
         else
            cycle
         end if
         if ((.not. strcmpi(groupname, 'Initial')) .and. (.not. strcmpi(groupname, 'Parameter'))) then
            cycle
         end if

      !! Step 2: operation for each block
         if (filetype == field1D) then
            ierr_loc = init1dField(filename, inifilename, qid, specified_indices, global_value, global_value_provided)
            if (ierr_loc /= DFM_NOERR) then
               success = .false.
               exit ! Or, consider cycle instead, to try all remaining blocks and return with an error only at the very end.
            end if
            if (strcmpi(qid, 'waterlevel') .or. strcmpi(qid, 'waterdepth') .or. strcmpi(qid, 'initialvelocity')) then
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
               call prepare_for_initial_items(qid, inifilename, target_location_type, time_dependent_array, target_array)
            else
               call prepare_for_parameter_items(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                                target_array_integer)
            end if

            if (.not. success) then
               ierr = DFM_WRONGINPUT
               cycle
            end if

            select case (target_location_type)
            case (UNC_LOC_S)
               call realloc(kcsini, ndx, keepExisting=.false.)
               call prepare_lateral_mask(kcsini, iloctype)
               x_loc => xz
               y_loc => yz
               num_items = ndx
            case (UNC_LOC_U)
               call realloc(kcsini, lnx, keepExisting=.false.)
               kcsini = 1
               x_loc => xu
               y_loc => yu
               num_items = lnx
            case default
            end select

            if (time_dependent_array) then
               kx = 1
               success = ec_addtimespacerelation(qid, x_loc, y_loc, kcs, kx, filename, filetype, method, operand, &
                                                 varname=varname)
            else
               if (associated(target_array)) then
                  success = timespaceinitialfield(x_loc, y_loc, target_array, num_items, filename, filetype, method, operand, &
                                                  transformcoef, target_location_type, kcsini) ! zie meteo module
               else if (associated(target_array_integer)) then
                  success = timespaceinitialfield_int(x_loc, y_loc, iadv, num_items, filename, filetype, operand, transformcoef)
               else
               end if
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

!> Reads all key values for a data provider from an IniFieldFile block.
!! All returned values will typically be used for a call to timespaceinitialfield().
   subroutine readIniFieldProvider(inifilename, node_ptr, groupname, quantity, filename, filetype, method, &
                                   iloctype, operand, transformcoef, ja, varname)
      use timespace_parameters
      use m_ec_interpolationsettings, only: RCEL_DEFAULT
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL
      use m_grw

      character(len=*), intent(in) :: inifilename !< Name of the ini file, only used in warning messages, actual data is read from node_ptr.
      type(tree_data), pointer :: node_ptr !< The tree structure containing a single ini-file chapter/block.
      character(len=*), intent(out) :: groupname !< Identifier of the read chapter (e.g., 'Initial')
      character(len=*), intent(out) :: quantity !< Identifier of current quantity (e.g., 'waterlevel')
      character(len=*), intent(out) :: filename !< Name of data file for current quantity.
      integer, intent(out) :: filetype !< File type of current quantity.
      integer, intent(out) :: method !< Time-interpolation method for current quantity.
      integer, intent(out) :: iloctype !< The spatial type of the target locations: 1D, 2D or all.
      character(len=1), intent(out) :: operand !< Operand w.r.t. previous data ('O'verride or '+'Append)
      double precision, intent(out) :: transformcoef(:) !< Transformation coefficients
      integer, intent(out) :: ja !< Whether a block was successfully read or not.
      character(len=*), intent(out) :: varname !< variable name within filename; only in case of NetCDF

      integer, parameter :: ini_key_len = 32
      integer, parameter :: ini_value_len = 256
      character(len=ini_value_len) :: dataFileType
      character(len=ini_value_len) :: interpolationMethod
      character(len=ini_value_len) :: averagingType
      character(len=ini_value_len) :: locationType
      integer :: iav, extrapolation, averagingNumMin
      logical :: retVal
      ja = 0
      groupname = tree_get_name(node_ptr)

      ! TODO: support reading from ini of varname, smask and maxSearchRadius.
      if (strcmpi(groupname, 'General')) then
         ja = 1
         goto 888
      end if

      transformcoef = -999d0

      if ((.not. strcmpi(groupname, 'Initial')) .and. (.not. (strcmpi(groupname, 'Parameter')))) then
         write (msgbuf, '(5a)') 'Unrecognized block in file ''', trim(inifilename), ''': [', trim(groupname), &
            ']. Ignoring this block.'
         call warn_flush()
         goto 888
      end if

      ! read quantity
      call prop_get(node_ptr, '', 'quantity', quantity, retVal)
      if (.not. retVal) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            ']. Field ''quantity'' is missing. Ignoring this block.'
         call warn_flush()
         goto 888
      end if

      ! read datafile
      call prop_get(node_ptr, '', 'dataFile', filename, retVal)
      if (retVal) then
      else
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            '] for quantity='//trim(quantity)//'. Field ''dataFile'' is missing. Ignoring this block.'
         call warn_flush()
         goto 888
      end if

      ! read dataFileType
      call prop_get(node_ptr, '', 'dataFileType ', dataFileType, retVal)
      if (.not. retVal) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), &
            '] for quantity='//trim(quantity)//'. Field ''dataFileType'' is missing. Ignoring this block.'
         call warn_flush()
         goto 888
      end if
      filetype = convert_file_type_string_to_integer(dataFileType)
      if (filetype < 0) then
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
            //trim(quantity)//'. Field ''dataFileType'' has invalid value '''//trim(dataFileType)//'''. Ignoring this block.'
         call warn_flush()
         goto 888
      end if

      ! if dataFileType is 1dField, then it is not necessary to read interpolationMethod, operand, averagingType,
      ! averagingRelSize, averagingNumMin, averagingPercentile, locationType, extrapolationMethod, value
      if (filetype /= field1D) then
         ! read interpolationMethod
         call prop_get(node_ptr, '', 'interpolationMethod ', interpolationMethod, retVal)
         if (.not. retVal) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
               //trim(quantity)//'. Field ''interpolationMethod'' is missing. Ignoring this block.'
            call warn_flush()
            goto 888
         end if
         method = convert_method_string_to_integer(interpolationMethod)
         if (method < 0 .or. (method == 4 .and. filetype /= inside_polygon)) then
            write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity=' &
               //trim(quantity)//'. Field ''interpolationMethod'' has invalid value '''//trim(interpolationMethod)// &
               '''. Ignoring this block.'
            call warn_flush()
            goto 888
         end if

         if (method == 6) then ! 'averaging'
            ! read averagingType
            call prop_get(node_ptr, '', 'averagingType ', averagingType, retVal)
            if (.not. retVal) then
               averagingType = 'mean'
            end if
            call averagingTypeStringToInteger(averagingType, iav)
            if (iav >= 0) then
               transformcoef(4) = dble(iav)
            else
               write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), '] for quantity='// &
                  trim(quantity)//'. Field ''averagingType'' has invalid value '''//trim(averagingType)//'''. Ignoring this block.'
               call warn_flush()
               goto 888
            end if

            ! read averagingRelSize
            call prop_get(node_ptr, '', 'averagingRelSize', transformcoef(5), retVal)
            if (.not. retVal) then
               transformcoef(5) = RCEL_DEFAULT
            else
               if (transformcoef(5) <= 0d0) then
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
               transformcoef(8) = 1d0
            else
               if (averagingNumMin <= 0) then
                  write (msgbuf, '(5a,i0,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                     '] for quantity='//trim(quantity)//'. Field ''averagingNumMin'' has invalid value ', averagingNumMin, &
                     '. Setting to default: 1.'
                  call warn_flush()
                  transformcoef(8) = 1d0
               else
                  transformcoef(8) = dble(averagingNumMin)
               end if
            end if

            ! read averagingPercentile
            call prop_get(node_ptr, '', 'averagingPercentile', transformcoef(7), retVal)
            if (.not. retVal) then
               transformcoef(7) = 0d0
            else
               if (transformcoef(7) < 0d0) then
                  write (msgbuf, '(5a,f10.3,a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                     '] for quantity='//trim(quantity)//'. Field ''averagingPercentile'' has invalid value ', &
                     transformcoef(7), '. Setting to default: 0.0.'
                  call warn_flush()
                  transformcoef(7) = 0d0
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
         if (.not. retVal) then
            extrapolation = 0
         end if
         method = method + 100 * extrapolation

         ! read value
         if (filetype == inside_polygon) then
            call prop_get(node_ptr, '', 'value', transformcoef(1), retVal)
            if (.not. retVal) then
               write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), ''': [', trim(groupname), &
                  '] for quantity='//trim(quantity)//'. Field ''value'' is missing. Ignore this block.'
               call warn_flush()
               goto 888
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
            goto 888
         end if
      end if

      varname = '' ! TODO: Support reading varname for NetCDF files as well.

      ! We've made it to here, success!
      ja = 1
      return

888   continue
      ! Some error occurred, return without setting ja=1
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
      double precision, intent(out) :: value !< The global value to be read
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
      if (.not. strcmpi(quantity, intended_quantity)) then
         num_errors = num_errors + 1
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(ini_file_name), &
            ''': [Global]. Field ''quantity'' does not match the "quantity" which is specified in iniField file ''', &
            trim(ini_field_file_name), '''.'
         call err_flush()
         return
      end if
      if ((.not. strcmpi(quantity, 'bedlevel')) .and. (.not. strcmpi(quantity, 'waterlevel')) .and. &
          (.not. strcmpi(quantity, 'waterdepth')) .and. (.not. strcmpi(quantity, 'frictioncoefficient')) .and. &
          (.not. strcmpi(quantity, 'initialvelocity'))) then
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
      double precision, intent(in) :: bed_levels(:) !< Bed levels
      double precision, intent(in) :: water_depths(:) !< Water depths
      double precision, intent(inout) :: water_levels(:) !< Water levels
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
      double precision, intent(in) :: bed_levels(:) !< Bed levels
      double precision, intent(inout) :: water_depths(:) !< Water depths
      double precision, intent(in) :: water_levels(:) !< Water levels
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
      double precision, intent(out) :: global_value !< Provides global value to be applied to unset values
      logical, intent(out) :: global_value_provided !< Indicates whether a global value was provided
      integer :: ierr !< Result status (DFM_NOERR on success)

      type(tree_data), pointer :: field_ptr !< tree of inifield-file's [Initial] or [Parameter] blocks
      type(tree_data), pointer :: node_ptr !
      integer :: istat !
      integer, parameter :: ini_key_len = 32 !
      integer, parameter :: ini_value_len = 256 !
      character(len=ini_key_len) :: groupname !
      character(len=ini_value_len) :: branchId !
      double precision, allocatable :: values(:) !
      integer :: numLocations !
      double precision, allocatable :: chainage(:) !
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

      if (strcmpi(quant, 'waterlevel') .or. strcmpi(quant, 'waterdepth') .or. strcmpi(quant, 'bedlevel') .or. &
          strcmpi(quant, 'initialvelocity')) then
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
         else if (strcmpi(quant, 'initialvelocity')) then
            call spaceInit1dfield(branchId, chainage, values, 1, u1(1:lnx1d), specified_indices)
         else if (strcmpi(quant, 'bedlevel')) then
            !call spaceInit1dfield(branchId, chainage, values, 2, zk(ndx2D+1:ndxi), specified_indices)
            ! TODO: UNST-2694, Reading bedlevel from 1dField file type is not yet supported.
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
      double precision, intent(in) :: sChainages(:) !< Sample chainages
      double precision, intent(in) :: sValues(:) !< Sample values
      integer, intent(in) :: ipos !< position: 1= u point location, 2= 1d flownode(netnode) location
      double precision, intent(inout) :: res(:) !< Flow state array into which the interpolated values will be stored.
                                                !!Should be only the 1D slice (especially in the case of ipos==2, flow nodes).
      logical(kind=c_bool), intent(inout) :: modified_elements(:) !< true for every index for which res was set

      integer :: nbrstart, ibr, k, j, i, ipre, ns, ncount
      integer :: is, ip1, ip2, ipe
      type(t_branch), pointer :: pbr
      double precision :: chai, sChaiPrev, sChai, sValPrev, sVal, minsChai, maxsChai

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

      use fm_external_forcings_data, only: operand, transformcoef
      use m_flow, only: ifrctypuni, ifrcutp, frcu
      use m_flowgeom, only: lnx
      use m_missing, only: dmiss

      implicit none

      integer :: link

      if (transformcoef(3) /= -999d0 .and. int(transformcoef(3)) /= ifrctypuni .and. (operand == 'O' .or. operand == 'V')) then
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
      use m_subsidence, only: sdu_blp, subsupl_t0, subsupl, subsout, subsupl_tp, jasubsupl
      use m_flowparameters, only: ibedlevtyp
      use m_meteo, only: ec_addtimespacerelation
      ! use m_flow, only:
      use network_data, only: numk, xk, yk
      use m_flowgeom, only: lnx
      use m_alloc, only: aerr

      implicit none

      integer, allocatable :: mask(:)
      integer :: kx, ierr
      logical :: success
      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6

      kx = 1
      if (allocated(subsupl)) deallocate (subsupl)
      if (allocated(subsupl_t0)) deallocate (subsupl_t0)
      if (allocated(subsupl_tp)) deallocate (subsupl_tp)
      if (allocated(subsout)) deallocate (subsout)
      if (allocated(sdu_blp)) deallocate (sdu_blp)

      select case (ibedlevtyp)
      case (enum_field1D)
         allocate (subsupl(lnx), stat=ierr)
         call aerr('subsupl(lnx)', ierr, lnx)
         subsupl = 0d0
         allocate (subsupl_t0(lnx), stat=ierr)
         call aerr('subsupl_t0(lnx)', ierr, lnx)
         subsupl_t0 = 0d0
         allocate (subsupl_tp(lnx), stat=ierr)
         call aerr('subsupl_tp(lnx)', ierr, lnx)
         subsupl_tp = 0d0
         allocate (subsout(lnx), stat=ierr)
         call aerr('subsout(lnx)', ierr, lnx)
         subsout = 0d0

      case (enum_field2D)
         if (allocated(mask)) deallocate (mask)
         allocate (mask(lnx), source=1, stat=ierr)
         call aerr('mask(lnx)', ierr, lnx)
         allocate (subsupl(lnx), stat=ierr)
         call aerr('subsupl(lnx)', ierr, lnx)
         subsupl = 0d0
         allocate (subsupl_t0(lnx), stat=ierr)
         call aerr('subsupl_t0(lnx)', ierr, lnx)
         subsupl_t0 = 0d0
         allocate (subsupl_tp(lnx), stat=ierr)
         call aerr('subsupl_tp(lnx)', ierr, lnx)
         subsupl_tp = 0d0
         allocate (subsout(lnx), stat=ierr)
         call aerr('subsout(lnx)', ierr, lnx)
         subsout = 0d0

      case (enum_field3D, enum_field4D, enum_field5D, enum_field6D)
         if (allocated(mask)) deallocate (mask)
         allocate (mask(numk), source=1, stat=ierr)
         call aerr('mask(numk)', ierr, numk)
         allocate (subsupl(numk), stat=ierr)
         call aerr('subsupl(numk)', ierr, numk)
         subsupl = 0d0
         allocate (subsupl_t0(numk), stat=ierr)
         call aerr('subsupl_t0(numk)', ierr, numk)
         subsupl_t0 = 0d0
         allocate (subsupl_tp(numk), stat=ierr)
         call aerr('subsupl_tp(numk)', ierr, numk)
         subsupl_tp = 0d0
         allocate (subsout(numk), stat=ierr)
         call aerr('subsout(numk)', ierr, numk)
         subsout = 0d0
      end select

      allocate (sdu_blp(lnx), stat=ierr)
      call aerr('sdu_blp(lnx)', ierr, lnx)
      sdu_blp = 0d0

      if (success) then
         jasubsupl = 1
      end if
   end subroutine initialize_subsupl

   !> Set the control parameters for the actual reading of the [Initial] type items from the inifields input file.
   subroutine prepare_for_initial_items(qid, inifilename, target_location_type, time_dependent_array, target_array)
      use stdlib_kinds, only: c_bool
      use system_utils, only: split_filename
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_alloc, only: realloc, aerr
      use unstruc_files, only: resolvePath
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN
      use m_flow, only: s1, hs
      use m_flow, only: h_unsat
      use m_flowgeom, only: ndx
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use fm_external_forcings_data, only: success
      use m_fm_icecover, only: fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation
      use unstruc_model, only: md_extfile
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, DFM_HYD_INTERCEPT_LAYER
      use m_hydrology_data, only: infiltcap, infiltrationmodel
      ! use network_data
      ! use dfm_error

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of the quantity.
      integer, intent(out) :: target_location_type !< Type of the quantity, either UNC_LOC_S or UNC_LOC_U.
      logical, intent(out) :: time_dependent_array !< Logical indicating, whether the quantity is time dependent or not.
      double precision, dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (double precision).

      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6
      integer :: ierr

      target_array => null()
      time_dependent_array = .false.

      select case (str_tolower(qid))
      case ('waterlevel')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => s1
      case ('waterdepth')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => hs
      case ('infiltrationcapacity')
         if (infiltrationmodel /= DFM_HYD_INFILT_CONST) then
            write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(qid) &
               //'''. This requires ''InfiltrationModel=', DFM_HYD_INFILT_CONST, ''' in the MDU file (constant).'
            call warn_flush() ! No error, just warning and continue
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => infiltcap
      case ('initial', 'bedlevel')
         ! Bed level was earlier set in setbedlevelfromextfile()
      case ('initialunsaturedzonethickness')

         if (.not. allocated(h_unsat)) then
            allocate (h_unsat(ndx), stat=ierr)
            call aerr('h_unsat(ndx)', ierr, ndx)
            h_unsat = -999d0
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => h_unsat
      case default
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), &
            ' Field '''//trim(qid)//''' is not allowed in the ''[Initial]'' section (refer to User Manual). Ignoring this block.'
         call warn_flush()
         success = .false.
      end select

   end subroutine prepare_for_initial_items

   !> Set the control parameters for the actual reading of the [Initial] type items from the input file/connecting
   !! the input to the EC-module.
   subroutine prepare_for_parameter_items(qid, inifilename, target_location_type, time_dependent_array, target_array, &
                                          target_array_integer)
      use stdlib_kinds, only: c_bool
      use system_utils, only: split_filename
      use tree_data_types
      use tree_structures
      use messageHandling
      use m_alloc, only: realloc, aerr
      use unstruc_files, only: resolvePath
      use m_missing, only: dmiss
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN
      use m_flowparameters, only: jatrt, javiusp, jafrcInternalTides2D, jadiusp, jafrculin, jaCdwusp, ibedlevtyp, jawave
      use m_flow, only: frcu, h_unsat
      use m_flow, only: jacftrtfac, cftrtfac, viusp, diusp, DissInternalTidesPerArea, frcInternalTides2D, frculin, Cdwusp
      use m_flowgeom, only: ndx, lnx, grounlay, iadv, jagrounlay, ibot
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use fm_external_forcings_data, only: success
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, DFM_HYD_INTERCEPT_LAYER
      use m_wind, only: ICdtyp
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation
      use m_vegetation, only: stemdiam, stemdens, stemheight
      use unstruc_model, only: md_extfile
      use m_hydrology_data, only: DFM_HYD_INFILT_CONST, &
                                  HortonMinInfCap, HortonMaxInfCap, HortonDecreaseRate, HortonRecoveryRate, &
                                  InterceptThickness, interceptionmodel, DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, InterceptHs
      use m_hydrology_data, only: infiltcap, infiltrationmodel

      implicit none

      character(len=*), intent(in) :: qid !< Name of the quantity.
      character(len=*), intent(in) :: inifilename !< Name of initial field file, should already be opened in inifield_ptr.
      integer, intent(out) :: target_location_type !< Type of the quantity either UNC_LOC_S or UNC_LOC_U.
      logical, intent(out) :: time_dependent_array !< Logical indicating whether the quantity is time dependent or not.
      double precision, dimension(:), pointer, intent(out) :: target_array !< pointer to the array that corresponds to the quantity (double precision).
      integer, dimension(:), pointer, intent(out) :: target_array_integer !< pointer to the array that corresponds to the quantity (integer).

      integer, parameter :: enum_field1D = 1, enum_field2D = 2, enum_field3D = 3, enum_field4D = 4, enum_field5D = 5, &
                            enum_field6D = 6
      integer :: ierr

      target_array => null()
      target_array_integer => null()
      time_dependent_array = .false.

      select case (str_tolower(qid))
      case ('frictioncoefficient')
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
         target_array => frcu
      case ('advectiontype')
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
         target_array_integer => iadv

      case ('groundlayerthickness')
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
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

      case ('frictiontrtfactor')

         if (jatrt /= 1) then
            call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting QUANTITY '//trim(qid)// &
                      ', but [trachytopes] is not switched on in MDU file. Ignoring this block.')
            success = .false.
         else
            if (.not. allocated(cftrtfac)) then
               allocate (cftrtfac(lnx), stat=ierr)
               call aerr('cftrtfac(lnx)', ierr, lnx)
               cftrtfac = 1d0
            end if
            target_location_type = UNC_LOC_U
            time_dependent_array = .false.
            target_array => cftrtfac
            jacftrtfac = 1
         end if

      case ('horizontaleddyviscositycoefficient')

         if (javiusp == 0) then
            if (allocated(viusp)) deallocate (viusp)
            allocate (viusp(lnx), stat=ierr)
            call aerr('viusp(lnx)', ierr, lnx)
            viusp = dmiss
            javiusp = 1
         end if
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
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
         time_dependent_array = .false.
         target_array => diusp

      case ('ibedlevtype')
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
         target_array_integer => ibot

      case ('internaltidesfrictioncoefficient')

         if (jaFrcInternalTides2D /= 1) then ! not added yet
            if (allocated(frcInternalTides2D)) deallocate (frcInternalTides2D)
            allocate (frcInternalTides2D(Ndx), stat=ierr)
            call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
            frcInternalTides2D = DMISS

            if (allocated(DissInternalTidesPerArea)) deallocate (DissInternalTidesPerArea)
            allocate (DissInternalTidesPerArea(Ndx), stat=ierr)
            call aerr(' DissInternalTidesPerArea(Ndx)', ierr, Ndx)
            DissInternalTidesPerArea = 0d0

            jaFrcInternalTides2D = 1
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => frcInternalTides2D

      case ('linearfrictioncoefficient')
         target_location_type = UNC_LOC_U
         time_dependent_array = .false.
         target_array => frculin
         jafrculin = 1
      case ('sea_ice_area_fraction', 'sea_ice_thickness')

! if ice properties not yet read before, initialize ...
         if (.not. (ja_ice_area_fraction_read .or. ja_ice_thickness_read)) then
            call fm_ice_activate_by_ext_forces(ndx)
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .true.
         if (qid == 'sea_ice_area_fraction') then
            ja_ice_area_fraction_read = 1
         else
            ja_ice_thickness_read = 1
         end if
      case ('stemdiameter')

         if (.not. allocated(stemdiam)) then
            allocate (stemdiam(ndx), stat=ierr)
            call aerr('stemdiam(ndx)', ierr, ndx)
            stemdiam = dmiss
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => stemdiam

      case ('stemdensity')

         if (.not. allocated(stemdens)) then
            allocate (stemdens(ndx), stat=ierr)
            call aerr('stemdens(ndx)', ierr, ndx)
            stemdens = dmiss
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => stemdens

      case ('stemheight')

         if (.not. allocated(stemheight)) then
            allocate (stemheight(ndx), stat=ierr)
            call aerr('stemheight(ndx)', ierr, ndx)
            stemheight = dmiss
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
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
         time_dependent_array = .false.
         target_array => Cdwusp
         iCdtyp = 1 ! only 1 coeff
      case ('wavesignificantheight')
         if (jawave == 6 .or. jawave == 7) then
            target_location_type = UNC_LOC_S
            time_dependent_array = .true.
         else
            call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)// &
                      ''', QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7')
            call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', &
                         'QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7', trim(qid))
            success = .false.
         end if
      case ('hortonmininfcap')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => HortonMinInfCap
      case ('hortonmaxinfcap')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => HortonMaxInfCap
      case ('hortondecreaserate')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => HortonDecreaseRate
      case ('hortonrecoveryrate')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => HortonRecoveryRate
      case ('interceptionlayerthickness')
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         call realloc(InterceptHs, ndx, keepExisting=.false., fill=0d0)
         call realloc(h_unsat, ndx, keepExisting=.true., fill=0d0)
         call realloc(InterceptThickness, ndx, keepExisting=.false.)
         target_array => InterceptThickness
         interceptionmodel = DFM_HYD_INTERCEPT_LAYER
         jadhyd = 1
      case ('potentialevaporation')
         call realloc(PotEvap, ndx, keepExisting=.true., fill=0d0)
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => PotEvap
      case ('infiltrationcapacity')
         if (infiltrationmodel /= DFM_HYD_INFILT_CONST) then
            write (msgbuf, '(a,i0,a)') 'File '''//trim(inifilename)//''' contains quantity '''//trim(qid) &
               //'''. This requires ''InfiltrationModel=', DFM_HYD_INFILT_CONST, ''' in the MDU file (constant).'
            call warn_flush() ! No error, just warning and continue
         end if
         target_location_type = UNC_LOC_S
         time_dependent_array = .false.
         target_array => infiltcap
      case default
         write (msgbuf, '(5a)') 'Wrong block in file ''', trim(inifilename), &
            ' Field '''//trim(qid)//''' is not allowed in the ''[Parameters]'' section (refer to User Manual). Ignoring this block.'
         call warn_flush()
         success = .false.
      end select

   end subroutine prepare_for_parameter_items

   !> Perform finalization after reading the input file.
   subroutine finish_initialization(qid)
      use stdlib_kinds, only: c_bool
      use tree_data_types
      use tree_structures
      use m_missing, only: dmiss
      use m_alloc, only: realloc
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT
      use messageHandling
      use unstruc_files, only: resolvePath
      use system_utils, only: split_filename
      ! use m_ec_interpolationsettings
      use m_flow, only: s1, hs, evap, h_unsat
      use m_flowgeom, only: ndxi, ndx, bl
      use timespace_parameters, only: FIELD1D
      use m_wind, only: jaevap
      use timespace, only: timespaceinitialfield
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      ! use network_data
      use m_hydrology_data, only: infiltcap, DFM_HYD_INFILT_CONST, &
                                  DFM_HYD_INTERCEPT_LAYER, jadhyd, &
                                  PotEvap, ActEvap
      use unstruc_model, only: md_extfile
      use m_grw, only: jaintercept2D
      use m_fm_icecover, only: fm_ice_activate_by_ext_forces
      use m_meteo, only: ec_addtimespacerelation
      use m_vegetation, only: stemheight, stemheightstd
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U

      implicit none

      character(len=*), intent(in) :: qid !< Quantity identifier.

      integer :: idum
      double precision, external :: ran0

      select case (str_tolower(qid))
      case ('waterdepth')
         s1(1:ndxi) = bl(1:ndxi) + hs(1:ndxi)
      case ('infiltrationcapacity')
         where (infiltcap /= dmiss)
            infiltcap = infiltcap * 1d-3 / (24d0 * 3600d0) ! mm/day => m/s
         end where
      case ('potentialevaporation')
         where (PotEvap /= dmiss)
            PotEvap = PotEvap * 1d-3 / (3600d0) ! mm/hr => m/s
         end where
         jaevap = 1
         if (.not. allocated(evap)) then
            call realloc(evap, ndx, keepExisting=.false., fill=0d0)
         end if
         evap = -PotEvap ! evap and PotEvap are now still doubling

         if (.not. allocated(ActEvap)) then
            call realloc(ActEvap, ndx, keepExisting=.false., fill=0d0)
         end if
         jadhyd = 1
      case ('frictioncoefficient')
         call set_friction_type_values()
      case ('initialunsaturedzonethickness', 'interceptionlayerthickness')
         where (h_unsat == -999d0)
            h_unsat = 0d0
         end where
         if (qid == 'interceptionlayerthickness') then
            jaintercept2D = 1
         end if
      case ('stemheight')
         if (stemheightstd > 0d0) then
            stemheight = stemheight * (1d0 + stemheightstd * (ran0(idum) - 0.5d0))
         end if
      end select
   end subroutine finish_initialization

end module unstruc_inifields
