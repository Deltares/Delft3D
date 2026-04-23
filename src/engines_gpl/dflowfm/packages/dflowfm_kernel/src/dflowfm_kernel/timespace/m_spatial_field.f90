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
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of
!  Stichting Deltares, and remain the property of Stichting Deltares.
!  All rights reserved.
!
!-------------------------------------------------------------------------------

!> Struct definitions and block readers for spatial/meteo and initial/parameter fields.
!!
!! Design rules:
!!  - t_spatial_field_input carries exactly what ec_addtimespacerelation needs:
!!    quantity, file, filetype, method, operand, mask file, variable name,
!!    extrapolation settings. Nothing quantity-specific lives here.
!!  - t_averaging_input groups the four averaging keywords that are only
!!    meaningful when method=averaging. It is a separate type so that the
!!    parent struct stays flat. Used locally by callers that need averaging
!!    (e.g. qext, inifields); never embedded in t_spatial_field_input.
!!  - Quantity-specific keywords (frictionType, value for insidePolygon, etc.)
!!    are read locally in the branch that needs them.
module m_spatial_field
   use precision, only: dp
   use timespace_parameters, only: OPERAND_OVERRIDE
   implicit none(type, external)

   private

   public :: t_spatial_field_input, t_averaging_input
   public :: read_spatial_field_block, validate_spatial_field_input
   public :: read_averaging_params

   integer, parameter :: INI_VALUE_LEN = 256

   !> All parsed keyword values from a single [Spatial] or [Meteo] block.
   !! Contains exactly what is needed to call ec_addtimespacerelation.
   !! Quantity-specific keywords are NOT stored here; read them locally
   !! in the branch that handles that quantity.
   type :: t_spatial_field_input
      character(len=INI_VALUE_LEN) :: quantity            = ' ' !< quantity= e.g. 'windx', 'rainfall_rate'.
      character(len=INI_VALUE_LEN) :: forcing_file        = ' ' !< forcingFile= resolved relative to base_dir.
      character(len=INI_VALUE_LEN) :: forcing_file_type   = ' ' !< forcingFileType= e.g. 'netcdf', 'arcinfo', 'bcascii'.
      character(len=INI_VALUE_LEN) :: target_mask_file    = ' ' !< targetMaskFile= optional polygon mask; empty = no mask.
      character(len=INI_VALUE_LEN) :: variable_name       = ' ' !< forcingVariableName= only meaningful when is_variable_name_available.
      character(len=INI_VALUE_LEN) :: interpolation_method = ' '!< interpolationMethod= optional; default derived from forcingFileType.
      character(len=INI_VALUE_LEN) :: operand_string      = ' ' !< operand= optional string; default produces OPERAND_OVERRIDE.
      integer                      :: oper                = OPERAND_OVERRIDE !< Derived from operand_string.
      real(dp)                     :: max_search_radius   = -1.0_dp           !< extrapolationSearchRadius= negative = no limit.
      logical                      :: invert_mask         = .false.           !< targetMaskInvert= invert the mask polygon selection.
      logical                      :: is_variable_name_available = .false.    !< .true. when forcingVariableName= was present.
      logical                      :: is_extrapolation_allowed   = .false.    !< extrapolationAllowed=
      integer                      :: method              = -1                !< FM method enum, set by validate_spatial_field_input.
      integer                      :: filetype            = -1                !< FM filetype enum, set by validate_spatial_field_input.
   end type t_spatial_field_input

   !> Averaging parameters, only meaningful when method = averaging.
   !! Grouped here so they do not pollute any parent type.
   !! Read and used locally by any caller that needs averaging
   !! (qext, inifields); never stored on t_spatial_field_input.
   type :: t_averaging_input
      character(len=INI_VALUE_LEN) :: type_string = 'mean' !< averagingType=
      real(dp)                     :: rel_size    = -1.0_dp !< averagingRelSize= negative = use EC default.
      integer                      :: num_min     = 1       !< averagingNumMin=
      real(dp)                     :: percentile  = 0.0_dp  !< averagingPercentile=
   end type t_averaging_input

contains

   !> Read all keyword values from a [Spatial] or [Meteo] block into a t_spatial_field_input.
   !! Does no validation; call validate_spatial_field_input afterwards.
   function read_spatial_field_block(block_ptr) result(res)
      use tree_data_types, only: tree_data
      use properties, only: prop_get

      type(tree_data), pointer, intent(in) :: block_ptr !< Tree node for the current [Spatial]/[Meteo] block.
      type(t_spatial_field_input) :: res

      call prop_get(block_ptr, '', 'quantity',                res%quantity)
      call prop_get(block_ptr, '', 'forcingFileType',         res%forcing_file_type)
      call prop_get(block_ptr, '', 'forcingFile',             res%forcing_file)
      call prop_get(block_ptr, '', 'targetMaskFile',          res%target_mask_file)
      call prop_get(block_ptr, '', 'targetMaskInvert',        res%invert_mask)
      call prop_get(block_ptr, '', 'forcingVariableName',     res%variable_name, res%is_variable_name_available)
      call prop_get(block_ptr, '', 'interpolationMethod',     res%interpolation_method)
      call prop_get(block_ptr, '', 'extrapolationAllowed',    res%is_extrapolation_allowed)
      call prop_get(block_ptr, '', 'extrapolationSearchRadius', res%max_search_radius)
      call prop_get(block_ptr, '', 'operand ',                res%operand_string)

   end function read_spatial_field_block

   !> Read averaging keywords from any ini-file block into a t_averaging_input.
   !! Call this locally in any branch that requires averaging, not at general read time.
   subroutine read_averaging_params(block_ptr, avg)
      use tree_data_types, only: tree_data
      use properties, only: prop_get
      use m_ec_interpolationsettings, only: RCEL_DEFAULT

      type(tree_data), pointer, intent(in) :: block_ptr !< Tree node to read from.
      type(t_averaging_input), intent(out) :: avg        !< Populated on return.

      logical :: is_read

      avg = t_averaging_input() ! initialise to defaults

      call prop_get(block_ptr, '', 'averagingType',       avg%type_string, is_read)
      call prop_get(block_ptr, '', 'averagingRelSize',    avg%rel_size,    is_read)
      if (is_read .and. avg%rel_size <= 0.0_dp) avg%rel_size = RCEL_DEFAULT
      call prop_get(block_ptr, '', 'averagingNumMin',     avg%num_min,     is_read)
      if (is_read .and. avg%num_min < 1) avg%num_min = 1
      call prop_get(block_ptr, '', 'averagingPercentile', avg%percentile,  is_read)
      if (is_read .and. avg%percentile < 0.0_dp) avg%percentile = 0.0_dp

   end subroutine read_averaging_params

   !> Validate a t_spatial_field_input read by read_spatial_field_block.
   !! Derives method and filetype. Returns .false. and writes error messages on failure.
   function validate_spatial_field_input(input, file_name, group_name, base_dir) result(is_successful)
      use messageHandling, only: err_flush, msgbuf
      use timespace, only: convert_method_string_to_integer, get_default_method_for_file_type, &
                           update_method_with_weightfactor_fallback, update_method_in_case_extrapolation, &
                           convert_file_type_string_to_integer
      use m_wind, only: jaQext
      use string_module, only: strcmpi
      use unstruc_files, only: resolvePath
      use timespace_parameters, only: OPERAND_UNKNOWN, convert_operand_string_to_integer

      type(t_spatial_field_input), intent(inout) :: input     !< The block to validate; method and filetype are set on success.
      character(len=*), intent(in)               :: file_name  !< Ext file name, used only in error messages.
      character(len=*), intent(in)               :: group_name !< Block name, e.g. 'Spatial', used only in error messages.
      character(len=*), intent(in)               :: base_dir   !< Base directory for resolving relative paths.

      logical :: is_successful
      logical :: has_interpolation_method, target_mask_file_exists

      is_successful = .false.

      if (len_trim(input%quantity) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
         call err_flush()
         return
      end if

      if (len_trim(input%forcing_file_type) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFileType'' is missing.'
         call err_flush()
         return
      end if

      if (len_trim(input%forcing_file) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
         call err_flush()
         return
      end if

      ! Resolve paths relative to the ext file base directory
      call resolvePath(input%forcing_file, base_dir)
      if (len_trim(input%target_mask_file) > 0) then
         call resolvePath(input%target_mask_file, base_dir)
         inquire (file=trim(input%target_mask_file), exist=target_mask_file_exists)
         if (.not. target_mask_file_exists) then
            write (msgbuf, '(7a)') 'Invalid block in file ''', file_name, ''': [', group_name, &
               ']. targetMaskFile ''', trim(input%target_mask_file), ''' does not exist.'
            call err_flush()
            return
         end if
      end if

      ! Check for file extension conflicts
      if (file_extension_conflicts_with_type(input%forcing_file, input%forcing_file_type)) then
         write (msgbuf, '(9a)') 'Invalid block in file ''', file_name, ''': [', group_name, &
            ']. forcingFile ''', trim(input%forcing_file), ''' has a file extension that conflicts with forcingFileType ''', &
            trim(input%forcing_file_type), '''.'
         call err_flush()
         return
      end if

      if (len_trim(input%operand_string) > 0) then
         input%oper = convert_operand_string_to_integer(input%operand_string)
         if (input%oper == OPERAND_UNKNOWN) then
            write (msgbuf, '(5a)') 'Invalid block in file ''', file_name, ''': [', group_name, ']. Unknown operand.'
            call err_flush()
            return
         end if
      end if

      ! Derive method
      has_interpolation_method = len_trim(input%interpolation_method) > 0
      if (has_interpolation_method) then
         input%method = convert_method_string_to_integer(input%interpolation_method)
         call update_method_with_weightfactor_fallback(input%forcing_file_type, input%method)
      else
         input%method = get_default_method_for_file_type(input%forcing_file_type)
      end if

      if (input%method == -1) then
         if (has_interpolation_method) then
            write (msgbuf, '(7a)') 'There is no method associated with ''interpolationMethod'' ', &
               trim(input%interpolation_method), ' in block in file ''', file_name, ''': [', group_name, '].'
         else
            write (msgbuf, '(7a)') 'Block contains no ''interpolationMethod'' in file ''', file_name, ''': [', group_name, &
               '] nor an internal value associated with given ''forcingFileType'':', trim(input%forcing_file_type), '.'
         end if
         call err_flush()
         return
      end if

      call update_method_in_case_extrapolation(input%method, input%is_extrapolation_allowed)

      ! Derive filetype
      input%filetype = convert_file_type_string_to_integer(input%forcing_file_type)

      ! Quantity-specific constraints
      select case (trim(input%quantity))
      case ('qext')
         if (jaQext == 0) then
            write (msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, &
               ']. quantity ''qext'' requires QExt=1 in MDU.'
            call err_flush()
            return
         end if
         if (.not. strcmpi(input%forcing_file_type, 'sample')) then
            write (msgbuf, '(7a)') 'Invalid block in file ''', file_name, ''': [', group_name, &
               ']. quantity ''qext'' requires forcingFileType=sample, got: ', trim(input%forcing_file_type), '.'
            call err_flush()
            return
         end if
      end select

      is_successful = .true.

   end function validate_spatial_field_input

   function file_extension_conflicts_with_type(forcing_file, forcing_file_type) result(conflicts)
      use string_module, only: str_tolower
      character(len=*), intent(in) :: forcing_file
      character(len=*), intent(in) :: forcing_file_type
      logical :: conflicts

      integer :: dot_pos
      character(len=16) :: ext

      conflicts = .false.
      dot_pos = index(trim(forcing_file), '.', back=.true.)
      if (dot_pos == 0) return

      ext = str_tolower(trim(forcing_file(dot_pos:)))

      select case (ext)
      case ('.nc')
         conflicts = str_tolower(trim(forcing_file_type)) /= 'netcdf'
      case ('.tif', '.tiff')
         conflicts = str_tolower(trim(forcing_file_type)) /= 'geotiff'
      case ('.spw')
         conflicts = str_tolower(trim(forcing_file_type)) /= 'spiderweb'
      end select

   end function file_extension_conflicts_with_type

end module m_spatial_field