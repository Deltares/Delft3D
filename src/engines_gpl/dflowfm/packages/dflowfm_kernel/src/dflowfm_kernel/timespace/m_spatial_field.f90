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

module m_spatial_field
   use precision, only: dp
   use timespace_parameters, only: OPERAND_OVERRIDE
   implicit none(type, external)

   private

   public t_spatial_field_input, read_spatial_field_block, validate_spatial_field_input

   integer, parameter :: INI_VALUE_LEN = 256

      !> Holds all parsed keyword values from a single [Spatial] / [Meteo] block.
   type :: t_spatial_field_input
      character(len=INI_VALUE_LEN) :: quantity = ' '             !< Physical quantity name, e.g. 'windx', 'rainfall_rate'.
      character(len=INI_VALUE_LEN) :: forcing_file = ' '         !< Path to the forcing data file; resolved relative to base_dir during validation.
      character(len=INI_VALUE_LEN) :: forcing_file_type = ' '    !< File format identifier, e.g. 'netcdf', 'arcinfo', 'bcascii'.
      character(len=INI_VALUE_LEN) :: target_mask_file = ' '     !< Optional polygon file (.pol) masking the target element set. Empty means no masking.
      character(len=INI_VALUE_LEN) :: variable_name = ' '        !< Optional variable name within the forcing file. Only meaningful when is_variable_name_available is .true..
      character(len=INI_VALUE_LEN) :: interpolation_method = ' ' !< Optional interpolation method string, e.g. 'triangulation'. When absent, a default is derived from forcing_file_type.
      character(len=INI_VALUE_LEN) :: operand_string = ' '       !< Optional operand string, e.g. 'override'. When absent, OPERAND_OVERRIDE is used.
      integer :: oper = OPERAND_OVERRIDE                        !< Operand enum, derived from operand_string, defaulting to OPERAND_OVERRIDE.                            
      real(dp) :: max_search_radius = -1.0_dp                    !< Maximum search radius (m) for spatial extrapolation. Negative means no limit.
      logical :: invert_mask = .false.                           !< .true., the mask polygon selection must be inverted.
      logical :: is_variable_name_available = .false.            !< .true. when the forcingVariableName= keyword was present in the block.
      logical :: is_extrapolation_allowed = .false.              !< .true. when extrapolation beyond the source data extent is permitted.
      integer :: method = -1                                     !< FM interpolation method enum, derived by validate_spatial_field_input. -1 = not yet derived.
      integer :: filetype = -1                                   !< FM file type enum, derived by validate_spatial_field_input. -1 = not yet derived.
   end type t_spatial_field_input

contains

!> Read all keyword values from a [Spatial] block into a t_spatial_field_input.
   function read_spatial_field_block(block_ptr) result(res)
      use tree_data_types, only: tree_data
      use properties, only: prop_get

      type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to the ini-file tree node for the current [Spatial] / [Meteo] block.
      type(t_spatial_field_input) :: res

      call prop_get(block_ptr, '', 'quantity', res%quantity)
      call prop_get(block_ptr, '', 'forcingFileType', res%forcing_file_type)
      call prop_get(block_ptr, '', 'forcingFile', res%forcing_file)
      call prop_get(block_ptr, '', 'targetMaskFile', res%target_mask_file)
      call prop_get(block_ptr, '', 'targetMaskInvert', res%invert_mask)
      call prop_get(block_ptr, '', 'forcingVariableName', res%variable_name, res%is_variable_name_available)
      call prop_get(block_ptr, '', 'interpolationMethod', res%interpolation_method)
      call prop_get(block_ptr, '', 'extrapolationAllowed', res%is_extrapolation_allowed)
      call prop_get(block_ptr, '', 'extrapolationSearchRadius', res%max_search_radius)
      call prop_get(block_ptr, '', 'operand ', res%operand_string)

   end function read_spatial_field_block

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

      type(t_spatial_field_input), intent(inout) :: input !< The spatial field input to validate; method and filetype are set on success.
      character(len=*), intent(in) :: file_name           !< Name of the ext file, used only in error messages.
      character(len=*), intent(in) :: group_name          !< Name of the current block (e.g. 'Spatial'), used only in error messages.
      character(len=*), intent(in) :: base_dir            !< Base directory of the ext file, used to resolve relative paths for forcing_file and target_mask_file.


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
            ']. forcingFile ''', trim(input%forcing_file), ''' has a file extension that conflicts with forcingFileType ''', trim(input%forcing_file_type), '''.'
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

      if (input%method == -1) then !> No method could be derived, neither from interpolationMethod nor as a default for the given forcingFileType
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