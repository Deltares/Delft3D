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

!> Utilities module with functions for initializing and updating external forcings.
module fm_external_forcings_utils
   use precision_basics, only: dp

   implicit none(type, external)

   private

   public :: split_qid
   public :: get_tracername
   public :: get_sedfracname
   public :: get_constituent_name
   public :: read_tracer_properties
   public :: read_bubblescreen_forcing_attributes
   public :: print_error_message
   public :: select_wave_variables_subgroup

   integer, parameter :: INI_VALUE_LEN = 256

   abstract interface
      subroutine fill_open_boundary_cells_with_inner_values_any(number_of_links, link2cell)
         integer, intent(in) :: number_of_links !< number of links
         integer, dimension(:,:), intent(in) :: link2cell !< indices of cells connected by links
      end subroutine
   end interface

   procedure(fill_open_boundary_cells_with_inner_values_any), pointer :: fill_open_boundary_cells_with_inner_values !< boundary update routine to be called

   public :: fill_open_boundary_cells_with_inner_values

contains

   !> Split quantity id (qid) into base qid and specific qid.
   !! The base qid is the part of the qid that is common for all tracers of the same type.
   !! The specific qid is the part of the qid that is specific for a certain tracer.
   subroutine split_qid(qid, qid_base, qid_specific)
      use m_fm_wq_processes_sub, only: get_waqinputname
      use mass_balance_areas_routines, only: get_mbainputname

      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'tracerbndfluor'.
      character(len=*), intent(out) :: qid_base !< The base quantity name, e.g., 'tracerbnd'.
      character(len=*), intent(out) :: qid_specific !< The specific quantity name, e.g., 'fluor'.

      call get_tracername(qid, qid_specific, qid_base)
      ! As soon as the qid is different from the base qid, the qid is split and this routine is finished.
      if (qid_base /= qid) then
         return
      end if
      call get_sedfracname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_waqinputname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_mbainputname(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
      call get_constituent_name(qid, qid_specific, qid_base)
      if (qid_base /= qid) then
         return
      end if
   end subroutine split_qid

   !> Convert quantity id (from .ext file) to tracer name (split in generic qidname and specific tracer name).
   !! If the input qid is not tracer, then the same qid is returned (and no tracer name)
   subroutine get_tracername(qid, trname, qidname)
      use m_transportdata, only: DEFTRACER
      use string_module, only: istarts_with

      ! Arguments
      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'tracerbndfluor'.
      character(len=*), intent(out) :: trname !< The trimmed tracer name, e.g., 'fluor'.
      character(len=*), intent(out) :: qidname !< The base quantity name for further use in external forcing, e.g., 'tracerbnd'.

      ! Initialization
      trname = ''
      qidname = qid

      if (istarts_with(qid, 'tracerbnd')) then
         qidname = qid(1:9)
         if (len_trim(qid) > 9) then
            trname = trim(qid(10:))
         else
            trname = trim(DEFTRACER)
         end if
      else if (istarts_with(qid, 'initialtracer')) then
         qidname = qid(1:13)
         if (len_trim(qid) > 13) then
            trname = trim(qid(14:))
         else
            trname = trim(DEFTRACER)
         end if
      end if

   end subroutine get_tracername

   !> Convert quantity id (from .ext file) to sediment fraction name (split in generic qidname and specific fraction name).
   !! If the input qid is no sediment fraction, then the same qid is returned (and no fraction name)
   subroutine get_sedfracname(qid, sfname, qidname)
      use string_module, only: istarts_with

      ! Arguments
      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'sedfracbndsediment1'.
      character(len=*), intent(out) :: sfname !< The trimmed tracer name, e.g., 'sediment1'.
      character(len=*), intent(inout) :: qidname !< The base quantity name for further use in external forcing, e.g., 'sedfracbnd'.

      ! Initialization
      sfname = ''
      
      if (istarts_with(qid, 'sedfracbnd')) then
         qidname = qid(1:10)
         if (len_trim(qid) > 10) then
            sfname = trim(qid(11:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (istarts_with(qid, 'initialsedfrac')) then
         qidname = qid(1:14)
         if (len_trim(qid) > 14) then
            sfname = trim(qid(15:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (istarts_with(qid, 'initialverticalsedfracprofile')) then
         qidname = qid(1:29)
         if (len_trim(qid) > 29) then
            sfname = trim(qid(30:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      else if (istarts_with(qid, 'initialverticalsigmasedfracprofile')) then
         qidname = qid(1:34)
         if (len_trim(qid) > 34) then
            sfname = trim(qid(35:))
         else
            sfname = trim('unknown_sediment_fraction')
         end if
      end if

   end subroutine get_sedfracname

   !> Convert quantity (from .ext file) to constituent name (split in generic base_quantity and specific constituent_name).
   !! If the original_quantity does not involve consituents, then the passed base_quantity is unchanged (and empty constituent name).
   !! The quantity can have a postfix 'Delta', but this is optional:
   !! For example: 'sourcesink_salinityDelta' -> 'sourcesink_constituentDelta', 'salinity'.
   !! Or:          'sourcesink_salinity' -> 'sourcesink_constituentDelta', 'salinity'.
   !!
   !! This subroutine currently only covers source sinks, because they are the only external forcings that generalize on
   !! constituents. Other external forcings are handled in get_tracername, get_sedfracname, etc.
   subroutine get_constituent_name(original_quantity, constituent_name, base_quantity)
      use string_module, only: strcmpi

      ! Arguments
      character(len=*), intent(in) :: original_quantity !< Original quantity id, e.g., 'sourcesink_salinityDelta'.
      character(len=*), intent(out) :: constituent_name !< The trimmed constituent name, e.g., 'salinity', or 'sand', or 'fluor'. Empty '' if not a constituent.
      character(len=*), intent(out) :: base_quantity !< The base quantity name for further use in external forcing, e.g., 'sourcesink_constituentDelta'. Unchanged original_quantity if not a constituent.

      ! Local variables
      integer :: quantity_length
      integer :: index_prefix_end, index_suffix_start

      ! Initialization
      constituent_name = ''

      quantity_length = len_trim(original_quantity)
      index_prefix_end = min(len_trim('sourcesink_'), quantity_length)
      index_suffix_start = max(1, quantity_length - len_trim('Delta') + 1)

      ! First, remove the 'sourcesink_' and (optionally) 'Delta' parts from the original quantity.
      if (strcmpi(original_quantity(1:index_prefix_end), 'sourcesink_')) then
         if (strcmpi(original_quantity(index_prefix_end + 1: quantity_length), 'discharge')) then
            return  ! Discharge is not a constituent. Do nothing.
         end if

         base_quantity = 'sourcesink_constituentDelta'
         if (strcmpi(original_quantity(index_suffix_start:quantity_length), 'Delta')) then
            constituent_name = original_quantity(index_prefix_end + 1:index_suffix_start - 1)
         else
            constituent_name = original_quantity(index_prefix_end + 1:)
         end if

         ! Then, optionally remove the special constituent group name 'tracer' or 'sedFrac' part from the constituent name.
         if (strcmpi(constituent_name(1:6), 'tracer')) then
            constituent_name = constituent_name(7:)
         else if (strcmpi(constituent_name(1:7), 'sedFrac')) then
            constituent_name = constituent_name(8:)
         end if
      end if

   end subroutine get_constituent_name

   !> Read tracer properties from an ini file node.
   subroutine read_tracer_properties(node_ptr, transformcoef)
      use properties
      use tree_data_types, only: tree_data

      ! Arguments
      type(tree_data), pointer :: node_ptr !< The tree structure containing a single ini-file chapter/block.
      real(kind=dp), dimension(:), intent(inout) :: transformcoef !< Transformation coefficients

      ! Local variables
      real(kind=dp) :: tracer_fall_velocity ! Tracer fall velocity
      real(kind=dp) :: tracer_decay_time ! Tracer decay time

      tracer_fall_velocity = 0.0_dp
      call prop_get(node_ptr, '', 'tracerFallVelocity', tracer_fall_velocity)
      transformcoef(24) = tracer_fall_velocity

      tracer_decay_time = 0.0_dp
      call prop_get(node_ptr, '', 'tracerDecayTime', tracer_decay_time)
      transformcoef(25) = tracer_decay_time

   end subroutine read_tracer_properties

   !> Read bubblescreen forcing attributes from block pointer
   function read_bubblescreen_forcing_attributes(block_ptr, base_dir, file_name, group_name, id, x_coordinates, y_coordinates, &
         z_coordinates, num_columns, z_level, discharge_input) result(success)
      use MessageHandling, only: err_flush, msgbuf
      use properties, only: prop_get
      use tree_data_types, only: tree_data
      use m_read_location_info, only: read_polyline_coordinates

      ! Arguments
      type(tree_data), pointer, intent(in) :: block_ptr
      character(len=*), intent(in) :: base_dir
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: group_name
      character(len=255), intent(out) :: id
      real(kind=dp), dimension(:), allocatable, intent(out) :: x_coordinates
      real(kind=dp), dimension(:), allocatable, intent(out) :: y_coordinates
      real(kind=dp), dimension(:), allocatable, intent(out) :: z_coordinates
      integer, intent(out) :: num_columns
      real(kind=dp), intent(out) :: z_level
      character(len=:), allocatable, intent(out) :: discharge_input
      logical :: success

      ! Local variables
      character(len=INI_VALUE_LEN) :: readout_id
      character(len=INI_VALUE_LEN) :: readout_discharge_input
      logical :: is_read

      ! Initialization
      success = .false.
      num_columns = 0

      ! (required) id
      call prop_get(block_ptr, '', 'id', readout_id, is_read)
      if (.not. is_read .or. len_trim(readout_id) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(file_name), ''': [', trim(group_name), ']. Field ''id'' is missing.'
         call err_flush()
         return
      end if
      id = trim(readout_id)

      ! (required) polyline coordinates (from locationFile or inline keys)
      call read_polyline_coordinates(block_ptr, trim(id), file_name, base_dir, group_name, &
                                    x_coordinates, y_coordinates, z_coordinates, num_columns, success)
      if (.not. success) return

      ! (required) zLevel
      call prop_get(block_ptr, '', 'zLevel', z_level, is_read)
      if (.not. is_read) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(file_name), ''': [', trim(group_name), ']. Field ''zLevel'' is missing or invalid.'
         call err_flush()
         success = .false.
         return
      end if

      ! (required) discharge
      call prop_get(block_ptr, '', 'discharge', readout_discharge_input, is_read)
      if (.not. is_read .or. len_trim(readout_discharge_input) == 0) then
         write (msgbuf, '(5a)') 'Incomplete block in file ''', trim(file_name), ''': [', trim(group_name), ']. Key "discharge" is missing.'
         call err_flush()
         success = .false.
         return
      end if
      discharge_input = trim(readout_discharge_input)

      success = .true.

   end function read_bubblescreen_forcing_attributes

   !> print_error_message
   subroutine print_error_message(time_in_seconds)
      use m_ec_message, only: dump_ec_message_stack
      use unstruc_messages, only: callback_msg
      use messagehandling, only: LEVEL_WARN, mess

      ! Arguments
      real(kind=dp), intent(in) :: time_in_seconds !< Current time when doing this action

      ! Local variables
      character(len=255) :: tmpstr

      write (tmpstr, '(f22.11)') time_in_seconds
      call mess(LEVEL_WARN, 'Error while updating meteo/structure forcing at time='//trim(tmpstr))
      tmpstr = dump_ec_message_stack(LEVEL_WARN, callback_msg)
   end subroutine print_error_message

   !> select_wave_variables_subgroup
   !! select routine depending on whether all or a subgroup of wave variables are allocated
   subroutine select_wave_variables_subgroup(all_wave_variables)
      ! Arguments
      logical, intent(in) :: all_wave_variables

      if (all_wave_variables) then
         fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_all
      else
         fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_fewer
      end if

   end subroutine select_wave_variables_subgroup

   !> fill_open_boundary_cells_with_inner_values_all
   subroutine fill_open_boundary_cells_with_inner_values_all(number_of_links, link2cell)
      use m_waves

      ! Arguments
      integer, intent(in) :: number_of_links !< number of links
      integer, dimension(:,:), intent(in) :: link2cell !< indices of cells connected by links

      ! Local variables
      integer :: link !< link counter
      integer :: kb !< cell index of boundary cell
      integer :: ki !< cell index of internal cell

      do link = 1, number_of_links
         kb = link2cell(1, link)
         ki = link2cell(2, link)
         hwav(kb) = hwav(ki)
         twav(kb) = twav(ki)
         phiwav(kb) = phiwav(ki)
         uorbwav(kb) = uorbwav(ki)
         sxwav(kb) = sxwav(ki)
         sywav(kb) = sywav(ki)
         mxwav(kb) = mxwav(ki)
         mywav(kb) = mywav(ki)
         sbxwav(kb) = sbxwav(ki)
         sbywav(kb) = sbywav(ki)
         dsurf(kb) = dsurf(ki)
         dwcap(kb) = dwcap(ki)
      end do

   end subroutine fill_open_boundary_cells_with_inner_values_all

   !> fill_open_boundary_cells_with_inner_values_fewer
   subroutine fill_open_boundary_cells_with_inner_values_fewer(number_of_links, link2cell)
      use m_waves
      use m_waveconst, only: wave_input_is_required, WAVE_INPUT_SIGNIFICANT_HEIGHT, WAVE_INPUT_PERIOD, WAVE_INPUT_DIRECTION, &
         WAVE_INPUT_FORCE_X, WAVE_INPUT_FORCE_Y, WAVE_INPUT_DISSIPATION_TOTAL, WAVE_INPUT_DISSIPATION_SURFACE, &
         WAVE_INPUT_DISSIPATION_WHITE_CAPPING

      ! Arguments
      integer, intent(in) :: number_of_links !< number of links
      integer, dimension(:,:), intent(in) :: link2cell !< indices of cells connected by links

      ! Local variables
      integer :: link !< link counter
      integer :: kb !< cell index of boundary cell
      integer :: ki !< cell index of internal cell
      logical :: significant_height_required
      logical :: period_required
      logical :: direction_required
      logical :: force_x_required
      logical :: force_y_required
      logical :: dissipation_total_required
      logical :: dissipation_surface_required
      logical :: dissipation_white_capping_required
      logical :: wave_kinematics_required

      significant_height_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_SIGNIFICANT_HEIGHT)
      period_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_PERIOD)
      direction_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_DIRECTION)
      force_x_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_FORCE_X)
      force_y_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_FORCE_Y)
      dissipation_total_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_DISSIPATION_TOTAL)
      dissipation_surface_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_DISSIPATION_SURFACE)
      dissipation_white_capping_required = wave_input_is_required(offline_wave_input_requirements, WAVE_INPUT_DISSIPATION_WHITE_CAPPING)
      wave_kinematics_required = significant_height_required .and. period_required .and. direction_required

      do link = 1, number_of_links
         kb = link2cell(1, link)
         ki = link2cell(2, link)
         if (significant_height_required) then
            hwav(kb) = hwav(ki)
         end if
         if (period_required) then
            twav(kb) = twav(ki)
         end if
         if (direction_required) then
            phiwav(kb) = phiwav(ki)
         end if
         if (force_x_required) then
            sxwav(kb) = sxwav(ki)
         end if
         if (force_y_required) then
            sywav(kb) = sywav(ki)
         end if
         if (dissipation_total_required) then
            distot(kb) = distot(ki)
         end if
         if (dissipation_surface_required) then
            dsurf(kb) = dsurf(ki)
            sbxwav(kb) = sbxwav(ki)
            sbywav(kb) = sbywav(ki)
         end if
         if (dissipation_white_capping_required) then
            dwcap(kb) = dwcap(ki)
         end if
         if (wave_kinematics_required) then
            uorbwav(kb) = uorbwav(ki)
            mxwav(kb) = mxwav(ki)
            mywav(kb) = mywav(ki)
         end if
      end do

   end subroutine fill_open_boundary_cells_with_inner_values_fewer

end module fm_external_forcings_utils
