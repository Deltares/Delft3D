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

!> Configuration module for all possible quantities supported as external forcings.
submodule(fm_external_forcings) fm_external_forcings_config
   use fm_location_types
   use m_flowgeom, only: ndx, lnx, xz, yz, xu, yu
   use precision_basics, only: hp
   implicit none

contains

   !> Get several target grid properties for a given quantity.
   !!
   !! For example, wind is known to be placed on velocity points,
   !! so target_location_type UNC_LOC_U will be returned.
   module function get_quantity_target_properties(quantity, target_location_type, target_num_points, target_x, target_y, target_mask) result(ierr)
      use dfm_error
      character(len=*), intent(in) :: quantity                   !< Quantity identifier, as given in external forcings input file.
      integer, intent(out) :: target_location_type               !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      integer, intent(out) :: target_num_points                  !< Number of points in target element set.
      real(hp), dimension(:), pointer, intent(out) :: target_x   !< Pointer to x-coordinates array of target element set.
      real(hp), dimension(:), pointer, intent(out) :: target_y   !< Pointer to y-coordinates array of target element set.
      integer, dimension(:), pointer, intent(out) :: target_mask !< Pointer to x-coordinates array of target element set.
      integer :: ierr                                            !< Result status (DFM_NOERR if succesful, or different if unknown quantity was given).

      integer :: len_quantity
      character(len=len(quantity)) :: tracer_name, sedfrac_name, wqinput_name, mbainput_name, quantity_basename

      ierr = DFM_GENERICERROR
      target_location_type = UNC_LOC_UNKNOWN

      quantity_basename = quantity
      call get_tracername(quantity, tracer_name, quantity_basename)
      call get_sedfracname(quantity, sedfrac_name, quantity_basename)
      call get_waqinputname(quantity, wqinput_name, quantity_basename)
      call get_mbainputname(quantity, mbainput_name, quantity_basename)

      len_quantity = len_trim(quantity_basename)

      if (quantity_basename(max(1, len_quantity - 2):len_quantity) == 'bnd') then ! All-in-one handler for boundary qids
         ! No generic location (differs  per boundary). But, also currently not needed in addtimespacerelation_boundaries().
         ierr = DFM_NOTIMPLEMENTED
         return
      end if

      ! Select location type based on quantity name
      select case (quantity_basename)
      case ('airdensity', 'airpressure', 'airpressure_stressx_stressy', 'airpressure_windx_windy', &
            'airpressure_windx_windy_charnock', 'airtemperature', 'atmosphericpressure', 'charnock', 'cloudiness', &
            'dewpoint', 'dewpoint_airtemperature_cloudiness', 'dewpoint_airtemperature_cloudiness_solarradiation', &
            'humidity', 'humidity_airtemperature_cloudiness', 'humidity_airtemperature_cloudiness_solarradiation', &
            'longwaveradiation', 'rainfall', 'rainfall_rate', 'solarradiation', 'solarradiationfactor')
         target_location_type = UNC_LOC_S
         ierr = DFM_NOERR

      case ('stressx', 'stressxy', 'stressy', 'windspeedfactor', 'windstresscoefficient', 'windx', 'windxy', 'windy')
         target_location_type = UNC_LOC_U
         ierr = DFM_NOERR

      case default
         ierr = DFM_NOTIMPLEMENTED
      end select
      
      ! Set other target grid properties, based on location type just found.
      select case(target_location_type)
      case(UNC_LOC_S)
         target_num_points = ndx
         target_x => xz
         target_x => xz
         ! TODO: mask => kcs
      case(UNC_LOC_U)
         target_num_points = lnx
         target_x => xu
         target_x => xu
         ! TODO: mask => mask_u
      case default
         ierr = DFM_NOTIMPLEMENTED
      end select

   end function get_quantity_target_properties

end submodule fm_external_forcings_config
