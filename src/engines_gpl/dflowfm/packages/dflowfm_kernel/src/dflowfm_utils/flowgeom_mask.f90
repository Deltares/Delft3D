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

module m_flowgeom_mask
   use precision_basics, only: dp
   use fm_location_types, only: UNC_LOC_S, UNC_LOC_S3D, UNC_LOC_U, SPATIAL_LOCATION_INVALID, SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL

   implicit none(type, external)

   private

   public :: construct_mask

   interface construct_mask
      module procedure construct_mask_string_spatial_location
      module procedure construct_mask_integer_spatial_location
   end interface construct_mask

contains

   !> Construct a spatial mask array for flow geometry; either based on the location type (1D, 2D, 1D2D) or based on a target mask file (polygon file).
   !! Can be called for the various topological target location types (e.g., cells, flow links, etc.)
   subroutine construct_mask_string_spatial_location(mask, location_type, spatial_location_type, target_mask_file, invert_mask, ierr)
      ! Parameters
      integer, dimension(:), allocatable, intent(inout) :: mask !< Mask array for the target element set.
      integer, intent(in) :: location_type !< Location type (one of UNC_LOC_S/S3D/U/...).
      character(len=*), intent(in) :: spatial_location_type !< Spatial location type (one of SPATIAL_LOCATION_ALL/1D/2D).
      character(len=*), intent(in), optional :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      logical, intent(in), optional :: invert_mask !< Flag to invert the mask (1s to 0s and vice versa).
      integer, intent(out), optional :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      if (present(target_mask_file) .and. present(invert_mask) .and. present(ierr)) then
         call construct_mask_integer_spatial_location(mask, location_type, parse_spatial_location_type(spatial_location_type), target_mask_file, invert_mask, ierr)
      else
         call construct_mask_integer_spatial_location(mask, location_type, parse_spatial_location_type(spatial_location_type))
      end if

   end subroutine construct_mask_string_spatial_location

   !> Construct a spatial mask array for flow geometry; either based on the location type (1D, 2D, 1D2D) or based on a target mask file (polygon file).
   !! Can be called for the various topological target location types (e.g., cells, flow links, etc.)
   subroutine construct_mask_integer_spatial_location(mask, location_type, spatial_location_type, target_mask_file, invert_mask, ierr)
      use m_flowgeom, only: lnx, ndx

      ! Parameters
      integer, dimension(:), allocatable, intent(inout) :: mask !< Mask array for the target element set.
      integer, intent(in) :: location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      integer, intent(in) :: spatial_location_type !< The spatial location parameter (one from fm_location_types::SPATIAL_LOCATION_*) for this quantity's target element set.
      character(len=*), intent(in), optional :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      logical, intent(in), optional :: invert_mask !< Flag to invert the mask (1s to 0s and vice versa).
      integer, intent(out), optional :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      ! Allocate and initialize mask array based on location type.
      if (.not. allocated(mask)) then
         select case (location_type)
         case (UNC_LOC_S, UNC_LOC_S3D)

            allocate (mask(ndx))

         case (UNC_LOC_U)

            allocate (mask(lnx))

         end select
      end if
      mask = 0

      if (any(spatial_location_type == [SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL])) then
         call apply_spatial_location_mask(mask, location_type, spatial_location_type)
      end if

      if (present(target_mask_file) .and. present(ierr)) then
         call apply_polygon_mask(mask, location_type, target_mask_file, ierr)
      end if

      if (present(invert_mask)) then
         if (invert_mask) then
            mask = 1 - mask
         end if
      end if

   end subroutine construct_mask_integer_spatial_location

   !> Apply a spatial location mask to the provided mask array, based on the provided spatial location parameter (1D, 2D, or all).
   subroutine apply_spatial_location_mask(mask, location_type, spatial_location_type)
      use m_flowgeom, only: lnx1d, ln, ndx2d, lnxi, prof1d

      ! Parameters
      integer, dimension(:), intent(inout) :: mask !< Mask array for the target element set.
      integer, intent(in) :: location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*).
      integer, intent(in) :: spatial_location_type !< The spatial location parameter (one from fm_location_types::SPATIAL_LOCATION_*).

      ! Local variables
      integer :: L !< Loop variable for links.
      integer :: n1 !< The left flow cell number in the link.
      integer :: n2 !< The right flow cell number in the link.

      select case (spatial_location_type)
      case (SPATIAL_LOCATION_1D)

         select case (location_type)
         case (UNC_LOC_S, UNC_LOC_S3D)

            do L = 1, lnx1D
               n1 = ln(1, L)
               if (n1 > ndx2d) then
                  mask(n1) = 1
               end if
               n2 = ln(2, L)
               if (n2 > ndx2d) then
                  mask(n2) = 1
               end if
            end do

         case (UNC_LOC_U)

            do L = 1, lnx1D
               n1 = ln(1, L)
               n2 = ln(2, L)
               if (n1 > ndx2d .and. n2 > ndx2d) then
                  mask(L) = 1
               end if
            end do

         end select

      case (SPATIAL_LOCATION_2D)

         select case (location_type)
         case (UNC_LOC_S, UNC_LOC_S3D)

            do L = lnx1D + 1, lnxi
               n1 = ln(1, L)
               mask(n1) = 1
               n2 = ln(2, L)
               mask(n2) = 1
            end do

         case (UNC_LOC_U)

            do L = lnx1D + 1, lnxi
               mask(L) = 1
            end do

         end select

      case (SPATIAL_LOCATION_ALL) ! Everything in 1D and 2D, except 1D pipes

         select case (location_type)
         case (UNC_LOC_S, UNC_LOC_S3D)

            do L = 1, lnx1D
               ! When is lateral allowed?
               ! * (X)YZ profiles pointering to profiles number: always allow
               ! * direct profiles (rect/circle, etc.):no pipes pos or neg, others only if pos (==non-closed)
               if (prof1D(1, L) < 0 .or. (abs(prof1D(3, L)) /= 1 .and. prof1D(3, L) > 0)) then
                  n1 = ln(1, L)
                  mask(n1) = 1
                  n2 = ln(2, L)
                  mask(n2) = 1
               else
                  continue
               end if
            end do
            do L = lnx1D + 1, lnxi
               n1 = ln(1, L)
               mask(n1) = 1
               n2 = ln(2, L)
               mask(n2) = 1
            end do

         case (UNC_LOC_U)

            do L = 1, lnx1D
               if (prof1D(1, L) < 0 .or. (abs(prof1D(3, L)) /= 1 .and. prof1D(3, L) > 0)) then
                  mask(L) = 1
               else
                  continue
               end if
            end do

            do L = lnx1D + 1, lnxi
               mask(L) = 1
            end do

         end select
      end select

   end subroutine

   !> Apply a polygon mask to the provided mask array, based on the provided target mask file and location type (flow nodes, flow links, etc.).
   subroutine apply_polygon_mask(mask, location_type, target_mask_file, ierr)
      use m_flowgeom, only: xz, yz, kcs, ndx, lnx
      use timespace_parameters, only: LOCTP_POLYGON_FILE
      use timespace, only: selectelset_internal_nodes, selectelset_internal_links
      use dfm_error, only: DFM_NOERR, DFM_WRONGINPUT, DFM_NOTIMPLEMENTED

      ! Parameters
      integer, dimension(:), intent(inout) :: mask !< Mask array for the target element set.
      integer, intent(in) :: location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      character(len=*), intent(in), optional :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      integer, intent(out), optional :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      ! Local variables
      integer :: i !< Loop variable for mask array.
      integer, dimension(:), allocatable :: selected_points !< Array of selected points based on the target mask file.
      integer :: number_of_selected_points !< The number of selected points based on the target mask file.
      integer :: point !< Loop variable for points.
      logical :: spatial_mask_applied !< Flag to indicate whether a spatial mask has already been applied to the mask array.

      ierr = DFM_NOERR

      ! Check if a spatial mask ha already been applied to the mask array.
      if (any(mask == 1)) then
         spatial_mask_applied = .true.
      else
         spatial_mask_applied = .false.
      end if

      if (len_trim(target_mask_file) > 0) then
         ! Mask flow nodes/links/etc. based on inside polygon(s), or outside.
         allocate (selected_points(size(mask)))
         selected_points = 0

         select case (location_type)
         case (UNC_LOC_S)

            ! in: kcs, all allowed flow nodes, out: mask: all masked flow nodes.
            call selectelset_internal_nodes(xz, yz, kcs, ndx, selected_points, number_of_selected_points, &
               LOCTP_POLYGON_FILE, target_mask_file)

         case (UNC_LOC_U)

            ! in: no link pre-mask, all flow links, out: mask: all masked flow links.
            call selectelset_internal_links(lnx, selected_points, number_of_selected_points, &
               LOCTP_POLYGON_FILE, target_mask_file)

         case default

            ierr = DFM_NOTIMPLEMENTED
            return

         end select

         if (spatial_mask_applied) then
            do i = 1, size(mask)
               if (mask(i) == 1 .and. any(i == selected_points)) then
                  mask(i) = 1
               else
                  mask(i) = 0
               end if
            end do
         else
            do point = 1, number_of_selected_points
               mask(selected_points(point)) = 1
            end do
         end if

      else

         if (.not. spatial_mask_applied) then
            if (location_type == UNC_LOC_S) then
               ! 100% masking: accept all flow locations that were already active in their own mask array.
               where (kcs /= 0)
                  mask = 1
               end where
            else
               mask = 1
            end if
         end if

      end if

   end subroutine apply_polygon_mask

   !> Parse a locationType= string ('1d', '2d', '1d2d', 'all') to the
   !! ILATTP_* enum used by prepare_lateral_mask.
   !! Returns SPATIAL_LOCATION_INVALID when the string is absent, returns SPATIAL_LOCATION_INVALID when unrecognized.
   function parse_spatial_location_type(location_type_string) result(spatial_location_type)
      use string_module, only: str_tolower
      use fm_location_types, only: SPATIAL_LOCATION_1D, SPATIAL_LOCATION_2D, SPATIAL_LOCATION_ALL, SPATIAL_LOCATION_INVALID

      ! Parameters
      character(len=*), intent(in) :: location_type_string
      integer :: spatial_location_type

      if (len_trim(location_type_string) == 0) then
         spatial_location_type = SPATIAL_LOCATION_INVALID
         return
      end if

      select case (str_tolower(trim(location_type_string)))
      case ('1d')
         spatial_location_type = SPATIAL_LOCATION_1D
      case ('2d')
         spatial_location_type = SPATIAL_LOCATION_2D
      case ('1d2d', 'all')
         spatial_location_type = SPATIAL_LOCATION_ALL
      case default
         spatial_location_type = SPATIAL_LOCATION_ALL
      end select

   end function parse_spatial_location_type

end module m_flowgeom_mask
