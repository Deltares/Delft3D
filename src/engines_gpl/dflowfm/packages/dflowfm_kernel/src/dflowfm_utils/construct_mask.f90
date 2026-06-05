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

module m_construct_mask
   use precision_basics, only: dp
   use fm_location_types, only: UNC_LOC_S, UNC_LOC_S3D, UNC_LOC_U
   use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL, ILATTP_INVALID

   implicit none(type, external)

   private

   public :: construct_mask
   public :: parse_location_type

contains

   subroutine construct_mask(mask, target_location_type, ilattype, target_num_points, target_mask_file, invert_mask, ierr)
      ! Parameters
      integer, dimension(:), allocatable, intent(inout) :: mask !< Mask array for the target element set.
      integer, intent(in) :: target_location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      integer, intent(in), optional :: ilattype !< Type of the new lateral.
      integer, intent(in), optional :: target_num_points !< Number of points in target element set. Will be used to allocate the mask array.
      character(len=*), intent(in), optional :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      logical, intent(in), optional :: invert_mask !< Flag to invert the mask (1s to 0s and vice versa).
      integer, intent(out), optional :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      if (present(ilattype)) then
         if (ilattype /= ILATTP_INVALID .and. any(target_location_type == [UNC_LOC_S, UNC_LOC_S3D])) then
            call prepare_lateral_mask(mask, ilattype)
         end if
      else if (present(target_num_points) .and. present(target_mask_file) .and. present(invert_mask) .and. present(ierr)) then
         call construct_target_mask(mask, target_num_points, target_mask_file, target_location_type, invert_mask, ierr)
      end if

   end subroutine construct_mask

   !> Prepare the 'kclat' mask array for a specific type of lateral.
   subroutine prepare_lateral_mask(kc, ilattype)
      use m_flowgeom, only: lnx1d, ln, ndxi, ndx2d, lnxi, prof1d
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL

      integer, allocatable, intent(inout) :: kc(:) !< (ndx) The mask array that is to be filled.
      integer, intent(in) :: ilattype !< Type of the new lateral (one of ILATTP_1D|2D|1D2D)

      integer :: L, k1, k2

      if (.not. allocated(kc)) then
         allocate(kc(ndxi))
      end if
      kc = 0
      select case (ilattype)
      case (ILATTP_1D) ! in everything 1D
         do L = 1, lnx1D
            !if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1, L)
            if (k1 > ndx2d) then
               kc(k1) = 1
            end if
            k2 = ln(2, L)
            if (k2 > ndx2d) then
               kc(k2) = 1
            end if
            !endif
         end do
      case (ILATTP_2D) ! in everything 2D
         do L = lnx1D + 1, lnxi
            k1 = ln(1, L)
            kc(k1) = 1
            k2 = ln(2, L)
            kc(k2) = 1
         end do
      case (ILATTP_ALL) ! both to everything 2D, and 1D, except to 1D pipes
         do L = 1, lnx1D
            ! When is lateral allowed?
            ! * (X)YZ profiles pointering to profiles number: always allow
            ! * direct profiles (rect/circle, etc.):no pipes pos or neg, others only if pos (==non-closed)
            if (prof1D(1, L) < 0 .or. (abs(prof1D(3, L)) /= 1 .and. prof1D(3, L) > 0)) then
               k1 = ln(1, L)
               kc(k1) = 1
               k2 = ln(2, L)
               kc(k2) = 1
            else
               continue
            end if
         end do
         do L = lnx1D + 1, lnxi
            k1 = ln(1, L)
            kc(k1) = 1
            k2 = ln(2, L)
            kc(k2) = 1
         end do
      end select
   end subroutine prepare_lateral_mask

   !> Construct target mask array for later ec_addtimespacerelation/timespaceinitialfield calls.
   subroutine construct_target_mask(mask, target_num_points, target_mask_file, target_location_type, invert_mask, ierr)
      use m_flowgeom, only: ndx, lnx, xz, yz, kcs
      use timespace_parameters, only: LOCTP_POLYGON_FILE
      use timespace, only: selectelset_internal_nodes, selectelset_internal_links
      use dfm_error, only: DFM_NOTIMPLEMENTED, DFM_NOERR

      integer, dimension(:), allocatable, intent(out) :: mask !< Mask array for the target element set.
      integer, intent(in) :: target_num_points !< Number of points in target element set. Will be used to allocate the mask array.
      character(len=*), intent(in) :: target_mask_file !< File name of the target mask file (*.pol). When empty, 100% masking is assumed.
      integer, intent(in) :: target_location_type !< The location type parameter (one from fm_location_types::UNC_LOC_*) for this quantity's target element set.
      logical, intent(in) :: invert_mask !< Flag to invert the mask (1s to 0s and vice versa).
      integer, intent(out) :: ierr !< Result status (DFM_NOERR if succesful, or different if mask could not be constructed for this quantity's location).

      integer, dimension(:), allocatable :: selected_points !< Array of selected points based on the target mask file.
      integer :: number_of_selected_points, point

      ierr = DFM_NOERR

      allocate (mask(target_num_points), source=0)

      if (len_trim(target_mask_file) > 0) then
         ! Mask flow nodes/links/etc. based on inside polygon(s), or outside.
         allocate (selected_points(target_num_points), source=0)
         select case (target_location_type)
         case (UNC_LOC_S)
            ! in: kcs, all allowed flow nodes, out: mask: all masked flow nodes.
            call selectelset_internal_nodes(xz, yz, kcs, ndx, selected_points, number_of_selected_points, LOCTP_POLYGON_FILE, &
                                             target_mask_file)
         case (UNC_LOC_U)
            ! in: no link pre-mask, all flow links, out: mask: all masked flow links.
            call selectelset_internal_links(lnx, selected_points, number_of_selected_points, LOCTP_POLYGON_FILE, &
                                             target_mask_file)
         case default
            ierr = DFM_NOTIMPLEMENTED
            return
         end select

         do point = 1, number_of_selected_points
            mask(selected_points(point)) = 1
         end do
         if (invert_mask) then
            mask = ieor(mask, 1)
         end if
      else
         if (target_location_type == UNC_LOC_S) then
            ! 100% masking: accept all flow locations that were already active in their own mask array.
            where (kcs /= 0) mask = 1
         else
            mask = 1
         end if
      end if
   end subroutine construct_target_mask

   !> Parse a locationType= string ('1d', '2d', '1d2d', 'all') to the
   !! ILATTP_* enum used by prepare_lateral_mask.
   !! Returns ILATTP_INVALID when the string is absent, returns ILATTP_INVALID when unrecognized.
   function parse_location_type(location_type_string) result(ilattype)
      use string_module, only: str_tolower

      ! Parameters
      character(len=*), intent(in) :: location_type_string
      integer :: ilattype

      if (len_trim(location_type_string) == 0) then
         ilattype = ILATTP_INVALID
         return
      end if

      select case (str_tolower(trim(location_type_string)))
      case ('1d')
         ilattype = ILATTP_1D
      case ('2d')
         ilattype = ILATTP_2D
      case ('1d2d', 'all')
         ilattype = ILATTP_ALL
      case default
         ilattype = ILATTP_ALL
      end select

   end function parse_location_type

end module m_construct_mask