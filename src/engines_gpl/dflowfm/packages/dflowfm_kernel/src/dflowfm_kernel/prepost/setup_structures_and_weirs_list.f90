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

module m_setup_structures_and_weirs_list
   implicit none
contains
   !> Find flow links that contain structures or weirs
   pure function build_structures_and_weirs_list() result(links_with_structures_or_weirs)
      use m_flowgeom, only: lnx, bob, bob0
      use m_flowparameters, only: ChangeVelocityAtStructures
      use fm_external_forcings_data, only: ncdamsg, L1cdamsg, L2cdamsg, kcdam, ncgensg, L1cgensg, L2cgensg, kcgen
      use m_dambreak_breach, only: indicate_links_that_contain_dambreaks
      use unstruc_channel_flow, only: network

      use array_module, only: convert_mask_to_indices

      integer, allocatable, dimension(:) :: links_with_structures_or_weirs !< List of indices of the flow links that contain structures or weirs
      integer :: L, L0, ng, istru, n
      logical, allocatable, dimension(:) :: does_link_contain_structures

      if (.not. ChangeVelocityAtStructures) then
         allocate (links_with_structures_or_weirs(0))
         return
      end if

      allocate (does_link_contain_structures(lnx), source=.false.)

      ! Generate a list for all possible flow links, where bob0 /= bob, resulting in a difference between au_nostrucs and au.
      ! In general this will be the locations of fixed weirs. Because this check is not completely water tight., all structures
      ! a fixed weir with crest level == bed level will be skipped.
      ! All other structures are added to the list seperately.
      do L = 1, lnx
         if (bob(1, L) /= bob0(1, L) .or. bob(2, L) /= bob0(2, L)) then
            does_link_contain_structures(L) = .true.
         end if
      end do

      do ng = 1, ncdamsg
         do n = L1cdamsg(ng), L2cdamsg(ng)
            L = kcdam(3, n)
            does_link_contain_structures(L) = .true.
         end do
      end do

      do ng = 1, ncgensg
         do n = L1cgensg(ng), L2cgensg(ng)
            L = kcgen(3, n)
            does_link_contain_structures(L) = .true.
         end do
      end do

      do istru = 1, network%sts%count
         do L0 = 1, network%sts%struct(istru)%numlinks
            L = abs(network%sts%struct(istru)%linknumbers(L0))
            does_link_contain_structures(L) = .true.
         end do
      end do

      call indicate_links_that_contain_dambreaks(does_link_contain_structures)

      ! Convert mask to array of indices
      links_with_structures_or_weirs = convert_mask_to_indices(does_link_contain_structures)
   end function build_structures_and_weirs_list
end module m_setup_structures_and_weirs_list
