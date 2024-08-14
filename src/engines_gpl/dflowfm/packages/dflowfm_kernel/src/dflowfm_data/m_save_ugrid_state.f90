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

!> Save data for later writing of Ugrid
module m_save_ugrid_state

   use meshdata
   use m_hash_search

   type(t_ug_meshgeom) :: meshgeom1d
   character(len=ug_idsLen), allocatable :: nbranchids(:), nnodeids(:), nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable :: nbranchlongnames(:), nnodelongnames(:), nodelongnames(:)
   character(len=255) :: network1dname, mesh2dname, mesh1dname, contactname !MAXSTRLEN = 255
   character(len=ug_idsLen), allocatable :: mesh1dNodeIds(:)
   integer, allocatable, dimension(:) :: mesh1dUnmergedToMerged(:)
   !integer, allocatable, dimension(:)                 :: mesh1dMergedToUnMerged(:)
   integer :: numMesh1dBeforeMerging
   integer, allocatable :: contactnetlinks(:) !< netlink number for each contact
   integer, allocatable :: netlink2contact(:) !< Inverse mapping of contactnetlinks (only for first numl1d net links)
   integer :: contactnlinks !< Total number of links in all mesh contacts (typically we'll have one mesh contact with many netlinks part of it)
   integer, allocatable :: contact1d2didx(:, :) !< Mapping 1D net node to 2D net face (for later use in 2D flow node snapping)

   type(t_hashlist) :: hashlist_contactids !< Hash list for quick search for contact ids.

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_save_ugrid_state() instead.
contains
   subroutine default_save_ugrid_state()
      implicit none

      network1dname = 'network1d'
      mesh1dname = 'mesh1d'
      mesh2dname = 'mesh2d'
      contactname = 'contacts'
      numMesh1dBeforeMerging = 0

      if (allocated(mesh1dNodeIds)) deallocate (mesh1dNodeIds)
      if (allocated(mesh1dUnmergedToMerged)) deallocate (mesh1dUnmergedToMerged)
      !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)

      if (allocated(contactnetlinks)) deallocate (contactnetlinks)
      if (allocated(netlink2contact)) deallocate (netlink2contact)
      if (allocated(contact1d2didx)) deallocate (contact1d2didx)
      contactnlinks = 0
      call dealloc(hashlist_contactids)

      call reset_save_ugrid_state()

   end subroutine default_save_ugrid_state

   !> Resets only variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_save_ugrid_state() instead.
   subroutine reset_save_ugrid_state()
      implicit none

   end subroutine reset_save_ugrid_state

end module m_save_ugrid_state
