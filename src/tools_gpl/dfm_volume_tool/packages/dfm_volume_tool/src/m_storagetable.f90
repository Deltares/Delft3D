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
!
!
!> Module for using volume tables at 1d nodes for the computation of the total volume of water in a node.
module m_storage_table
   use messagehandling
   use m_globalparameters
   use m_volumetables
   use m_flowgeom
   use m_flowparameters
   use unstruc_channel_flow, only: tableincrement

   implicit none
   private

   public generate_total_volume_table
   public generate_volume_table_on_branches
   public get_bed_top_level
   public calculate_dead_storage
   public add_volume_and_surface

contains

!> Generate the total volume/surface
   subroutine generate_total_volume_table(volume, surface, storage, deadstorage, wl_deadstorage, voltb, &
                                          bedlevel, increment, numpoints, numlevels, nodes)

      use unstruc_channel_flow
      double precision, dimension(:), intent(out) :: volume !< Aggregated volume per level
      double precision, dimension(:), intent(out) :: surface !< Aggregated areas per level
      double precision, dimension(:), intent(out) :: storage !< Aggregated storage per level (=volume - deadstorage)
      double precision, dimension(:), intent(out) :: deadstorage !< Aggregated dead storage per level
      double precision, dimension(:), intent(in) :: wl_deadstorage !< Dead storage waterlevel on each grid point
      type(t_voltable), dimension(:), intent(in) :: voltb !< Volume tables on grid points
      double precision, intent(in) :: bedlevel !< Bed level of the model
      double precision, intent(in) :: increment !< Requested increment
      integer, intent(in) :: numpoints !< number of 1d grid points also the size of voltb
      integer, intent(in) :: numlevels !< number of levels in volume, surface and levels
      integer, dimension(:), intent(in) :: nodes !< mask array for defining a sub set on nodes.

      integer :: j, ipoint

      volume = 0d0
      deadstorage = 0d0
      storage = 0d0
      surface = 0d0
      do j = 1, numpoints
         ipoint = nodes(j)
         call add_volume_and_surface(volume, surface, deadstorage, wl_deadstorage(ipoint), voltb(ipoint), bedlevel, increment, numlevels)
      end do

      call compute_surface_and_storage(volume, surface, storage, deadstorage, increment, numlevels)

   end subroutine generate_total_volume_table

!> Generate the total volume/surface (TODO: and the totalised volume/surface for each branch).
   subroutine generate_volume_table_on_branches(volume, surface, storage, deadstorage, wl_deadstorage, voltb_on_links, &
                                                bedlevel, increment, numpoints, numlevels, links, ln2nd)

      use unstruc_channel_flow
      double precision, dimension(:), intent(out) :: volume !< Aggregated volume per level
      double precision, dimension(:), intent(out) :: surface !< Aggregated areas per level
      double precision, dimension(:), intent(out) :: storage !< Aggregated storage per level (=volume - deadstorage)
      double precision, dimension(:), intent(out) :: deadstorage !< Aggregated dead storage per level
      double precision, dimension(:), intent(in) :: wl_deadstorage !< Water level for dead storage
      type(t_voltable), dimension(:, :), intent(in) :: voltb_on_links !< Volume tables on links
      double precision, intent(in) :: bedlevel !< Bed level of the model
      double precision, intent(in) :: increment !< Requested increment
      integer, intent(in) :: numpoints !< Number of 1d grid points also the size of voltb
      integer, intent(in) :: numlevels !< Number of levels in volume, surface and levels
      integer, dimension(:), intent(in) :: links !< Mask array for defining a sub set on flow links.
      integer, dimension(:, :), intent(in) :: ln2nd !< Indirection array links to nodes.

      integer :: j, l

      volume = 0d0
      surface = 0d0
      storage = 0d0
      deadstorage = 0d0

      do j = 1, numpoints
         l = links(j)
         call add_volume_and_surface(volume, surface, deadstorage, wl_deadstorage(ln2nd(1, l)), &
                                     voltb_on_links(1, l), bedlevel, increment, numlevels)
         call add_volume_and_surface(volume, surface, deadstorage, wl_deadstorage(ln2nd(2, l)), &
                                     voltb_on_links(2, l), bedlevel, increment, numlevels)
      end do

      call compute_surface_and_storage(volume, surface, storage, deadstorage, increment, numlevels)

   end subroutine generate_volume_table_on_branches

!> Add the volume and surface for this gridpoints volume table to the aggregated volume and surface
   subroutine add_volume_and_surface(vol, sur, deadstorage, wl_deadstorage, voltb, bedlevel, increment, numlevels)
      double precision, dimension(:), intent(inout) :: vol !< Aggregated volume
      double precision, dimension(:), intent(inout) :: sur !< Aggregated surface
      double precision, dimension(:), intent(inout) :: deadstorage !< Aggregated dead storage
      double precision, intent(in) :: wl_deadstorage !< Aggregated dead storage
      type(t_voltable), intent(in) :: voltb !< Volume table
      integer, intent(in) :: numlevels !< Number of levels in the aggregated data
      double precision, intent(in) :: bedlevel !< Bed level
      double precision, intent(in) :: increment !< Requested increment

      integer :: i
      double precision :: waterlevel

      do i = 2, numlevels
         waterlevel = bedlevel + (i - 1) * increment
         ! Compute 1d volume, using volume tables (still excl. 1D2D contributions)
         vol(i) = vol(i) + voltb%getvolume(waterlevel)
         sur(i) = sur(i) + voltb%getsurface(waterlevel)
         if (wl_deadstorage >= waterlevel) then
            deadstorage(i) = deadstorage(i) + voltb%getvolume(waterlevel)
         else
            deadstorage(i) = deadstorage(i) + voltb%getvolume(wl_deadstorage)
         end if
      end do

   end subroutine

!> derive the surface areas from the volume table
   subroutine compute_surface_and_storage(vol, sur, storage, deadstorage, increment, numlevels)
      double precision, dimension(:), intent(inout) :: vol !< Aggregated volume
      double precision, dimension(:), intent(inout) :: sur !< Aggregated surface
      double precision, dimension(:), intent(inout) :: storage !< Aggregated storage
      double precision, dimension(:), intent(in) :: deadstorage !< Aggregated dead storage
      integer, intent(in) :: numlevels !< Number of levels in the aggregated data
      double precision, intent(in) :: increment !< Requested increment

      integer :: i

      storage(1) = 0d0
      do i = 2, numlevels
         sur(i - 1) = (vol(i) - vol(i - 1)) / increment
         storage(i) = vol(i) - deadstorage(i)
      end do
   end subroutine compute_surface_and_storage

!> Determine the lowest level and the highest level in the volume table.
   subroutine get_bed_top_level(voltb, numpoints, toplevel, bedlevel)
      type(t_voltable), pointer, dimension(:), intent(in) :: voltb !< (numpoints) volume table for individual grid points.
      integer, intent(in) :: numpoints !< Number of grid points in the input volume table.
      double precision, intent(out) :: toplevel !< Highest absolute level in the model, where cross sections or storage nodes are defined.
      double precision, intent(out) :: bedlevel !< Lowest (bed)level in the model, where cross sections or storage nodes are defined.

      integer :: n
      toplevel = -huge(1d0)
      bedlevel = huge(1d0)

      do n = 1, numpoints
         toplevel = max(toplevel, voltb(n)%topheight + voltb(n)%bedlevel)
         bedlevel = min(bedlevel, voltb(n)%bedlevel)
      end do

   end subroutine get_bed_top_level

!> Calculate the dead storage using boundary conditions, turn off levels of pumping stations and the geometry of the network.
   subroutine calculate_dead_storage(wl_deadstorage, network, bndvalues, inslevtube, bndindex, &
                                     ln2nd, kcu2, numlinks, numpoints, numboundaries)

      use m_flowgeom
      use m_network
      use m_1d_structures
      use m_pump

      type(t_network), intent(in) :: network !< Network definition of the model.
      double precision, dimension(:), intent(inout) :: wl_deadstorage !< On output this array contains the minimal water levels, after the model is
      !< drained.
      double precision, dimension(:), intent(in) :: bndvalues !< Boundary condition values. See ZBNDZ in D-FlowFM.
      double precision, dimension(:, :), intent(in) :: inslevtube !< Inside level tubes. See BOB in D-FlowFM.
      integer, dimension(:, :), intent(in) :: bndindex !< Index numbers for boundary condtions. See KBNDZ in D-FfowFM.
      integer, dimension(:, :), intent(in) :: ln2nd !< Link nodes to node numbers. See LN in D-FfowFM.
      integer, dimension(:), intent(in) :: kcu2 !< Link type
      integer, intent(in) :: numpoints !< Number of 1d points in volume tables.
      integer, intent(in) :: numlinks !< Number of 1d internal links
      integer, intent(in) :: numboundaries !< Number of water level boundaries

      integer :: number_of_changes
      integer :: i, n, k1, k2, kb, l, j
      integer :: boundary_type
      integer :: istru
      integer :: suction_side_node
      integer :: nstages
      type(t_structure), pointer, dimension(:) :: structs
      integer, pointer, dimension(:) :: indices
      type(t_pump), pointer :: pump

      ! Set water levels to large value
      wl_deadstorage = huge(1d0)

      ! Set the boundary conditions

      do n = 1, numboundaries
         kb = bndindex(1, n)
         k2 = bndindex(2, n)
         l = bndindex(3, n)
         boundary_type = bndindex(4, n)
         wl_deadstorage(kb) = bndvalues(n)
      end do

      ! Set the waterlevel at the suction side of a pump to the turn off level.
      ! Also set the bob of this flow link to 0.5d0 * huge
      if (network%sts%count > 0) then
         structs => network%sts%struct
         indices => network%sts%pumpindices
         do i = 1, network%sts%numpumps
            istru = indices(i)
            pump => structs(istru)%pump
            l = abs(structs(istru)%linknumbers(1))
            if (pump%direction * pump%capacity(1) > 0) then
               suction_side_node = ln2nd(1, l)
            else
               suction_side_node = ln2nd(2, l)
            end if

            nstages = pump%nrstages
            do j = 1, nstages
               wl_deadstorage(suction_side_node) = min(wl_deadstorage(suction_side_node), pump%ss_offlevel(j))
            end do
         end do
      end if

      ! Now iterate over all flow links in order to find the lowest level in the model
      number_of_changes = numpoints
      do while (number_of_changes > 0)
         number_of_changes = 0
         do l = 1, numlinks
            if (kcu2(l) /= 1) then
               cycle
            end if
            do k1 = 1, 2
               if (k1 == 1) then
                  k2 = 2
               else
                  k2 = 1
               end if
               if (wl_deadstorage(ln2nd(k1, l)) > max(inslevtube(1, l), inslevtube(2, l), wl_deadstorage(ln2nd(k2, l)))) then
                  number_of_changes = number_of_changes + 1
                  wl_deadstorage(ln2nd(k1, l)) = max(inslevtube(1, l), inslevtube(2, l), wl_deadstorage(ln2nd(k2, l)))
               end if
            end do
         end do
         continue
      end do

   end subroutine calculate_dead_storage

end module m_storage_table
