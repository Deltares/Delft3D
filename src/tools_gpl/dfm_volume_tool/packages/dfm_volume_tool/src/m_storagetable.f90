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
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   use precision, only: DP
   use messagehandling
   use m_globalparameters
   use m_volumetables
   use m_flowgeom
   use m_flowparameters
   use unstruc_channel_flow, only: tableincrement

   implicit none(type, external)
   private

   public :: generate_total_volume_table
   public :: generate_volume_table_on_branches
   public :: get_bed_top_level
   public :: calculate_dead_storage
   public :: add_volume_and_surface

contains

!> Generate the total volume/surface
   subroutine generate_total_volume_table(volume, surface, storage, dead_storage, wl_dead_storage, voltb, &
                                          bedlevel, increment, num_points, num_levels, nodes)

      use unstruc_channel_flow
      real(kind=DP), dimension(:), intent(out) :: volume !< Aggregated volume per level
      real(kind=DP), dimension(:), intent(out) :: surface !< Aggregated areas per level
      real(kind=DP), dimension(:), intent(out) :: storage !< Aggregated storage per level (=volume - dead_storage)
      real(kind=DP), dimension(:), intent(out) :: dead_storage !< Aggregated dead storage per level
      real(kind=DP), dimension(:), intent(in) :: wl_dead_storage !< Dead storage waterlevel on each grid point
      type(t_voltable), dimension(:), intent(inout) :: voltb !< Volume tables; summer-dike state is updated during evaluation.
      real(kind=DP), intent(in) :: bedlevel !< Bed level of the model
      real(kind=DP), intent(in) :: increment !< Requested increment
      integer, intent(in) :: num_points !< number of 1d grid points also the size of voltb
      integer, intent(in) :: num_levels !< number of levels in volume, surface and levels
      integer, dimension(:), intent(in) :: nodes !< mask array for defining a sub set on nodes.

      integer :: j, ipoint

      volume = 0.0_dp
      dead_storage = 0.0_dp
      storage = 0.0_dp
      surface = 0.0_dp
      do j = 1, num_points
         ipoint = nodes(j)
         call add_volume_and_surface(volume, surface, dead_storage, wl_dead_storage(ipoint), &
                                     voltb(ipoint), bedlevel, increment, num_levels)
      end do

      call compute_surface_and_storage(volume, surface, storage, dead_storage, increment, num_levels)

   end subroutine generate_total_volume_table

!> Generate the total volume/surface (TODO: and the totalised volume/surface for each branch).
   subroutine generate_volume_table_on_branches(volume, surface, storage, dead_storage, wl_dead_storage, voltb_on_links, &
                                                bedlevel, increment, num_points, num_levels, links, ln2nd)

      use unstruc_channel_flow
      real(kind=DP), dimension(:), intent(out) :: volume !< Aggregated volume per level
      real(kind=DP), dimension(:), intent(out) :: surface !< Aggregated areas per level
      real(kind=DP), dimension(:), intent(out) :: storage !< Aggregated storage per level (=volume - dead_storage)
      real(kind=DP), dimension(:), intent(out) :: dead_storage !< Aggregated dead storage per level
      real(kind=DP), dimension(:), intent(in) :: wl_dead_storage !< Water level for dead storage
      !> Volume tables; summer-dike state is updated during evaluation.
      type(t_voltable), dimension(:, :), intent(inout) :: voltb_on_links
      real(kind=DP), intent(in) :: bedlevel !< Bed level of the model
      real(kind=DP), intent(in) :: increment !< Requested increment
      integer, intent(in) :: num_points !< Number of 1d grid points also the size of voltb
      integer, intent(in) :: num_levels !< Number of levels in volume, surface and levels
      integer, dimension(:), intent(in) :: links !< Mask array for defining a sub set on flow links.
      integer, dimension(:, :), intent(in) :: ln2nd !< Indirection array links to nodes.

      integer :: j, l

      volume = 0.0_dp
      surface = 0.0_dp
      storage = 0.0_dp
      dead_storage = 0.0_dp

      do j = 1, num_points
         l = links(j)
         call add_volume_and_surface(volume, surface, dead_storage, wl_dead_storage(ln2nd(1, l)), &
                                     voltb_on_links(1, l), bedlevel, increment, num_levels)
         call add_volume_and_surface(volume, surface, dead_storage, wl_dead_storage(ln2nd(2, l)), &
                                     voltb_on_links(2, l), bedlevel, increment, num_levels)
      end do

      call compute_surface_and_storage(volume, surface, storage, dead_storage, increment, num_levels)

   end subroutine generate_volume_table_on_branches

!> Add the volume and surface for this gridpoints volume table to the aggregated volume and surface
   subroutine add_volume_and_surface(vol, sur, dead_storage, wl_dead_storage, voltb, bedlevel, increment, num_levels)
      real(kind=DP), dimension(:), intent(inout) :: vol !< Aggregated volume
      real(kind=DP), dimension(:), intent(inout) :: sur !< Aggregated surface
      real(kind=DP), dimension(:), intent(inout) :: dead_storage !< Aggregated dead storage
      real(kind=DP), intent(in) :: wl_dead_storage !< Aggregated dead storage
      type(t_voltable), intent(inout) :: voltb !< Volume table; summer-dike state is updated during evaluation.
      integer, intent(in) :: num_levels !< Number of levels in the aggregated data
      real(kind=DP), intent(in) :: bedlevel !< Bed level
      real(kind=DP), intent(in) :: increment !< Requested increment

      integer :: i
      real(kind=DP) :: water_level

      do i = 2, num_levels
         water_level = bedlevel + real(i - 1, kind=DP) * increment
         ! Compute 1d volume, using volume tables (still excl. 1D2D contributions)
         vol(i) = vol(i) + voltb%get_volume(water_level)
         sur(i) = sur(i) + voltb%get_surface(water_level)
         if (wl_dead_storage >= water_level) then
            dead_storage(i) = dead_storage(i) + voltb%get_volume(water_level)
         else
            dead_storage(i) = dead_storage(i) + voltb%get_volume(wl_dead_storage)
         end if
      end do

   end subroutine add_volume_and_surface

!> derive the surface areas from the volume table
   subroutine compute_surface_and_storage(vol, sur, storage, dead_storage, increment, num_levels)
      real(kind=DP), dimension(:), intent(in) :: vol !< Aggregated volume
      real(kind=DP), dimension(:), intent(inout) :: sur !< Aggregated surface
      real(kind=DP), dimension(:), intent(inout) :: storage !< Aggregated storage
      real(kind=DP), dimension(:), intent(in) :: dead_storage !< Aggregated dead storage
      integer, intent(in) :: num_levels !< Number of levels in the aggregated data
      real(kind=DP), intent(in) :: increment !< Requested increment

      integer :: i

      if (.not. ieee_is_finite(increment) .or. increment <= tiny(1.0_dp)) then
         call setmessage(LEVEL_ERROR, 'Volume-table increment must be finite and greater than tiny(1.0_DP).')
         error stop 1
      end if

      storage(1) = 0.0_dp
      do i = 2, num_levels
         sur(i - 1) = (vol(i) - vol(i - 1)) / increment
         storage(i) = vol(i) - dead_storage(i)
      end do
   end subroutine compute_surface_and_storage

!> Determine the lowest level and the highest level in the volume table.
   subroutine get_bed_top_level(voltb, num_points, toplevel, bedlevel)
      type(t_voltable), dimension(:), pointer, intent(in) :: voltb !< (num_points) volume table for individual grid points.
      integer, intent(in) :: num_points !< Number of grid points in the input volume table.
      !> Highest absolute level in the model at cross sections or storage nodes.
      real(kind=DP), intent(out) :: toplevel
      real(kind=DP), intent(out) :: bedlevel !< Lowest (bed)level in the model, where cross sections or storage nodes are defined.

      integer :: n
      toplevel = -huge(1.0_dp)
      bedlevel = huge(1.0_dp)

      do n = 1, num_points
         toplevel = max(toplevel, voltb(n)%topheight + voltb(n)%bedlevel)
         bedlevel = min(bedlevel, voltb(n)%bedlevel)
      end do

   end subroutine get_bed_top_level

!> Calculate the dead storage using boundary conditions, turn off levels of pumping stations and the geometry of the network.
   subroutine calculate_dead_storage(wl_dead_storage, network, bnd_values, inslevtube, bnd_index, &
                                     ln2nd, kcu2, num_links, num_points, num_boundaries)

      use m_flowgeom
      use m_network
      use m_1d_structures
      use m_pump

      type(t_network), intent(in) :: network !< Network definition of the model.
      real(kind=DP), dimension(:), intent(out) :: wl_dead_storage !< Minimal water levels after the model is drained.
      real(kind=DP), dimension(:), intent(in) :: bnd_values !< Boundary condition values. See ZBNDZ in D-FlowFM.
      real(kind=DP), dimension(:, :), intent(in) :: inslevtube !< Inside level tubes. See BOB in D-FlowFM.
      integer, dimension(:, :), intent(in) :: bnd_index !< Index numbers for boundary condtions. See KBNDZ in D-FfowFM.
      integer, dimension(:, :), intent(in) :: ln2nd !< Link nodes to node numbers. See LN in D-FfowFM.
      integer, dimension(:), intent(in) :: kcu2 !< Link type
      integer, intent(in) :: num_points !< Number of 1d points in volume tables.
      integer, intent(in) :: num_links !< Number of 1d internal links
      integer, intent(in) :: num_boundaries !< Number of water level boundaries

      integer :: number_of_changes
      integer :: i, n, k1, k2, kb, l, j
      integer :: boundary_type
      integer :: istru
      integer :: suction_side_node
      integer :: nstages
      type(t_structure), dimension(:), pointer :: structs
      integer, dimension(:), pointer :: indices
      type(t_pump), pointer :: pump

      ! Set water levels to large value
      wl_dead_storage = huge(1.0_dp)

      ! Set the boundary conditions

      do n = 1, num_boundaries
         kb = bnd_index(1, n)
         k2 = bnd_index(2, n)
         l = bnd_index(3, n)
         boundary_type = bnd_index(4, n)
         wl_dead_storage(kb) = bnd_values(n)
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
            if (real(pump%direction, kind=DP) * pump%capacity(1) > 0.0_dp) then
               suction_side_node = ln2nd(1, l)
            else
               suction_side_node = ln2nd(2, l)
            end if

            nstages = pump%nrstages
            do j = 1, nstages
               wl_dead_storage(suction_side_node) = min(wl_dead_storage(suction_side_node), pump%ss_offlevel(j))
            end do
         end do
      end if

      ! Now iterate over all flow links in order to find the lowest level in the model
      number_of_changes = num_points
      do while (number_of_changes > 0)
         number_of_changes = 0
         do l = 1, num_links
            if (kcu2(l) /= 1) then
               cycle
            end if
            do k1 = 1, 2
               if (k1 == 1) then
                  k2 = 2
               else
                  k2 = 1
               end if
               if (wl_dead_storage(ln2nd(k1, l)) > max(inslevtube(1, l), inslevtube(2, l), wl_dead_storage(ln2nd(k2, l)))) then
                  number_of_changes = number_of_changes + 1
                  wl_dead_storage(ln2nd(k1, l)) = max(inslevtube(1, l), inslevtube(2, l), wl_dead_storage(ln2nd(k2, l)))
               end if
            end do
         end do
         continue
      end do

   end subroutine calculate_dead_storage

end module m_storage_table
