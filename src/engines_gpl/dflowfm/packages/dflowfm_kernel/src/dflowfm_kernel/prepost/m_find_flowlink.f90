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
   
module m_find_flowlink
   use precision, only: dp
   
   implicit none
   
   private
   
   public :: find_nearest_1D_or_boundary_flowlink_bruteforce, find_nearest_flowlinks
   
   contains
   
   !> Find for each input point the flow link with the shortest perpendicular distance to it, given a set of points [xx, yy].
   subroutine find_nearest_flowlinks(xx, yy, link_nrs_nearest)
      use MessageHandling, only: mess, LEVEL_WARN, LEVEL_ERROR
      use m_GlobalParameters, only: INDTP_ALL
      use m_partitioninfo, only: jampi
      use mpi
      
      real(dp), dimension(:), intent(in   ) :: xx               !< x-coordinate of input points
      real(dp), dimension(:), intent(in   ) :: yy               !< y-coordinate of input points
      integer,  dimension(:), intent(  out) :: link_nrs_nearest !< Link numbers with the shortest perpendicular distance to the points [xx, yy]
      
      integer                       :: ii,k,jaoutside
      character(len=255)            :: str
      real(dp), dimension(size(xx)) :: distances
      integer                       :: ierror
      
      if (size(xx) /= size(yy) .or. size(xx) /= size(link_nrs_nearest)) then
         call mess(LEVEL_ERROR,'find_flowlinks: unmatched input array size')
      end if
      
      ! Return warnings for points that lie outside the grid
      do ii = 1, size(xx)
         k = 0
         jaoutside = -1
         call inflowcell(xx(ii), yy(ii), k, jaoutside, INDTP_ALL)
         if (jampi == 1) then
            call mpi_allreduce( mpi_in_place, k, 1, mpi_integer, mpi_max, mpi_comm_world, ierror)
         end if
         if (k == 0) then
            write(str,'(A,I6,A,F14.4,A,F14.4,A)') 'find_flowlinks: point ', ii, '([x, y] = [', xx(ii), ',', yy(ii), & 
                                                  ']) lies outside of the model grid; closest flowlink might be inaccurate'
            call mess(LEVEL_WARN,trim(str))
         end if
      end do
      
      ! First try it using a k-d tree. If that fails, use a brute-force search instead
      call find_nearest_flowlinks_kdtree(xx, yy, link_nrs_nearest, distances, ierror)
      if (ierror /= 0) then
         call find_nearest_flowlinks_bruteforce(xx, yy, link_nrs_nearest, distances)
      end if
      
      call reduce_nearest_flowlinks(distances, link_nrs_nearest)
      
   end subroutine find_nearest_flowlinks
   
   !> Find for each input point the flow link with the shortest perpendicular distance to it, given a set of points [xx, yy].
   !! Uses the k-d tree routines
   subroutine find_nearest_flowlinks_kdtree(xx, yy, link_nrs_nearest, distances, ierror)
      use MessageHandling, only: mess, LEVEL_ERROR
      use m_flowgeom, only: lnx, ln, xz, yz
      use m_alloc, only: realloc
      use kdtree2Factory, only: kdtree_instance, find_nearest_sample_kdtree
      use geometry_module, only: dlinedis
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dmiss
      
      real(dp), dimension(:), intent(in   ) :: xx               !< x-coordinate of input points
      real(dp), dimension(:), intent(in   ) :: yy               !< y-coordinate of input points
      integer,  dimension(:), intent(  out) :: link_nrs_nearest !< Link numbers with the shortest perpendicular distance to the points [xx, yy]
      real(dp), dimension(:), intent(  out) :: distances        !< Perpendicular distances between those flowlinks to the points
      integer,                intent(  out) :: ierror           !< Error code (0 if no error)
      
      real(dp), dimension(lnx)             :: flowlink_midpoints_x
      real(dp), dimension(lnx)             :: flowlink_midpoints_y
      integer                              :: link_id, ka, kb
      integer                              :: number_of_points, i_point
      real(dp), dimension(lnx)             :: zs_dummy
      type(kdtree_instance)                :: treeinst
      integer, parameter                   :: n_nearest_kdtree = 100
      integer                              :: n_nearest_kdtree_
      integer, dimension(:), allocatable   :: link_nrs_nearest_midpoint
      integer                              :: i_sample
      real(dp)                             :: dist_perp
      
      link_nrs_nearest = 0
      distances = huge(distances)
      ierror = 0
      
      ! Calculate the x,y-coordinates of the midpoints of all the flowlinks
      do link_id = 1, lnx
         ka = ln(1,link_id)
         kb = ln(2,link_id)
         flowlink_midpoints_x(link_id) = (xz(ka) + xz(kb)) / 2.0_dp
         flowlink_midpoints_y(link_id) = (yz(ka) + yz(kb)) / 2.0_dp
      end do
      
      n_nearest_kdtree_ = min(n_nearest_kdtree, lnx)
      allocate(link_nrs_nearest_midpoint(n_nearest_kdtree_), source = 0)
      
      number_of_points = size(xx)
      do i_point = 1, number_of_points
         
         ! The k-d tree uses the distance to the midpoints of the flowlinks
         ! which is not actually what we want (namely the perpendicular distance).
         ! To solve this, we query the nearest n points (default 100) and then
         ! do a brute-force search on this (very) small subset
         call find_nearest_sample_kdtree(treeinst, lnx, 2, flowlink_midpoints_x, flowlink_midpoints_y, zs_dummy, &
                                         xx(i_point), yy(i_point), n_nearest_kdtree_, link_nrs_nearest_midpoint, &
                                         ierror, jsferic, dmiss)
         if (ierror /= 0) then  ! Failed to build a k-d tree
            exit
         end if
         if (link_nrs_nearest_midpoint( 1) == 0) then ! Failed to build a k-d tree?
            ierror = 1
            exit
         end if
         
         ! Now loop over the n nearest points to find the one with the shortest perpendicular distance
         do i_sample = 1, n_nearest_kdtree_
            link_id = link_nrs_nearest_midpoint(i_sample)
            call perpendicular_distance_to_flowlink(xx(i_point), yy(i_point), link_id, dist_perp)
            if (dist_perp < distances(i_point)) then
               link_nrs_nearest(i_point) = link_id
               distances(i_point) = dist_perp
            end if
         end do
         
      end do
      
   end subroutine find_nearest_flowlinks_kdtree
   
   !> Find for each input point the flow link with the shortest perpendicular distance to it, given a set of points [xx, yy].
   !! Brute-force approach: simply check all flowlinks in the entire grid
   subroutine find_nearest_flowlinks_bruteforce(xx, yy, link_nrs_nearest, distances)
      use m_flowgeom, only: lnx
      
      real(dp), dimension(:), intent(in   ) :: xx               !< x-coordinate of input points
      real(dp), dimension(:), intent(in   ) :: yy               !< y-coordinate of input points
      integer,  dimension(:), intent(  out) :: link_nrs_nearest !< Link numbers with the shortest perpendicular distance to the points [xx, yy]
      real(dp), dimension(:), intent(  out) :: distances        !< Perpendicular distances between those flowlinks to the points
      
      integer  :: number_of_points, i_point
      integer  :: link_id
      real(dp) :: dist_perp, dist_perp_min
      
      link_nrs_nearest = 0
      distances = huge(distances)
      
      number_of_points = size(xx)
      do i_point = 1, number_of_points
         do link_id = 1, lnx
            call perpendicular_distance_to_flowlink(xx(i_point), yy(i_point), link_id, dist_perp)
            if (dist_perp < distances(i_point)) then
               link_nrs_nearest(i_point) = link_id
               distances(i_point) = dist_perp
            end if
         end do
      end do
      
   end subroutine find_nearest_flowlinks_bruteforce
   
   !> Find the 1-D or boundary flowlink with the shortest perpendicular distance to the point [x, y]
   !! Brute-force approach: simply check all flowlinks in the entire grid
   subroutine find_nearest_1D_or_boundary_flowlink_bruteforce(x, y, link_nr_nearest)
      use m_flowgeom, only: lnx, lnx1D, lnxi
      
      real(dp), intent(in   ) :: x               !< x-Coordinate of input point.
      real(dp), intent(in   ) :: y               !< y-Coordinate of input point.
      integer,  intent(  out) :: link_nr_nearest !< Link number of nearest 1D or boundary flow link.
      
      integer  :: link_id
      real(dp) :: dist_perp, dist_perp_min
      integer  :: ja
      
      link_nr_nearest = 0
      dist_perp_min   = huge(dist_perp_min)
   
      do link_id = 1, lnx
         if (link_id <= lnx1D .or. link_id > lnxi) then ! Only check 1-D and boundary flowlinks
            call perpendicular_distance_to_flowlink(x, y, link_id, dist_perp, ja)
            if (ja == 1) then
               if (dist_perp < dist_perp_min) then
                  link_nr_nearest = link_id
                  dist_perp_min = dist_perp
               end if
            end if
         end if
      end do
      
   end subroutine find_nearest_1D_or_boundary_flowlink_bruteforce
   
   !> Calculate the perpendicular distance from the point [x, y] to a flowlink
   subroutine perpendicular_distance_to_flowlink(x, y, link_id, perpendicular_distance, ja)
      use m_flowgeom, only: lnx, ln, xz, yz
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss
      
      real(dp),          intent(in   ) :: x                      !< x-Coordinate of input point.
      real(dp),          intent(in   ) :: y                      !< y-Coordinate of input point.
      integer,           intent(in   ) :: link_id                !< id of the flowlink
      real(dp),          intent(  out) :: perpendicular_distance !< Perpendicular distance from the point [x, y] to a flowlink
      integer, optional, intent(  out) :: ja                     !< Whether or not (1/0) the computation was possible. If line points 1 and 2
                                                                 !! coincide, ja==0, and distance is just Euclidean distance between 3 and 1. 
      integer  :: ka, kb
      real(dp) :: xa, ya, xb, yb
      integer  :: ja_
      real(dp) :: xn, yn
      
      ka = ln(1,link_id)
      kb = ln(2,link_id)
      xa = xz(ka)
      ya = yz(ka)
      xb = xz(kb)
      yb = yz(kb)
      
      call dlinedis(x, y, xa, ya, xb, yb, ja_, perpendicular_distance, xn, yn, jsferic, jasfer3D, dmiss)
      
      if (present(ja)) then
         ja = ja_
      end if
            
   end subroutine perpendicular_distance_to_flowlink
   
   !> Reduce the indices of the nearest flowlinks across the processes,
   !! so that the index is only non-zero for the process that actually owns the nearest flowlink
   subroutine reduce_nearest_flowlinks(distances, link_nrs_nearest)
      use MessageHandling, only: mess, LEVEL_ERROR
      use mpi
      use m_partitioninfo, only: jampi, my_rank
      
      real(dp), dimension(:), intent(in   ) :: distances        !< Distances to nearest flowlink reported by each process
      integer,  dimension(:), intent(inout) :: link_nrs_nearest !< id of the flowlink whose midpoint lies closest to the point [x, y]
      
      integer                :: number_of_links, i
      real(dp), dimension(2) :: distance_and_rank_pair
      integer                :: ierr, rank_with_shortest_distance
      
      if (jampi == 0) then
         return
      end if
      
      if (size(distances) /= size(link_nrs_nearest)) then
         call mess(LEVEL_ERROR,'reduce_nearest_flowlinks: unmatched input array size')
      end if
      
      number_of_links = size(distances)
      
      do i = 1, number_of_links
         
         ! Use mpi_allreduce with mpi_minloc to determine which process reported the shortest distance
         distance_and_rank_pair = [distances(i), real(my_rank,dp)]  ! Because mpi_minloc needs a pair of values of the same type
         call mpi_allreduce(mpi_in_place, distance_and_rank_pair, 1, mpi_2double_precision, mpi_minloc, mpi_comm_world, ierr)
         rank_with_shortest_distance = nint(distance_and_rank_pair(2))
         
         ! Let the process with the shortest distance keep their flowlink id; all others set it to zero
         if (my_rank /= rank_with_shortest_distance) then
            link_nrs_nearest(i) = 0
         end if
         
      end do
      
   end subroutine reduce_nearest_flowlinks
   
end module m_find_flowlink