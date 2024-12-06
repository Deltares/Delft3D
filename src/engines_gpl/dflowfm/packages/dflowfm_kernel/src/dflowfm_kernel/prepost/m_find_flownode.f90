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

module m_find_flownode

   implicit none

   private

   public :: find_nearest_flownodes, find_nearest_flownodes_kdtree

contains

!> Find for each input point the nearest flow node, given a set of points [xx, yy].
   subroutine find_nearest_flownodes(n, xx, yy, names, node_nrs_nearest, jakdtree, jaoutside, iLocTp)
      use precision, only: dp
      use unstruc_messages
      use m_partitioninfo
      use m_flowgeom
      use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
      use kdtree2Factory
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use m_inflowcell

      implicit none

      integer, intent(in) :: n !< number of points
      real(kind=dp), dimension(n), intent(in) :: xx !< x-coordinate of input points
      real(kind=dp), dimension(n), intent(in) :: yy !< y-coordinate of input points
      character(len=IdLen), dimension(n), intent(in) :: names !< names of points
      integer, dimension(n), intent(inout) :: node_nrs_nearest !< associated flow node numbers, if any found.
      integer, intent(inout) :: jakdtree !< use kdtree (1) or not (other)
      integer, intent(in) :: jaoutside !< allow outside cells (for 1D) (1) or not (0)
      integer, intent(in) :: iLocTp !< Node type, one of INDTP_1D/2D/ALL.
      integer :: ierror !< error (1) or not (0)
      integer :: i, k, k1b
      integer, dimension(1) :: idum
      real(kind=dp) :: d1, d2

      ierror = 1

      if (jakdtree == 1) then
         call find_nearest_flownodes_kdtree(treeglob, n, xx, yy, node_nrs_nearest, jaoutside, iLocTp, ierror)

         if (jampi == 1) then
            ! globally reduce ierror
            idum(1) = ierror
            call reduce_int_max(1, idum)
            ierror = idum(1)
         end if

         if (ierror /= 0) then
            jakdtree = 0 ! retry without kdtree
         end if

         ! disable observation stations without attached flowlinks
         do i = 1, n
            k = node_nrs_nearest(i)
            if (k > 0) then
               if (nd(k)%lnx < 1) then
                  node_nrs_nearest(i) = 0
               end if
            end if
         end do

      end if

      if (jakdtree == 0) then

         do i = 1, n
            call inflowcell(xx(i), yy(i), k, jaoutside, iLocTp)
            if (jaoutside == 1 .and. (iLocTp == INDTP_1D .or. iLocTp == INDTP_ALL)) then
               call find_nearest_1D_or_boundary_flownode_bruteforce(xx(i), yy(i), k1b)
               if (k /= 0 .and. k1b /= 0) then
                  d1 = dbdistance(xz(k1b), yz(k1b), xx(i), yy(i), jsferic, jasfer3D, dmiss)
                  d2 = dbdistance(xz(k), yz(k), xx(i), yy(i), jsferic, jasfer3D, dmiss)
                  if (d1 < d2) then
                     k = k1b
                  end if
               else if (k1b /= 0) then
                  k = k1b
               end if
            end if
            node_nrs_nearest(i) = 0
            if (k /= 0) then
               if (nd(k)%lnx > 0) then
                  node_nrs_nearest(i) = k
               end if
            end if
         end do
      end if

      if (jampi == 1 .and. n > 0) then
         ! check which subdomains own which points
         call reduce_kobs(n, node_nrs_nearest, xx, yy, jaoutside)
      end if

      do i = 1, n
         if (node_nrs_nearest(i) == 0) then
            write (msgbuf, '(a,i0,a,a,a)') 'Could not find flowcell for point #', i, ' (', trim(names(i)), '). Discarding.'
            call msg_flush()
         end if
      end do

      ierror = 0
1234  continue

   end subroutine find_nearest_flownodes

!> Find for each input point the nearest flow node, given a set of points [xx, yy].
!! Uses the k-d tree routines
   subroutine find_nearest_flownodes_kdtree(treeinst, Ns, xs, ys, node_nr_nearest, jaoutside, iLocTp, ierror)
      use precision, only: dp

      use m_missing
      use m_flowgeom
      use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
      use kdtree2Factory
      use m_sferic
      use unstruc_messages
      use gridoperations
      use geometry_module, only: dbdistance, pinpok
      use m_wall_clock_time

      implicit none

      type(kdtree_instance), intent(inout) :: treeinst
      integer, intent(in) :: Ns !< number of samples
      real(kind=dp), dimension(Ns), intent(in) :: xs, ys !< observation coordinates
      integer, dimension(Ns), intent(out) :: node_nr_nearest !< node number of nearest flow node
      integer, intent(in) :: jaoutside !< allow outside cells (for 1D) (1) or not (0)
      integer, intent(in) :: iLocTp !< (0) not for obs, or obs with locationtype==0, (1) for obs with locationtype==1, (2) for obs with locationtype==2
      integer, intent(out) :: ierror !< error (>0), or not (0)

      character(len=128) :: mesg

      integer, parameter :: Msize = 10

      real(kind=dp), dimension(Msize) :: xloc, yloc
      integer, dimension(Msize) :: Lorg
      integer, dimension(Msize) :: LnnL

      real(kind=dp) :: dmaxsize, R2search, t0, t1, zz

      integer :: i, ip1, isam, in, k, N, NN
      integer :: nstart, nend
      real(kind=dp) :: dist_old, dist_new

      ierror = 1

      node_nr_nearest = 0

      call wall_clock_time(t0)

      ! build kdtree
      call build_kdtree(treeinst, Ns, xs, ys, ierror, jsferic, dmiss)

      if (ierror /= 0) then
         goto 1234
      end if

      ! define the searching range, this is especially for the purpose of snapping obs to 1D, 2D or 1D+2D flownodes.
      ! For other purpose it should stay as before
      select case (iLocTp)
      case (INDTP_ALL)
         nstart = 1
         nend = ndx
      case (INDTP_1D) ! 1d flownodes coordinates
         nstart = ndx2D + 1
         nend = ndx
      case (INDTP_2D) ! 2d flownodes coordinates
         nstart = 1
         nend = ndx2D
      end select

      call mess(LEVEL_INFO, 'Finding flow nodes...')

      ! loop over flownodes
      do k = nstart, nend

         ! fill query vector
         call make_queryvector_kdtree(treeinst, xz(k), yz(k), jsferic)

         ! compute maximum flowcell dimension
         dmaxsize = 0d0
         N = size(nd(k)%x)
         do i = 1, N
            ip1 = i + 1
            if (ip1 > N) then
               ip1 = ip1 - N
            end if
            dmaxsize = max(dmaxsize, dbdistance(nd(k)%x(i), nd(k)%y(i), nd(k)%x(ip1), nd(k)%y(ip1), jsferic, jasfer3D, dmiss))
         end do

         ! determine square search radius
         R2search = 1.1d0 * dmaxsize**2 ! 1.1d0: safety

         ! get the cell polygon that is safe for periodic, spherical coordinates, inluding poles
         call get_cellpolygon(k, Msize, N, 1d0, xloc, yloc, LnnL, Lorg, zz)

         if (N < 1) then
            if (k <= Ndxi) then
               continue
            end if
            cycle
         end if

         ! count number of points in search area
         NN = kdtree2_r_count(treeinst%tree, treeinst%qv, R2search)

         if (NN == 0) cycle ! no links found

         ! reallocate if necessary
         call realloc_results_kdtree(treeinst, NN)

         ! find nearest NN samples
         call kdtree2_n_nearest(treeinst%tree, treeinst%qv, NN, treeinst%results)

         ! check if samples are in cell
         do i = 1, NN
            isam = treeinst%results(i)%idx

            if (k > ndx2D .and. k < ndxi + 1 .and. jaoutside == 1) then ! For 1D nodes, skip point-in-cell check
               in = 1 ! These are always accepted if closest.
            else
               call pinpok(xs(isam), ys(isam), N, xloc, yloc, in, jins, dmiss)
            end if
            if (in == 1) then
               if (node_nr_nearest(isam) /= 0) then ! should not happen, but it can: for example in case of overlapping 1D branches
                  write (mesg, "('find_nearest_flownodes_kdtree: sample/point ', I0, ' in cells ', I0, ' and ', I0)") isam, node_nr_nearest(isam), k
                  call mess(LEVEL_INFO, mesg)
                  ! goto 1234
                  if (k > ndx2D .and. k < ndxi + 1 .and. jaoutside == 1) then ! ONLY in case of a 1D node, consider replacing, if the 1D node is closer
                     dist_old = dbdistance(xs(isam), ys(isam), xz(node_nr_nearest(isam)), yz(node_nr_nearest(isam)), jsferic, jasfer3D, dmiss)
                     dist_new = dbdistance(xs(isam), ys(isam), xz(k), yz(k), jsferic, jasfer3D, dmiss)
                     if (dist_new < dist_old) then ! if the new candidate is nearer to the observation station  ...
                        node_nr_nearest(isam) = k ! ... adopt the new candidate as primary candidate
                     end if
                     write (mesg, "('   selected : ',I0,' based on distance comparison.')") node_nr_nearest(isam)
                     call mess(LEVEL_INFO, mesg)
                  end if
               else
                  node_nr_nearest(isam) = k
               end if
            end if
         end do
      end do

      call wall_clock_time(t1)

      write (mesg, "('done in ', F12.5, ' sec.')") t1 - t0
      call mess(LEVEL_INFO, trim(mesg))

      ierror = 0
1234  continue

      ! deallocate
      if (treeinst%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeinst)

   end subroutine find_nearest_flownodes_kdtree

!> Find the 1-D or boundary flownode with the shortest distance to the point [x, y]
!! Brute-force approach: simply check all flowlinks in the entire grid
   subroutine find_nearest_1D_or_boundary_flownode_bruteforce(x, y, node_nr_nearest)
      use stdlib_kinds, only: dp
      use m_find_flowlink, only: find_nearest_1D_or_boundary_flowlink_bruteforce
      use m_flowgeom, only: ln, xz, yz
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dmiss

      implicit none

      real(dp), intent(in) :: x !< x-Coordinate of input point.
      real(dp), intent(in) :: y !< y-Coordinate of input point.
      integer, intent(out) :: node_nr_nearest !< Node number of nearest 1D or boundary flow node.

      integer :: link_nr_nearest, ka, kb

      node_nr_nearest = 0

      call find_nearest_1D_or_boundary_flowlink_bruteforce(x, y, link_nr_nearest)

      if (link_nr_nearest /= 0) then
         ka = ln(1, link_nr_nearest)
         kb = ln(2, link_nr_nearest)
         if (dbdistance(x, y, xz(ka), yz(ka), jsferic, jasfer3D, dmiss) < &
             dbdistance(x, y, xz(kb), yz(kb), jsferic, jasfer3D, dmiss)) then
            node_nr_nearest = ka
         else
            node_nr_nearest = kb
         end if
      end if

   end subroutine find_nearest_1D_or_boundary_flownode_bruteforce

end module m_find_flownode
