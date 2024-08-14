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

module m_sedtrails_network
   use m_sedtrails_data
   use m_alloc

   implicit none

   private :: reset_sedtrails_geom, &
              find_nodes_idom_int, &
              sedtrails_savenet, &
              sedtrails_restore

contains

   subroutine default_sedtrails_geom()

      ! remaining of variables is handled in reset_sedtrails_geom()
      call reset_sedtrails_geom()
   end subroutine default_sedtrails_geom

   subroutine reset_sedtrails_geom()
      ! node (s) related : dim=numk
      numk = 0
   end subroutine reset_sedtrails_geom

   !> increase the number of sedtrails nodes
   subroutine sedtrails_increasenetwork(k0)
      use m_alloc
      use m_missing, only: xymis

      implicit none
      integer, intent(in) :: k0 !< new number of sedtrails nodes.

      integer :: ierr

      if (k0 < kmax) return

      call sedtrails_savenet()

      if (kmax <= k0) then
         kmax = k0 + 100000
         if (allocated(xk)) then
            deallocate (xk, yk)
         end if
         allocate (xk(kmax), yk(kmax), stat=ierr)
         call aerr('xk (kmax), yk (kmax)', ierr, 2 * kmax)

         xk = xymis; yk = xymis
      end if

      call sedtrails_restore()

   end subroutine sedtrails_increasenetwork

!> restore variables with backup data
   subroutine sedtrails_restore()
      use m_sedtrails_data
      implicit none
      integer :: kx

      if (.not. allocated(xk0)) return

      kx = size(xk0) ! restore everything present (in case numk/numk0 has not yet been increased)

      xk(1:kx) = xk0(1:kx)
      yk(1:kx) = yk0(1:kx)

      numk = numk0

      return
   end subroutine sedtrails_restore

   subroutine sedtrails_savenet()
      use m_sedtrails_data
      implicit none
      integer :: ierr
      integer :: kx

      if (.not. allocated(xk)) return

      kx = kmax ! backup everything present (in case numk has not yet been increased) ! kx = numk

      if (allocated(xk0)) deallocate (xk0, yk0, zk0)
      allocate (xk0(kx), yk0(kx), zk0(kx), stat=ierr)

      xk0(1:kx) = xk(1:kx)
      yk0(1:kx) = yk(1:kx)
      zk0(1:kx) = zk(1:kx)

      numk0 = numk

      return
   end subroutine sedtrails_savenet

   ! set mask to determine which sedtrails xk,yk points lie on present grid
   ! determine interpolation weights to transfer data from flowgeom to sedtrails output
   subroutine sedtrails_get_grid_on_network()
      use m_sedtrails_data
      use m_polygon
      use m_tpoly
      use m_partitioninfo, only: my_rank, jampi, generate_partition_pol_from_idomain
      use network_data, only: netstat, netstat_ok
      use geometry_module, only: get_startend, dbdistance
      use m_missing
      use m_flowgeom, only: xz, yz, ndx, bl
      use m_ec_triangle, only: jagetwf, indxx, wfxx
      use m_ec_basic_interpolation, only: triinterp2
      use m_sferic

      implicit none

      integer :: k
      integer :: ierr, netstat_store
      integer :: ipoint, ipoly, numpoints
      integer :: istart, iend
      integer :: inside
      integer :: jakdtree, jdla

      integer, allocatable :: indices(:)
      integer, allocatable :: sedtrails_idom(:)
      double precision, allocatable :: dumout(:)
      type(tpoly), dimension(:), allocatable :: pli
      double precision, dimension(6) :: transformcoef ! don't override externalforcing setting

      transformcoef = 0d0

      ! detect grid enclosure for this partition/overlapping part of grids
      call savepol()
      if (jampi > 0) then
         netstat_store = netstat
         netstat = netstat_ok
         call generate_partition_pol_from_idomain(ierr, myrank=my_rank)
         netstat = netstat_store
      else
         call copynetboundstopol(0, 0, 1, 0)
      end if
      call realloc(iistart, maxpoly, keepexisting=.false.)
      call realloc(iiend, maxpoly, keepexisting=.false.)
      ipoint = 1
      ipoly = 0
      numpoints = 0
      do while (ipoint <= npl)
         ipoly = ipoly + 1
         if (ipoly > maxpoly) then
            maxpoly = ceiling(maxpoly * 1.1)
            call realloc(iistart, maxpoly, keepexisting=.true.)
            call realloc(iiend, maxpoly, keepexisting=.true.)
         end if

         ! get polygon start and end pointer respectively
         call get_startend(npl - ipoint + 1, xpl(ipoint:npl), ypl(ipoint:npl), istart, iend, dmiss)
         istart = istart + ipoint - 1
         iend = iend + ipoint - 1

         if (istart >= iend .or. iend > npl) exit ! done

         iistart(ipoly) = istart
         iiend(ipoly) = iend
         numpoints = numpoints + (iend - istart + 1)

!     advance pointer
         ipoint = iend + 2
      end do
      npoly = ipoly

      !
      ! allocate poly index
      if (.not. allocated(sedtrails_idom)) then
         allocate (sedtrails_idom(1:numk))
         sedtrails_idom = 0
      end if
      !
      ! convert to tpolies
      call pol_to_tpoly(npoly, pli, .false.)
      !
      ! get sedtrials points inside domain
      inside = -1
      jins = 1
      do k = 1, numk
         call dbpinpol_tpolies(pli, xk(k), yk(k), inside) ! takes into account inner pols
         if (inside == 1) then
            sedtrails_idom(k) = 1
         end if
      end do
      !
      call dealloc_tpoly(pli)
      call restorepol()
      !
      ! get own nodes
      indices = find_nodes_idom_int(sedtrails_idom, 1)
      numk = size(indices)
      !
      ! reallocate nodes arrays and copy values
      call realloc(xk1, size(xk), keepexisting=.false., fill=0d0)
      call realloc(yk1, size(xk), keepexisting=.false., fill=0d0)
      xk1 = xk
      yk1 = yk
      if (jampi > 0) then
         call realloc(iwork, size(xk), keepexisting=.false., fill=0)
         iwork = iglobal_s
         call realloc(iglobal_s, numk, keepexisting=.false., fill=0)
         iglobal_s = iwork(indices)
         deallocate (iwork)
      end if

      call realloc(xk, numk, keepexisting=.false., fill=0d0)
      call realloc(yk, numk, keepexisting=.false., fill=0d0)
      xk = xk1(indices)
      yk = yk1(indices)
      deallocate (xk1, yk1)
      !
      ! generate interpolation weights from flowgeom cell centres
      ! use dummy interpolation
      ! save in module variables st_ind, st_wf
      jagetwf = 1
      jakdtree = 1
      jdla = 1
      call realloc(indxx, (/3, numk/), keepexisting=.false., fill=0)
      call realloc(wfxx, (/3, numk/), keepexisting=.false., fill=0d0)
      call realloc(dumout, numk, keepexisting=.false., fill=dmiss)

      transformcoef(6) = 1.1d0
      call triinterp2(xk, yk, dumout, numk, jdla, &
                      xz, yz, bl, ndx, dmiss, jsferic, jins, jasfer3d, npl, 0, 0, xpl, ypl, zpl, transformcoef)
      !
      call realloc(st_ind, (/3, numk/), keepexisting=.false., fill=0)
      call realloc(st_wf, (/3, numk/), keepexisting=.false., fill=0d0)
      do k = 1, numk
         st_ind(:, k) = indxx(:, k)
         st_wf(:, k) = wfxx(:, k)
      end do
      !
      ! and now that we have the correct number of nodes:
      if (jampi > 0) then
         call realloc(idomain, numk, keepexisting=.false., fill=my_rank)
      end if
      !
      deallocate (indxx, wfxx, dumout)

   end subroutine sedtrails_get_grid_on_network

   function find_nodes_idom_int(array, min) result(indices)
      use precision

      implicit none

      integer, intent(in) :: array(:)
      integer, intent(in) :: min
      integer, allocatable :: indices(:)
      integer :: ii
      indices = pack([(ii, ii=1, size(array))], array == min)
   end function find_nodes_idom_int

end module m_sedtrails_network
