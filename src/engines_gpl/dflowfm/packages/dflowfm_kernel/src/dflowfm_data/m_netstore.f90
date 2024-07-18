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

module m_netstore
   use network_data
   implicit none

   integer, parameter :: maxnodespercell = 6 !< maximum number of nodes per cell

   integer, save :: maxlinkspernode = 1 !< maximum number of links attached to a node
   integer, save :: maxnodes = 1
   integer, save :: maxlinks = 1
   integer, save :: maxcells = 1

   type(tnod), allocatable, dimension(:) :: nod_st ! dimension(maxnodes)
   type(tface), allocatable, dimension(:) :: netcell_st ! dimension(maxcells)

   double precision, allocatable, dimension(:) :: xk_st, yk_st ! dimension(maxnodes)
   integer, allocatable, dimension(:) :: nmk_st ! dimension(maxnodes)
   integer, allocatable, dimension(:) :: nb_st ! dimension(maxnodes)
   integer, allocatable, dimension(:) :: lnn_st ! dimension(maxlinks)
   integer, allocatable, dimension(:, :) :: lne_st ! dimension(2,maxlinks)
   integer, allocatable, dimension(:, :) :: kn_st ! dimension(3,maxlinks)

   integer :: numnodes, numlinks, numcells

!  pointers
   integer, allocatable, dimension(:) :: ik_st !< nodes, dimension(maxnodes)
   integer, allocatable, dimension(:) :: iL_st !< links, dimension(maxlinks)
   integer, allocatable, dimension(:) :: ip_st !< cells, dimension(maxcells)

contains

   !> save network data locally
   subroutine local_netstore(k)
      use m_netw
      use m_alloc

      implicit none

      integer, dimension(:), intent(in) :: k !< list of cells

      integer :: i, j
      integer :: inode, ilink, icell, M, N

      do i = 1, size(k)
         N = netcell(k(i))%N
         if (N > maxnodespercell) then
            call qnerror('local_netstore: array size error', ' ', ' ')
            return
         end if
      end do

      M = sum(netcell(k)%N)

      !  determine upper bounds for arrays
      maxnodes = max(M, maxnodes)
      maxlinks = max(M, maxlinks)
      maxcells = max(size(k), maxcells)

      maxlinkspernode = max(0, maxlinkspernode)
      do i = 1, size(k)
         icell = k(i)
         do j = 1, netcell(icell)%N
            inode = netcell(icell)%nod(j)
            maxlinkspernode = max(nmk(inode), maxlinkspernode)
         end do
      end do

      if (.not. allocated(ik_st)) then
         !     allocation
         allocate (nod_st(maxnodes), xk_st(maxnodes), yk_st(maxnodes), nmk_st(maxnodes), nb_st(maxnodes), ik_st(maxnodes))
         allocate (lnn_st(maxlinks), lne_st(2, maxlinks), kn_st(3, maxlinks), iL_st(maxlinks))
         allocate (netcell_st(maxcells), ip_st(maxcells))
         do i = 1, maxcells
            allocate (netcell_st(i)%lin(maxnodespercell), netcell_st(i)%nod(maxnodespercell))
         end do
         do i = 1, maxnodes
            allocate (nod_st(i)%lin(maxlinkspernode))
         end do
      else
         !     memory checks and reallocation if necessary
         !     maxnodes check
         if (size(ik_st) < maxnodes) then
            deallocate (nod_st)
            allocate (nod_st(maxnodes))
            do i = 1, maxnodes
               allocate (nod_st(i)%lin(maxlinkspernode))
            end do
            call realloc(xk_st, maxnodes)
            call realloc(yk_st, maxnodes)
            call realloc(nmk_st, maxnodes)
            call realloc(nb_st, maxnodes)
            call realloc(ik_st, maxnodes)
         end if

         !     maxlinks check
         if (size(iL_st) < maxlinks) then
            call realloc(lnn_st, maxlinks)
            call realloc(lne_st, (/2, maxlinks/))
            call realloc(kn_st, (/3, maxlinks/))
            call realloc(iL_st, maxlinks)
         end if

         !     maxcells check
         if (size(ip_st) < maxcells) then
            deallocate (netcell_st)
            allocate (netcell_st(maxcells))
            do i = 1, maxcells
               allocate (netcell_st(i)%lin(maxnodespercell), netcell_st(i)%nod(maxnodespercell))
            end do
            call realloc(ip_st, maxcells)
         end if

         !     maxlinkspernode check
         if (size(nod_st(1)%lin) < maxlinkspernode) then
            do i = 1, maxnodes
               call realloc(nod_st(i)%lin, maxlinkspernode)
            end do
         end if
      end if

      numnodes = 0
      numlinks = 0
      numcells = size(k)

      ip_st(1:numcells) = k

      do i = 1, numcells
         icell = ip_st(i)
         N = netcell(icell)%N

         if (icell == 5005) then
            continue
         end if

         !     store cell data
         netcell_st(i)%lin(1:N) = netcell(icell)%lin(1:N)
         netcell_st(i)%nod(1:N) = netcell(icell)%nod(1:N)
         netcell_st(i)%N = N

         !     store node and link data
         !        use nmk<-100 to mask nodes
         !        use lnn<-100 to mask links
         do j = 1, netcell(icell)%N
            inode = netcell(icell)%nod(j)
            ilink = netcell(icell)%lin(j)

            if (nmk(inode) > maxlinkspernode) then
               call qnerror('local_netstore: array size error', ' ', ' ')
               return
            end if

            if (nmk(inode) >= 0) then
               numnodes = numnodes + 1

               ik_st(numnodes) = inode
               nod_st(numnodes)%lin(1:nmk(inode)) = nod(inode)%lin(1:nmk(inode))
               nmk_st(numnodes) = nmk(inode)
               nb_st(numnodes) = nb(inode)
               xk_st(numnodes) = xk(inode)
               yk_st(numnodes) = yk(inode)

               nmk(inode) = -nmk(inode) - 100
            end if

            if (lnn(ilink) >= 0) then
               numlinks = numlinks + 1

               iL_st(numlinks) = ilink
               lnn_st(numlinks) = lnn(ilink)
               lne_st(:, numlinks) = lne(:, ilink)
               kn_st(:, numlinks) = kn(:, ilink)

               lnn(ilink) = -lnn(ilink) - 100
            end if
         end do
      end do

      !  reset mask
      do i = 1, numnodes
         nmk(ik_st(i)) = -nmk(ik_st(i)) - 100
      end do

      do i = 1, numlinks
         lnn(iL_st(i)) = -lnn(iL_st(i)) - 100
      end do

      return
   end subroutine local_netstore

   !> restore network data locally
   subroutine local_netrestore()
      use m_netw
      use m_alloc

      implicit none

      integer :: i, N
      integer :: icell, iLink, inode

      do i = 1, numcells
         icell = ip_st(i)

         N = netcell_st(i)%N
         netcell(icell)%N = N
         call realloc(netcell(icell)%nod, N)
         call realloc(netcell(icell)%lin, N)
         netcell(icell)%nod(1:N) = netcell_st(i)%nod(1:N)
         netcell(icell)%lin(1:N) = netcell_st(i)%lin(1:N)
      end do

      do i = 1, numlinks
         iLink = iL_st(i)
         lnn(ilink) = lnn_st(i)
         lne(:, ilink) = lne_st(:, i)
         kn(:, ilink) = kn_st(:, i)
      end do

      do i = 1, numnodes
         inode = ik_st(i)
         N = nmk_st(i)
         call realloc(nod(inode)%lin, N)
         nod(inode)%lin(1:N) = nod_st(i)%lin(1:N)
         nmk(inode) = nmk_st(i)
         nb(inode) = nb_st(i)
         xk(inode) = xk_st(i)
         yk(inode) = yk_st(i)
      end do

      return
   end subroutine local_netrestore

   subroutine local_netdealloc()
      implicit none

      integer :: i

      if (allocated(xk_st)) deallocate (xk_st)
      if (allocated(yk_st)) deallocate (yk_st)
      if (allocated(nmk_st)) deallocate (nmk_st)
      if (allocated(nb_st)) deallocate (nb_st)
      if (allocated(ik_st)) deallocate (ik_st)
      if (allocated(ip_st)) deallocate (ip_st)
      if (allocated(iL_st)) deallocate (iL_st)

      if (allocated(lnn_st)) deallocate (lnn_st)
      if (allocated(lne_st)) deallocate (lne_st)
      if (allocated(kn_st)) deallocate (kn_st)

      if (allocated(netcell_st)) then
         do i = 1, ubound(netcell_st, 1)
            if (allocated(netcell_st(i)%lin)) deallocate (netcell_st(i)%lin)
            if (allocated(netcell_st(i)%nod)) deallocate (netcell_st(i)%nod)
         end do
         deallocate (netcell_st)
      end if

      if (allocated(nod_st)) then
         do i = 1, ubound(nod_st, 1)
            if (allocated(nod_st(i)%lin)) deallocate (nod_st(i)%lin)
         end do
         deallocate (nod_st)
      end if

      return
   end subroutine local_netdealloc

end module m_netstore
