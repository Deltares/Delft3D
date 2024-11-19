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

!> This routine removes (mesh) nodes and net links (i.e. mesh edges) that are
!! not used in the definition of the mesh faces (i.e. grid cells). This routine
!! uses the initial network geometry as defined in network_data.
!!
!! This implementation does not yet take into account 1D links, hence the call
!! is subject to the condition numl == 0 in flow_geominit.
subroutine remove_unused_nodes_and_links()
   use network_data, only: numk, numl, nump, kc, kn, lne, lnn, netcell, nmk, nod, tnod, xk, yk, zk
   use m_alloc
   implicit none

   ! local variables
   logical, dimension(:), allocatable :: nod_used !< flag specifying whether a node is used in the face definitions
   integer, dimension(:), allocatable :: k2knew !< array containing the new node number for each original node (0 if unused)
   integer, dimension(:), allocatable :: l2lnew !< array containing the new netlink number for each original netlink (0 if unused)
   logical, dimension(:), allocatable :: lin_used !< flag specifying whether a netlink is used in the face definitions
   type(tnod), allocatable :: nod_org(:) !< Backup for nod.

   integer :: i !< generic loop index
   integer :: ic !< loop index for faces/netcells
   integer :: ierr !< error code memory allocation
   integer :: k !< original node index
   integer :: knew !< new node index; knew <= k
   integer :: l !< original netlink index
   integer :: lnew !< new netlink index; lnew <= l
   integer :: numk_new !< new number of nodes
   integer :: numl_new !< new number of netlinks

   ! allocate flag arrays to check if nodes and links are used
   allocate (nod_used(numk), k2knew(numk), stat=ierr)
   call aerr('nod_used(numk), k2knew(numk)', ierr, numk * 2)
   allocate (lin_used(numl), l2lnew(numl), stat=ierr)
   call aerr('lin_used(numl), l2lnew(numl)', ierr, numl * 2)

   ! loop over faces/netcells to check which nodes are used
   nod_used = .false.
   do ic = 1, nump
      do i = 1, netcell(ic)%n
         nod_used(netcell(ic)%nod(i)) = .true.
      end do
   end do

   ! loop over netlinks to check which ones connect used nodes
   lin_used = .false.
   do l = 1, numl
      if (nod_used(kn(1, l)) .and. nod_used(kn(2, l))) then
         lin_used(l) = .true.
      end if
   end do

   ! generate new node numbering
   k2knew = 0
   knew = 0
   do k = 1, numk
      if (nod_used(k)) then
         knew = knew + 1
         k2knew(k) = knew
      end if
   end do
   numk_new = knew

   ! generate new edge/netlink numbering
   l2lnew = 0
   lnew = 0
   do l = 1, numl
      if (lin_used(l)) then
         lnew = lnew + 1
         l2lnew(l) = lnew
      end if
   end do
   numl_new = lnew

   ! check if there is anything to strip
   if (numk_new < numk .or. numl_new < numl) then

      ! prepare nod_org for temporarily receiving the data from nod
      allocate (nod_org(numk_new), stat=ierr)
      call aerr('nod_org(numk_new)', ierr, numk_new)

      ! update the arrays running over the number of nodes
      do k = 1, numk
         knew = k2knew(k)
         if (knew > 0) then
            ! update the edges/netlinks connected to this node
            i = 0
            do l = 1, nmk(k)
               lnew = l2lnew(nod(k)%lin(l))
               if (lnew > 0) then
                  i = i + 1
                  nod(k)%lin(i) = lnew
               end if
            end do
            nmk(k) = i
            !
            allocate (nod_org(knew)%lin(i), stat=ierr)
            call aerr('nod_org(knew)%lin(i)', ierr, i)
            nod_org(knew)%lin = nod(k)%lin
            !
            xk(knew) = xk(k)
            yk(knew) = yk(k)
            zk(knew) = zk(k)
            kc(knew) = kc(k)
            nmk(knew) = nmk(k)
         end if
      end do

      ! adjust the size of the node arrays
      call realloc(xk, numk_new)
      call aerr('xk [realloc]', ierr, numk_new)
      call realloc(yk, numk_new)
      call aerr('yk [realloc]', ierr, numk_new)
      call realloc(zk, numk_new)
      call aerr('zk [realloc]', ierr, numk_new)
      call realloc(kc, numk_new)
      call aerr('kc [realloc]', ierr, numk_new)
      call realloc(nmk, numk_new)
      call aerr('nmk [realloc]', ierr, numk_new)
      ! finally move nod_org back to nod
      do k = 1, numk
         deallocate (nod(k)%lin)
      end do
      deallocate (nod)
      call move_alloc(nod_org, nod)

      ! update the arrays running over the number of edges/netlinks
      do l = 1, numl
         lnew = l2lnew(l)
         if (lnew > 0) then
            kn(1, l) = k2knew(kn(1, l))
            kn(2, l) = k2knew(kn(2, l))
            !
            lnn(lnew) = lnn(l)
            lne(:, lnew) = lne(:, l)
            kn(:, lnew) = kn(:, l)
         end if
      end do

      ! adjust the size of the node arrays
      call realloc(lnn, numl_new)
      call aerr('lnn [realloc]', ierr, numl_new)
      call realloc(lne, (/2, numl_new/))
      call aerr('lne [realloc]', ierr, 2 * numl_new)
      call realloc(kn, (/3, numl_new/))
      call aerr('kn [realloc]', ierr, 3 * numl_new)

      ! update the arrays running over the number of faces/netcells
      do ic = 1, nump
         do i = 1, netcell(ic)%n
            netcell(ic)%nod(i) = k2knew(netcell(ic)%nod(i))
            netcell(ic)%lin(i) = l2lnew(netcell(ic)%lin(i))
         end do
      end do

      ! update the number of nodes and edges/netlinks
      numk = numk_new
      numl = numl_new
   end if

   ! clean up the temporary arrays
   deallocate (nod_used, k2knew)
   deallocate (lin_used, l2lnew)

end subroutine remove_unused_nodes_and_links
