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

!> delete cell and update administration (no direct need for findcells afterwards)
subroutine deletecell(k, ndirect, nindirect, kdirect, kindirect, kne, Lprompt_nogo, jadeleted)
   use m_netw
   use m_missing
   use m_netstore
   use m_polygon, only: NPL, xpl, ypl, zpl
   use geometry_module, only: dbpinpol
   use gridoperations
   use m_mergenodes

   implicit none

   integer, intent(in) :: k !< cell number
!
   integer, intent(in) :: ndirect !< number of directly connected cells
   integer, intent(in) :: nindirect !< number of indirectly connected cells
   integer, dimension(ndirect), intent(in) :: kdirect !< directly connected cells, i.e. cells sharing a link with cell k
   integer, dimension(nindirect), intent(in) :: kindirect !< indirectly connected cells, i.e. cells sharing a node, but not a link, with cell k
   integer, dimension(2, ndirect), intent(in) :: kne !< left and right neighboring (in)direct cell that neighbors the directly connected cells
   logical, intent(in) :: Lprompt_nogo !< prompt for cells that cannot be delete (.true.) or not (.false.)
   integer, intent(out) :: jadeleted !< cell has been deleted (1) or not (0)

   double precision :: xc, yc, fac, factot

   integer :: k1, k2, kk, N, ja
   integer :: i, iR, im1, in, j, kcell, kcell1, L, L1, L2
   integer :: kcL, kcR, Ndum
   integer :: klin, knod

   integer :: isconvexcell

   logical :: Lnogo

   jadeleted = 0

   if (ndirect < 1 .or. nindirect < 1) return

!  Firstly, check and see if
!     the cell to be deleted is not a corner cell, but has a link that
!        is internal and whose both nodes are marked as non-internal nodes
!  return if so
   Lnogo = .false.
   kk = 0
   do while (.not. Lnogo .and. kk < netcell(k)%N)
      kk = kk + 1
      klin = netcell(k)%lin(kk)
      if (kn(1, klin) < 1 .or. kn(2, klin) < 1) then
         Lnogo = .true.
         exit
      end if
      if (lnn(klin) == 2) then
         if (nb(kn(1, klin)) /= 1 .and. nb(kn(2, klin)) /= 1) Lnogo = .true.
      end if
   end do
!  check if the cell is a cornercell
   kk = 0
   do while (kk < netcell(k)%N .and. Lnogo)
      kk = kk + 1
      knod = netcell(k)%nod(kk)
      if (nb(knod) == 3 .and. nmk(knod) <= 2) Lnogo = .false.
   end do

!  check if all nodes are in the selecting polygon
   in = -1
   do kk = 1, netcell(k)%N
      k1 = netcell(k)%nod(kk)
      call dbpinpol(xk(k1), yk(k1), in, dmiss, JINS, NPL, xpl, ypl, zpl)
      if (in /= 1) then
         Lnogo = .true.
         exit
      end if
   end do

   if (Lnogo) then
      if (Lprompt_nogo) then
         call confrm('It is advised not to remove this cell. Do you still want to remove this cell? ', ja)
      else
         ja = 0
      end if
      if (ja == 0) then
         jadeleted = 0
         return
      end if
   end if

!   if ( Lnogo ) goto 1234

!  store the affected part of the network
   call local_netstore((/k, kdirect(1:ndirect), kindirect(1:nindirect)/))

!  compute new node coordinates
   N = netcell(k)%N
   xc = 0d0
   yc = 0d0
   factot = 0d0
   do kk = 1, N
      fac = 1d0
      k1 = netcell(k)%nod(kk)
      if (nb(k1) == 2 .or. nb(k1) == 4) then
         fac = 1d45
      else if (nb(k1) == 3) then
         factot = 1d0
         xc = xk(k1)
         yc = yk(k1)
         exit
      end if
      xc = xc + fac * xk(k1)
      yc = yc + fac * yk(k1)
      factot = factot + fac
   end do
   xc = xc / factot
   yc = yc / factot

!  move nodes
   xk(netcell(k)%nod(1:N)) = xc
   yk(netcell(k)%nod(1:N)) = yc

!  alter directly connected cells
   do kk = 1, ndirect
      kcell1 = kdirect(kk)

      if (netcell(kcell1)%N < 4) then
!        remove boundary link of directly connected cell
         do j = 1, netcell(kcell1)%N
            L = netcell(kcell1)%lin(j)
!            if ( lnn(L).lt.2 ) kn(1:2,L) = 0
            if (lnn(L) < 2) call cleanup_link(L)
         end do

!        find adjacent direct neighbors
         L1 = 0
         do i = 1, 2
            kcL = kne(i, kk)
            if (kcL == 0) cycle
            iR = i + 1; if (iR > 2) iR = iR - 2
            kcR = kne(iR, kk)

            if (kcL < 0 .or. kcR < 0) then
!               call qnerror('deletecell: kcL<0 | kcR<0', ' ', ' ')
               cycle
            end if

!           find the common link
            do j = 1, netcell(kcL)%N
               L = netcell(kcL)%lin(j)

               if (lnn(L) < 2) cycle

               if (lne(1, L) == kcell1 .and. lne(2, L) == kcL) then
                  if (kcR /= 0) then
                     lne(1, L) = kcR
                  else
                     lne(1, L) = lne(2, L)
                     lne(2, L) = 0
                     lnn(L) = 1
                  end if
                  exit
               elseif (lne(2, L) == kcell1 .and. lne(1, L) == kcL) then
                  if (kcR /= 0) then
                     lne(2, L) = kcR
                  else
                     lne(2, L) = 0
                     lnn(L) = 1
                  end if
                  exit
               end if
            end do

            if (L1 /= 0) then
               netcell(kcL)%lin(j) = L1
               call cleanup_link(L)
            end if

            L1 = L

         end do

!        deactivate cell
         netcell(kcell1)%N = 0
      else
!        polygons of degree higher than three: remove node and link
         do j = 1, netcell(kcell1)%N
            L = netcell(kcell1)%lin(j)

            if (lnn(L) < 2) cycle

            if (lne(1, L) == k .or. lne(2, L) == k) then
               Ndum = netcell(kcell1)%N - 1
               netcell(kcell1)%lin(j:Ndum) = netcell(kcell1)%lin(j + 1:Ndum + 1)
!              remove one node per removed link
!              take the first node that has not been removed before, but not the node that is kept, which is the first of the center cell
               i = 1
               do while (netcell(kcell1)%nod(i) /= kn(1, L) .and. netcell(kcell1)%nod(i) /= kn(2, L) .and. &
                         i < netcell(kcell1)%N .and. netcell(kcell1)%nod(i) /= netcell(k)%nod(1)); i = i + 1; end do
               if (kk <= netcell(kcell1)%N) then
                  netcell(kcell1)%nod(i:Ndum) = netcell(kcell1)%nod(i + 1:Ndum + 1)
               else
                  call qnerror('coarsen_mesh: no node found', ' ', ' ')
               end if

               netcell(kcell1)%N = Ndum
            end if
         end do
      end if
   end do

!  set the node code
   k1 = netcell(k)%nod(1)
   nb(k1) = maxval(nb(netcell(k)%nod))

!  merge nodes
   xk(k1) = xc
   yk(k1) = yc
   do kk = 2, N
      call mergenodes(netcell(k)%nod(kk), k1, ja)
   end do

!  redirect nodes of indirectly connected cells, deactivate polygons of degree smaller than three and
!     remove unwanted boundary node
   do kk = 1, nindirect
      kcell = kindirect(kk)
      if (netcell(kcell)%N < 3) netcell(kcell)%N = 0 ! deactivate
      do i = 1, netcell(kcell)%N
         k2 = netcell(kcell)%nod(i)
         L = netcell(kcell)%lin(i)
         do j = 2, netcell(k)%N
            if (k2 == netcell(k)%nod(j)) then
               netcell(kcell)%nod(i) = k1
            end if
         end do
      end do
   end do

!  remove unwanted boundary node: a non-corner node that is shared by two boundary links
   in = -1
   kklp: do kk = 1, nindirect
      kcell = kindirect(kk)
      if (netcell(kcell)%N < 3) netcell(kcell)%N = 0 ! deactivate
      do i = 1, netcell(kcell)%N
         k2 = netcell(kcell)%nod(i)
         L = netcell(kcell)%lin(i)
         if (lnn(L) == 1) then
            im1 = i - 1; if (im1 < 1) im1 = im1 + netcell(kcell)%N
            L1 = netcell(kcell)%lin(im1)
            if (lnn(L1) == 1) then
               call find_common_node(L, L1, k2)
               if (k2 < 1) then ! weird
                  cycle
               end if
!              this node may be outside polygon: ignore
               call dbpinpol(xk(k2), yk(k2), in, dmiss, JINS, NPL, xpl, ypl, zpl)

               if (nb(k2) /= 3 .and. in == 1) then ! not a corner node
!                 remove node and link
                  N = netcell(kcell)%N
                  if (N > 3) then ! polynomials of degree higher than three
                     netcell(kcell)%nod(i:N - 1) = netcell(kcell)%nod(i + 1:N)
                     netcell(kcell)%lin(i:N - 1) = netcell(kcell)%lin(i + 1:N)
                     netcell(kcell)%N = N - 1
!                    redirect node of the link that is kept
                     if (kn(1, L1) == k2) then
                        kn(1, L1) = kn(1, L) + kn(2, L) - k2
                     else
                        kn(2, L1) = kn(1, L) + kn(2, L) - k2
                     end if
!                    delete other link
                     call cleanup_link(L)
!                    delete node
                     xk(k2) = dmiss
                     yk(k2) = dmiss
                     cycle kklp
                  else ! triangles: completely remove
                     netcell(kcell)%N = 0
                     call cleanup_link(L)
                     call cleanup_link(L1)
                     L2 = sum(netcell(kcell)%lin(1:3)) - L - L1
                     if (lnn(L2) > 1) then
                        if (lne(1, L2) == kcell) then
                           lnn(L2) = 1
                           lne(1, L2) = lne(2, L2)
                        elseif (lne(2, L2) == kcell) then
                           lnn(L2) = 1
                        end if
                     end if
                     cycle kklp
                  end if
               end if
            end if
         end if
      end do
   end do kklp

!  redirect nodes of directly connected cells and deactivate polygons of degree smaller than three
   do kk = 1, ndirect
      kcell = kdirect(kk)
      if (netcell(kcell)%N < 3) netcell(kcell)%N = 0 ! deactivate
      do i = 1, netcell(kcell)%N
         k2 = netcell(kcell)%nod(i)
         L = netcell(kcell)%lin(i)
         do j = 2, netcell(k)%N
            if (k2 == netcell(k)%nod(j)) then
               netcell(kcell)%nod(i) = k1
            end if
         end do
      end do
   end do

!  deactivate links
   do kk = 1, netcell(k)%N
      L = netcell(k)%lin(kk)
      call cleanup_link(L)
   end do

!  deactivate cell
   netcell(k)%N = 0

   jadeleted = 1

1234 continue

   if (.not. Lnogo) then
!     check and see if
!        the altered indirectly connected cells are non convex
!     restore and return if so

!     check convexity
      Lnogo = .false.
      kk = 0
      do while (kk < nindirect .and. .not. Lnogo)
         kk = kk + 1
         if (isconvexcell(kindirect(kk)) == 0) Lnogo = .true.
      end do

      if (Lnogo) then
         if (Lprompt_nogo) then
            call confrm('It is advised not to remove this cell. Do you still want to remove this cell? ', ja)
         else
            ja = 0
         end if
         if (ja == 0) then
            !        restore
            call local_netrestore()
            jadeleted = 0
            return
         end if
      end if
   end if

   return

contains

!> clean up nod%lin and nmk for nodes comprising a deleted link
   subroutine cleanup_link(L)
      implicit none

      integer :: L !< link number
      integer :: i, j, k, N

!     clean up nod%lin and nmk
      do i = 1, 2
         k = kn(i, L)
         if (k < 1) cycle ! already cleaned up
         if (.not. allocated(nod(k)%lin)) cycle
         N = nmk(k)

!        find link position in nod%lin array
         j = 1
         do while (nod(k)%lin(j) /= L .and. j < N); j = j + 1; end do
         if (nod(k)%lin(j) /= L) then
            call qnerror('cleanup_nod: link not found', ' ', ' ')
            netstat = NETSTAT_CELLS_DIRTY
            return
         end if

!        clean up
         if (j < N) then
            nod(k)%lin(j:N - 1) = nod(k)%lin(j + 1:N)
         end if
         nmk(k) = nmk(k) - 1
      end do

      kn(:, L) = 0

      return
   end subroutine cleanup_link

end subroutine deletecell
