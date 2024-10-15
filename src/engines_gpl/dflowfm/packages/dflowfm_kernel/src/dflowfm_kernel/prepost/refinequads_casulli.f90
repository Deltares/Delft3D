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

!> "Casulli"-type refinement of quads
subroutine refinequads_casulli
   use m_getlink_gui
   use m_confrm
   use m_netw
   use m_inverse_map
   use m_missing, only: dmiss
   use gridoperations
   use m_qnerror
   use m_delpol
   use m_copynetboundstopol
   use m_makenetnodescoding
   use m_set_nod_adm
   use m_cirr

   implicit none

   integer, allocatable, dimension(:, :) :: newnodes ! four new nodes per existing link
   integer, allocatable, dimension(:) :: kc_old ! copy of kc

   type(tadm) :: adm ! structure with administration

   double precision :: xc, yc, xp, yp

   integer :: Lstart

   integer :: ierror

   integer :: k, k0, k1, k2, k3, k4
   integer :: kk, kkm1, kkp1, kkp2, kkk, L, Lm1
   integer :: link1, link2
   integer :: kcell, knode, knew, Lnew
   integer :: N, Ncell, node1, node2

   integer :: numL_old, numk_old

   integer :: jatolan

   integer :: idirectional

   integer, parameter :: Mmax = 4 ! maximum number of nodes in newly created cells

   call confrm('Directional?', idirectional)

   call findcells(100)
   call makenetnodescoding()

   numL_old = numL
   numk_old = numk

!  new nodes have kc<0
!  new boundary nodes have kc=-1
!  active original boundary nodes have kc=1234
!  active original nodes that need to be kept and will be connected to other new nodes have kc=1235
!  active original nodes that need to be kept have kc=1236
!
!  first new nodes are created, then the new links
!  hereafter, old nodes and links are disabled
!
!  new nodes are created in two step:
!    -firstly the inner nodes
!    -secondly the boundary nodes
!
!  new links are created in four steps:
!    -firsty the links that can be associated to the old links, both normal and parallel to them
!    -secondly the "corner-quad" links, which are diagonal links in quadrilaterals that connect new nodes to the old ones
!    -thirdly the missing boundary links that aren't associated with the original netcells, but with the original nodes
!    -fourthly the original-node to new-node links when there are more then Mmax links connected to an original node, Mmax being the maximum numbers of nodes in newly created cells

!  here we go

   if (idirectional == 1) then
!     user interaction
      call getlink_GUI(xp, yp, Lstart)
      if (Lstart < 1) goto 1234
   end if

!  perform the administration and node masking
   call admin_mask()
!
!  resize network
!    NEEDS TO BE MORE ACCURATE
   call increasenetw(4 * numk, 6 * numL)

!  allocate
   allocate (newnodes(4, numL))
   newnodes = 0

!  create the new nodes
   if (idirectional == 0) then
      call makenodes()
   else
      call makenodes_directional(xp, yp, Lstart, ierror)
      if (ierror /= 0) goto 1234
   end if

!  make the new links
   call makelinks()

!  disable old nodes
   do k0 = 1, numk_old
      if (kc(k0) > 0 .and. kc(k0) < 1235) xk(k0) = DMISS
   end do
!  disable old links
   do L = 1, numL_old
      node1 = kn(1, L)
      node2 = kn(2, L)

      if (kc(node1) > 0 .and. kc(node2) > 0 .or. (kc(node1) == 1235 .and. kc(node2) == 1235) .or. &
          (kc(node1) == 0 .and. kc(node2) == 1235) .or. (kc(node1) == 1235 .and. kc(node2) == 0) .or. &
          (kc(node1) == 0 .and. kc(node2) == 1236) .or. (kc(node1) == 1236 .and. kc(node2) == 0) .or. &
          (kc(node1) == -1 .and. kc(node2) == -1) .or. (kc(node1) == -2 .and. kc(node2) == -2)) then ! last two lines for directional refinement

         if (lnn(L) == 0 .or. kn(3, L) == 0) cycle ! a 1D-link: keep it

         kn(1, L) = 0
         kn(2, L) = 0
         kn(3, L) = -1
      end if
   end do
   call setnodadm(0)

   jatolan = 1
   call confrm('Copy refinement border to polygon?', jatolan)
   if (jatolan == 1) then
!     store original node mask
      allocate (kc_old(numk))
!      kc_old = min(kc,1)  ! see admin_mask
      kc_old = kc
      where (kc_old /= 0) kc_old = 1

!     deative polygon
      call savepol()
      call delpol()
      call findcells(100)
!     reactivate polygon
      call restorepol()
!     mark cells crossed by polygon, by setting lnn of their links appropriately
!      kc_old(numk_old+1:numk) = 1
      call mark_cells_crossed_by_poly(numk, kc_old)
      call delpol()
      call copynetboundstopol(0, 0, 0, 1)

      deallocate (kc_old)
   end if

1234 continue ! error handling

!  deallocate
   if (allocated(newnodes)) deallocate (newnodes)

!  set network status
   netstat = NETSTAT_CELLS_DIRTY

contains

!> perform the administration and node masking in refinequads_casulli
   subroutine admin_mask()
      use m_orthonet_admin
      implicit none

      integer :: icell

      !  mark active boundary nodes (1234)
      do L = 1, numL
         node1 = kn(1, L)
         node2 = kn(2, L)
         if (lnn(L) == 1) then
            if (kc(node1) /= 0) kc(node1) = 1234
            if (kc(node2) /= 0) kc(node2) = 1234
         end if
      end do

      !  set nodes with disjunct cells to kc=1235, i.e. keep them
      do k = 1, numk
         do kk = 1, nmk(k) ! loop over the cells connected to this node
            link1 = nod(k)%lin(kk)

            if (lnn(link1) /= 1) cycle ! we are looking for disjunct cells

            icell = lne(1, link1)
            N = netcell(icell)%N

!            if ( N.ne.4 ) cycle           ! quads only

            !        find the other link in this cell connected to the node
            kkk = 1
            link2 = netcell(icell)%lin(kkk)
            do while (((kn(1, link2) /= k .and. kn(2, link2) /= k) .or. link2 == link1) .and. kkk < N)
               kkk = kkk + 1
               link2 = netcell(icell)%lin(kkk)
            end do

            if (lnn(link2) == 1) then ! disjunct cell found
               if (kc(k) > 0) kc(k) = 1235
               exit
            end if
         end do
      end do

      !  keep corner nodes
      do k = 1, numk
         if (nb(k) == 3) then
            kc(k) = 1235
         end if
      end do

      !  determine the maximum number of links connected to a node
      nmkx = 0
      kp: do k0 = 1, numk
         if (kc(k0) == 0) cycle

         if (nmk(k0) > 1) then
            call orthonet_admin(k0, adm, ierror)
            if (ierror /= 0) then
               kc(k0) = 0 ! weird node -> deactivated
               cycle
            end if
         else ! hanging node -> deactivated
            kc(k0) = 0
            cycle
         end if

         !     mask the nodes that are are connected to non-quads *update: deactivated, non-quads are refined too*
         Ncell = 0 ! the number of valid cells counted
         do kk = 1, adm%Ncell
            kcell = adm%icell(kk)
            if (kcell > 0) then
               Ncell = Ncell + 1
!               if ( netcell(kcell)%N.ne.4 ) then
!                  kc(k0) = 0
!                  cycle kp
!               end if
            end if
         end do
         if (Ncell == 0) kc(k0) = 0 ! no cells connected to node -> deactivated

!        weird boundary nodes -> keep them
         if (Ncell < nmk(k0) - 1 .and. kc(k0) == 1234) kc(k0) = 1235
!!        boundary nodes with more than two cells connected -> keep them !DEACTIVED (triangular meshes)
!         if ( Ncell.gt.2 .and. kc(k0).eq.1234 ) kc(k0) = 1235

!        inner nodes with more than Mmax links connected -> keep them
         if (nmk(k0) > Mmax .and. kc(k0) > 0 .and. kc(k0) < 1234) kc(k0) = 1235

         nmkx = max(nmkx, nmk(k0))

      end do kp
   end subroutine admin_mask

!> create and store the new nodes in refinequads_casulli
   subroutine makenodes()
      implicit none

      !  create and store inner nodes
      klp: do k = 1, nump
         N = netcell(k)%N
         if (sum(kc(netcell(k)%nod(1:N))) == 0) cycle ! no active nodes in cell

         !  compute cell center
         xc = sum(xk(netcell(k)%nod(1:N))) / dble(N)
         yc = sum(yk(netcell(k)%nod(1:N))) / dble(N)

         !     loop over the nodes
         do kk = 1, N
            knode = netcell(k)%nod(kk)

            !        find the links connected to this node in the cell
            link1 = 0
            link2 = 0
            do kkk = 1, N
               L = netcell(k)%lin(kkk)
               if (kn(1, L) == knode .or. kn(2, L) == knode) then
                  if (link1 == 0) then
                     link1 = L
                  else
                     link2 = L
                     exit
                  end if
               end if
            end do

            if (link1 == 0 .or. link2 == 0) cycle klp ! no links found

            !     create new node
            if (kc(knode) > 0) then
               call dsetnewpoint(0.5d0 * (xk(knode) + xc), 0.5d0 * (yk(knode) + yc), knew)
               call cirr(xk(knew), yk(knew), 31)
               kc(knew) = -2 ! mark as inactive, non-boundary, so it will not be disabled later on
            else ! original node not active, let knew point to original node for mesh connection later
               knew = knode
            end if
            !     store new node for both links
            call store_newnode(knode, link1, link2, knew, newnodes)
            !         end if

         end do
      end do klp

      !  create and store boundary nodes
      do L = 1, numL_old
         if (lnn(L) /= 1) cycle
         node1 = kn(1, L)
         node2 = kn(2, L)

         if (kc(node1) == 0 .and. kc(node2) == 0) cycle ! no active nodes in link

         !     compute link center
         xc = 0.5d0 * (xk(node1) + xk(node2))
         yc = 0.5d0 * (yk(node1) + yk(node2))

         !     create new node near node1
         if (kc(node1) /= 0) then
            call dsetnewpoint(0.5d0 * (xk(node1) + xc), 0.5d0 * (yk(node1) + yc), knew)
            kc(knew) = -1 ! mark as inactive and on the boundary
         else ! original node not active, let knew point to original node for mesh connection later
            knew = node1
         end if
         !     store new node
         call store_newnode(node1, L, L, knew, newnodes)
         !     create new node near node2
         if (kc(node2) /= 0) then
            call dsetnewpoint(0.5d0 * (xk(node2) + xc), 0.5d0 * (yk(node2) + yc), knew)
            kc(knew) = -1 ! mark as inactive and on the boundary
         else ! original node not active, let knew point to original node for mesh connection later
            knew = node2
         end if
         !     store new node
         call store_newnode(node2, L, L, knew, newnodes)
      end do

   end subroutine makenodes

!> create and store the new nodes in directional refinequads_casulli
   subroutine makenodes_directional(xp, yp, Lstart, ierror)
      use m_confrm
      use unstruc_colors, only: ncolln
      use m_tek_link

      implicit none

      double precision, intent(in) :: xp, yp !> coordinates of clicked point
      integer, intent(in) :: Lstart !> clicked link number
      integer, intent(out) :: ierror !> error (1) or not (0)

      integer, dimension(:), allocatable :: linkmask
      integer, dimension(:), allocatable :: ic, jc

      double precision :: x0, y0, xnew, ynew, xc, yc

      integer :: k1, k2, L, Link, iSE, iexit, idiff, jdiff

      ierror = 1

!     make linkmask
      allocate (linkmask(numL))
      linkmask = 0

!---------------------------------------------------
!     get the netnode indices ic and jc in the curvi-grid
      x0 = xk(50)
      y0 = yk(50)
      allocate (ic(numk), jc(numk))
      ic = 0
      jc = 0
      call assign_icjc(xp, yp, ic, jc, iexit)
      if (iexit /= 1) goto 1234
!---------------------------------------------------
!     deselect nodes that are members of non-quads
      do k = 1, nump
         if (netcell(k)%N /= 4) then
            do kk = 1, netcell(k)%N
               kc(netcell(k)%nod(kk)) = 0
            end do
         end if
      end do
!---------------------------------------------------
!     make the linkmask
      idiff = abs(ic(kn(2, Lstart)) - ic(kn(1, Lstart)))
      jdiff = abs(jc(kn(2, Lstart)) - jc(kn(1, Lstart)))

      if (idiff == jdiff) then ! no valid link clicked
         goto 1234
      end if

      do L = 1, numL
         k1 = kn(1, L)
         k2 = kn(2, L)
         if (abs(ic(k2) - ic(k1)) == idiff .and. abs(jc(k2) - jc(k1)) == jdiff) then
            linkmask(L) = 1
            if (kc(k1) /= 0 .and. kc(k2) /= 0) call teklink(L, ncolln)
         end if
      end do

      call confrm('Refine these links?', iexit)
      if (iexit /= 1) goto 1234
!---------------------------------------------------

      !   create and store inner nodes
      klp: do k = 1, nump
         N = netcell(k)%N
         if (sum(kc(netcell(k)%nod(1:N))) == 0) cycle ! no active nodes in cell

         !  compute cell center
         xc = sum(xk(netcell(k)%nod(1:N))) / dble(N)
         yc = sum(yk(netcell(k)%nod(1:N))) / dble(N)

         !     loop over the nodes
         do kk = 1, N
            knode = netcell(k)%nod(kk)

            !        find the links connected to this node in the cell
            link1 = 0
            link2 = 0
            do kkk = 1, N
               L = netcell(k)%lin(kkk)
               if (kn(1, L) == knode .or. kn(2, L) == knode) then
                  if (link1 == 0) then
                     link1 = L
                  else
                     link2 = L
                     exit
                  end if
               end if
            end do

            if (link1 == 0 .or. link2 == 0) cycle klp ! no links found

            if (linkmask(link1) == 1 .or. linkmask(link2) == 1) then
               !     create new node
               if (kc(knode) > 0) then
                  !     compute active-link center
                  if (linkmask(link1) == 1) then
!                    only one link may be active
                     if (linkmask(link2) /= 1) then
                        Link = link1
                     else
                        call qnerror('makenodes_directional: more than one active link', ' ', ' ')
                        goto 1234
                     end if
                  else
                     Link = link2
                  end if

                  xc = 0.5d0 * (xk(kn(1, Link)) + xk(kn(2, Link)))
                  yc = 0.5d0 * (yk(kn(1, Link)) + yk(kn(2, Link)))

!                 in this case: Left and Right node must be the same on a link
!                 first check if a node already exists on this link (from the other side)
                  iSE = isstartend(knode, Link)
                  if (iSE >= 0 .and. newnodes(max(1 + iSE, 1), Link) > 0) then
                     knew = newnodes(1 + iSE, Link)
                  else if (iSE >= 0 .and. newnodes(max(1 + iSE + 2, 1), Link) > 0) then
                     knew = newnodes(1 + iSE + 2, Link)
                  else
                     xnew = 0.5d0 * (xk(knode) + xc)
                     ynew = 0.5d0 * (yk(knode) + yc)
                     call dsetnewpoint(xnew, ynew, knew)
                     call cirr(xk(knew), yk(knew), 31)
                     kc(knew) = -2 ! mark as inactive, non-boundary, so it will not be disabled later on
                  end if

               else ! original node not active, let knew point to original node for mesh connection later
                  knew = knode
               end if
               !     store new node for both links
               call store_newnode(knode, link1, link2, knew, newnodes)
            else
               if (kc(knode) > 0) then
                  knew = knode
                  kc(knew) = 1236 ! mark as persistent node, so it will not be disabled later on
               else ! original node not active, let knew point to original node for mesh connection later
                  knew = knode
               end if
               !     store new node for both links
               call store_newnode(knode, link1, link2, knew, newnodes)
            end if

         end do
      end do klp

      !  create and store boundary nodes
      do L = 1, numL_old
         if (lnn(L) /= 1) cycle
         node1 = kn(1, L)
         node2 = kn(2, L)

         if (kc(node1) == 0 .and. kc(node2) == 0) cycle ! no active nodes in link

         if (linkmask(L) == 1) then

            !     compute link center
            xc = 0.5d0 * (xk(node1) + xk(node2))
            yc = 0.5d0 * (yk(node1) + yk(node2))

            !     create new node near node1
            if (kc(node1) /= 0) then
!                 in this case: Left and Right node must be the same on a link
!                 first check if a node already exists on this link (from the other side)
               iSE = isstartend(node1, L) ! should be 0
               if (iSE >= 0 .and. newnodes(max(1 + iSE, 1), L) > 0) then
                  knew = newnodes(1 + iSE, L)
               else if (iSE >= 0 .and. newnodes(max(1 + iSE + 2, 1), L) > 0) then
                  knew = newnodes(1 + iSE + 2, L)
               else
                  call dsetnewpoint(0.5d0 * (xk(node1) + xc), 0.5d0 * (yk(node1) + yc), knew)
                  call cirr(xk(knew), yk(knew), 31)
                  kc(knew) = -1
               end if

            else ! original node not active, let knew point to original node for mesh connection later
               knew = node1
            end if
            !     store new node
            call store_newnode(node1, L, L, knew, newnodes)

            !     create new node near node2
            if (kc(node2) /= 0) then
!                 in this case: Left and Right node must be the same on a link
!                 first check if a node already exists on this link (from the other side)
               iSE = isstartend(node2, L) ! should be 1
               if (iSE >= 0 .and. newnodes(max(1 + iSE, 1), L) > 0) then
                  knew = newnodes(1 + iSE, L)
               else if (iSE >= 0 .and. newnodes(max(1 + iSE + 2, 1), L) > 0) then
                  knew = newnodes(1 + iSE + 2, L)
               else
                  call dsetnewpoint(0.5d0 * (xk(node2) + xc), 0.5d0 * (yk(node2) + yc), knew)
                  call cirr(xk(knew), yk(knew), 31)
                  kc(knew) = -1
               end if

            else ! original node not active, let knew point to original node for mesh connection later
               knew = node2
            end if
            !     store new node
            call store_newnode(node2, L, L, knew, newnodes)
         else
            !     node near node1
            if (kc(node1) /= 0) then
               knew = node1
               kc(knew) = 1236
            else ! original node not active, let knew point to original node for mesh connection later
               knew = node1
            end if
            !     store new node
            call store_newnode(node1, L, L, knew, newnodes)
            !     node near node2
            if (kc(node2) /= 0) then
               knew = node2
               kc(knew) = 1236
            else ! original node not active, let knew point to original node for mesh connection later
               knew = node2
            end if
            !     store new node
            call store_newnode(node2, L, L, knew, newnodes)
         end if
      end do

      ierror = 0

!     error handling
1234  continue

!     deallocate
      if (allocated(linkmask)) deallocate (linkmask)

   end subroutine makenodes_directional

!> make links in refinequads_casulli
   subroutine makelinks()
      use m_new_link
      implicit none

      integer, dimension(nmkx) :: node, link ! nodes and links connected to boundary node resp.
      integer, dimension(nmkx) :: oldn, newn ! old and new nodes in quad resp.

      integer :: numlinks

      !  make the original-link based new links
      do L = 1, numL_old
         k1 = newnodes(1, L)
         k2 = newnodes(2, L)
         k3 = newnodes(3, L)
         k4 = newnodes(4, L)
         !     parallel links: these are the start-end connections
         if (k1 > 0 .and. k2 > 0 .and. k1 /= k2) call newlink(k1, k2, Lnew)
         if (k3 > 0 .and. k4 > 0 .and. k3 /= k4) call newlink(k3, k4, Lnew)
         !     normal links: these are the left-right connections
         if (k1 > 0 .and. k3 > 0 .and. k1 /= k3) call newlink(k1, k3, Lnew)
         if (k2 > 0 .and. k4 > 0 .and. k2 /= k4) call newlink(k2, k4, Lnew)
      end do

      !  create the diagonal links in quads that connect the new mesh with the old mesh
      do k = 1, nump
         N = netcell(k)%N
         if (N /= 4) cycle ! quads only
         if (sum(kc(netcell(k)%nod(1:N))) == 0) cycle ! no active nodes in cell

         !     find the old and new nodes
         oldn = 0
         newn = 0
         do kk = 1, N
            kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N
            L = netcell(k)%lin(kk)
            Lm1 = netcell(k)%lin(kkm1)

            oldn(kk) = kn(1, L)
            newn(kk) = newnodes(3, L)
            if (oldn(kk) /= kn(1, Lm1) .and. oldn(kk) /= kn(2, Lm1)) then
               oldn(kk) = kn(2, L)
               newn(kk) = newnodes(2, L)
            end if
         end do

         do kk = 1, N
            kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + N
            kkp1 = kk + 1; if (kkp1 > N) kkp1 = kkp1 - N
            kkp2 = kk + 2; if (kkp2 > N) kkp2 = kkp2 - N

            k1 = newn(kk)
            k2 = oldn(kkm1)
            k3 = oldn(kkp1)
            k4 = oldn(kkp2)

            !        only one new node: new diagonal link connects new node with one old node
            if (kc(k1) < 0 .and. kc(k2) == 0 .and. kc(k3) == 0 .and. kc(k4) == 0) then
               call newlink(k1, k4, Lnew)
               exit
            end if

            !        only one old node: new diagonal link connects new nodes only (i.e. perpendicular to previous one)
            if (kc(k1) < 0 .and. kc(k2) > 0 .and. kc(k3) > 0 .and. kc(k4) == 0) then
               call newlink(newn(kkm1), newn(kkp1), Lnew)
               exit
            end if

            !        two new and opposing nodes: new diagonal link connects the new nodes
            if (kc(k1) < 0 .and. kc(k2) == 0 .and. kc(k3) == 0 .and. kc(k4) == 1) then
               call newlink(k1, newn(kkp2), Lnew)
               exit
            end if
         end do
      end do

      !  make the missing boundary links
      do k = 1, numk_old
         if (kc(k) < 1234) cycle ! boundary and kept nodes only

         !     find the links connected
         numlinks = 0
         link = 0
         do kkk = 1, nmk(k)
            L = nod(k)%lin(kkk)
            if (lnn(L) == 1) then
               numlinks = numlinks + 1
               link(numlinks) = L
            else ! non-boundary link connected to boundary node -> create links
               if (kn(1, L) == k) then
                  call newlink(k, newnodes(1, L), Lnew)
                  call newlink(k, newnodes(3, L), Lnew)
               else
                  call newlink(k, newnodes(2, L), Lnew)
                  call newlink(k, newnodes(4, L), Lnew)
               end if
            end if
         end do

         if (numlinks == 0) cycle ! no links found

         !     find the two new boundary nodes
         node = 0
         do kk = 1, numlinks
            if (kn(1, link(kk)) == k .and. kc(newnodes(1, link(kk))) == -1) node(kk) = newnodes(1, link(kk))
            if (kn(1, link(kk)) == k .and. kc(newnodes(3, link(kk))) == -1) node(kk) = newnodes(3, link(kk))
            if (kn(2, link(kk)) == k .and. kc(newnodes(2, link(kk))) == -1) node(kk) = newnodes(2, link(kk))
            if (kn(2, link(kk)) == k .and. kc(newnodes(4, link(kk))) == -1) node(kk) = newnodes(4, link(kk))

!           for directional refinement (1236 are persistent nodes)
            if (kn(1, link(kk)) == k .and. kc(newnodes(1, link(kk))) == 1236) node(kk) = newnodes(1, link(kk))
            if (kn(1, link(kk)) == k .and. kc(newnodes(3, link(kk))) == 1236) node(kk) = newnodes(3, link(kk))
            if (kn(2, link(kk)) == k .and. kc(newnodes(2, link(kk))) == 1236) node(kk) = newnodes(2, link(kk))
            if (kn(2, link(kk)) == k .and. kc(newnodes(4, link(kk))) == 1236) node(kk) = newnodes(4, link(kk))
         end do

         !     make the new links
         if (kc(k) /= 1235 .and. kc(k) /= 1236) then ! node is not kept
            if (numlinks /= 2) then
               call qnerror('refinequads_casulli, makelinks: boundary link error', ' ', ' ')
            else
               if (node(1) > 0 .and. node(2) > 0 .and. node(1) /= node(2)) then
                  call newlink(node(1), node(2), Lnew)
               end if
            end if
         else ! node is kept
            do kk = 1, numlinks
               if (node(kk) > 0 .and. node(kk) /= k) call newlink(k, node(kk), Lnew)
            end do
         end if
      end do

      !  make the original-node to new-node links for the kept original nodes, which have more than Mmax links attached
      do k = 1, numk_old
         if (kc(k) /= 1235 .or. nmk(k) <= Mmax) cycle

         do kk = 1, nmk(k)
            L = nod(k)%lin(kk)
            if (kn(1, L) == k) then
               call newlink(k, newnodes(1, L), Lnew)
               call newlink(k, newnodes(3, L), Lnew)
            else
               call newlink(k, newnodes(2, L), Lnew)
               call newlink(k, newnodes(4, L), Lnew)
            end if
         end do
      end do

   end subroutine makelinks

!> administer the new node by figuring out the start/end and left/right position w.r.t. corresponding link
!!    the order is as follows:
!!       1: right, start
!!       2: right, end
!!       3: left,  start
!!       4: left,  end
!!
!!    important: first store the non-boundary nodes
   subroutine store_newnode(k0, L1_, L2_, knew, newnodes)

      implicit none

      integer, intent(in) :: k0 ! center node
      integer, intent(in) :: L1_, L2_ ! link numbers
      integer :: knew ! new node
      integer, dimension(:, :), intent(inout) :: newnodes ! new-node administration

      integer :: L1, L2
      integer :: iLR1, iLR2, iSE1, iSE2, ipoint1, ipoint2
      integer :: icell

      integer, external :: common_cell_for_two_net_links

!     if L1.eq.0 or L2.eq.0 or L1.eq.L2 and L1 or L2 is a boundary link, store at the "ghost"-side
      L1 = L1_
      L2 = L2_

      if (L1 > 0) then
         if (L2 == 0) then
            L2 = L1
         end if
      else
         if (L2 > 0) then
            L1 = L2
         else
            call qnerror('store_newnode: no links specified', ' ', ' ')
            return
         end if
      end if

!     find common cell
      icell = common_cell_for_two_net_links(L1, L2)
      if (icell < 1) then
         call qnerror('store_newnode: no cell found', ' ', ' ')
      end if

!     determine the orientations
      iLR1 = isleftright(icell, L1)
      iLR2 = isleftright(icell, L2)
      iSE1 = isstartend(k0, L1)
      iSE2 = isstartend(k0, L2)

!     select other side if only one link is specified
      if (L1 == L2) then
         iLR1 = 1 - iLR1
         iLR2 = 1 - iLR2
      end if

      ipoint1 = 1 + iSE1 + 2 * (1 - iLR1)
      ipoint2 = 1 + iSE2 + 2 * (1 - iLR2)

      if (newnodes(ipoint1, L1) < 1) newnodes(ipoint1, L1) = knew
      if (newnodes(ipoint2, L2) < 1) newnodes(ipoint2, L2) = knew

   end subroutine store_newnode

!> determine if a node is at the start (0) or end (1) of a link or not in the link at all (-1)
   integer function isstartend(k, L)
      implicit none

      integer, intent(in) :: k !> node number
      integer, intent(in) :: L !> link number

      isstartend = -1
      if (kn(1, L) == k) isstartend = 0
      if (kn(2, L) == k) isstartend = 1

      return
   end function isstartend

!> determine if a cell is at the left (0) or right (1) of a link or not neighboring a link at all (-1)
!> note: it is assumed that the links in a cell are in counterclockwise order
   integer function isleftright(icell, L)
      implicit none

      integer, intent(in) :: icell !> cell number
      integer, intent(in) :: L !> link number

      integer :: kend, kk, kself, knext, L1, N

      isleftright = -1

!     find a neighbor of link L: a link in the cell that is connected to the endpoint of link L
      N = netcell(icell)%N
      kend = kn(2, L)
      kself = 0
      knext = 0
      do kk = 1, N
         L1 = netcell(icell)%lin(kk)
         if (L1 == L) then
            kself = kk
         else if (kn(1, L1) == kend .or. kn(2, L1) == kend) then
            knext = kk
         end if
      end do

      if (knext - kself == 1 .or. knext + N - kself == 1) then ! cell at the left
         isleftright = 0
      else if (kself - knext == 1 .or. kself + N - knext == 1) then ! cell at the right
         isleftright = 1
      end if

      return
   end function isleftright

end subroutine refinequads_casulli

