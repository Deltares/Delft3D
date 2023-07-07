!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> set pointer
subroutine part_setmesh()
   use network_data, only: kn, xk, yk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
   use m_flowgeom, only: lne2ln, ba
   use m_alloc
   use m_missing
   use m_partmesh
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter

   implicit none

   integer                        :: i, node1, node2, icell, j, k, L, N
   integer                        :: im1, ip1, Lm1, Lp1, L1, L2
   integer                        :: newnode, newedge
   integer                        :: isign, ja, k1, k2, k3, kL, kR
   integer                        :: jaswap

   integer                        :: numnontris
   integer                        :: numaddedges

   double precision, dimension(3) :: xv, yv
   double precision, dimension(3) :: t, t1, t2    ! edge tangential vector


   numnodes = numk
   numedges = numL
   numcells = nump

!  count number of non-triangles and additional (internal) edges
   numnontris=0
   numaddedges=0
   do k=1,nump
      N = netcell(k)%N
      if ( N.gt.3 ) then
         numnontris = numnontris+1
         numaddedges = numaddedges+N
      end if
   end do

!  compute sizes
   numnodes = numk + numnontris                 ! add centers of non-triangles
   numedges = numL + numaddedges                ! add internal edges of non-triangles
   numcells = nump - numnontris + numaddedges   ! add internal triangles of non-triangles

!  (re)allocate
   call realloc_partmesh()

!  nodes
   if ( jsferic.eq.0 ) then
      do k=1,numk
         xnode(k) = xk(k)
         ynode(k) = yk(k)
      end do
   else
      do k=1,numk
         call sphertocart3D(xk(k),yk(k),xnode(k),ynode(k),znode(k))
      end do
   end if

!  edges
   do L=1,numL
      edge2node(1,L) = kn(1,L)
      edge2node(2,L) = kn(2,L)
      if ( lne2ln(L).ge.0 ) then
         edge2link(L) = lne2ln(L)
      else
!        not a flowlink number, see flow_geominit
         continue
      end if
   end do

!  set jcell2edge startpointers and add new triangles
   icell=0
   jcell2edge(1) = 1
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 ) then
         icell = icell+1
         jcell2edge(icell+1) = jcell2edge(icell) + 3
!         nod2cell(k) = icell
         cell2nod(icell) = k
      else if ( N.gt.3 ) then
         do j=1,N
            icell = icell+1
            jcell2edge(icell+1) = jcell2edge(icell) + 3
!            if ( j.eq.1 ) nod2cell(k) = -icell
            cell2nod(icell) = -k
         end do
      else  ! should not happen
         continue
      end if
   end do

!  allocate jcell2edge
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)

!  copy netcell data into cell2edge and add new triangles (nodes, edges)
   icell = 0
   newnode = numk
   numorigedges = numL
   newedge = numorigedges
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 )then
         icell = icell+1   ! add existing triangle
         i = 0
         do j=jcell2edge(icell),jcell2edge(icell+1)-1
            i = i+1
            L = netcell(k)%lin(i)
            icell2edge(j) = L
         end do
         if ( jsferic.eq.0 ) then
            xzwcell(icell) = xzw(k)
            yzwcell(icell) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xzwcell(icell),yzwcell(icell),zzwcell(icell))
         end if
         areacell(icell) = ba(k)
      else
!        add node
         newnode = newnode+1
         if ( jsferic.eq.0 ) then
            xnode(newnode) = xzw(k)
            ynode(newnode) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xnode(newnode),ynode(newnode),znode(newnode))
         end if

         xv(1) = xzw(k)
         yv(1) = yzw(k)

         do i=1,N ! add new edges and triangles
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
            L = netcell(k)%lin(i)
            call find_common_node(netcell(k)%lin(im1),L,node1)
            call find_common_node(netcell(k)%lin(ip1),L,node2)

            icell = icell+1
!           add edges
            newedge = newedge+1
            edge2node(1,newedge) = newnode
            edge2node(2,newedge) = node1
            edge2link(newedge) = -1

!           add edges to cell2edge
            j = jcell2edge(icell)
            icell2edge(j) = newedge
            icell2edge(j+1) = L
            icell2edge(j+2) = newedge+1
            if ( i.eq.N ) then
               icell2edge(j+2) = icell2edge(j+2) - N
            end if

!           compute mass center
            xv(2) = xk(node1)
            yv(2) = yk(node1)
            xv(3) = xk(node2)
            yv(3) = yk(node2)
            call comp_masscenter(3, xv, yv, xzwcell(icell), yzwcell(icell), areacell(icell), ja, jsferic, jasfer3D, dmiss)

            if ( jsferic.eq.1 ) then
               xv(1) = xzwcell(icell)  ! reuse xv
               yv(1) = yzwcell(icell)   ! reuse yv
               call sphertocart3D(xv(1), yv(1),xzwcell(icell),yzwcell(icell),zzwcell(icell))
            end if

 !           call cirr(xzwcell(k), yzwcell(k), 31)
         end do
      end if
   end do

!  set edge2cell
   edge2cell = 0
   do icell=1,numcells
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         L = icell2edge(j)
         if ( edge2cell(1,L).eq.0 ) then
            edge2cell(1,L) = icell
         else
            edge2cell(2,L) = icell
         end if
      end do
   end do

!  fix orientation of edge2cell
   do L=1,numL ! exclude new edges
      k1 = edge2cell(1,L)
      if ( k1.eq.0 ) cycle

      k1 = iabs(cell2nod(k1))

      jaswap = 0

      if ( lnn(L).eq.1 ) then  !     boundaries: inward normal
         jaswap = 1
      else if ( k1.eq.lne(2,L) .and. lnn(L).gt.1 ) then
         jaswap = 1
      end if

      if ( jaswap.eq.1 ) then
         icell = edge2cell(1,L)
         edge2cell(1,L) = edge2cell(2,L)
         edge2cell(2,L) = icell
      end if
   end do

   if ( jsferic.ne.0 ) then
!     compute cell normal vectors from first two edges
      do k=1,numcells
         L1 = icell2edge(jcell2edge(k))
         L2 = icell2edge(jcell2edge(k)+1)
         k1 = edge2node(1,L1)
         k2 = edge2node(2,L1)
         k3 = edge2node(1,L2)
         if ( k3.eq.k1 .or. k3.eq.k2 ) then
            k3 = edge2node(2,L2)
         end if
         t1 = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1)/)
         t2 = (/ xnode(k3)-xnode(k2), ynode(k3)-ynode(k2), znode(k3)-znode(k2)/)

         dnn(:,k) = (/ t1(2)*t2(3) - t1(3)*t2(2), t1(3)*t2(1) - t1(1)*t2(3), t1(1)*t2(2) - t1(2)*t2(1) /)
         dnn(:,k) = dnn(:,k) / sqrt( dnn(1,k)**2 + dnn(2,k)**2 + dnn(3,k)**2 )

!        fix orientation
         if ( dnn(1,k)*xnode(k1) + dnn(2,k)*ynode(k1) + dnn(3,k)*znode(k1) .lt. 0d0 ) then
            dnn(:,k) = -dnn(:,k)
         end if
      end do
   end if

!  nx, ny, w
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)

      kL = edge2cell(1,L)
      kR = edge2cell(2,L)

      k = kL   ! outward positive

      isign = 1
      if ( k.le.0 ) then
         k = edge2cell(2,L)   ! inward positive
         isign = -1
      end if

      if ( k.eq.0 ) cycle  ! isolated edge

!     compute normal vector (outward positive)
      if ( jsferic.eq.0 ) then
         call normaloutchk(xnode(k1),ynode(k1),xnode(k2),ynode(k2),xzwcell(k),yzwcell(k),dnx(1,L),dny(1,L),ja, jsferic, jasfer3D, dmiss, dxymis)

         if ( isign.eq.-1 ) then
            dnx(1,L) = -dnx(1,L)
            dny(1,L) = -dny(1,L)
         end if

         w(L) = dbdistance(xnode(k1),ynode(k1),xnode(k2),ynode(k2),jsferic, jasfer3D, dmiss)
      else
!        compute outward normal with respect to the left cell
         t = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1) /)
         w(L) = sqrt( t(1)**2 + t(2)**2 + t(3)**2)

         t = t/w(L)

         if ( kL.gt.0 ) then
!           left cell normal : nn X t
            dnx(1,L) = dnn(2,kL) * t(3) - dnn(3,kL) * t(2)
            dny(1,L) = dnn(3,kL) * t(1) - dnn(1,kL) * t(3)
            dnz(1,L) = dnn(1,kL) * t(2) - dnn(2,kL) * t(1)

!           fix orientation
            if ( (xnode(k1)-xzwcell(kL))*dnx(1,L) + (ynode(k1)-yzwcell(kL))*dny(1,L) + (znode(k1)-zzwcell(kL))*dnz(1,L) .lt. 0d0 ) then
               dnx(1,L) = -dnx(1,L)
               dny(1,L) = -dny(1,L)
               dnz(1,L) = -dnz(1,L)
            end if
         end if

         if ( kR.gt.0 ) then
!           left cell normal : nn X t
            dnx(2,L) = dnn(2,kR) * t(3) - dnn(3,kR) * t(2)
            dny(2,L) = dnn(3,kR) * t(1) - dnn(1,kR) * t(3)
            dnz(2,L) = dnn(1,kR) * t(2) - dnn(2,kR) * t(1)

!           fix orientation
            if ( (xnode(k1)-xzwcell(kR))*dnx(2,L) + (ynode(k1)-yzwcell(kR))*dny(2,L) + (znode(k1)-zzwcell(kR))*dnz(2,L) .lt. 0d0 ) then
               dnx(2,L) = -dnx(2,L)
               dny(2,L) = -dny(2,L)
               dnz(2,L) = -dnz(2,L)
            end if
         end if
      end if
   end do


   return
end subroutine part_setmesh
