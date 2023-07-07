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

!> compute mapping from prescribed (at flowlinks) to all fluxes (at all edges, including "internal")
subroutine comp_fluxcoeffs()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use unstruc_messages
   use geometry_module, only: gaussj
   implicit none

   integer                                  :: N         ! number of subtriangles

   integer,          dimension(MAXSUBCELLS) :: L1        ! "internal" edges
   integer,          dimension(MAXSUBCELLS) :: L2        ! original "non-internal" edges
   integer,          dimension(MAXSUBCELLS) :: icell     ! subcells
   integer,          dimension(MAXSUBCELLS) :: isign1    ! orientation of "internal: edges L1, all cw or
   integer,          dimension(MAXSUBCELLS) :: isign2    ! orientation of "non-internal: edges L2, outward normal
   double precision, dimension(MAXSUBCELLS) :: circ      ! weight of edge L1 in computation of circulation

   double precision, dimension(MAXSUBCELLS,MAXSUBCELLS) :: A, B

   double precision                         :: areasum, dlen

   integer                                  :: i, im1, ip1, j, j2, k, L, L3, Lf

   integer, external                        :: icommonval

!  allocate
   call realloc_partfluxes()
!   call realloc(jflux2link, numedges+1, fill=0, keepExisting=.false.)

!  set startpointers
   jflux2link(1) = 1
   L = 1
   do while ( L.le.numedges )
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
!        original flowlink
         jflux2link(L+1) = jflux2link(L) + 1

!        proceed to next edge
         L = L+1
      else if ( L.gt.numorigedges ) then
!        internal edge: find number of subtriangels
         N = 0
         do while ( icommonval(edge2cell(:,L+N), edge2cell(:,L+N+1)).ne.0 )
            N = N+1
            if ( L+N.ge.numedges ) exit
         end do

         if ( N.gt.0 ) then
!           check connection first and last subtriangle
            if ( icommonval(edge2cell(:,L), edge2cell(:,L+N)).ne.0 ) then
               N = N+1

               do i=1,N
                  jflux2link(L+1) = jflux2link(L) + N
   !              proceed to next edge
                  L = L+1
               end do
            else
               call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
            end if

         else  ! should not happen
            call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
         end if

         if ( L.ge.numedges ) exit
      else  ! other
         jflux2link(L+1) = jflux2link(L)
!        proceed to next edge
         L = L+1
      end if
   end do

!  reallocate
   call realloc(iflux2link, jflux2link(numedges+1)-1, fill=0, keepExisting=.false.)
   call realloc(Aflux2link, jflux2link(numedges+1)-1, fill=0d0, keepExisting=.false.)

!  fill iflux2link (first in edge-numbers) and compute coefficients
   L = 1
   do while ( L.le.numedges )
      j=jflux2link(L)
      N = jflux2link(L+1)-j

      if ( N.eq.1 ) then
!        original "non-internal" link
         iflux2link(j) = L
         Aflux2link(j) = 1d0
!        proceed to next edge
         L = L+1
      else if ( N.gt.1 ) then
!        "internal" link
         do i=1,N
            ip1 = i+1; if ( ip1.gt.N ) ip1 = ip1-N
            L1(i) = L+i-1
            L3 = L1(i)+ip1

!           find subcell
            icell(i) = icommonval(edge2cell(:,L+i-1), edge2cell(:,L+ip1-1))

!           find original edge
            j2 = jcell2edge(icell(i))+1   ! order of cell edges: "left" internal (L1), orginal (L2), "right internal (L3)
            L2(i) = icell2edge(j2)

!           get orientation of "internal" edges (i-dir positive)
!           note: will always be (/ -1, 1, 1, ... /), due to generation of edge2cells in part_setmesh
            isign1(i) = 1
            if ( edge2cell(1,L1(i)).eq.icell(i) ) isign1(i) = -1

!           get orientation of original "non-internal" edges (outward normal)
            isign2(i) = 1
            if ( edge2cell(2,L2(i)).eq.icell(i) ) isign2(i) = -1
         end do

!        compute summed area and circulation weights
         areasum = 0d0
         do i=1,N
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            areasum = areasum + areacell(icell(i))
            dlen = 0.25*(areacell(icell(im1))+areacell(icell(i)))/w(L1(i))
            circ(i) = dlen/w(L1(i))
         end do

!        build system: A qe = B qlink
         A = 0d0
         B = 0d0
!        continuity equations
         do i=1,N-1
            A(i,i)   = -isign1(i)   ! outward
            A(i,i+1) =  isign1(i+1) ! outward
            B(i,i)   = -isign2(i)   ! outward, rhs
            do k=1,N
               B(i,k) = B(i,k) + areacell(icell(i))*isign2(k)/areasum
            end do
         end do
!        circulation
         do k=1,N
            A(N,k) = circ(k) * isign1(k)
         end do

!        invert matrix: qe = inv(A) B qlink
         call gaussj(A,N,MAXSUBCELLS,B,N,MAXSUBCELLS)

!        fill data
         do i=1,N
            iflux2link(j:j+N-1) = L2(1:N)
            Aflux2link(j:j+N-1) = B(i,1:N)

!           proceed to next edge
            j = j+N
            L = L+1
         end do
      else
!        closed boundary: proceed to next edge
         L = L+1
      end if
   end do

!  convert to link number
   do j=1,jflux2link(numedges+1)-1
      L  = iflux2link(j)
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
         iflux2link(j) = Lf
      else  ! closed boundary
         iflux2link(j) = 0
      end if
   end do

   return
end subroutine comp_fluxcoeffs
