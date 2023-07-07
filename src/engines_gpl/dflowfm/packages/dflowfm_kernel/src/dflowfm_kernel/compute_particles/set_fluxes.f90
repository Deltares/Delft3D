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

!> set all fluxes, including internal
subroutine set_fluxes(Lnx,q,qe)
   use m_partmesh
   use m_partfluxes
   implicit none

   integer,                               intent(in)  :: Lnx
   double precision, dimension(Lnx),      intent(in)  :: q

   double precision, dimension(numedges), intent(out) :: qe

   integer                                            :: j, L, Lf

   do L=1,numedges
      qe(L) = 0d0
      do j=jflux2link(L),jflux2link(L+1)-1
         Lf = iflux2link(j)
         if ( Lf.gt.0 ) then
            qe(L) = qe(L) + Aflux2link(j)*q(Lf)
         end if
      end do
   end do

!   double precision, dimension(:),        allocatable :: dum

!   integer                                            :: i, j, k, k1, k2
!   integer                                            :: icL, icR, n1, n2
!   integer                                            :: L, L1, L2, L3, Lf, N

!   double precision                                   :: sumq, sumarea, circ, dcirc, dlen(MAXSUBCELLS), sumlen, dq

!   qe = 0d0
!
!!  set non-internal fluxes
!   do L=1,numedges
!      Lf = edge2link(L) ! flow link
!      if ( Lf.gt.0 ) then
!         qe(L) = q(Lf)
!      end if
!   end do
!
!!  compute flux balances of original non-triangular cells (internal fluxes are still 0d0)
!   allocate(dum(numcells))
!   dum = 0d0
!   do L=1,numedges
!      k1 = edge2cell(1,L)
!      k2 = edge2cell(2,L)
!
!      if ( k1.gt.0 ) dum(k1) = dum(k1) - qe(L)
!      if ( k2.gt.0 ) dum(k2) = dum(k2) + qe(L)
!   end do
!
!!  set internal fluxes
!   k = 1 ! cell number
!   do while ( k.le.numcells )
!
!      if ( cell2nod(k).lt.0 ) then  ! new triangles
!!        count number of sub-triangles
!         N = 0
!         sumq = 0d0
!         sumarea = 0d0
!         do while ( cell2nod(k+N).eq.cell2nod(k) )
!            sumq = sumq + dum(k+N)
!            sumarea = sumarea + areacell(k+N)
!            N = N+1
!            if ( k+N.gt.numcells ) exit
!         end do
!
!         if ( N.eq.0 ) then
!            k = k+1
!            cycle
!         end if
!
!         sumq = sumq/sumarea  ! div(q)
!
!!        loop over all sub-triangles
!         circ = 0d0  ! circulation
!         sumlen = 0d0  ! summed area of triangles
!         j = jcell2edge(k)
!         do i=1,N
!!           get edge numbers
!            L1 = icell2edge(j+(i-1)*3)   ! "left" internal
!            L2 = icell2edge(j+(i-1)*3+1) ! edge of original non-triangle
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!
!            if ( i.eq.1 ) then
!               qe(L1) = 0d0
!            end if
!
!!           determine "right" internal flux from flux balance with other two fluxes
!            qe(L3) = sumq*areacell(k)  ! outward normal assumed (corrected later)
!
!            if ( edge2cell(1,L1).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L1)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L1)
!            end if
!
!            if ( edge2cell(1,L2).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L2)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L2)
!            end if
!
!!           account for orientation
!            if ( edge2cell(2,L3).eq.k ) then ! outward normal
!               qe(L3) = -qe(L3)
!            end if
!
!!           add to circulation
!            icL = edge2cell(1,L3)
!            icR = edge2cell(2,L3)
!            dlen(i) = 0.25d0*(areacell(icL)+areacell(icR))/w(L3)
!            sumlen = sumlen + dlen(i)/w(L3)
!            if ( icR.eq.k ) then
!               dlen(i) = -dlen(i)
!            end if
!
!            dcirc = qe(L3)/w(L3)*dlen(i)
!            circ = circ + dcirc
!
!!           update cell number
!            k = k+1
!         end do
!
!!        correct for circulation
!         dq = -circ/sumlen
!         do i=1,N
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!            if ( dlen(i).gt.0d0 ) then
!               qe(L3) = qe(L3) + dq
!            else
!               qe(L3) = qe(L3) - dq
!            end if
!         end do
!      else
!         k = k+1
!      end if
!   end do
!
!   if ( allocated(dum) ) deallocate(dum)

    return
end subroutine set_fluxes
