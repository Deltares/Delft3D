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

!> reconstruct velocity in cells
subroutine reconst_vel(q, s0, s1, ierror)
   use m_flowgeom, only: Ndx, Lnx, bl  !, lne2ln  !, csu, snu, wu  !, xu, yu
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   use m_sferic
   use geometry_module, only: dbdistance

   implicit none

   double precision, dimension(Lnx), intent(in)  :: q    ! flowlink-based discharge (m3/s)
   double precision, dimension(Ndx), intent(in)  :: s0   ! flownode-based water level (m) at begin of interval
   double precision, dimension(Ndx), intent(in)  :: s1   ! flownode-based water level (m) at end of interval

   integer,                          intent(out) :: ierror

   integer,                          parameter   :: N = 4

   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs

   integer                                       :: i, icell, j, k, L, Lf
   integer                                       :: k1, k2
   integer                                       :: ja
   integer                                       :: i12, isign

   double precision                              :: cs, sn, wwu, h0, h1, h, dfac, un

   double precision, parameter                   :: DTOL=1d-10

   ierror = 1

!  get fluxes at all edges, including internal
   call set_fluxes(Lnx, q, qe)

!  initialize
   u0x = 0d0
   u0y = 0d0
   if ( jsferic.eq.1 ) then
      u0z = 0d0
   end if
   alpha = 0d0

   do icell=1,numcells
!     get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

!     get water depth
      h0 = s0(k)-bl(k)
      h1 = s1(k)-bl(k)
      if ( abs(h1-h0).gt.DTOL ) then
         if ( h0.gt.epshs .and. h1.gt.epshs ) then
            h = (h1-h0)/(log(h1)-log(h0))
         else if ( h0.gt.epshs .or. h1.gt.epshs ) then
            h = 0.5d0*(h0+h1)
         else
            h = 0d0
         end if
      else
         h = 0.5d0*(h0+h1)
      end if

      if ( h.le.epshs ) cycle

      if ( jsferic.eq.0 ) then
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)   = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)   = u0y(icell)   + Areconst(2,j)*un
            alpha(icell) = alpha(icell) + Areconst(3,j)*un
         end do
      else
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)   = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)   = u0y(icell)   + Areconst(2,j)*un
            u0z(icell)   = u0z(icell)   + Areconst(3,j)*un
            alpha(icell) = alpha(icell) + Areconst(4,j)*un
         end do
      end if
   end do








!!  loop over internal cells
!   do icell=1,numcells
!!     check for triangles
!!      if ( jcell2edge(k+1)-jcell2edge(k).ne.3 ) then
!!         cycle
!!      end if
!
!!     get flownode number (for s, bl)
!      k = iabs(cell2nod(icell))
!
!!     fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)
!
!!     loop over edges (netlinks)
!      i = 0
!      do j=jcell2edge(icell),jcell2edge(icell+1)-1
!         i = i+1
!         L = icell2edge(j) ! netlink
!
!         k1 = edge2node(1,L)
!         k2 = edge2node(2,L)
!
!         wwu = w(L)
!
!         if ( jsferic.eq.0 ) then
!            cs = dnx(1,L)
!            sn = dny(1,L)
!
!!           add to system
!            Amat(i,1) = cs
!            Amat(i,2) = sn
!            Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
!         else
!            i12 = 1
!            isign = 1
!            if ( edge2cell(2,L).eq.icell ) then
!              i12 = 2
!              isign = -1d0
!            end if
!
!            Amat(i,1) = dnx(i12,L)*isign
!            Amat(i,2) = dny(i12,L)*isign
!            Amat(i,3) = dnz(i12,L)*isign
!            Amat(i,4) = ( (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*dnx(i12,L) + &
!                          (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*dny(i12,L) + &
!                          (0.5d0*(znode(k1)+znode(k2))-zzwcell(icell))*dnz(i12,L) ) * isign
!         end if
!
!!        u.n = q / ( h wu )
!         h0 = s0(k)-bl(k)
!         h1 = s1(k)-bl(k)
!         if ( abs(h1-h0).gt.DTOL ) then
!            if ( h0.gt.epshs .and. h1.gt.epshs ) then
!               h = (h1-h0)/(log(h1)-log(h0))
!            else if ( h0.gt.epshs .or. h1.gt.epshs ) then
!               h = 0.5d0*(h0+h1)
!            else
!               h = 0d0
!            end if
!         else
!            h = 0.5d0*(h0+h1)
!         end if
!
!         if ( h.gt.epshs ) then
!            rhs(i) = qe(L) / ( h*wwu )
!         else
!
!            rhs(i) = 0d0
!         end if
!      end do
!
!      if ( jsferic.eq.0 ) then
!!        solve system
!         call gaussj(Amat,3,N,rhs,1,1)
!
!         if ( sqrt( (u0x(icell)-rhs(1))**2 + (u0y(icell)-rhs(2))**2 ).gt.1d-16 ) then
!            continue
!         else if ( abs(alpha(icell)-rhs(3)).gt.1d-16 ) then
!            continue
!         end if
!
!!         u0x(icell)   = rhs(1)
!!         u0y(icell)   = rhs(2)
!!         alpha(icell) = rhs(3)
!      else
!!        impose zero cell normal velocity
!         Amat(4,1:3) = dnn(:,icell)
!         Amat(4,4) = 0d0
!         rhs(4) = 0d0
!
!!        solve system
!         call gaussj(Amat,4,N,rhs,1,1)
!
!!         u0x(icell)   = rhs(1)
!!         u0y(icell)   = rhs(2)
!!         u0z(icell)   = rhs(3)
!!         alpha(icell) = rhs(4)
!
!         if ( sqrt( (u0x(icell)-rhs(1))**2 + (u0y(icell)-rhs(2))**2 + (u0z(icell)-rhs(3))**2 ).gt.1d-16 ) then
!            continue
!         else if ( abs(alpha(icell)-rhs(4)).gt.1d-16 ) then
!            continue
!         end if
!      end if
!
!!     BEGIN DEBUG
!!      if ( k.eq.16 ) then
!!         write(6,*) (u0x(k) + alpha(k)*(xu(Lf)-xz(k))) * csu(Lf) +  &
!!            (u0y(k) + alpha(k)*(yu(Lf)-yz(k))) * snu(Lf), q(Lf) / ( (s(k)-bl(k))*wu(Lf) )
!!      end if
!!     END DEBUG
!   end do

   ierror = 0
!  error handling
1234 continue

   return
end subroutine reconst_vel
