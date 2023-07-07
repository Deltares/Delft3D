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

!> update positions of particles within triangles
subroutine update_particles_in_cells(numremaining, ierror)
   use m_particles
   use m_partrecons
   use m_partmesh
   use m_sferic, only: jsferic
   implicit none

   integer,        intent(out) :: numremaining !< number of remaining particles to be updated
   integer,        intent(out) :: ierror       !< error (1) or not (0)

   integer                     :: ipart
   integer                     :: i, k, k1, k2, L, Lf
   integer                     :: ja
   integer                     :: Lexit

   double precision            :: d, d1, un
   double precision            :: t, tex, dt
   double precision            :: ux0, uy0, uz0, cs, sn
   double precision            :: xn, yn, zn, rl
   double precision            :: dvar, dis, dn

   double precision, dimension(3) :: ddn

   logical                     :: isboundary

   double precision, parameter :: DTOL = 1d-4
   double precision, parameter :: DTOLd  = 1d-4
   double precision, parameter :: DTOLun_rel = 1d-4
   double precision, parameter :: DTOLun = 1e-14

   integer,          parameter :: MAXNUMZERO = 10

   ierror = 1

   numremaining = 0

   do ipart=1,Npart
!     check if this particle needs to be updated
      if ( dtremaining(ipart).eq.0d0 .or. kpart(ipart).lt.1 ) cycle
!     get cell (flownode) particle in in
      k = kpart(ipart)

!     compute exit time <= dtremaining
      tex = dtremaining(ipart)

      Lexit = 0   ! exit edge (flowlink)

 !    compute velocity at current position
      ux0 = u0x(k) + alpha(k)*(xpart(ipart)-xzwcell(k))
      uy0 = u0y(k) + alpha(k)*(ypart(ipart)-yzwcell(k))
      if ( jsferic.ne.0 ) then
         uz0 = u0z(k) + alpha(k)*(zpart(ipart)-zzwcell(k))
      end if

!     loop over edges (netlinks) of cells
      do i=jcell2edge(k),jcell2edge(k+1)-1
         L = icell2edge(i)   ! edge

         k1 = edge2node(1,L)
         k2 = edge2node(2,L)

         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)
            if ( edge2cell(2,L).eq.k ) then
               cs = -cs
               sn = -sn
            end if
         else
            if ( edge2cell(1,L).eq.k ) then
               ddn = (/ dnx(1,L), dny(1,L), dnz(1,L) /)
            else
               ddn = (/ dnx(2,L), dny(2,L), dnz(2,L) /)
            end if
         end if

!        check for boundary edge
         isboundary = ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 )

!        compute normal distance to edge
         if ( jsferic.eq.0 ) then
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1)+cs*DTOLd,ynode(k1)+sn*DTOLd,xnode(k2)+cs*DTOLd,ynode(k2)+sn*DTOLd,ja,d,xn,yn,rl)
            else
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1),ynode(k1),xnode(k2),ynode(k2),ja,d,xn,yn,rl)
            end if
            dis = (xn-xpart(ipart))*cs + (yn-ypart(ipart))*sn
         else
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1)+DTOLd*ddn(1),  &
                                                                      ynode(k1)+DTOLd*ddn(2),  &
                                                                      znode(k1)+DTOLd*ddn(3),  &
                                                                      xnode(k2)+DTOLd*ddn(1),   &
                                                                      ynode(k2)+DTOLd*ddn(2),   &
                                                                      znode(k2)+DTOLd*ddn(3),   &
                                                                      ja,d,xn,yn,zn,rl)
            else
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1),ynode(k1),znode(k1),xnode(k2),ynode(k2),znode(k2),ja,d,xn,yn,zn,rl)
            end if
            dis = (xn-xpart(ipart))*ddn(1) + (yn-ypart(ipart))*ddn(2) + (zn-zpart(ipart))*ddn(3)
         end if


!        BEGIN DEBUG
!         if ( ipart.eq.1 .and. kpart(ipart).eq.5298 ) then
!            write(6,*) i, ':', d, rL, dis
!         end if
!
!         if ( abs(dis-d).gt.1d-1 ) then
!            write(6,*) i, dis, d
!         end if
!        END DEBUG

!        check inside or outside triangle
!         if ( dis.lt.-DTOLd .and. rL.ge.0d0 .and. rL.le.1d0 .and. .not.isboundary ) then
         if ( dis.lt.-DTOLd .and. .not.isboundary ) then
!           outside triangle
            tex = 0d0
            Lexit = L
            exit
         else
!           inside triangle

   !        compute normal velocity to edge (outward positive)
            if ( jsferic.eq.0 ) then
               un =  ux0*cs + uy0*sn
            else
               un =  ux0*ddn(1) + uy0*ddn(2) + uz0*ddn(3)
            end if

!!           BEGIN DEBUG
!!           check normal velocity at closed boundary
!            if ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 ) then
!               dvar = (u0x(k) + alpha(k)* (xn-xzwcell(k)))*ddn(1) + (u0y(k) + alpha(k)*(yn-yzwcell(k)))*ddn(2) + (u0z(k) + alpha(k)*(zn-zzwcell(k)))*ddn(3)
!               if ( abs(dvar) .gt. 1d-4 ) then
!                  continue
!               end if
!            end if
!!           END DEBUG

            if ( un.gt.max(DTOLun_rel*d,DTOLun) ) then   ! normal velocity does not change sign: sufficient to look at u0.n
   !           compute exit time for this edge: ln(1+ d/un alpha) / alpha
               dvar = alpha(k)*dis/un
               if ( dvar.gt.-1d0) then
                  t = dis/un
                  if ( abs(dvar).ge.DTOL ) then
                     t = t * log(1d0+dvar)/dvar
                  end if
               else
                  t = huge(1d0)
               end if

   !           update exit time/edge (flowlink)
!               if ( t.le.tex .and. t.ge.0d0 ) then
               if ( t.le.tex ) then

                  tex = t
                  Lexit = L
               end if
            else
               continue
            end if

         end if
      end do

      if ( dtremaining(ipart).eq.0d0 ) then
         continue
      end if

!     compute timestep in cell (flownode)
      dt = min(dtremaining(ipart), tex)

!     update particle
      if ( abs(alpha(k)).lt.DTOL ) then
         dvar = dt
      else
         dvar = (exp(alpha(k)*dt)-1d0)/alpha(k)
      end if

      xpart(ipart) = xpart(ipart) + dvar * ux0
      ypart(ipart) = ypart(ipart) + dvar * uy0

      if ( jsferic.ne.0 ) then
         zpart(ipart) = zpart(ipart) + dvar * uz0
      end if

!!     BEGIN DEBUG
!      if ( jsferic.eq.1 ) then
!!        project node on triangle
!         dn = (xpart(ipart) - xzwcell(k)) * dnn(1,k) +  &
!              (ypart(ipart) - yzwcell(k)) * dnn(2,k) +  &
!              (zpart(ipart) - zzwcell(k)) * dnn(3,k)
!         xpart(ipart) = xpart(ipart) - dn * dnn(1,k)
!         ypart(ipart) = ypart(ipart) - dn * dnn(2,k)
!         zpart(ipart) = zpart(ipart) - dn * dnn(3,k)
!      end if
!!     END DEBUG

      dtremaining(ipart) = dtremaining(ipart) - dt
!      Lpart(ipart) = Lexit

      if ( dt.eq.0d0 ) then
         numzero(ipart) = numzero(ipart) + 1
      end if

      if ( numzero(ipart).gt.MAXNUMZERO ) then
!        disable particle that is not moving
         kpart(ipart) = 0
         dtremaining(ipart) = 0d0

!     proceed to neighboring cell (if applicable)
      else if ( Lexit.gt.0 ) then
         numremaining = numremaining + 1  ! number of remaining particles for next substep
         if ( edge2cell(1,Lexit).gt.0 .and. edge2cell(2,Lexit).gt.0 ) then   ! internal edge (netlink)
            kpart(ipart) = edge2cell(1,Lexit) + edge2cell(2,Lexit) - k

!            if ( kpart(ipart).eq.5298 ) then
!               call qnerror(' ', ' ', ' ')
!            end if

            if ( kpart(ipart).eq.0 ) then
               continue
            else
               if ( jsferic.eq.1 ) then
!                 project node on triangle
                  k = kpart(ipart)
                  k1 = edge2node(1,Lexit)
                  k2 = edge2node(2,Lexit)
                  xn = 0.5d0*(xnode(k1)+xnode(k2))
                  yn = 0.5d0*(ynode(k1)+ynode(k2))
                  zn = 0.5d0*(znode(k1)+znode(k2))
                  dn = (xpart(ipart) - xn) * dnn(1,k) +  &
                       (ypart(ipart) - yn) * dnn(2,k) +  &
                       (zpart(ipart) - zn) * dnn(3,k)
                  xpart(ipart) = xpart(ipart) - dn * dnn(1,k)
                  ypart(ipart) = ypart(ipart) - dn * dnn(2,k)
                  zpart(ipart) = zpart(ipart) - dn * dnn(3,k)
               end if
            end if
         else  ! on boundary
            kpart(ipart) = 0
         end if
      else
!        safety check
         if ( dtremaining(ipart).ne.0d0 ) then
            ierror = 1
            call qnerror('update_particles_in_cells: dtremaining <> 0', ' ', ' ')
            goto 1234
         end if
      end if

   end do

   ierror = 0
1234 continue

   return
end subroutine update_particles_in_cells
