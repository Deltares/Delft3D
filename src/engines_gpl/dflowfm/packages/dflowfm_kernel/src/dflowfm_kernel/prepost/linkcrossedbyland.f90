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

!> check if a link is close to a land boundary segment
subroutine linkcrossedbyland(L, jstart, jend, netboundonly, jland, jacross)

   use m_netw
   use m_landboundary
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer, intent(in)           :: L              !< link number
   integer, intent(in)           :: jstart, jend   !< start and end point of land boundary segment respectively
   integer, intent(in)           :: netboundonly   !< consider only the net boundary (1) or not (0)
   integer, intent(inout)        :: jland          !< point in land boundary that is (in:) visited first (out:) found

   integer, intent(out)          :: jacross        !< crossed (1) or not (0)

   integer                       :: k1, k2         ! nodes comprising the link

   integer                       :: iter, j, j_
   integer                       :: ja, jastop

   double precision              :: x1, y1, x2, y2 !  node coordinates
   double precision              :: x3, y3, x4, y4 ! land boundary point coordinates

   double precision              :: sl, sm, xcr, ycr, crp, DL, Dm, Dtol, dismin

   double precision              :: dis,xn,yn, rL, rL1, rL2  ! for dlinedis3

   jacross = 0

   j = max(min(jland,jend-1), jstart)

   k1 = kn(1,L)
   k2 = kn(2,L)

   if ( k1.lt.0 .or. k2.lt.0 ) then    ! safety
      return
   end if

   x1 = xk(k1)
   y1 = yk(k1)

   x2 = xk(k2)
   y2 = yk(k2)

   DL = dbdistance(x1,y1,x2,y2,jsferic, jasfer3D, dmiss)
   Dtol = DCLOSE * DL
   dismin = 1d99

!  loop over the segments of the land boundary
   jacross = 0
   jastop  = 0
   j_ = 0
   do
      x3 = xlan(j)
      y3 = ylan(j)
      x4 = xlan(j+1)
      y4 = ylan(j+1)
      Dm = dbdistance(x3,y3,x4,y4,jsferic, jasfer3D, dmiss)
      if ( x3.ne.dmiss .and. x4.ne.dmiss .and. Dm.gt.0d0 ) then
         rL1 = 0d0
         rL2 = 1d0
         call dlinedis3(x1,y1,x3,y3,x4,y4,ja,dis,xn,yn,rL1)
         if ( dis.le.dtol ) then
            jacross = 1
            jland   = j
            if ( rL1.ge.0d0 .and. rL1.le.1d0 ) jastop  = 1
         else
            call dlinedis3(x2,y2,x3,y3,x4,y4,ja,dis,xn,yn,rL2)
            if ( dis.le.dtol ) then
               jacross = 1
               jland   = j
               if ( rL2.ge.0d0 .and. rL2.le.1d0 ) jastop  = 1
            end if
         end if

         dismin = min(dis, dismin)

         if ( jastop.eq.1 ) exit
      end if

!     move pointer left-right-left-right-left-right etc.
      iter = 0
      do while( ( iter.eq.0 .or. j.lt.jstart .or. j.gt.jend-1 ) .and. iter.lt.3)
         iter = iter+1
         if ( j_.lt.0 ) then  ! to right
            j_ = -j_ + 1
         else                 ! to left
            j_ = -j_ - 1
         end if
         j = j + j_
      end do
      if ( iter.eq.3 ) exit
   end do

!   if ( jacross.eq.1 .and. max(rL1,rL2).gt.0d0 .and. min(rL1,rL2).le.1d0 ) then
   if ( jacross.eq.1  ) then
      j = jland
!     set outer land boundary segment points
!     minimum
      sm = min(rL1,rL2)
      if ( j.lt.jleft ) then
         jleft = j
         rLleft = min(max(sm,0d0),1d0)
      else if ( j.eq.jleft ) then
         rLleft = min(max(sm,0d0),rLleft)
      end if
!     maximum
      sm = max(rL1,rL2)
      if ( j.gt.jright ) then
         jright = j
         rLright = min(max(sm,0d0),1d0)
      else if ( j.eq.jright ) then
         rLright = max(min(sm,1d0),rLright)
      end if

   end if

   return
end subroutine linkcrossedbyland
