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

!> make the masks for netboundtopoly
subroutine netboundtopoly_makemasks()
   use m_netw
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol

   implicit none

   integer              :: inside, k1, k2, L

!  make node and link masks
   Lc = 0
   kc = -1
   inside = -1
   do L=1,numL
      if ( lnn(L).ne.1 ) cycle      ! not a net boundary link
      if ( kn(3,L).ne.2) cycle      ! not a 2D link
      k1 = kn(1,L)
      k2 = kn(2,L)
      if ( k1.lt.1 .or. k1.gt.numk .or. k2.lt.1 .or. k2.gt.numk ) then  ! safety, should not happen
         continue
         cycle
      end if
      if ( kc(k1).eq.-1 ) then      ! mask of node k1 not yet determined
         call dbpinpol(xk(k1),yk(k1),inside,dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( inside.eq.1 ) then
            kc(k1) = 1
         else
            kc(k1) = 0
         end if
      end if
      if ( kc(k1).eq.1 ) then
         if ( kc(k2).eq.-1 ) then   ! mask of node k1 not yet determined
            call dbpinpol(xk(k2),yk(k2),inside,dmiss, JINS, NPL, xpl, ypl, zpl)
            if ( inside.eq.1 ) then
               kc(k2) = 1
            else
               kc(k2) = 0
            end if
         end if

         if ( kc(k2).eq.1 ) then    ! both nodes inside selecting polygon
            Lc(L) = 1
         end if
      end if
   end do

   return
end subroutine netboundtopoly_makemasks
