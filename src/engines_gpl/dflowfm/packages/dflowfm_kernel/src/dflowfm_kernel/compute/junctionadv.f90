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

   ! =================================================================================================
   ! =================================================================================================
   subroutine junctionadv()
   use m_flowgeom , only: lnx1d, ln, nd
   use m_flow     , only: q1
   use m_fm_erosed, only: q_zeta
   implicit none
   integer          :: i, L, Li, Lf, La, k
   double precision :: s_l, s_m

   q_zeta = 0d0

   do L = 1,lnx1d                                       ! loop over flow links
      !if (kfu(m)==1) then                                      !.and. kcu(m)==1
      do i = 1,2
         k = ln(i,L)
         do Li = 1,nd(k)%lnx                              ! loop over all flow links for each zeta point
            Lf = nd(k)%ln(Li)
            La = iabs(Lf)
            if (La /= L) then                               ! if (m1 /= current flow link)
               s_l = sign(1d0,Lf+0d0)
               q_zeta(i,L) = q_zeta(i,L) + q1(La) * s_l
            else
               s_m = sign(1d0,Lf+0d0)
            endif
         enddo
         if (nd(k)%lnx == 1) then                           ! if boundary or end node
            q_zeta(i,L)  = q1(L)
         else
            q_zeta(i,L) = ( - s_m * q_zeta(i,L) + ( nd(k)%lnx - 1 ) * q1(L) ) / nd(k)%lnx
         endif
      enddo
   enddo

   end subroutine junctionadv
