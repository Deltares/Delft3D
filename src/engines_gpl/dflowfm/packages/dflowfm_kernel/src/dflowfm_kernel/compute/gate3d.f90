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

 subroutine gate3D(jazerou1)
 use m_flow
 use m_flowgeom
 use fm_external_forcings_data
 implicit none
 integer          :: jazerou1 ! 1 set u1 zero above gate
 integer          :: L, LL, Lb, Lt, ng, n, k, kb, kt
 double precision :: zga, bup, zLu, fac

 do ng = 1, ngatesg
    zga   = zgate(ng)
    do n  = L1gatesg(ng), L2gatesg(ng)
       LL = kgate(3,n)
       if (hu(LL) > 0d0) then
         bup = 0.5d0*( bob(1,LL) + bob(2,LL) )

         call getLbotLtop(LL,Lb,Lt)
         do L   = Lb, Lt
            ZLu = bup + hu(L-1)
            fac =  (zga-zLu) / ( hu(L) - hu(L-1) )
            if (fac < 0.1d0) then
               Ltop(LL) = L - 1 ; exit
            else
               fac   = max(0d0, min(1d0, fac ) )
            endif
            hu(L) = hu(L-1) + fac*( hu(L) - hu(l-1) )
            au(L) = au(L)*fac
         enddo
         au( Ltop(LL)+1 : Lbot(LL)+kmxL(LL)-1 ) = 0d0  ! -12346d0 ! 6 not 5

      endif
   enddo
 enddo
 end subroutine gate3D
