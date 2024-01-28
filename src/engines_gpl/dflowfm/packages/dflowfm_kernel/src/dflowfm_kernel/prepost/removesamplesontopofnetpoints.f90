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

   SUBROUTINE REMOVESAMPLESONTOPOFNETPOINTS(XS, YS, NS)
   use m_netw
   implicit none
   double precision :: XS(NS), YS(NS)
   integer :: ns

   double precision :: dx
   double precision :: dy
   integer :: jaontop
   integer :: k
   integer :: ks
   integer :: n
   double precision :: tolnet
   TOLNET = 0.1d0
   N = 0
   DO KS = 1,NS
      JAONTOP = 0
      DO K  = 1,NUMK
         DX = ABS( XK(K) - XS(KS) ) ; DY = ABS( YK(K) - YS(KS) )
         IF (DX < TOLNET .AND. DY < TOLNET) THEN
            JAONTOP = 1 ; CYCLE
         ENDIF
      ENDDO
      IF (JAONTOP == 0) THEN
         N = N + 1
         XS(N) = XS(KS) ; YS(N) = YS(KS)
      ENDIF
   ENDDO
   NS = N
   END SUBROUTINE REMOVESAMPLESONTOPOFNETPOINTS
