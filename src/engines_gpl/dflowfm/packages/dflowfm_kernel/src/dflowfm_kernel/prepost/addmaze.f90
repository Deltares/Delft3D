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

   SUBROUTINE ADDMAZE(X,Y,Z,N,JAFIVE)    ! FOR FLOW GRIDS
   use m_netw
   use gridoperations

   implicit none
   double precision :: X(N), Y(N), Z(N)
   integer :: N, nn
   integer :: k
   integer :: k2
   integer :: lnu
   INTEGER KK(8), JAFIVE

   DO K = 1,N
      CALL ISNODEDB( KK(k), X(k), Y(k))
      if (kk(k) == 0) then
          numk   = numk + 1
          XK(numk) = X(K) ; YK(numk) = Y(K) ; ZK(numk)  = Z(K) ; KC(numk) = 1; kk(k) = numk
      endif
    enddo

   DO K  = 1,N
      K2 = K+1 ; IF (K .EQ. N) K2 = 1
      CALL CONNECTDB(kk(k),kk(k2),lnu)
   ENDDO

   IF (JAFIVE == 1) THEN
      CALL CONNECTDB(kk(2),kk(4),lnu)
      CALL CONNECTDB(kk(2),kk(5),lnu)
   ENDIF

   RETURN
   END SUBROUTINE ADDMAZE
