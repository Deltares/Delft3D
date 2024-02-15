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

   SUBROUTINE GETQUAD(LN,K1,K2,K3N,K4N)
   use m_netw
   use gridoperations
   implicit none
   integer :: LN,K1,K2,K3N,K4N

   integer :: k
   integer :: k1a
   integer :: k3
   integer :: k4
   integer :: kk
   integer :: kkk
   integer :: l
   integer :: ll
   integer :: lll

   K3N = 0 ; K4N = 0

   DO K = 1,NMK(K2)
      L = NOD(K2)%LIN(K)
      IF (L == LN) CYCLE
      CALL OTHERNODE(K2,L,K3)
      DO KK = 1,NMK(K3)
         LL = NOD(K3)%LIN(KK)
         IF (LL == L) CYCLE
         CALL OTHERNODE(K3,LL,K4)
         DO KKK = 1,NMK(K4)
            LLL = NOD(K4)%LIN(KKK)
            IF (LLL == LL) CYCLE
            CALL OTHERNODE(K4,LLL,K1A)
            IF (K1A == K1) THEN
                K3N = K3 ; K4N = K4
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   END SUBROUTINE GETQUAD
