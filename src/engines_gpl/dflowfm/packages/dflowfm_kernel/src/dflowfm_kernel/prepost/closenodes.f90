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

  SUBROUTINE CLOSENODES(K,KK,JA) ! ARE THESE NODES CLOSE, BUT UNCONNECTED?

  use m_netw
  use m_wearelt
  use gridoperations

  implicit none
  INTEGER          :: K,KK,JA

  integer :: k2
  integer :: l1
  integer :: n
  integer :: nx

  DOUBLE PRECISION :: R0, R1, R2, DLENGTH, SHORTESTLINK
  JA = 0
  R0 = DLENGTH(K,KK)
  IF (R0 > 6d0*RCIR ) RETURN

  L1 = NOD(K)%LIN(1)
  R1 = SHORTESTLINK(K) ; R2 = SHORTESTLINK(KK); R1 = MIN(R1,R2)*0.4d0
  CALL CLOSEENOUGH(XK(K), YK(K), XK(KK), YK(KK), R1, JA)
  IF (JA == 0) RETURN

  JA = 0
  NX = SIZE(NOD(K)%LIN)
  DO N = 1, NX
     L1 = NOD(K)%LIN(N)
     CALL OTHERNODE(K,L1,K2)
     IF (K2 == KK) THEN
        JA = 0 ; RETURN
     ENDIF
  ENDDO

  NX = SIZE(NOD(KK)%LIN)
  DO N  = 1, NX
     L1 = NOD(KK)%LIN(N)
     CALL OTHERNODE(KK,L1,K2)
     IF (K2 == K) THEN
        JA = 0 ; RETURN
     ENDIF
  ENDDO
  JA = 1 ! KENNELIJK UNCONNECTED

  RETURN
  END SUBROUTINE CLOSENODES
