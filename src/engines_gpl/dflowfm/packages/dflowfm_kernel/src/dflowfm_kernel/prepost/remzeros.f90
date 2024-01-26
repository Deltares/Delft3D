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

  SUBROUTINE REMZEROS()
  use m_netw
  use gridoperations
  implicit none

  integer :: k
  integer :: k1
  integer :: k2
  integer :: l
  integer :: ll
  integer :: n
  INTEGER, ALLOCATABLE :: NN(:)

  ALLOCATE (NN(NUMK)); NN = 0


  KC = 0
  DO L = 1,NUML
     K1 = KN(1,L) ; K2 = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
         KC(K1) = 1 ; KC(K2) = 1
     ENDIF
  ENDDO

  N = 0
  DO K = 1,NUMK
     IF (KC(K) .NE. 0) THEN
        N = N + 1
        XK(N) = XK(K)
        YK(N) = YK(K)
        ZK(N) = ZK(K)
        KC(N) = KC(K)
        KC(K) = 0
        NN(K) = N
     ENDIF
  ENDDO


  LL = 0
  DO L = 1,NUML
     IF (KN(1,L) .NE. 0 .AND. KN(2,L) .NE. 0) THEN
        LL = LL + 1
        K1 = KN(1,L)
        K2 = KN(2,L)
        KN(1,LL) = NN(K1)
        KN(2,LL) = NN(K2)
     ENDIF
  ENDDO

  NUML = LL

  KC = 0
  DO L = 1,NUML
     K1 = KN(1,L) ; K2 = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
         KC(K1) = 1 ; KC(K2) = 1
     ENDIF
  ENDDO

  DO L = 1,NUML
     IF (KN(1,L) == 0 .OR. KN(2,L) == 0) THEN
        N = N
     ENDIF
  ENDDO


  CALL SETNODADM(0)
  DEALLOCATE (NN)

  RETURN
  END SUBROUTINE REMZEROS
