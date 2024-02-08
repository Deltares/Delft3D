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

  SUBROUTINE REMZERONODE(KP)
  use m_netw
  implicit none
  integer :: KP

  integer :: k
  integer :: l

  NUMK = NUMK -1         ! Administratie aanschuiven
  DO K = KP,NUMK
     XK(K)  = XK(K+1)
     YK(K)  = YK(K+1)
     ZK(K)  = ZK(K+1)
!     IF (NETFLOW .EQ. 1)
     KC(K)  = KC(K+1)
     NMK(K) = NMK(K+1)
     DO L = 1,NMK(K)
        NOD(K)%LIN(L) = NOD(K+1)%LIN(L)
     ENDDO
  ENDDO

  DO L = 1,NUML
     IF (KN(1,L) .GT. KP) KN(1,L) = KN(1,L) - 1
     IF (KN(2,L) .GT. KP) KN(2,L) = KN(2,L) - 1
  ENDDO

  RETURN
  END SUBROUTINE REMZERONODE
