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

  SUBROUTINE GETMIDDLEKNOT(K1,K2,K12,A12,R12)
  use m_netw
  use gridoperations
  implicit none
  integer :: K1,K2,K12,K22
  double precision :: A12, R12

  integer :: l1
  integer :: l2
  integer :: n1
  integer :: n2

  DO N1  = 1,NMK(K1)
     L1  = NOD(K1)%LIN(N1)
     CALL OTHERNODE(K1,L1,K12)
     DO N2 = 1,NMK(K2)
        L2 = NOD(K2)%LIN(N2)
        CALL OTHERNODE(K2,L2,K22)
        IF (K12 .EQ. K22) THEN
           A12 = 0 ! ( EA(L1) + EA(L2) ) /2
           R12 = 0 ! ( RL(L1) + RL(L2) ) /2
           RETURN
        ENDIF
     ENDDO
  ENDDO
  K12 = 0
  RETURN
  END SUBROUTINE GETMIDDLEKNOT
