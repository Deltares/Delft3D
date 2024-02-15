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

  DOUBLE PRECISION FUNCTION SHORTESTLINK(K)
  use m_netw
  implicit none
  INTEGER :: K

  integer :: k1
  integer :: k2
  integer :: l1
  double precision :: r1
  INTEGER :: KK, L, NX
  DOUBLE PRECISION :: DLENGTH

  SHORTESTLINK = 1D9
  NX = SIZE(NOD(K)%LIN)
  DO KK = 1, NX
     L1 = NOD(K)%LIN(KK)
     IF (L1 .NE. 0) THEN
        K1 = KN(1,L1) ; K2 = KN(2,L1)
        IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
           R1 = DLENGTH( K1, K2 )
           SHORTESTLINK = MIN(SHORTESTLINK, R1)
        ENDIF
     ENDIF
  ENDDO
  RETURN
  END FUNCTION SHORTESTLINK
