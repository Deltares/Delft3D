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

  SUBROUTINE ALREADYTRI(K1,K2,K3,JA)
  use m_netw
  implicit none
  integer :: K1,K2,K3,JA

  integer :: n1
  integer :: n2
  integer :: n3
  integer :: np
  JA = 0
  DO NP = NUMP, 1, -1
     IF (netcell(NP)%N .EQ. 3) THEN
        N1 = netcell(NP)%NOD(1)
        N2 = netcell(NP)%NOD(2)
        N3 = netcell(NP)%NOD(3)
        IF ((K1.EQ.N1 .OR. K1.EQ.N2  .OR. K1.EQ.N3 ) .AND. &
            (K2.EQ.N1 .OR. K2.EQ.N2  .OR. K2.EQ.N3 ) .AND. &
            (K3.EQ.N1 .OR. K3.EQ.N2  .OR. K3.EQ.N3 ) ) THEN
            JA = np
            call qnerror('already 3', ' ',' ')
            RETURN
        ENDIF
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE ALREADYTRI
