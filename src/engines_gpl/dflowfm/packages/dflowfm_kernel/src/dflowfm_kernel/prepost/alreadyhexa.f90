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

  SUBROUTINE ALREADYHEXA(K1,K2,K3,K4,K5,K6,JA)
  use m_netw
  implicit none
  integer :: K1,K2,K3,K4,K5,K6,JA

  integer :: n1
  integer :: n2
  integer :: n3
  integer :: n4
  integer :: n5
  integer :: n6
  integer :: np
  JA = 0

!  IF (K1 .EQ. 61 .OR. K2 .EQ. 61 .OR. K3 .EQ. 61 .OR. K4 .EQ. 61 .OR. K5 .EQ. 61 .OR. K6 .EQ. 61) THEN
!     JA = 1
!     RETURN ! FOOTBALL
!  ENDIF



  DO NP = NUMP, 1, -1
     IF (netcell(NP)%N .EQ. 6) THEN
        N1 = netcell(NP)%NOD(1)
        N2 = netcell(NP)%NOD(2)
        N3 = netcell(NP)%NOD(3)
        N4 = netcell(NP)%NOD(4)
        N5 = netcell(NP)%NOD(5)
        N6 = netcell(NP)%NOD(6)
        IF ((K1.EQ.N1 .OR. K1.EQ.N2  .OR. K1.EQ.N3 .OR. K1.EQ.N4 .OR. K1.EQ.N5 .OR. K1.EQ.N6) .AND. &
            (K2.EQ.N1 .OR. K2.EQ.N2  .OR. K2.EQ.N3 .OR. K2.EQ.N4 .OR. K2.EQ.N5 .OR. K2.EQ.N6) .AND. &
            (K3.EQ.N1 .OR. K3.EQ.N2  .OR. K3.EQ.N3 .OR. K3.EQ.N4 .OR. K3.EQ.N5 .OR. K3.EQ.N6) .AND. &
            (K4.EQ.N1 .OR. K4.EQ.N2  .OR. K4.EQ.N3 .OR. K4.EQ.N4 .OR. K4.EQ.N5 .OR. K4.EQ.N6) .AND. &
            (K5.EQ.N1 .OR. K5.EQ.N2  .OR. K5.EQ.N3 .OR. K5.EQ.N4 .OR. K5.EQ.N5 .OR. K5.EQ.N6) .AND. &
            (K6.EQ.N1 .OR. K6.EQ.N2  .OR. K6.EQ.N3 .OR. K6.EQ.N4 .OR. K6.EQ.N5 .OR. K6.EQ.N6) ) THEN
            JA = np
            call qnerror('already 6', ' ',' ')
            RETURN
        ENDIF
     ENDIF
  ENDDO
  RETURN
   END SUBROUTINE ALREADYHEXA
