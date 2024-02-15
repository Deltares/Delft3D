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

  SUBROUTINE GAANWESTOPPEN(K,KN316,JASTOP,LO)  !SET JASTOP = 1 ALS WE GAAN STOPPEN
  USE M_NETW
  IMPLICIT NONE
  INTEGER :: K2,KN316,JASTOP,N1,N6,KK,L,K1,K,LO

  JASTOP = 0 ; N1 = 0 ; N6 = 0

  IF (NMK0(K) == 1) THEN
      JASTOP = 1 ; RETURN
  ENDIF

  DO KK = 1,NMK(K)
     L  = NOD(K)%LIN(KK)
     IF (KN(3,L) == 1) THEN
        N1 = N1 + 1
     ELSE IF (KN(3,L) == 6) THEN
        N6 = N6 + 1
     ENDIF
  ENDDO
  IF (KN316 == 1) THEN
     IF (N1 + N6 .NE. 2) THEN  ! altijd stoppen bij niet doorgaande node
        JASTOP = 1
     ENDIF
  ELSE IF (KN316 == 6) THEN    ! alleen stoppen bij aantal 6 jes ongelijk 2
     IF (N6 .NE. 2) THEN
        JASTOP = 1
     ENDIF
  ENDIF

  END SUBROUTINE GAANWESTOPPEN
