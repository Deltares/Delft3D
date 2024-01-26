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

  RECURSIVE SUBROUTINE WALK1D(K1,IBR,NRL,JASTOP,KN316)

  use m_netw
  use gridoperations

  IMPLICIT NONE
  INTEGER :: K1,K2,K,IBR,NRL,JASTOP,LX,KN316

  INTEGER :: KK,L, KA

  JASTOP = 0
  DO KK = 1,NMK(K1)
     L  = NOD(K1)%LIN(KK)
     IF (LC(L) == 0 .AND. KN(3,L) == KN316) THEN

        CALL OTHERNODE (K1,L,K2)
        CALL GAANWESTOPPEN(K2,KN316,JASTOP,L)

        LC(L) = IBR ; NRL = NRL + 1
        LIB(NRL) = L ; K1BR(NRL) = K1 ; IBN(NRL) = IBR; NRLB(L) = NRL

        IF (JASTOP == 1) THEN
           RETURN
        ENDIF

        KA = K2
        CALL WALK1D(KA,IBR,NRL,JASTOP,KN316)

        IF (JASTOP == 1) THEN
           RETURN
        ENDIF
     ENDIF
  ENDDO
  END SUBROUTINE WALK1D
