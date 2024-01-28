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

      SUBROUTINE READXYMIS(MINP)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: l
      integer :: minp
!     snelheidsdrempel
      CHARACTER REC*132, TEX*8
      CALL ZOEKAL(MINP,REC,'MISSING VALUE XY',JA)
      XYMIS = 0d0
      IF (JA .EQ. 1) THEN
         L = INDEX(REC,'XY') + 4
         READ(REC(L:),*,ERR = 888) XYMIS
         WRITE(TEX,'(F8.3)') XYMIS
         CALL MESSAGE('MISSING VALUE XY = ', TEX,' ')
      ENDIF
      RETURN
  888 CALL READERROR('READING MISSING VALUE XY, BUT GETTING', REC, MINP)
      END
