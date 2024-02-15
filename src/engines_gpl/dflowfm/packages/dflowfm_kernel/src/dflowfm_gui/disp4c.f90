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

      SUBROUTINE DISP4C(X,Y,N)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS
      double precision :: X(N), Y(N)

      IF (N .LE. 0) RETURN
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL LNABS(X(I),Y(I))
            ELSE
               CALL MOVABS(X(I),Y(I))
               ISTART = 1
            ENDIF
            CALL RCIRC(X(I),Y(I))
         ELSE
            ISTART = 0
         ENDIF
         IF (MOD(I,50) .EQ. 0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
