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

!
      SUBROUTINE DISP2P(X,Y,MMAX,MC,NC,NCOL)
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: ncol
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET PUNTJES
      double precision :: X(MMAX,MMAX), Y(MMAX,MMAX)
      CALL SETCOL(NCOL)
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (X(I,J) .NE. 0) THEN
               CALL MOVABS(X(I,J),Y(I,J))
               CALL CIR(0d0)
            ENDIF
   10 CONTINUE
      RETURN
      END
