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

      SUBROUTINE DISP3CAB(X,Y,Z,NCL,N,RCIR,NCOL,A,B)
      USE M_MISSING
      implicit none
      double precision :: a
      double precision :: b
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
      integer :: ncol
      double precision :: rcir
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS EN KLEUREN
      DOUBLE PRECISION X(N), Y(N), Z(N)
      INTEGER NCL(N)

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL DLNABS(A*X(I)+B,Y(I),Z(I))
            ELSE
               IF (NCL(I) .NE. 0) CALL SETCOL(NCL(I))
               CALL DMOVABS(A*X(I)+B,Y(I),Z(I))
               ISTART = 1
            ENDIF
            CALL CIR(RCIR)
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
