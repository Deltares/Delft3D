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

      SUBROUTINE TEKLN2(X, Y, mmax, nmax, M1, N1, M2, N2, NCOL)
!     TEKEN EEN LIJN IN GRID (MET CIRKELS ROND DE UITEINDEN)
      use m_missing
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: istart, i, j, in, jn

      CALL SETCOL(NCOL)
      ISTART = 0
      IF (M1 .NE. 0) CALL CIRR(X(M1,N1),Y(M1,N1),NCOL)
      IF (M2 .NE. 0) CALL CIRR(X(M2,N2),Y(M2,N2),NCOL)
      IF (M1 .NE. 0 .AND. M2 .NE. 0) THEN
         IN = SIGN(1,M2-M1)
         JN = SIGN(1,N2-N1)
         DO 10 I = M1,M2,IN
            DO 10 J = N1,N2,JN
               IF (X(I,J) .NE. XYMIS) THEN
                  IF (ISTART .EQ. 0) THEN
                     CALL MOVABS(X(I,J),Y(I,J))
                     ISTART = 1
                  ELSE
                     CALL LNABS(X(I,J),Y(I,J))
                  ENDIF
               ELSE
                  ISTART = 0
               ENDIF
    10   CONTINUE
      ENDIF
      RETURN
      END subroutine tekln2
