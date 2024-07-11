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

      SUBROUTINE SOMDIST(X,Y,A,B,C,D,M1,N1,M2,N2)
      use m_grid
      use m_missing
      implicit none
      integer :: i
      integer :: i2
      integer :: ii
      integer :: j
      integer :: j2
      integer :: jj
      integer :: k
      integer :: l
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      DOUBLE PRECISION ::   X(MMAX,NMAX),  Y(MMAX,NMAX),        &
                            A(MMAX,NMAX),  B(MMAX,NMAX),        &
                            C(MMAX,NMAX),  D(MMAX,NMAX)
!
      do I = M1+1,M2
         do J = N1+1,N2
            IF (IJC(I,J) .EQ. 11) THEN
               II = -1
            ELSE IF (IJC(I,J) .EQ. 12) THEN
               II =  1
            ELSE IF (IJC(I,J) .EQ. 13) THEN
               II =  1
            ELSE IF (IJC(I,J) .EQ. 14) THEN
               II = -1
            ENDIF
            IF (IJC(I,J) .GE. 11 .AND. IJC(I,J) .LE. 14) THEN
               K = I
    20         CONTINUE
               K  = K + II
               I2 = K
               IF (IJC(K,J) .EQ. 10) GOTO 20
               DO K = I,I2,II
                  IJC(K,J) = 21
               END DO
            ENDIF
         end do
      end do
      
      CALL INULARR(IJYES,MMAX,NMAX)
      DO I = M1,M2
         DO J = N1+1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.          &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J-1) .NE. dmiss .AND.   &
                   IJC(I,J) .NE. 21) THEN
                  B(I,J) = B(I,J) + B(I,J-1)
                  D(I,J) = D(I,J) + D(I,J-1)
                  IJYES(I,J) = IJYES(I,J-1) + 1
               ENDIF
            ENDIF
         end do
      end do
      

      DO I = M1,M2
         DO J = N2-1,N1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J+1) .NE. dmiss .AND.  &
                   IJC(I,J+1) .NE. 21) THEN
                  B(I,J) = B(I,J+1)
                  D(I,J) = D(I,J+1)
                  IJYES(I,J) = IJYES(I,J+1)
               ENDIF
            ENDIF
         end do
      end do
      
      DO I = M1,M2
         DO J = N1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               B(I,J) = B(I,J) / (IJYES(I,J) + 1)
               D(I,J) = D(I,J) / (IJYES(I,J) + 1)
            ENDIF
         end do
      end do
      
      CALL ISITU ( )
      CALL INULARR(IJYES,MMAX,NMAX)

      DO I = M1+1,M2
         DO J = N1+1,N2
            IF (IJC(I,J) .EQ. 11) THEN
               JJ = -1
            ELSE IF (IJC(I,J) .EQ. 12) THEN
               JJ = -1
            ELSE IF (IJC(I,J) .EQ. 13) THEN
               JJ =  1
            ELSE IF (IJC(I,J) .EQ. 14) THEN
               JJ =  1
            ENDIF
            IF (IJC(I,J) .GE. 11 .AND. IJC(I,J) .LE. 14) THEN
               L = J
   120         CONTINUE
               L  = L + JJ
               J2 = L
               IF (IJC(I,L) .EQ. 10) GOTO 120
               DO L = J,J2,JJ
                  IJC(I,L) = 22
               end do
            ENDIF
         end do
      end do
      
      DO J = N1,N2
         DO I = M1+1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I-1,J) .NE. dmiss .AND. &
                   IJC(I,J) .NE. 22) THEN
                  A(I,J) = A(I,J) + A(I-1,J)
                  C(I,J) = C(I,J) + C(I-1,J)
                  IJYES(I,J) = IJYES(I-1,J) + 1
               ENDIF
            ENDIF
         end do
      end do
      
      DO J = N1,N2
         DO I = M2-1,M1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I+1,J) .NE. dmiss .AND.  &
                   IJC(I+1,J) .NE. 22) THEN
                  A(I,J) = A(I+1,J)
                  C(I,J) = C(I+1,J)
                  IJYES(I,J) = IJYES(I+1,J)
               ENDIF
            ENDIF
         end do
      end do
      
      DO J = N1,N2
         DO I = M1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               A(I,J) = A(I,J) / (IJYES(I,J) + 1)
               C(I,J) = C(I,J) / (IJYES(I,J) + 1)
            ENDIF
         end do
      end do
      
!     Herstellen
      CALL ISITU (     )

      RETURN
      END SUBROUTINE SOMDIST
