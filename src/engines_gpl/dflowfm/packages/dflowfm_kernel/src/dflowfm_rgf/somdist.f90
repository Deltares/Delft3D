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
      DO 10 I = M1+1,M2
         DO 10 J = N1+1,N2
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
               DO 30 K = I,I2,II
                  IJC(K,J) = 21
    30         CONTINUE
            ENDIF
    10 CONTINUE

!     DO 40 I = 1,MC
!        DO 40 J = 1,NC
!           IF (IJC(I,J) .NE. 0) THEN
!              R = IJC(I,J)
!              CALL HTEXT(R, X(I,J), Y(I,J))
!           ENDIF
!   40 CONTINUE
!     CALL WAITESC()
      CALL INULARR(IJYES,MMAX,NMAX)
      DO 50 I = M1,M2
         DO 50 J = N1+1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.          &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J-1) .NE. dmiss .AND.   &
                   IJC(I,J) .NE. 21) THEN
                  B(I,J) = B(I,J) + B(I,J-1)
                  D(I,J) = D(I,J) + D(I,J-1)
                  IJYES(I,J) = IJYES(I,J-1) + 1
               ENDIF
            ENDIF
    50 CONTINUE

      DO 55 I = M1,M2
         DO 55 J = N2-1,N1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J+1) .NE. dmiss .AND.  &
                   IJC(I,J+1) .NE. 21) THEN
                  B(I,J) = B(I,J+1)
                  D(I,J) = D(I,J+1)
                  IJYES(I,J) = IJYES(I,J+1)
               ENDIF
            ENDIF
    55 CONTINUE

      DO 56 I = M1,M2
         DO 56 J = N1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               B(I,J) = B(I,J) / (IJYES(I,J) + 1)
               D(I,J) = D(I,J) / (IJYES(I,J) + 1)
            ENDIF
    56 CONTINUE

!     DO 60 I = 1,MC-1
!        DO 60 J = 1,NC-1
!           IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.
!               IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
!              R = B(I,J)
!              R = IJYES(I,J) + 1
!              XX = ( X(I,J) + X(I,J+1) + X(I+1,J+1) + X(I+1,J) ) / 4
!              YY = ( Y(I,J) + Y(I,J+1) + Y(I+1,J+1) + Y(I+1,J) ) / 4
!              CALL HTEXT(R, XX, YY)
!           ENDIF
!   60 CONTINUE
!     CALL WAITESC()

      CALL ISITU ( )
      CALL INULARR(IJYES,MMAX,NMAX)

      DO 110 I = M1+1,M2
         DO 110 J = N1+1,N2
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
               DO 130 L = J,J2,JJ
                  IJC(I,L) = 22
   130         CONTINUE
            ENDIF
   110 CONTINUE

      DO 150 J = N1,N2
         DO 150 I = M1+1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I-1,J) .NE. dmiss .AND. &
                   IJC(I,J) .NE. 22) THEN
                  A(I,J) = A(I,J) + A(I-1,J)
                  C(I,J) = C(I,J) + C(I-1,J)
                  IJYES(I,J) = IJYES(I-1,J) + 1
               ENDIF
            ENDIF
   150 CONTINUE

      DO 155 J = N1,N2
         DO 155 I = M2-1,M1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I+1,J) .NE. dmiss .AND.  &
                   IJC(I+1,J) .NE. 22) THEN
                  A(I,J) = A(I+1,J)
                  C(I,J) = C(I+1,J)
                  IJYES(I,J) = IJYES(I+1,J)
               ENDIF
            ENDIF
   155 CONTINUE

      DO 156 J = N1,N2
         DO 156 I = M1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               A(I,J) = A(I,J) / (IJYES(I,J) + 1)
               C(I,J) = C(I,J) / (IJYES(I,J) + 1)
            ENDIF
   156 CONTINUE

!     CALL SETCOL(66)
!     DO 160 I = 1,MC-1
!        DO 160 J = 1,NC-1
!           IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.
!               IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
!              R = A(I,J)
!              R = IJYES(I,J) + 1
!              XX = ( X(I,J) + X(I,J+1) + X(I+1,J+1) + X(I+1,J) ) / 4
!              YY = ( Y(I,J) + Y(I,J+1) + Y(I+1,J+1) + Y(I+1,J) ) / 4
!              CALL HTEXT(R, XX, YY)
!           ENDIF
!  160 CONTINUE
!     CALL WAITESC()

!     Herstellen
      CALL ISITU (     )

      RETURN
      END SUBROUTINE SOMDIST
