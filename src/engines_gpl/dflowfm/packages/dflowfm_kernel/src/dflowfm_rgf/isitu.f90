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

      SUBROUTINE ISITU()
      use m_grid
      USE M_MISSING

      implicit none

!C     IJYES, WELKE CELLEN DOEN MEE 1 OF 0
!C     IJC  , CODE VAN PUNT, ZIE FIGUUR
!C                            0  14   3  13
!C     9  4  3               14  12  10   2
!C     6  1  2                4  10  10   2
!C     8  5  7               11   1   1  12
!C
!C     ALS IK NOG EENS TIJD HEB, ZAL IK DE NUMMERING MOOIER MAKEN
!C

      double precision :: x1, x2, x3, x4, x5, x6, x7, x8, x9, y1
      integer :: i, j, i1, i2, i3, i4, IJYES2, IJYES3, IJYES4, jaontop, jaunconnected

      if ( allocated (ijc) )   deallocate (ijc   )
      if ( allocated (ijyes) ) deallocate (ijyes )
      allocate (ijc(mmax,nmax), ijyes(mmax,nmax) )


    5 CONTINUE
      ijc = 0 ; ijyes = 0

      DO 10 I = 1,MC-1
         DO 10 J = 1,NC-1
            X1 = Xc(I,J)
            X2 = Xc(I+1,J)
            X3 = Xc(I+1,J+1)
            X4 = Xc(I,J+1)
            IF (X1 .NE. dXYMIS .AND. X2 .NE. dXYMIS .AND.   &
                X3 .NE. dXYMIS .AND. X4 .NE. dXYMIS ) IJYES(I,J) = 1
   10 CONTINUE


      DO 11 I = 1,MC
         DO 11 J = 1,NC
            X1 = Xc(I,J)
            IF (I .NE. 1)  X6 = Xc(I-1,J)
            IF (I .NE. MC) X2 = Xc(I+1,J)
            IF (J .NE. 1)  X5 = Xc(I,J-1)
            IF (J .NE. NC) X4 = Xc(I,J+1)
            IF (I .NE. MC .AND. J .NE. NC ) X3 = Xc(I+1,J+1)
            IF (I .NE.  1 .AND. J .NE.  1 ) X8 = Xc(I-1,J-1)
            IF (I .NE.  1 .AND. J .NE.  NC) X9 = Xc(I-1,J+1)
            IF (I .NE. MC .AND. J .NE.  1 ) X7 = Xc(I+1,J-1)
!           POSITIE BENOEMEN
            IF (X1 .EQ. dXYMIS) THEN
               IJC(I,J) = 0
            ELSE IF (I .EQ. 1) THEN
!              LINKS
               IF (J .EQ. 1) THEN
                  IJC(I,J) = 11
               ELSE IF (J .EQ. NC) THEN
                  IJC(I,J) = 14
               ELSE IF (X5 .EQ. dXYMIS) THEN
                  IJC(I,J) = 11
               ELSE IF (X4 .EQ. dXYMIS) THEN
                  IJC(I,J) = 14
               ELSE
                  IJC(I,J) = 4
               ENDIF
            ELSE IF (I .EQ. MC) THEN
!              RECHTS
               IF (J .EQ. 1) THEN
                  IJC(I,J) = 12
               ELSE IF (J .EQ. NC) THEN
                  IJC(I,J) = 13
               ELSE IF (X5 .EQ. dXYMIS) THEN
                  IJC(I,J) = 12
               ELSE IF (X4 .EQ. dXYMIS) THEN
                  IJC(I,J) = 13
               ELSE
                  IJC(I,J) = 2
               ENDIF
            ELSE IF (J .EQ. 1) THEN
!              ONDER
               IF (X6 .EQ. dXYMIS) THEN
                  IJC(I,J) = 11
               ELSE IF (X2 .EQ. dXYMIS) THEN
                  IJC(I,J) = 12
               ELSE
                  IJC(I,J) = 1
               ENDIF
            ELSE IF (J .EQ. NC) THEN
!              BOVEN
               IF (X6 .EQ. dXYMIS) THEN
                  IJC(I,J) = 14
               ELSE IF (X2 .EQ. dXYMIS) THEN
                  IJC(I,J) = 13
               ELSE
                  IJC(I,J) = 3
               ENDIF
            ELSE
               I1 = IJYES(I,J)
               I2 = IJYES(I-1,J)
               I3 = IJYES(I-1,J-1)
               I4 = IJYES(I,J-1)
               IF (I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  10
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  11
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  12
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  13
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  14
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  1
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  4
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  3
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  2
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  11
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  12
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  13
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  14
               ENDIF
            ENDIF
   11 CONTINUE



      JAUNCONNECTED = 0
      DO 20 I = 2,MC
         DO 20 J = 2,NC
            X1 = Xc(I,J)
            IF (X1 .NE. dXYMIS) THEN
!              ALS ER EEN PUNT IS, MAAR GEEN VAN DE OMLIGGENDE CELLEN IS
!              GESLOTEN
               IF (IJYES(I,J) .EQ. 0) THEN
                  IJYES2 = IJYES(I-1,J)
                  IJYES3 = IJYES(I-1,J-1)
                  IJYES4 = IJYES(I,J-1)
                  IF (IJYES2 .EQ. 0 .AND. IJYES3 .EQ. 0 .AND. &
                     IJYES4 .EQ. 0                         ) THEN
                 !    WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
                     JAUNCONNECTED = 1
                     Xc(I,J) = dXYMIS
                     Yc(I,J) = dXYMIS
                  ENDIF
               ENDIF
            ENDIF
   20 CONTINUE
      J = 1
      DO 25 I = 2,MC
         X1 = Xc(I,J)
         IF (X1 .NE. dXYMIS) THEN
            IF (IJYES(I-1,J) .EQ. 0 .AND. IJYES(I,J) .EQ. 0) THEN
             !  WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I,J) = dXYMIS
               Yc(I,J) = dXYMIS
            ENDIF
         ENDIF
   25 CONTINUE
      I = 1
      DO 26 J = 2,NC
         X1 = Xc(I,J)
         IF (X1 .NE. dXYMIS) THEN
            IF (IJYES(I,J-1) .EQ. 0 .AND. IJYES(I,J) .EQ. 0) THEN
            !   WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I,J) = dXYMIS
               Yc(I,J) = dXYMIS
            ENDIF
         ENDIF
   26 CONTINUE
      J = 1
      I = 1
      IF (IJYES(I,J) .EQ. 0 .AND. Xc(I,J) .NE. dXYMIS) THEN
        ! WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
         Xc(I,J) = dXYMIS
         Yc(I,J) = dXYMIS
      ENDIF
      IF (JAUNCONNECTED .EQ. 1) then
         GOTO 5
      endif

!     spline2curvi: do not check on corrupt grids
!!     CHECK OP CORRUPTE ROOSTERS
!      JAONTOP = 0
!      DO 30 I = 1,MC-1
!         DO 30 J = 1,NC-1
!            X1 = Xc(I,J)
!            Y1 = Yc(I,J)
!            IF (X1 .NE. dXYMIS) THEN
!               IF (X1.EQ.Xc(I,J+1)  .AND. Y1.EQ.Yc(I,J+1) .OR. &
!                   X1.EQ.Xc(I+1,J)  .AND. Y1.EQ.Yc(I+1,J) .OR. &
!                   X1.EQ.Xc(I+1,J+1).AND. Y1.EQ.Yc(I+1,J+1)) THEN
!                   JAONTOP = 1
!                   Xc(I,J)  = dXYMIS
!                   Yc(I,J)  = dXYMIS
!               ENDIF
!            ENDIF
!   30 CONTINUE
!      IF (JAONTOP .EQ. 1) THEN
!         ! CALL QNERROR('IDENTICAL NEIGHBOURPOINT DELETED,', 'CHECK GRID',' ')
!         GOTO 5
!      ENDIF

      RETURN
      END SUBROUTINE ISITU
