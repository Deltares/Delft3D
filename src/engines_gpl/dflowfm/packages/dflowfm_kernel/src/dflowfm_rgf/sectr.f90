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

     SUBROUTINE SECTR (      X,      Y,    TIJ,mmax, nmax, imax,                     &
                          merr,  NUMI,                     &
                         NUMSPL,  NUMPX,   NTYP, MN12, XI, YI, XJ, YJ)
     use unstruc_colors
     use unstruc_messages
     use unstruc_display

     implicit none
     integer :: mmax, nmax, imax
     double precision, dimension(mmax,nmax), intent(inout) :: X, Y
     double precision, dimension(mmax,nmax), intent(out) :: TIJ
     integer, intent(out) :: merr, numi, numspl, numpx
     integer, dimension(imax) :: NTYP
     integer, dimension(imax,3), intent(out) :: MN12
     double precision, dimension(imax), intent(out) :: XI, YI, XJ, YJ

!      INTEGER :: NTYP(IMAX), MN12(IMAX,3)
      CHARACTER TEX1*4, TEX2*4
      double precision :: crp, ti, tj, xspc, yspc
      integer :: mcs, ncs, i, j, numpi, j2, ionbenoemd, numpj, numcro, &
                 L, jachange, icount, JK, maxm, maxn, jjlast, jj, iilast, ii
      integer :: jadubbel
      JADUBBEL = 0

      CALL CHECKSPL(X, Y, mmax, nmax, MCS, NCS)

     5 CONTINUE
      CALL NUMS  (      X, mmax, nmax, NUMSPL,  NUMPX)
      IF (NUMSPL .LT. 4) THEN
         CALL QNERROR('You Need 4 Splines or More to Create a Grid', ' ' ,' ')
         merr = 1
         RETURN
      ELSE IF (NUMSPL .GT. IMAX) THEN
         WRITE(TEX1,'(I4)') NUMSPL
         WRITE(TEX2,'(I4)') IMAX
         CALL QNERROR('Number of Splines Larger than IMAX',TEX1,TEX2)
         CALL QNERROR('REDUCE THE NUMBER OF SPLINES',' ',' ')
         MERR = 1
         RETURN
      ENDIF

      CALL NULARR(TIJ,MMAX,NMAX)
      CALL INULAR(NTYP,IMAX)
      NTYP(1) = 1

      IF (JADUBBEL .GE. 1) THEN
!        VERDUBBEL AANTAL STEUNPUNTEN ALS
         DO 15 I = 1,NUMSPL
            CALL NUMPold(X,mmax, nmax, I,NUMPI)
            CALL GETIJ( X,     XI,mmax, nmax, imax,      I,    I,      1,  NUMPI)
            CALL GETIJ( Y,     YI,mmax, nmax, imax,      I,    I,      1,  NUMPI)
            CALL SPLINE(XI,NUMPI,XJ)
            CALL SPLINE(YI,NUMPI,YJ)
            DO 16 J = 2*NUMPI-1,2,-2
               J2     = 1 + J/2
               X(I,J) = X(I,J2)
               Y(I,J) = Y(I,J2)
    16      CONTINUE
            DO 17 J = 1,NUMPI-1
               TI = J - 0.5
               J2 = 2*J
               CALL SPLINT(XI,XJ,NUMPI,TI,X(I,J2))
               CALL SPLINT(YI,YJ,NUMPI,TI,Y(I,J2))
    17      CONTINUE
    15   CONTINUE
         CALL NUMS(      X, mmax, nmax, NUMSPL,  NUMPX)
      ENDIF

      IONBENOEMD = 0
     6 CONTINUE
      DO 10 I = 1,NUMSPL
         CALL READYY(' ', 0.01d0 + 0.3d0*dble(I-1)/dble(NUMSPL) )
         DO 10 J = I+1,NUMSPL
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            CALL GETIJ (      X,     XI,mmax, nmax, imax,     I,    I,      1, NUMPI)
            CALL GETIJ (      Y,     YI,mmax, nmax, imax,     I,    I,      1, NUMPI)
            CALL GETIJ (      X,     XJ,mmax, nmax, imax,     J,    J,      1, NUMPJ)
            CALL GETIJ (      Y,     YJ,mmax, nmax, imax,     J,    J,      1, NUMPJ)
            CALL SECT3r(     XI,     YI,     XJ,  YJ,    mmax, nmax, imax,CRP,   &
                          NUMPI,  NUMPJ, NUMCRO,  TI,      TJ, XSPc,     YSPc)
            IF (NUMCRO .EQ. 1) THEN
               IF (NTYP(I)*NTYP(J) .EQ. 1) THEN
!                 al gelijk benoemd
                  IF (NUMPX .GT. NMAX/2) THEN
                     CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                     CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                     CALL QNERROR(' ',' ', 'Spaghetty; spline both in m- and n-direction')
                     MERR = MERR + 1
                     RETURN
                  ELSE
                     JADUBBEL = JADUBBEL + 1
                     call mess(LEVEL_DEBUG, 'SPLINE SUPPORT POINTS DOUBLED')
                     GOTO 5
                  ENDIF
               ELSE IF (NTYP(I) .EQ. 0 .AND. NTYP(J) .EQ. 0) THEN
                  call mess(LEVEL_DEBUG, ' BOTH UNDEFINED YET')
               ELSE IF (NTYP(J) .EQ. 0) THEN
                  NTYP(J) = - NTYP(I)
                  IF (CRP*NTYP(I) .LT. 0) THEN
                     call mess(LEVEL_DEBUG, ' SWITCHED J')
                     CALL SWITCH(X,Y, mmax, nmax,J,NUMPJ)
                     TJ = dble(NUMPJ) - 1 - TJ
                  ENDIF
               ELSE IF (NTYP(I) .EQ. 0) THEN
                  NTYP(I) = - NTYP(J)
                  IF (CRP*NTYP(J) .GT. 0) THEN
                     call mess(LEVEL_DEBUG, ' SWITCHED I')
                     CALL SWITCH(X,Y, mmax, nmax,I,NUMPI)
                     TI = dble(NUMPI) - 1 - TI
                  ENDIF
               ENDIF
               TIJ(I,J) = TI
               TIJ(J,I) = TJ
            ELSE IF (NUMCRO .GE. 2) THEN
               IF (NUMPX .GT. NMAX/2) THEN
                  CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                  CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                  CALL QNERROR(' ',' ', '2 splines appear to intersect more than once; modify splines')
                  MERR = MERR + 1
                  RETURN
               ELSE
                  JADUBBEL = JADUBBEL + 1
                  GOTO 5
               ENDIF
            ENDIF
    10 CONTINUE

      DO 20 I = 1,NUMSPL
         CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
         IF (NTYP(I) .EQ. 0) THEN
            IONBENOEMD = IONBENOEMD + 1
!           IF (IONBENOEMD .GT. NUMSPL) THEN
            IF (IONBENOEMD .GT. 1000) THEN
               CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
               CALL QNERROR(' ',' ', 'ONE OF THE SPLINES CANNOT BE ATTACHED IN THE GRID')
                MERR = MERR + 1
                RETURN
            ENDIF
            GOTO 6
         ENDIF
    20 CONTINUE

!     sorteren op type, eerst de horizontalen (N = CONSTANT)
      DO 40 I = 1,NUMSPL
         IF (NTYP(I) .EQ. -1) THEN
            DO 50 L = I+1,NUMSPL
               IF (NTYP(L) .EQ. 1) THEN
                  CALL CHAROW(    X  ,mmax, nmax,    I,    L, NUMPX)
                  CALL CHAROW(    Y  ,mmax, nmax,    I,    L, NUMPX)
                  CALL CHAROW(  TIJ  ,mmax, nmax,    I,    L, NUMSPL)
                  CALL CHACOL(  TIJ  ,mmax, nmax,    I,    L, NUMSPL)
                  NTYP(I) =  1
                  NTYP(L) = -1
                  GOTO 40
               ENDIF
    50      CONTINUE
         ENDIF
    40 CONTINUE

      DO 45 I = 1,NUMSPL
         IF (NTYP(I) .EQ. 1) NUMI = I
    45 CONTINUE

    59 CONTINUE
!     Sorteer de M
      JACHANGE = 0
      ICOUNT   = 0
      DO 60 I = 1,NUMI
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
         DO 60 J = NUMI+1,NUMSPL
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            IF (TIJ(I,J) .NE. 0) THEN
               DO 70 JK = J+1,NUMSPL
                  IF (TIJ(I,JK) .NE. 0) THEN
                     IF (TIJ(I,J) .GT. TIJ(I,JK) ) THEN
                        CALL CHAROW(    X,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(    Y,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        CALL CHACOL(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        JACHANGE = 1
                        ICOUNT   = ICOUNT + 1
                        IF (ICOUNT .GT. NUMSPL) THEN
                           CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                           CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                           CALL QNERROR(' ',' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                           MERR = MERR + 1
                        ENDIF
                        GOTO 59
                     ENDIF
                  ENDIF
    70         CONTINUE
            ENDIF
    60 CONTINUE

    79 CONTINUE
      ICOUNT = 0
!     Sorteer de N
      DO 80 I = NUMI+1,NUMSPL
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
         DO 80 J = 1,NUMI
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            IF (TIJ(I,J) .NE. 0) THEN
               DO 90 JK = J+1,NUMI
                  IF (TIJ(I,JK) .NE. 0) THEN
                     IF (TIJ(I,J) .GT. TIJ(I,JK) ) THEN
                        CALL CHAROW(    X,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(    Y,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        CALL CHACOL(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        JACHANGE = 1
                        ICOUNT   = ICOUNT + 1
                        IF (ICOUNT .GT. NUMSPL) THEN
                           CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                           CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                           CALL QNERROR(' ',' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                           MERR = MERR + 1
                        ENDIF
                        GOTO 79
                     ENDIF
                  ENDIF
    90         CONTINUE
            ENDIF
    80 CONTINUE
      IF (JACHANGE .EQ. 1) GOTO 59


!     Initialiseer ranking, start en eind, 1,2,3
      DO 100 I = 1,NUMSPL
         MN12(I,1) = 0
         MN12(I,2) = 0
         MN12(I,3) = 0
   100 CONTINUE

!     CALL SHOWADM(TIJ,MMAX,NMAX)

!     Eerst alles ranken in N richting
      DO 110 I  = 1,NUMI
         DO 120 J = NUMI+1, NUMSPL
            MAXN   = 0
            JJLAST = 1
            DO 130 JJ = 1,I
               IF (TIJ(J,JJ) .NE. 0) THEN
                  MAXN   = MN12(JJLAST,1) + 1
                  JJLAST = JJ
               ENDIF
   130      CONTINUE
            MN12(J,2) = MAXN
   120   CONTINUE
         MAXN = 0
         DO 140 J = NUMI+1,NUMSPL
            IF (TIJ(J,I) .NE. 0) MAXN = MAX(MN12(J,2),MAXN)
   140   CONTINUE
         MN12(I,1) = MAXN
   110 CONTINUE

!     Dan alles ranken in M richting
      DO 210 I  = NUMI+1,NUMSPL
         DO 220 J = 1, NUMI
            MAXM   = 0
            IILAST = NUMI+1
            DO 230 II = NUMI+1,I
               IF (TIJ(J,II) .NE. 0) THEN
                  MAXM   = MN12(IILAST,1) + 1
                  IILAST = II
               ENDIF
   230      CONTINUE
            MN12(J,3) = MAXM
   220   CONTINUE
         MAXM = 0
         DO 240 J = 1,NUMI
            IF (TIJ(J,I) .NE. 0) MAXM = MAX(MN12(J,3),MAXM)
   240   CONTINUE
         MN12(I,1) = MAXM
   210 CONTINUE

      DO 250 I = 1,NUMSPL
         MN12(I,2) = 0
         MN12(I,3) = 0
   250 CONTINUE

!     Daarna per spline begin- en eindpunt tellen, eerst N = constant
      DO 300 I = 1,NUMI
         DO 300 J = NUMI+1,NUMSPL
            IF (TIJ(I,J) .NE. 0) THEN
               IF (MN12(I,2) .EQ. 0) MN12(I,2) = MN12(J,1)
               MN12(I,3) = MN12(J,1)
            ENDIF
   300 CONTINUE

!     Dan M = constant
      DO 310 I = NUMI+1,NUMSPL
         DO 310 J = 1,NUMI
            IF (TIJ(I,J) .NE. 0) THEN
               IF (MN12(I,2) .EQ. 0) MN12(I,2) = MN12(J,1)
               MN12(I,3) = MN12(J,1)
            ENDIF
   310 CONTINUE
      CALL READYY(' ',0.95d0)

      DO 400 I = 1,NUMSPL
         WRITE(msgbuf,*) I, (MN12(I,J), J = 1,3)
         call dbg_flush()
   400 CONTINUE

      RETURN
      END subroutine sectr
