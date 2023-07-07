C----------------------------------------------------------------------C
C                                                                      C
C     SUBROUTINE INVERM                                                C
C                                                                      C
C     SOLVES SETS OF LINEAR EQUATIONS BY LU-DECOMPOSITION OF           C
C     THE COEFFICIENT -  MATRIX, PARTIAL PIVOTING ON ROWS IS           C
C     APPLIED ON THE MATRIX                                            C
C                                                                      C
C     AUTHOR : J.A.G. van Gils                                         C
C              van Renswoudestraat 2                                   C
C              2612 HX Delft                                           C
C                                                                      C
C     VARIABLES:                                                       C
C                                                                      C
C     A          SQUARE COEFFICIENT MATRIX  (COLUMN,ROW)               C
C                                         = (UNKNOWN,EQUATION)         C
C     B          RIGHTHANDSIDE (RHS) -VECTOR MATRIX (ROW,SET)          C
C                                         = (EQUATION,NR. OF RHS)      C
C     N          ORDER OF MATRIX A, NUMBER OF UNKNOWNS/EQUATIONS       C
C     M          NUMBER OF RHS-VECTORS                                 C
C     NMAX       RANGE OF FIRST INDEX IN A AND B MATRICES              C
C                (CONCERNS FORTRAN DECLARATION (NMAX,*)                C
C     IH         INTEGER WORK-ARRAY, LENGTH N                          C
C     WORK       REAL    WORK-ARRAY, LENGTH N                          C
C     IER        ERROR SWITCH =  0 : NO ERRORS DETECTED                C
C                             = -1 : SINGULARITY IN MATRIX A DETECTED  C
C                                                                      C
C----------------------------------------------------------------------C
      SUBROUTINE INVERM (A,B,N,M,NMAX,IH,WORK,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NMAX,*),B(NMAX,*),IH(*),WORK(*)
      IER = 0
      DO 10 IR = 1,N
   10 IH(IR) = IR
      DO 100 IK = 1,N
      DO  20 IR = IK+1,N
      IF (ABS(A(IK,IH(IR))).GT.ABS(A(IK,IH(IK)))) THEN
      IDUM = IH(IR)
      IH(IR) = IH(IK)
      IH(IK) = IDUM
      ENDIF
   20 CONTINUE
      IF (ABS(A(IK,IH(IK))).LT.1D-10) THEN
      IER = -IK
      RETURN
      ENDIF
      DO 50 IR = IK+1,N
      F = A(IK,IH(IR))/A(IK,IH(IK))
      IF (ABS(F).LT.1E-10) GOTO 50
      DO 30 J=1,M
   30 B(IH(IR),J) = B(IH(IR),J) - F*B(IH(IK),J)
      DO 40 IK2 = IK,N
   40 A(IK2,IH(IR)) = A(IK2,IH(IR)) - F*A(IK2,IH(IK))
   50 CONTINUE
  100 CONTINUE
      DO 200 IR2 = 1,N
      IR = N + 1 - IR2
      DO 120 J  = 1,M
      DO 110 IK = IR+1,N
  110 B(IH(IR),J) = B(IH(IR),J) - A(IK,IH(IR)) * B(IH(IK),J)
  120 B(IH(IR),J) = B(IH(IR),J) / A(IR,IH(IR))
  200 CONTINUE
      DO 300 J=1,M
      DO 210 I=1,N
  210 WORK(I) = B(IH(I),J)
      DO 220 I=1,N
  220 B(I,J) = WORK(I)
  300 CONTINUE
      RETURN
      END
