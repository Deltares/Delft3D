SUBROUTINE LUDCMS (A, N, NP, INDX, D, VV, KERLU)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             LUDCMS (LU DeCoMposition Single precision)
!
! Module description: Solve matrix by LU decomposition.
!
!                     The 'water levels' in the nodes result from the
!                     solution of the Nodal Equations. In subroutine
!                     LUDCMS (library routine) an LU-decomposition will
!                     be performed on the matrix containing these
!                     equations.
!                     LUDCMS is the single precision version of routine
!                     LUDCMP.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 a(..,..)          IO Matrix with coefficients
!  5 d                 IO -
!  4 indx(nnode)       O  Contains row permutations (LU decomp.).
!                         odd (-1).
!  7 kerlu             O  -
!  2 n                 I  -
!  3 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  6 vv(nnode)         IO Implicit scaling factor of rows (LU decomp.).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: ludcms.pf,v $
! Revision 1.3  1999/03/15  15:52:30  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:40  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:14  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Parameter declaration
!
   INTEGER N, NP, INDX(N) ,KERLU
   REAL    D
   REAL    A(NP,NP), VV(NP)
!
!     Declaration of local variables
!
   INTEGER I, IMAX, J, K
   REAL    AAMAX, DUM, SUM, TINY
!
   PARAMETER (TINY=1.0E-10)
!
!     No row interchanges yet
!
   D = 1.
   KERLU = 0
!
!     Loop over rows to get the implicit scaling information in VV
!
   DO 12 I = 1, N
      AAMAX = 0.
      DO 11 J = 1, N
         IF ( ABS(A(I,J)) .GT. AAMAX ) THEN
            AAMAX = ABS(A(I,J))
         ENDIF
11    CONTINUE
      IF ( AAMAX .EQ. 0.) THEN
         KERLU = 1
         GOTO 100
      ENDIF
      VV(I) = 1. / AAMAX
12 CONTINUE
!
!     Loop over columns of Crout's method
!
   DO 19 J = 1, N

      DO 14 I = 1, J-1
         SUM = A(I,J)
         DO 13 K = 1, I-1
            SUM = SUM - A(I,K) * A(K,J)
13       CONTINUE
         A(I,J) = SUM
14    CONTINUE
!
!        Search for the largest pivot element
!
      AAMAX = 0.
      DO 16 I = J, N
         SUM = A(I,J)
         DO 15 K = 1, J-1
            SUM = SUM - A(I,K) * A(K,J)
15       CONTINUE
         A(I,J) = SUM
!
!           Figure of merit for the pivot
!
         DUM = VV(I) * ABS(SUM)
!
!           It is better than the best so far ?
!
         IF ( DUM .GE. AAMAX ) THEN
            IMAX  = I
            AAMAX = DUM
         ENDIF
16    CONTINUE
!
!        Do we need to interchange rows, parity and scale factor ?
!
      IF ( J .NE. IMAX ) THEN
         DO 17 K = 1, N
            DUM       = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K)    = DUM
17       CONTINUE
         D = -D
         VV(IMAX) = VV(J)
      ENDIF
      INDX(J) = IMAX
      IF ( J .NE. N ) THEN
         IF ( A(J,J) .EQ. 0. ) THEN
            A(J,J) = TINY
         ENDIF
         DUM = 1. / A(J,J)
         DO 18 I = J+1, N
            A(I,J) = A(I,J) * DUM
18       CONTINUE
      ENDIF
19 CONTINUE
   IF ( A(N,N) .EQ. 0. ) THEN
      A(N,N) = TINY
   ENDIF
100 CONTINUE
   RETURN
END
