      SUBROUTINE LUDCMS (A, N, NP, INDX, D, VV, KERLU)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             LUDCMS (LU DeCoMposition Single precision)
c
c Module description: Solve matrix by LU decomposition.
c
c                     The 'water levels' in the nodes result from the
c                     solution of the Nodal Equations. In subroutine
c                     LUDCMS (library routine) an LU-decomposition will
c                     be performed on the matrix containing these
c                     equations.
c                     LUDCMS is the single precision version of routine
c                     LUDCMP.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 a(..,..)          IO Matrix with coefficients
c  5 d                 IO -
c  4 indx(nnode)       O  Contains row permutations (LU decomp.).
c                         odd (-1).
c  7 kerlu             O  -
c  2 n                 I  -
c  3 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  6 vv(nnode)         IO Implicit scaling factor of rows (LU decomp.).
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: ludcms.pf,v $
c Revision 1.3  1999/03/15  15:52:30  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:40  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:14  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Parameter declaration
c
      INTEGER N, NP, INDX(N) ,KERLU
      REAL    D
      REAL    A(NP,NP), VV(NP)
c
c     Declaration of local variables
c
      INTEGER I, IMAX, J, K
      REAL    AAMAX, DUM, SUM, TINY
c
      PARAMETER (TINY=1.0E-10)
c
c     No row interchanges yet
c
      D = 1.
      KERLU = 0
c
c     Loop over rows to get the implicit scaling information in VV
c
      DO 12 I = 1, N
         AAMAX = 0.
         DO 11 J = 1, N
            IF ( ABS(A(I,J)) .GT. AAMAX ) THEN
              AAMAX = ABS(A(I,J))
            ENDIF
 11      CONTINUE
         IF ( AAMAX .EQ. 0.) THEN
           KERLU = 1
           GOTO 100
         ENDIF
         VV(I) = 1. / AAMAX
 12   CONTINUE
c
c     Loop over columns of Crout's method
c
      DO 19 J = 1, N

         DO 14 I = 1, J-1
            SUM = A(I,J)
            DO 13 K = 1, I-1
               SUM = SUM - A(I,K) * A(K,J)
 13         CONTINUE
            A(I,J) = SUM
 14      CONTINUE
c
c        Search for the largest pivot element
c
         AAMAX = 0.
         DO 16 I = J, N
            SUM = A(I,J)
            DO 15 K = 1, J-1
               SUM = SUM - A(I,K) * A(K,J)
 15         CONTINUE
            A(I,J) = SUM
c
c           Figure of merit for the pivot
c
            DUM = VV(I) * ABS(SUM)
c
c           It is better than the best so far ?
c
            IF ( DUM .GE. AAMAX ) THEN
               IMAX  = I
               AAMAX = DUM
            ENDIF
 16      CONTINUE
c
c        Do we need to interchange rows, parity and scale factor ?
c
         IF ( J .NE. IMAX ) THEN
            DO 17 K = 1, N
               DUM       = A(IMAX,K)
               A(IMAX,K) = A(J,K)
               A(J,K)    = DUM
 17         CONTINUE
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
 18         CONTINUE
         ENDIF
 19   CONTINUE
      IF ( A(N,N) .EQ. 0. ) THEN
        A(N,N) = TINY
      ENDIF
 100  CONTINUE
      RETURN
      END
