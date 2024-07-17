      SUBROUTINE LUBKSS (A, N, NP, INDX, B)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             LUBKSS (LU decomp. BacKSubst. Single Prec.)
c
c Module description: Solve matrix.
c
c                     The 'water levels' in the nodes result from the
c                     solution of the Nodal Equations. In subroutine
c                     LUBKSS (library routine) this equation is solved
c                     by back substitution in the LU-decomposition.
c                     LUBKSS is the single precision version of routine
c                     LUBKSB.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 a(..,..)          I  Matrix with coefficients
c  5 b(n)              IO Right hand side
c  4 indx(nnode)       I  Contains row permutations (LU decomp.).
c                         odd (-1).
c  2 n                 I  -
c  3 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: lubkss.pf,v $
c Revision 1.2  1996/04/12  13:05:39  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:13  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      INTEGER N, NP, INDX(N)
      REAL A(NP,NP), B(N)
c
c     Declaration of local variables
c
      INTEGER I, II, J, LL
      REAL SUM
c
      II = 0
      DO 12 I = 1, N
         LL    = INDX(I)
         SUM   = B(LL)
         B(LL) = B(I)
         IF ( II .NE. 0 ) THEN
            DO 11 J = II, I-1
               SUM = SUM - A(I,J) * B(J)
11          CONTINUE
         ELSE IF ( SUM .NE. 0.0 ) THEN
            II = I
         ENDIF
         B(I) = SUM
12    CONTINUE
      DO 14 I = N, 1, -1
         SUM = B(I)
         IF ( I .LT. N ) THEN
            DO 13 J = I+1, N
               SUM = SUM - A(I,J) * B(J)
13          CONTINUE
         ENDIF
         B(I) = SUM / A(I,I)
14    CONTINUE
      RETURN
      END
