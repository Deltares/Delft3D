      SUBROUTINE CHBKSB (A, N, B)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             CHBKSB (CHolesky decomp.: BacKSuBstitution)
c
c Module description: Solve equations.
c
c                     This routine CHKKSB solves a set of equations.
c                     Note that this is just a prototype of the module!!
c                     Paul ten Brummelhuis   March 1, 1995
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 a(..,..)          I  Matrix with coefficients
c  3 b(n)              IO Right hand side
c  2 n                 I  -
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: chbksb.pf,v $
c Revision 1.2  1996/04/12  13:04:31  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:07  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      INTEGER N
      REAL    A(N,N), B(N)
c
c     Declaration of local variables
c
      INTEGER I, J
      REAL    SUM

      DO 12 I = 1, N
         SUM=B(I)
         IF (I.GT.1) THEN
            DO 11 J = 1, I-1
               SUM = SUM - A(I,J) * B(J)
11          CONTINUE
         ENDIF
         B(I)=SUM/A(I,I)
12    CONTINUE
Cptb
Cptb  In order to determine the scaled residuals from the actual residuals,
Cptb  the residual vector has to be multiplied with the inverse of L, the
Cptb  lower triangular matrix being the result of a Cholesky decomposition.
Cptb
Cptb  Therefore, loop 14 has to be ignored.
Cptb
Cptb  DO 14 I = N, 1, -1
Cptb     SUM = B(I)
Cptb     IF ( I .LT. N ) THEN
Cptb        DO 13 J = I+1, N
Cptb           SUM = SUM - A(I,J) * B(J)
Cptb 13          CONTINUE
Cptb     ENDIF
Cptb     B(I) = SUM / A(I,I)
Cptb 14    CONTINUE
      RETURN
      END
