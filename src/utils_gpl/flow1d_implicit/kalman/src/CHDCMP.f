      SUBROUTINE CHDCMP (A, N)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             CHDCMP (CHolesky DeCoMPosition)
c
c Module description: Decompose matrix.
c
c                     This routine CHDCMP produces a Cholesky decompo-
c                     sition of the matrix.
c                     Note that this is just a prototype of the module
c                     where pivoting is not considered!!
c                     Paul ten Brummelhuis   March 1, 1995
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 a(..,..)          IO Matrix with coefficients
c  2 n                 I  -
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: chdcmp.pf,v $
c Revision 1.2  1996/04/12  13:04:32  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:08  kuipe_j
c Kalman module added
c
c Revision 1.2  1996/02/14  16:32:03  kuipe_j
c
c***********************************************************************
c
      INTEGER N
      REAL    A(N,N)
c
c     Declaration of local variables
c
      INTEGER I, J, K
      REAL    SUM

      DO 19 J = 1, N
         DO 14 I = 1, J-1
            SUM = A(I,J)
            DO 13 K = 1, I-1
               SUM = SUM - A(I,K) * A(K,J)
 13         CONTINUE
            A(I,J) = SUM/A(I,I)
 14      CONTINUE
         DO 16 I = J, N
            SUM = A(I,J)
            DO 15 K = 1, J-1
               SUM = SUM - A(I,K) * A(K,J)
 15         CONTINUE
            IF (I.EQ.J) THEN
              A(I,J)= SQRT(SUM)
            ELSE
              A(I,J) = SUM/A(J,J)
            ENDIF
 16      CONTINUE
 19   CONTINUE

      RETURN
      END
