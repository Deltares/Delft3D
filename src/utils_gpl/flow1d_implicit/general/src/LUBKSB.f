      SUBROUTINE LUBKSB (A, N, NP, INDX, B)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             LUBKSB (LU decomposition; BacKSuBstitution)
c
c Module description: Solve matrix.
c
c                     The water levels in the nodes result from the
c                     solution of the Nodal Equation. In subroutine
c                     LUBKSB (library routine) this equation is solved
c                     by back substitution in the LU-decomposition. If
c                     the library routine incorporates (partial) pivo-
c                     ting, this option will be switched 'off', as pivo-
c                     ting will destroy the minimal band width of the
c                     Nodal administration matrix.
c
c-----------------------------------------------------------------------
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: lubksb.pf,v $
c Revision 1.3  1995/09/22  10:03:02  kuipe_j
c variable dimensions, new headers
c
c Revision 1.2  1995/05/30  07:02:27  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:31  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      INTEGER N, NP, INDX(N)
      DOUBLE PRECISION A(NP,NP), B(N)
c
c     Declaration of local variables
c
      INTEGER I, II, J, LL
      DOUBLE PRECISION SUM
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
         ELSE IF ( SUM .NE. 0.D0 ) THEN
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
