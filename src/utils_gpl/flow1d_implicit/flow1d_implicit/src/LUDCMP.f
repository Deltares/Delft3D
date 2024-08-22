      SUBROUTINE SRE_LUDCMP (A, N, NP, INDX, D, VV, KERLU)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             LUDCMP (LU DeCOMposition)
c
c Module description: Solve matrix.
c
c                     The water levels in the nodes result from the
c                     solution of the Nodal Equation. In subroutine
c                     LUDCMP (library routine) this equation is solved
c                     by LU-decomposition. If the library routine incor-
c                     porates (partial) pivoting, this option will be
c                     switched 'off', as pivoting will destroy the mini-
c                     mal band width of the Nodal administration matrix.
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
c $Log: ludcmp.pf,v $
c Revision 1.5  1999/03/15  15:51:22  kuipe_j
c tabs removed
c
c Revision 1.4  1996/09/03  14:54:17  kuipe_j
c frequency time hist,etc
c
c Revision 1.3  1995/09/22  10:03:04  kuipe_j
c variable dimensions, new headers
c
c Revision 1.2  1995/05/30  07:02:28  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:32  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameter declaration
c
      INTEGER N, NP, INDX(N) ,KERLU
      REAL    D
      DOUBLE PRECISION A(NP,NP), VV(NP)
c
c     Declaration of local variables
c
      INTEGER I, IMAX, J, K
      DOUBLE PRECISION  AAMAX, DUM, SUM, TINY
c
      PARAMETER (TINY=1.0D-20)
c
c     No row interchanges yet
c
      D = 1.
      KERLU = 0
c
c     Loop over rows to get the implicit scaling information in VV
c
      DO 12 I = 1, N
         AAMAX = 0.D0
         DO 11 J = 1, N
            IF ( ABS(A(I,J)) .GT. AAMAX ) THEN
              AAMAX = ABS(A(I,J))
            ENDIF
 11      CONTINUE
         IF ( AAMAX .EQ. 0.D0) THEN
C          KERLU = 1
           KERLU = i
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
         AAMAX = 0.D0
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
            IF ( A(J,J) .EQ. 0.D0 ) THEN
              A(J,J) = TINY
            ENDIF
            DUM = 1. / A(J,J)
            DO 18 I = J+1, N
               A(I,J) = A(I,J) * DUM
 18         CONTINUE
         ENDIF
 19   CONTINUE
      IF ( A(N,N) .EQ. 0.D0 ) THEN
        A(N,N) = TINY
      ENDIF
 100  CONTINUE
      RETURN
      END
