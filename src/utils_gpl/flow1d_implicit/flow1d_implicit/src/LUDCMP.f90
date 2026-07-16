SUBROUTINE SRE_LUDCMP (A, N, NP, INDX, D, VV, KERLU)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             LUDCMP (LU DeCOMposition)
!
! Module description: Solve matrix.
!
!                     The water levels in the nodes result from the
!                     solution of the Nodal Equation. In subroutine
!                     LUDCMP (library routine) this equation is solved
!                     by LU-decomposition. If the library routine incor-
!                     porates (partial) pivoting, this option will be
!                     switched 'off', as pivoting will destroy the mini-
!                     mal band width of the Nodal administration matrix.
!
!-----------------------------------------------------------------------
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: ludcmp.pf,v $
! Revision 1.5  1999/03/15  15:51:22  kuipe_j
! tabs removed
!
! Revision 1.4  1996/09/03  14:54:17  kuipe_j
! frequency time hist,etc
!
! Revision 1.3  1995/09/22  10:03:04  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:28  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:32  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameter declaration
!
   INTEGER N, NP, INDX(N) ,KERLU
   REAL    D
   DOUBLE PRECISION A(NP,NP), VV(NP)
!
!     Declaration of local variables
!
   INTEGER I, IMAX, J, K
   DOUBLE PRECISION  AAMAX, DUM, SUM, TINY
!
   PARAMETER (TINY=1.0D-20)
!
!     No row interchanges yet
!
   D = 1.
   KERLU = 0
!
!     Loop over rows to get the implicit scaling information in VV
!
   DO 12 I = 1, N
      AAMAX = 0.D0
      DO 11 J = 1, N
         IF ( ABS(A(I,J)) .GT. AAMAX ) THEN
            AAMAX = ABS(A(I,J))
         ENDIF
11    CONTINUE
      IF ( AAMAX .EQ. 0.D0) THEN
!          KERLU = 1
         KERLU = i
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
      AAMAX = 0.D0
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
         IF ( A(J,J) .EQ. 0.D0 ) THEN
            A(J,J) = TINY
         ENDIF
         DUM = 1. / A(J,J)
         DO 18 I = J+1, N
            A(I,J) = A(I,J) * DUM
18       CONTINUE
      ENDIF
19 CONTINUE
   IF ( A(N,N) .EQ. 0.D0 ) THEN
      A(N,N) = TINY
   ENDIF
100 CONTINUE
   RETURN
END
