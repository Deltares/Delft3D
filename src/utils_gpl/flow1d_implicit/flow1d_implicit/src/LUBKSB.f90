SUBROUTINE LUBKSB (A, N, NP, INDX, B)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             LUBKSB (LU decomposition; BacKSuBstitution)
!
! Module description: Solve matrix.
!
!                     The water levels in the nodes result from the
!                     solution of the Nodal Equation. In subroutine
!                     LUBKSB (library routine) this equation is solved
!                     by back substitution in the LU-decomposition. If
!                     the library routine incorporates (partial) pivo-
!                     ting, this option will be switched 'off', as pivo-
!                     ting will destroy the minimal band width of the
!                     Nodal administration matrix.
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
! $Log: lubksb.pf,v $
! Revision 1.3  1995/09/22  10:03:02  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:27  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:31  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   INTEGER N, NP, INDX(N)
   DOUBLE PRECISION A(NP,NP), B(N)
!
!     Declaration of local variables
!
   INTEGER I, II, J, LL
   DOUBLE PRECISION SUM
!
   II = 0
   DO 12 I = 1, N
      LL    = INDX(I)
      SUM   = B(LL)
      B(LL) = B(I)
      IF ( II .NE. 0 ) THEN
         DO 11 J = II, I-1
            SUM = SUM - A(I,J) * B(J)
11       CONTINUE
      ELSE IF ( SUM .NE. 0.D0 ) THEN
         II = I
      ENDIF
      B(I) = SUM
12 CONTINUE
   DO 14 I = N, 1, -1
      SUM = B(I)
      IF ( I .LT. N ) THEN
         DO 13 J = I+1, N
            SUM = SUM - A(I,J) * B(J)
13       CONTINUE
      ENDIF
      B(I) = SUM / A(I,I)
14 CONTINUE
   RETURN
END
