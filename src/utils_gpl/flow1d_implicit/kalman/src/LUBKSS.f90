SUBROUTINE LUBKSS (A, N, NP, INDX, B)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             LUBKSS (LU decomp. BacKSubst. Single Prec.)
!
! Module description: Solve matrix.
!
!                     The 'water levels' in the nodes result from the
!                     solution of the Nodal Equations. In subroutine
!                     LUBKSS (library routine) this equation is solved
!                     by back substitution in the LU-decomposition.
!                     LUBKSS is the single precision version of routine
!                     LUBKSB.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 a(..,..)          I  Matrix with coefficients
!  5 b(n)              IO Right hand side
!  4 indx(nnode)       I  Contains row permutations (LU decomp.).
!                         odd (-1).
!  2 n                 I  -
!  3 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: lubkss.pf,v $
! Revision 1.2  1996/04/12  13:05:39  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:13  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   INTEGER N, NP, INDX(N)
   REAL A(NP,NP), B(N)
!
!     Declaration of local variables
!
   INTEGER I, II, J, LL
   REAL SUM
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
      ELSE IF ( SUM .NE. 0.0 ) THEN
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
