SUBROUTINE CHBKSB (A, N, B)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             CHBKSB (CHolesky decomp.: BacKSuBstitution)
!
! Module description: Solve equations.
!
!                     This routine CHKKSB solves a set of equations.
!                     Note that this is just a prototype of the module!!
!                     Paul ten Brummelhuis   March 1, 1995
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 a(..,..)          I  Matrix with coefficients
!  3 b(n)              IO Right hand side
!  2 n                 I  -
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: chbksb.pf,v $
! Revision 1.2  1996/04/12  13:04:31  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:07  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   INTEGER N
   REAL    A(N,N), B(N)
!
!     Declaration of local variables
!
   INTEGER I, J
   REAL    SUM

   DO 12 I = 1, N
      SUM=B(I)
      IF (I.GT.1) THEN
         DO 11 J = 1, I-1
            SUM = SUM - A(I,J) * B(J)
11       CONTINUE
      ENDIF
      B(I)=SUM/A(I,I)
12 CONTINUE
!ptb
!ptb  In order to determine the scaled residuals from the actual residuals,
!ptb  the residual vector has to be multiplied with the inverse of L, the
!ptb  lower triangular matrix being the result of a Cholesky decomposition.
!ptb
!ptb  Therefore, loop 14 has to be ignored.
!ptb
!ptb  DO 14 I = N, 1, -1
!ptb     SUM = B(I)
!ptb     IF ( I .LT. N ) THEN
!ptb        DO 13 J = I+1, N
!ptb           SUM = SUM - A(I,J) * B(J)
!ptb 13          CONTINUE
!ptb     ENDIF
!ptb     B(I) = SUM / A(I,I)
!ptb 14    CONTINUE
   RETURN
END
