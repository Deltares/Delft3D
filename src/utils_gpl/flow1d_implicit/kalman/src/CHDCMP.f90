SUBROUTINE CHDCMP (A, N)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             CHDCMP (CHolesky DeCoMPosition)
!
! Module description: Decompose matrix.
!
!                     This routine CHDCMP produces a Cholesky decompo-
!                     sition of the matrix.
!                     Note that this is just a prototype of the module
!                     where pivoting is not considered!!
!                     Paul ten Brummelhuis   March 1, 1995
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 a(..,..)          IO Matrix with coefficients
!  2 n                 I  -
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: chdcmp.pf,v $
! Revision 1.2  1996/04/12  13:04:32  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:08  kuipe_j
! Kalman module added
!
! Revision 1.2  1996/02/14  16:32:03  kuipe_j
!
!***********************************************************************
!
   INTEGER N
   REAL    A(N,N)
!
!     Declaration of local variables
!
   INTEGER I, J, K
   REAL    SUM

   DO 19 J = 1, N
      DO 14 I = 1, J-1
         SUM = A(I,J)
         DO 13 K = 1, I-1
            SUM = SUM - A(I,K) * A(K,J)
13       CONTINUE
         A(I,J) = SUM/A(I,I)
14    CONTINUE
      DO 16 I = J, N
         SUM = A(I,J)
         DO 15 K = 1, J-1
            SUM = SUM - A(I,K) * A(K,J)
15       CONTINUE
         IF (I.EQ.J) THEN
            A(I,J)= SQRT(SUM)
         ELSE
            A(I,J) = SUM/A(J,J)
         ENDIF
16    CONTINUE
19 CONTINUE

   RETURN
END
