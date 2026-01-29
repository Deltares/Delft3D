SUBROUTINE MATRAN (NP     ,MATRIX )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             MATRAN (MAtrix TRANsposition        )
!
! Module description:                              T
!                     Transposition of a matrix [ A = A]
!                     Store result in original matrix.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 matrix            IO -
!  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: matran.pf,v $
! Revision 1.3  1999/03/15  15:52:32  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:43  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:16  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
   INTEGER NP
   REAL    MATRIX(NP,NP)
!
   INTEGER I, J
   REAL    HULP
!
   DO 20 J=1,NP
      DO 10 I=1,J-1
         HULP = MATRIX(J,I)
         MATRIX(J,I) = MATRIX(I,J)
         MATRIX(I,J) = HULP
10    CONTINUE
20 CONTINUE
!
   RETURN
END
