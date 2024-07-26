      SUBROUTINE MATRAN (NP     ,MATRIX )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             MATRAN (MAtrix TRANsposition        )
c
c Module description:                              T
c                     Transposition of a matrix [ A = A]
c                     Store result in original matrix.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 matrix            IO -
c  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: matran.pf,v $
c Revision 1.3  1999/03/15  15:52:32  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:43  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:16  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
C
      INTEGER NP
      REAL    MATRIX(NP,NP)
C
      INTEGER I, J
      REAL    HULP
C
      DO 20 J=1,NP
         DO 10 I=1,J-1
            HULP = MATRIX(J,I)
            MATRIX(J,I) = MATRIX(I,J)
            MATRIX(I,J) = HULP
   10    CONTINUE
   20 CONTINUE
C
      RETURN
      END
