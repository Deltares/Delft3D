      SUBROUTINE MATVC2( ALPHA, X, BETA, Y,mat,nnode ,nodnod ,numnod )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         Kian Tan 
c
c Module:             MATVC2 (MATVeC routine)
c
c Module description: This MatVec routine assumes the matrix is in dense
c                     format, and uses the BLAS DGEMV.
c
c-----------------------------------------------------------------------
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: matvc2.pf,v $
c Revision 1.3  1995/10/18  08:59:43  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/09/22  10:03:08  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
*
*     This MatVec routine assumes the matrix is in dense format,
*     and uses the BLAS DGEMV.
*
*     .. Scalar Arguments ..
      integer nnode,n,k,j
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      integer nodnod(nnode,*),numnod(nnode)
      DOUBLE PRECISION   X( * ), Y( * ), mat(nnode,nnode)
*     ..
      do 20 n=1,nnode
C--       KHT: I implemented strange way of computing y = y - Ax --C
          y(n) = -y(n)
          do 10 k=1,numnod(n)
              j = nodnod(n,k)
                  y(n) = y(n) + mat(n,j) * x(j)
   10     continue
          y(n) = -y(n)
   20 continue
*
      RETURN
*
      END
