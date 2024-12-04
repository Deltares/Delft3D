SUBROUTINE MATVC1( ALPHA, X, BETA, Y,mat,nnode ,nodnod ,numnod )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         Kian Tan
!
! Module:             MATVC1 (MATVeC routine)
!
! Module description: This MatVec routine assumes the matrix is in dense
!                     format, and uses the BLAS DGEMV.
!
!-----------------------------------------------------------------------
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: matvc1.pf,v $
! Revision 1.3  1995/10/18  08:59:42  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/09/22  10:03:06  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     This MatVec routine assumes the matrix is in dense format,
!     and uses the BLAS DGEMV.
!
!     .. Scalar Arguments ..
   integer nnode,n,k,j
   DOUBLE PRECISION   ALPHA, BETA
!     ..
!     .. Array Arguments ..
   integer nodnod(nnode,*),numnod(nnode)
   DOUBLE PRECISION   X( * ), Y( * ), mat(nnode,nnode)
!     ..
   do 20 n=1,nnode
      y(n) = 0.0d0
      do 10 k=1,numnod(n)
         j = nodnod(n,k)
         y(n) = y(n) + mat(n,j) * x(j)
10    continue
20 continue
!
   RETURN
!
END
