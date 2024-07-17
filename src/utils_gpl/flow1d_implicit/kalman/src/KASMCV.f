      subroutine KASMCV(nu, np, ncol, x, branch, nbran, ngrid, matrix,
     &                  hmat)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         Paul G.J. ten Brummelhuis
c
c Module:             KASMCV (KAlman SMooting CoVariance matrix)
c
c Module description:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c    nu                I  Smoothing parameter
c  5 np                I  Number of rows of matrix)
c  5 ncol              I  Number of columnss of matrix)
c 52 x                 I  Location gridpoints
c 33 branch(4,nbran)   I
c  3 nbran             I  Number of branches in network
c  1 ngrid             I  Number of grid points in network.
c  8 matrix(*,np)      IO Matrix to be smoothed
c    hmat(np)          -  Intermediate array
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasmcv.pf,v $
c Revision 1.3  1999/03/15  15:52:18  kuipe_j
c tabs removed
c
c Revision 1.2  1998/07/06  08:20:52  kuipe_j
c Improve smoothing
c
c Revision 1.1  1996/12/05  10:00:06  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer ngrid, nbran, np, ncol
      integer branch(4,nbran)
      real    nu
      real    matrix(np,ncol)
      real    x(ngrid)
      real    hmat(np)
c
c     Declaration of local variables
c
      integer i, i1, i2, ibr, j  
c
      do 120 j=1, ncol
c
            do 115 i = 1, np
               hmat(i) = 0.0
  115       continue
c
            do 118 ibr = 1, nbran

               i1 = branch(3,ibr)
               i2 = branch(4,ibr)


               hmat(i1) = matrix(i1,j)
               hmat(ngrid+i1) = matrix(ngrid+i1,j)

               do 116 i = i1+1, i2-1

                  hmat(i) = matrix(i,j) +
     +            nu * (
     +            ((x(i)-x(i-1))/(x(i+1)-x(i-1)))* matrix(i+1,j) -
     +            1.* matrix (i,j) +
     +            ((x(i+1)-x(i))/(x(i+1)-x(i-1)))* matrix(i-1,j))

                  hmat(ngrid+i) = matrix(ngrid+i,j) +
     +            nu * (
     +            ( (x(i)-x(i-1))/(x(i+1)-x(i-1)) )*
     +            matrix(ngrid+i+1,j)
     +            - 1.* matrix (ngrid+i,j) +
     +            ((x(i+1)-x(i))/(x(i+1)-x(i-1)))*
     +            matrix(ngrid+i-1,j))

  116          continue

               do 117 i = i1+2, i2-2
                  hmat(i) = hmat(i) +
     +            0.15* nu * (
     +            ((x(i)-x(i-2))/(x(i+2)-x(i-2)))* matrix(i+2,j) -
     +            1.* matrix (i,j) +
     +            ((x(i+2)-x(i))/(x(i+2)-x(i-2)))* matrix(i-2,j))

                  hmat(ngrid+i) = hmat(ngrid+i)  +
     +            0.15* nu * (
     +            ( (x(i)-x(i-2))/(x(i+2)-x(i-2)) )*
     +            matrix(ngrid+i+2,j)
     +            - 1.* matrix (ngrid+i,j) +
     +            ((x(i+2)-x(i))/(x(i+2)-x(i-2)))*
     +            matrix(ngrid+i-2,j))

  117          continue


               hmat(i2) = matrix(i2,j)
               hmat(ngrid+i2) = matrix(ngrid+i2,j)

  118       continue

            do 119 i=1,  2*ngrid
               matrix(i,j) = hmat(i)
  119       continue

  120 continue

      end
