      subroutine KACVSM (np     ,matrix )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KACVSM (KALman make CoVariance SyMmetric)
c
c Module description: Make covariance matrix symmetric.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 matrix            IO -
c  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kacvsm.pf,v $
c Revision 1.3  1999/03/15  15:51:38  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:42  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:18  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  np
      real     matrix(np,np)
c
c     Declaration of local variables
c
      integer  i, j
c
c     Make covariance matrix synmetric.
c     [ Doc. S-FO-004.2PB / Eq. 2-18 ]
c
      do 20 i = 2, np
         do 10 j = 1, i-1
            matrix(i,j) = (matrix(i,j) + matrix(j,i)) / 2.
            matrix(j,i) = matrix(i,j)
   10    continue
   20 continue
c
      end
