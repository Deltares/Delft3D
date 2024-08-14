      subroutine KARHP1 (np     ,p1     ,p2     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KARHP1 (KAlman add to Right Hand side Pq^t)
c
c Module description: The right hand side matrix for covariances
c                     B(.)Pp^t+Pq^t must be calculated. In this routine
c                     the last step of this calculation will be performed:
c                     Input: B(.)Pp^t (array P1)
c                            Pq.(array P2)
c                     Output: r.h.s (array P2)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 p1(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c  3 p2(np,np)         IO Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: karhp1.pf,v $
c Revision 1.2  1996/04/12  13:05:21  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:54  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer np
      real    p1(np,np), p2(np,np)
c
c     Declaration of local variables
c
      integer i, j
      real    t1
c
      do 20 j = 2,np
         do 10 i = 1, j-1
            t1      = p2(j,i) + p1(i,j)
            p2(j,i) = p2(i,j) + p1(j,i)
            p2(i,j) = t1
   10    continue
   20 continue
c
      do 30 i = 1, np
         p2(i,i) = p2(i,i) + p1(i,i)
   30 continue
c
      end
