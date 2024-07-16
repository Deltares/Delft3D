      subroutine KAINS3(np     ,p1     ,p2     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAINS3 (KAlman Initialize Next Step 3)
c
c Module description: Copy the predicted covariances from array P2 to P1,
c                     in case there is no filter step in the current
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 p1(np,np)         O  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c  3 p2(np,np)         I  Matrix with covariances of waterlevels,
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
c $Log: kains3.pf,v $
c Revision 1.2  1996/04/12  13:05:07  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:41  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer np
      real    p1(np,np), p2(np,np)
c
c     Declaration of local variable:
c
      integer i, j
c
      do 20 i = 1, np
         do 10 j = 1, np
            p1(i,j) = p2(i,j)
   10    continue
   20 continue
      end
