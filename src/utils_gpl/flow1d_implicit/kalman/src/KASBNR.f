      subroutine KASBNR(nnode  ,nbran  ,branch ,ngrid  ,
     +                  kbeta  ,v1     ,v2     ,rhs    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASBNR (KALman Substitute BouNdary in Right h.s.)
c
c Module description: The right hand side of the nodal administration
c                     system must be filled for a particular column of
c                     the P-matrix.  Use arrays v1,v2 and KBETA.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 kbeta(2,nbran)    I  (1,i) contains coefficient Beta at begin of
c                         branch i.
c                         (2,i) contains coefficient Beta at end of branch
c                         i.
c                         Beta is used to calcualte the right hind side od
c                         the nodal administation matrix.
c  2 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c  8 rhs(nnode)        IO (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c  6 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c  7 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
c                         tions. One value per grid point.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasbnr.pf,v $
c Revision 1.3  1999/03/15  15:52:17  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:27  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:00  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode, ngrid, nbran
c
      integer          ibr, i1, i2, n1, n2
      integer          branch(4,nbran)
      double precision rhs(nnode), kbeta(2,nbran)
      double precision v1(ngrid), v2(ngrid)
c
c     Loop over branches
c
      do 40 ibr = 1, nbran
c
c        n1 = node number at begin of branch
c        n2 = node number at end of branch
c
         n1 = branch (1,ibr)
         n2 = branch (2,ibr)
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
         rhs(n1) = rhs(n1) - kbeta(1,ibr) * v1(i1)
         rhs(n2) = rhs(n2) - kbeta(2,ibr) * v2(i2-1)
   40 continue
      end
