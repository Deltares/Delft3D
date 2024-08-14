      subroutine FLKSN2(nnode , nnn, nosdim, scnodl, scnode ,snnode ,
     +                  sclnod, rhs)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKSN2 (FLow Kalman add System Noise 2)
c
c Module description: Add mean of system noise to right hand side vector
c                     of nodal administration matrix.
c
c                     Only the mean of the system noise in nodes is added.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c  1 nnode             I  Number of nodes.
c  6 rhs(nnode)        IO (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c  5 sclnod(nnn+1)     I  sclnod(i) points to begin of group i of nodes
c                         (in array scnode) with correlated r.n. process
c                         for node equations.
c  3 scnode(scnodl)    I  Node numbers of all uncorrelated r.n.
c                         processes
c                         for node equations. The node numbers are
c                         grouped per process.
c  4 snnode(nosdim,nnn) I Mean and deviation (input) or variance
c                         output) etc. of the boundary noise for each
c                         group.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flksn2.pf,v $
c Revision 1.2  1996/04/12  13:04:06  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:40  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer nnode, nnn ,nosdim, scnodl
      integer sclnod(nnn+1), scnode(scnodl)
      real    snnode(nosdim,nnn)
      double precision rhs(nnode)
c
c     Declaration of local variables:
c
      integer ind, m, nr
c
c     Complete the r.h.s. = G * MUw and add to the r.h.s. array RHS of
c     the nodal administration matrix.
c
c     Add system noise for boundaries.
c
      do 30 m = 1, nnn
         do 20 ind = sclnod(m), sclnod(m+1)-1
            nr = scnode(ind)
            rhs(nr) = rhs(nr) + snnode(1,m)
   20    continue
   30 continue
c
      end
