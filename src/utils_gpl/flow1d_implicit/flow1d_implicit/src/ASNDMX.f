      subroutine ASNDMX(bnode  ,nnode  ,ngrid  ,
     +                  n1     ,n2     ,i1     ,i2     ,
     +                  alfa   ,beta   ,gamma  ,
     +                  r1     ,f1     ,v1     ,
     +                  r2     ,f2     ,v2     ,
     +                  mat    ,rhs    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             ASNDMX (ASsembly NoDal administration MatriX)
c
c Module description: In subroutine ASNDMX the nodal administration
c                     matrix will be assembled.
c
c                     For each node i there are Ni adjacent nodes. For
c                     adjacent node j the node equation for interaction
c                     of nodes i and j reads:
c
c                        pi . delta hi + qi . delta hj = ri
c
c                     The coefficients pi, qi and ri follow from for-
c                     mulae (9-16) in S-FO-001.5KV.
c
c                     In the nodal administration matrix the next
c                     elements have to be assigned:
c
c                     mat (i,i) := pi   and  mat (i,j) := qi
c
c                     Notice that the main diagonal element should be
c                     accumulated, rather than merely assigned, as each
c                     adjacent node contributes to the main diagonal.
c
c                     So,
c
c                     mat (i,i) := mat(i,i) + ... and not: mat (i,i) :=
c
c                     See, for example form. (9-16) in S-FO-001.5KV,
c                     where main diagonal element 2 accounts for the
c                     combined contribution from node I and node III.
c                     Actually this matrix element is the sum of the
c                     separate contributions from both nodes.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 alfa              I  Contribution of one branch in the coefficient
c                         of dh or cs in the current node equation.
c  9 beta              I  Contribution of one branch in the coefficient
c                         of dq or c's in the current node equation.
c  1 bnode             I  Node number at begin of branch.
c 12 f1(ngrid)         I  f1-coefficients (2*i-1,N) of set of branch
c                         equations. One value per grid point.
c 15 f2(ngrid)         I  f2-coefficients (2*i,N) of set of branch equa-
c                         tions. One value per grid point.
c 10 gamma             I  Contribution of one branch in the right hand
c                         side of the current node equation.
c  6 i1                I  Index of first grid point in actual branch.
c  7 i2                I  Index of last grid point in actual branch.
c 17 mat(nnode,nnode)  IO (i,j) contains coefficient j of the equation
c                         at node i of the set of node equations.
c  4 n1                I  Node number at begin of branch.
c  5 n2                I  Node number at end of branch.
c  3 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 11 r1(ngrid)         I  r1-coefficients (2*i-1,1) of set of branch
c                         equations. One value per grid point.
c 14 r2(ngrid)         I  r2-coefficients (2*i,1) of set of branch equa-
c                         tions. One value per grid point.
c 18 rhs(nnode)        IO (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c 13 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c 16 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
c                         tions. One value per grid point.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: asndmx.pf,v $
c Revision 1.3  1999/03/15  15:51:10  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:02:17  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:21  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer          nnode, ngrid, bnode, n1, n2, i1, i2
      double precision alfa, beta, gamma
      double precision r1(ngrid), f1(ngrid), v1(ngrid)
      double precision r2(ngrid), f2(ngrid), v2(ngrid)
      double precision mat(nnode,nnode), rhs(nnode)
c
c     n1 = node number at begin of branch
c     n2 = node number at end of branch
c     i1 = grid point number at node n1
c     i2 = grid point number at node n2
c
c     Positive branch direction is FROM NODE N1 TO NODE N2
c
      if ( bnode .eq. n1 ) then
c
c        Assemble row n1 of nodal administration matrix (= node n1)
c        Notice accumulation on main diagonal
c
c        Doc: S-FO-001.5KV  Eq. 9-17
c
         mat(n1,n1) = mat(n1,n1) + alfa - beta * r1(i1)
         mat(n1,n2) = mat(n1,n2) - beta * f1(i1)
         rhs(n1)    = rhs(n1) - beta * v1(i1) + gamma
      else if ( bnode .eq. n2 ) then
c
c        Assemble row n2 of nodal administration matrix (= node n2)
c        Notice accumulation on main diagonal
c
c        Doc: S-FO-001.5KV  Eq. 9-20
c
         mat(n2,n2) = mat(n2,n2) + alfa - beta * f2(i2-1)
         mat(n2,n1) = mat(n2,n1) - beta * r2(i2-1)
         rhs(n2)    = rhs(n2) - beta * v2(i2-1) + gamma
      endif
c
      end
