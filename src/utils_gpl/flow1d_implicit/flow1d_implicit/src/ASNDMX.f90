subroutine ASNDMX(bnode  ,nnode  ,ngrid  ,&
&n1     ,n2     ,i1     ,i2     ,&
&alfa   ,beta   ,gamma  ,&
&r1     ,f1     ,v1     ,&
&r2     ,f2     ,v2     ,&
&mat    ,rhs    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             ASNDMX (ASsembly NoDal administration MatriX)
!
! Module description: In subroutine ASNDMX the nodal administration
!                     matrix will be assembled.
!
!                     For each node i there are Ni adjacent nodes. For
!                     adjacent node j the node equation for interaction
!                     of nodes i and j reads:
!
!                        pi . delta hi + qi . delta hj = ri
!
!                     The coefficients pi, qi and ri follow from for-
!                     mulae (9-16) in S-FO-001.5KV.
!
!                     In the nodal administration matrix the next
!                     elements have to be assigned:
!
!                     mat (i,i) := pi   and  mat (i,j) := qi
!
!                     Notice that the main diagonal element should be
!                     accumulated, rather than merely assigned, as each
!                     adjacent node contributes to the main diagonal.
!
!                     So,
!
!                     mat (i,i) := mat(i,i) + ... and not: mat (i,i) :=
!
!                     See, for example form. (9-16) in S-FO-001.5KV,
!                     where main diagonal element 2 accounts for the
!                     combined contribution from node I and node III.
!                     Actually this matrix element is the sum of the
!                     separate contributions from both nodes.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 alfa              I  Contribution of one branch in the coefficient
!                         of dh or cs in the current node equation.
!  9 beta              I  Contribution of one branch in the coefficient
!                         of dq or c's in the current node equation.
!  1 bnode             I  Node number at begin of branch.
! 12 f1(ngrid)         I  f1-coefficients (2*i-1,N) of set of branch
!                         equations. One value per grid point.
! 15 f2(ngrid)         I  f2-coefficients (2*i,N) of set of branch equa-
!                         tions. One value per grid point.
! 10 gamma             I  Contribution of one branch in the right hand
!                         side of the current node equation.
!  6 i1                I  Index of first grid point in actual branch.
!  7 i2                I  Index of last grid point in actual branch.
! 17 mat(nnode,nnode)  IO (i,j) contains coefficient j of the equation
!                         at node i of the set of node equations.
!  4 n1                I  Node number at begin of branch.
!  5 n2                I  Node number at end of branch.
!  3 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 11 r1(ngrid)         I  r1-coefficients (2*i-1,1) of set of branch
!                         equations. One value per grid point.
! 14 r2(ngrid)         I  r2-coefficients (2*i,1) of set of branch equa-
!                         tions. One value per grid point.
! 18 rhs(nnode)        IO (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
! 13 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
!                         equations. One value per grid point.
! 16 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
!                         tions. One value per grid point.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: asndmx.pf,v $
! Revision 1.3  1999/03/15  15:51:10  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:02:17  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:21  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer          nnode, ngrid, bnode, n1, n2, i1, i2
   double precision alfa, beta, gamma
   double precision r1(ngrid), f1(ngrid), v1(ngrid)
   double precision r2(ngrid), f2(ngrid), v2(ngrid)
   double precision mat(nnode,nnode), rhs(nnode)
!
!     n1 = node number at begin of branch
!     n2 = node number at end of branch
!     i1 = grid point number at node n1
!     i2 = grid point number at node n2
!
!     Positive branch direction is FROM NODE N1 TO NODE N2
!
   if ( bnode .eq. n1 ) then
!
!        Assemble row n1 of nodal administration matrix (= node n1)
!        Notice accumulation on main diagonal
!
!        Doc: S-FO-001.5KV  Eq. 9-17
!
      mat(n1,n1) = mat(n1,n1) + alfa - beta * r1(i1)
      mat(n1,n2) = mat(n1,n2) - beta * f1(i1)
      rhs(n1)    = rhs(n1) - beta * v1(i1) + gamma
   else if ( bnode .eq. n2 ) then
!
!        Assemble row n2 of nodal administration matrix (= node n2)
!        Notice accumulation on main diagonal
!
!        Doc: S-FO-001.5KV  Eq. 9-20
!
      mat(n2,n2) = mat(n2,n2) + alfa - beta * f2(i2-1)
      mat(n2,n1) = mat(n2,n1) - beta * r2(i2-1)
      rhs(n2)    = rhs(n2) - beta * v2(i2-1) + gamma
   endif
!
end
