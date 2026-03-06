subroutine FLKSN2(nnode , nnn, nosdim, scnodl, scnode ,snnode ,&
&sclnod, rhs)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKSN2 (FLow Kalman add System Noise 2)
!
! Module description: Add mean of system noise to right hand side vector
!                     of nodal administration matrix.
!
!                     Only the mean of the system noise in nodes is added.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
!  1 nnode             I  Number of nodes.
!  6 rhs(nnode)        IO (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
!  5 sclnod(nnn+1)     I  sclnod(i) points to begin of group i of nodes
!                         (in array scnode) with correlated r.n. process
!                         for node equations.
!  3 scnode(scnodl)    I  Node numbers of all uncorrelated r.n.
!                         processes
!                         for node equations. The node numbers are
!                         grouped per process.
!  4 snnode(nosdim,nnn) I Mean and deviation (input) or variance
!                         output) etc. of the boundary noise for each
!                         group.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flksn2.pf,v $
! Revision 1.2  1996/04/12  13:04:06  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:40  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer nnode, nnn ,nosdim, scnodl
   integer sclnod(nnn+1), scnode(scnodl)
   real    snnode(nosdim,nnn)
   double precision rhs(nnode)
!
!     Declaration of local variables:
!
   integer ind, m, nr
!
!     Complete the r.h.s. = G * MUw and add to the r.h.s. array RHS of
!     the nodal administration matrix.
!
!     Add system noise for boundaries.
!
   do 30 m = 1, nnn
      do 20 ind = sclnod(m), sclnod(m+1)-1
         nr = scnode(ind)
         rhs(nr) = rhs(nr) + snnode(1,m)
20    continue
30 continue
!
end
