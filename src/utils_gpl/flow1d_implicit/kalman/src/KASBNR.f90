subroutine KASBNR(nnode  ,nbran  ,branch ,ngrid  ,&
&kbeta  ,v1     ,v2     ,rhs    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASBNR (KALman Substitute BouNdary in Right h.s.)
!
! Module description: The right hand side of the nodal administration
!                     system must be filled for a particular column of
!                     the P-matrix.  Use arrays v1,v2 and KBETA.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 kbeta(2,nbran)    I  (1,i) contains coefficient Beta at begin of
!                         branch i.
!                         (2,i) contains coefficient Beta at end of branch
!                         i.
!                         Beta is used to calcualte the right hind side od
!                         the nodal administation matrix.
!  2 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  8 rhs(nnode)        IO (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
!  6 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
!                         equations. One value per grid point.
!  7 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
!                         tions. One value per grid point.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kasbnr.pf,v $
! Revision 1.3  1999/03/15  15:52:17  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:27  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:00  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode, ngrid, nbran
!
   integer          ibr, i1, i2, n1, n2
   integer          branch(4,nbran)
   double precision rhs(nnode), kbeta(2,nbran)
   double precision v1(ngrid), v2(ngrid)
!
!     Loop over branches
!
   do 40 ibr = 1, nbran
!
!        n1 = node number at begin of branch
!        n2 = node number at end of branch
!
      n1 = branch (1,ibr)
      n2 = branch (2,ibr)
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      rhs(n1) = rhs(n1) - kbeta(1,ibr) * v1(i1)
      rhs(n2) = rhs(n2) - kbeta(2,ibr) * v2(i2-1)
40 continue
end
