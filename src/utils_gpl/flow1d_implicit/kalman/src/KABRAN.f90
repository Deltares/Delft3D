subroutine KABRAN(nbran  ,nnode  ,ngrid  ,&
&branch ,rfv1   ,rfv2   ,&
&phnod  ,ph     ,pq     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KABRAN (KAlman in BRANches)
!
! Module description: Subroutine KABRAN computes covariances in the grid
!                     points of all branches, given the covariances in the
!                     adjacent nodes.
!
!                     In subroutine KABRAN the double sweeped ABCDE
!                     coefficients (r1, r2, f1, f2, v1 and v2) are known
!                     (from subroutine KASWPR and KASWPC).
!
!                     The covariances follow directly from eq. (9-2) in
!                     [S-FO-001], where Delta-Q can be replaced by cova-
!                     riance, related to Q and Delta-h can be replaced by
!                     covariance, related to h.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  1 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
!  8 ph                O  -
!  7 phnod             I  -
!  9 pq                O  -
!  5 rfv1(ngrid,3)     I  Packed array for coefficients for all grid
!                         points (odd rows):
!                         (i,1) = r1(i)
!                         (i,2) = f1(i)
!                         (i,3) = v1(i)
!  6 rfv2(ngrid,3)     I  Packed array for coefficients for all grid
!                         points (even rows):
!                         (i,1) = r2(i)
!                         (i,2) = f2(i)
!                         (i,3) = v2(i)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kabran.pf,v $
! Revision 1.3  1999/03/15  15:51:37  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:41  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:17  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer nbran, nnode, ngrid, branch(4,nbran)
   real    ph(ngrid), pq(ngrid)
!
   double precision phnod(nnode)
   double precision rfv1(ngrid,3) ,rfv2(ngrid,3)
!
!     Declaration of local variables
!
   integer i, ibr, i1, i2, n1, n2
!
   do 100 ibr = 1, nbran
!
!        n1 = node number at begin of branch
!        n2 = node number at end of branch
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      n1 = branch (1,ibr)
      n2 = branch (2,ibr)
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
!        Computation of H and Q in branch ibr

!        Doc: S-FO-001.5KV  Eq. 9-2
!
      ph(i1) = sngl(phnod(n1))
!
      ph(i2) = sngl(phnod(n2))
!
      do 10 i = i1, i2-2
         ph(i+1) = sngl(- rfv2(i,1)*phnod(n1) - rfv2(i,2)*phnod(n2)&
         &+ rfv2(i,3))
10    continue
!
      do 20 i = i1, i2-1
         pq(i) = sngl(- rfv1(i,1)*phnod(n1) - rfv1(i,2)*phnod(n2)&
         &+ rfv1(i,3))
20    continue
!
      pq(i2) = sngl(- rfv2(i2-1,1)*phnod(n1) - rfv2(i2-1,2)*phnod(n2)&
      &+ rfv2(i2-1,3))
100 continue
end
