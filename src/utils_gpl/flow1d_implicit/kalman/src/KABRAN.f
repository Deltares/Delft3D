      subroutine KABRAN(nbran  ,nnode  ,ngrid  ,
     +                  branch ,rfv1   ,rfv2   ,
     +                  phnod  ,ph     ,pq     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KABRAN (KAlman in BRANches)
c
c Module description: Subroutine KABRAN computes covariances in the grid
c                     points of all branches, given the covariances in the
c                     adjacent nodes.
c
c                     In subroutine KABRAN the double sweeped ABCDE
c                     coefficients (r1, r2, f1, f2, v1 and v2) are known
c                     (from subroutine KASWPR and KASWPC).
c
c                     The covariances follow directly from eq. (9-2) in
c                     [S-FO-001], where Delta-Q can be replaced by cova-
c                     riance, related to Q and Delta-h can be replaced by
c                     covariance, related to h.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  1 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c  8 ph                O  -
c  7 phnod             I  -
c  9 pq                O  -
c  5 rfv1(ngrid,3)     I  Packed array for coefficients for all grid
c                         points (odd rows):
c                         (i,1) = r1(i)
c                         (i,2) = f1(i)
c                         (i,3) = v1(i)
c  6 rfv2(ngrid,3)     I  Packed array for coefficients for all grid
c                         points (even rows):
c                         (i,1) = r2(i)
c                         (i,2) = f2(i)
c                         (i,3) = v2(i)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kabran.pf,v $
c Revision 1.3  1999/03/15  15:51:37  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:41  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:17  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer nbran, nnode, ngrid, branch(4,nbran)
      real    ph(ngrid), pq(ngrid)
c
      double precision phnod(nnode)
      double precision rfv1(ngrid,3) ,rfv2(ngrid,3)
c
c     Declaration of local variables
c
      integer i, ibr, i1, i2, n1, n2
c
      do 100 ibr = 1, nbran
c
c        n1 = node number at begin of branch
c        n2 = node number at end of branch
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         n1 = branch (1,ibr)
         n2 = branch (2,ibr)
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
c        Computation of H and Q in branch ibr

c        Doc: S-FO-001.5KV  Eq. 9-2
c
         ph(i1) = sngl(phnod(n1))
c
         ph(i2) = sngl(phnod(n2))
c
         do 10 i = i1, i2-2
            ph(i+1) = sngl(- rfv2(i,1)*phnod(n1) - rfv2(i,2)*phnod(n2)
     +                     + rfv2(i,3))
   10    continue
c
         do 20 i = i1, i2-1
            pq(i) = sngl(- rfv1(i,1)*phnod(n1) - rfv1(i,2)*phnod(n2)
     +                   + rfv1(i,3))
   20    continue
c
         pq(i2) = sngl(- rfv2(i2-1,1)*phnod(n1) - rfv2(i2-1,2)*phnod(n2)
     +                 + rfv2(i2-1,3))
 100  continue
      end
