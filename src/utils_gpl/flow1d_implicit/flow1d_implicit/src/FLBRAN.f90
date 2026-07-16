subroutine FLBRAN(nbran  ,nnode   ,ngrid  ,&
&branch ,urelax  ,&
&rfv1   ,rfv2    ,&
&dh     ,&
&h1     ,h2      ,q1     ,q2     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLBRAN (FLow in BRANches)
!
! Module description: Subroutine FLBRAN will compute the water flow in
!                     the grid points of all branches, given the water
!                     flow in the adjacent nodes.
!
!                     In subroutine FLBRAN the double sweeped ABCDE
!                     coefficients (r1, r2, f1, f2, v1 and v2) are known
!                     (from subroutine DSWEEP). The water flow in terms
!                     of h and Q now follows directly from eq. (9-2) in
!                     S-FO-001.5KV.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  8 dh(nnode)         I  Water level increase in node.
!  9 h1(ngrid)         I  Water level in every grid point at time t(n).
! 10 h2(ngrid)         IO Water level in every grid point at time
!                         t(n+1).
!  1 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 11 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 12 q2(ngrid)         IO Discharge in every grid point at time t(n+1).
!  6 rfv1(ngrid,3)     I  Packed array for coefficients for all grid
!                         points (odd rows):
!                         (i,1) = r1(i)
!                         (i,2) = f1(i)
!                         (i,3) = v1(i)
!  7 rfv2(ngrid,3)     I  Packed array for coefficients for all grid
!                         points (even rows):
!                         (i,1) = r2(i)
!                         (i,2) = f2(i)
!                         (i,3) = v2(i)
!  5 urelax            I  Under relaxation parameter.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flbran.pf,v $
! Revision 1.5  1999/03/15  15:49:35  kuipe_j
! tabs removed
!
! Revision 1.4  1995/10/18  08:59:17  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:54:49  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:46  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:32  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer nbran, nnode, ngrid, branch(4,nbran)
   real    urelax
   double precision    h1(ngrid)       ,q1(ngrid)
   double precision    h2(ngrid)       ,q2(ngrid)
!
   double precision dh(nnode)
   double precision rfv1(ngrid,3) ,rfv2(ngrid,3)
!
!     Declaration of local variables
!
   integer i, ibr, i1, i2, n1, n2
   double precision    dhold, dhnew, dqold, dqnew
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
      dhold  = h2(i1) - h1(i1)
      h2(i1) = h1(i1) + urelax*dh(n1) + (1D0-urelax)*dhold
!
      dhold  = h2(i2) - h1(i2)
      h2(i2) = h1(i2) + urelax*dh(n2) + (1D0-urelax)*dhold
!
      do 10 i = i1, i2-2
         dhold = h2(i+1) - h1(i+1)
         dhnew = - rfv2(i,1)*dh(n1) - rfv2(i,2)*dh(n2)&
         &+ rfv2(i,3)
         h2(i+1) = h1(i+1) + urelax*dhnew + (1D0-urelax)*dhold
10    continue
!
      do 20 i = i1, i2-1
         dqold = q2(i) - q1(i)
         dqnew = - rfv1(i,1)*dh(n1) - rfv1(i,2)*dh(n2)&
         &+ rfv1(i,3)
         q2(i) = q1(i) + urelax*dqnew + (1D0-urelax)*dqold
20    continue
!
      dqold = q2(i2) - q1(i2)
      dqnew = - rfv2(i2-1,1)*dh(n1) - rfv2(i2-1,2)*dh(n2)&
      &+ rfv2(i2-1,3)
      q2(i2) = q1(i2) + urelax*dqnew + (1D0-urelax)*dqold
100 continue
end
