subroutine sabran(nbran  ,nnode  ,ngrid  ,branch ,rfv1 ,rfv2   ,&
&csan   ,csa2   ,csd2   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SABRAN (SAlt in BRANches)
!
! Module description: Computation of the concentrations (Cs and C'c) in
!                     the grid points of all branches, given the concen-
!                     trations in the adjacent nodes.
!
!                     The double sweeped A,B,D,E-etc coefficients (R1,
!                     R2, F1, F2, V1 and V2) are known (from subroutine
!                     DSWEEP). The salt concentrations in the nodes is
!                     known from the LU decomposition procedure. The
!                     salt concentrations in the branches now follow by
!                     substitution.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  8 csa2(ngrid)       IO Salt concentration in every grid point at time
!                         t(n+1).
!  7 csan(nnode)       I  (i) = salt concentration in node i.
!  9 csd2(ngrid)       O  Diffusion (c s) in every grid point at time
!                         t(n+1).
!  1 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
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
! $Log: sabran.pf,v $
! Revision 1.3  1995/10/18  09:00:13  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:05:53  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:34  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer nbran ,nnode    ,ngrid
   integer branch(4,nbran)
   real    csa2  (ngrid)   ,csd2(ngrid)
   double  precision        csan(nnode)     ,&
   &rfv1  (ngrid,3) ,rfv2(ngrid,3)
!
!     Declaration of local variables
!
   integer             i   ,ibr, i1, i2, n1, n2
   double precision    cs1 ,cs2
!
   do 20 ibr = 1, nbran
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
!        Computation of Cs and C`s in branch IBR.
!
!        [ Doc: S-FO-001.5KV  Eq 9-2 ]
!
      cs1 = csan(n1)
      cs2 = csan(n2)
!
      do 10 i = i1, i2-1
         csa2(i+1) = sngl(- rfv2(i,1)*cs1&
         &- rfv2(i,2)*cs2 + rfv2(i,3))
         csd2(i)   = sngl(- rfv1(i,1)*cs1&
         &- rfv1(i,2)*cs2 + rfv1(i,3))
10    continue
!
!        Remark: csa2(i2) contains now csd2(i2) !!!
!
      csd2(i2) = csa2(i2)
      csa2(i1) = sngl(cs1)
      csa2(i2) = sngl(cs2)
!
20 continue
!
end
