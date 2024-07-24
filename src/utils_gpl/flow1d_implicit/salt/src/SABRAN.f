      subroutine sabran(nbran  ,nnode  ,ngrid  ,branch ,rfv1 ,rfv2   ,
     &                  csan   ,csa2   ,csd2   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SABRAN (SAlt in BRANches)
c
c Module description: Computation of the concentrations (Cs and C'c) in
c                     the grid points of all branches, given the concen-
c                     trations in the adjacent nodes.
c
c                     The double sweeped A,B,D,E-etc coefficients (R1,
c                     R2, F1, F2, V1 and V2) are known (from subroutine
c                     DSWEEP). The salt concentrations in the nodes is
c                     known from the LU decomposition procedure. The
c                     salt concentrations in the branches now follow by
c                     substitution.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  8 csa2(ngrid)       IO Salt concentration in every grid point at time
c                         t(n+1).
c  7 csan(nnode)       I  (i) = salt concentration in node i.
c  9 csd2(ngrid)       O  Diffusion (c s) in every grid point at time
c                         t(n+1).
c  1 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
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
c $Log: sabran.pf,v $
c Revision 1.3  1995/10/18  09:00:13  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:05:53  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:34  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer nbran ,nnode    ,ngrid
      integer branch(4,nbran)
      real    csa2  (ngrid)   ,csd2(ngrid)
      double  precision        csan(nnode)     ,
     &                         rfv1  (ngrid,3) ,rfv2(ngrid,3)
c
c     Declaration of local variables
c
      integer             i   ,ibr, i1, i2, n1, n2
      double precision    cs1 ,cs2
c
      do 20 ibr = 1, nbran
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
c        Computation of Cs and C`s in branch IBR.
c
c        [ Doc: S-FO-001.5KV  Eq 9-2 ]
c
         cs1 = csan(n1)
         cs2 = csan(n2)
c
         do 10 i = i1, i2-1
            csa2(i+1) = sngl(- rfv2(i,1)*cs1
     &                       - rfv2(i,2)*cs2 + rfv2(i,3))
            csd2(i)   = sngl(- rfv1(i,1)*cs1
     &                       - rfv1(i,2)*cs2 + rfv1(i,3))
   10    continue
c
c        Remark: csa2(i2) contains now csd2(i2) !!!
c
         csd2(i2) = csa2(i2)
         csa2(i1) = sngl(cs1)
         csa2(i2) = sngl(cs2)
c
  20  continue
c
      end
