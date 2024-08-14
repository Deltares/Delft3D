      subroutine sasbco(nnode  ,nboun  ,nbran  ,ngrid ,node   ,branch,
     &                  q2     ,sbdpar ,sbdscr ,rfv1  ,rfv2   ,mat   ,
     &                  rhs    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SASBCO (SAlt Substitute Boundary Conditions)
c
c Module description: Substitution of the boundary conditions (internal
c                     and external) to create a Nodal Administration
c                     Matrix.
c
c                     For the assembly of the nodal administration ma-
c                     trix and the right hand side next two type of
c                     coefficients are needed:
c                     -   Double sweeped A,B,D,E-etc. coefficients R1,
c                         F1, V1 and R2, F2, V2. (from SADSCO)
c                     -   Boundary coefficients ALPHA, BETA and GAMMA.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 12 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
c                         at node i of the set of node equations.
c  2 nboun             I  Number of boundary nodes.
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c  5 node              P  -
c  7 q2                P  -
c 10 rfv1              P  -
c 11 rfv2              P  -
c 13 rhs(nnode)        O  (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c  8 sbdpar            P  -
c  9 sbdscr            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c asndmx  ASsembly NoDal administration MatriX
c sabnco  SAlt BouNdary COefficients
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
c $Log: sasbco.pf,v $
c Revision 1.3  1999/03/15  15:53:27  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:06:15  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:57  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:03  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode ,nboun       ,nbran   ,ngrid
      integer node(4,nnode)      ,branch(4,nbran)
      real    sbdpar(5,nboun)    ,sbdscr(3,nboun)
      double  precision
     &        q2    (ngrid)      ,
     &        mat   (nnode,nnode),rhs   (nnode)   ,
     &        rfv1  (ngrid,3)    ,rfv2  (ngrid,3)
c
c     Declaration of local variables
c
      integer bnode ,i    ,ibr  ,i1    ,i2  ,j  ,n1  ,n2  ,ix
      real    s
      double precision     alfa ,beta ,gamma
c
c     Zeroes to nodal administration matrix MAT and right hand side
c     vector RHS.
c
      do 20 i = 1, nnode
         do 10 j = 1, nnode
            mat(i,j) = 0.d0
   10    continue
         rhs(i) = 0.d0
   20 continue
c
c     Loop over branches.
c
      do 40 ibr = 1, nbran
c
c        n1 = node number at begin of branch
c        n2 = node number at end of branch
c        i1 = grid point at begin of branch
c        i2 = grid point at end of branch
c
         n1 = branch (1,ibr)
         n2 = branch (2,ibr)
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
         bnode = n1
         s     = 1.0
         ix    = i1
c
         do 30 j = 1, 2
c
            call sabnco (bnode  ,s      ,ix    ,nnode  ,nboun ,ngrid ,
     &                   node   ,q2     ,sbdpar,sbdscr ,
     &                   rfv1(1,1)      ,rfv1(1,2)     ,rfv1(1,3)    ,
     &                   rfv2(1,1)      ,rfv2(1,2)     ,rfv2(1,3)    ,
     &                   alfa   ,beta   ,gamma  )
c
            call asndmx (bnode  ,nnode  ,ngrid  ,n1    ,n2     ,i1   ,
     &                   i2     ,alfa   ,beta   ,gamma ,
     &                   rfv1(1,1)      ,rfv1(1,2)     ,rfv1(1,3)    ,
     &                   rfv2(1,1)      ,rfv2(1,2)     ,rfv2(1,3)    ,
     &                   mat    ,rhs    )
            bnode = n2
            s     = -1.0
            ix    = i2
   30    continue
   40 continue
c
      end
