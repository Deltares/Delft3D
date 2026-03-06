subroutine sasbco(nnode  ,nboun  ,nbran  ,ngrid ,node   ,branch,&
&q2     ,sbdpar ,sbdscr ,rfv1  ,rfv2   ,mat   ,&
&rhs    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SASBCO (SAlt Substitute Boundary Conditions)
!
! Module description: Substitution of the boundary conditions (internal
!                     and external) to create a Nodal Administration
!                     Matrix.
!
!                     For the assembly of the nodal administration ma-
!                     trix and the right hand side next two type of
!                     coefficients are needed:
!                     -   Double sweeped A,B,D,E-etc. coefficients R1,
!                         F1, V1 and R2, F2, V2. (from SADSCO)
!                     -   Boundary coefficients ALPHA, BETA and GAMMA.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 12 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
!                         at node i of the set of node equations.
!  2 nboun             I  Number of boundary nodes.
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  5 node              P  -
!  7 q2                P  -
! 10 rfv1              P  -
! 11 rfv2              P  -
! 13 rhs(nnode)        O  (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
!  8 sbdpar            P  -
!  9 sbdscr            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! asndmx  ASsembly NoDal administration MatriX
! sabnco  SAlt BouNdary COefficients
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
! $Log: sasbco.pf,v $
! Revision 1.3  1999/03/15  15:53:27  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:06:15  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:57  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:03  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode ,nboun       ,nbran   ,ngrid
   integer node(4,nnode)      ,branch(4,nbran)
   real    sbdpar(5,nboun)    ,sbdscr(3,nboun)
   double  precision&
   &q2    (ngrid)      ,&
   &mat   (nnode,nnode),rhs   (nnode)   ,&
   &rfv1  (ngrid,3)    ,rfv2  (ngrid,3)
!
!     Declaration of local variables
!
   integer bnode ,i    ,ibr  ,i1    ,i2  ,j  ,n1  ,n2  ,ix
   real    s
   double precision     alfa ,beta ,gamma
!
!     Zeroes to nodal administration matrix MAT and right hand side
!     vector RHS.
!
   do 20 i = 1, nnode
      do 10 j = 1, nnode
         mat(i,j) = 0.d0
10    continue
      rhs(i) = 0.d0
20 continue
!
!     Loop over branches.
!
   do 40 ibr = 1, nbran
!
!        n1 = node number at begin of branch
!        n2 = node number at end of branch
!        i1 = grid point at begin of branch
!        i2 = grid point at end of branch
!
      n1 = branch (1,ibr)
      n2 = branch (2,ibr)
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      bnode = n1
      s     = 1.0
      ix    = i1
!
      do 30 j = 1, 2
!
         call sabnco (bnode  ,s      ,ix    ,nnode  ,nboun ,ngrid ,&
         &node   ,q2     ,sbdpar,sbdscr ,&
         &rfv1(1,1)      ,rfv1(1,2)     ,rfv1(1,3)    ,&
         &rfv2(1,1)      ,rfv2(1,2)     ,rfv2(1,3)    ,&
         &alfa   ,beta   ,gamma  )
!
         call asndmx (bnode  ,nnode  ,ngrid  ,n1    ,n2     ,i1   ,&
         &i2     ,alfa   ,beta   ,gamma ,&
         &rfv1(1,1)      ,rfv1(1,2)     ,rfv1(1,3)    ,&
         &rfv2(1,1)      ,rfv2(1,2)     ,rfv2(1,3)    ,&
         &mat    ,rhs    )
         bnode = n2
         s     = -1.0
         ix    = i2
30    continue
40 continue
!
end
