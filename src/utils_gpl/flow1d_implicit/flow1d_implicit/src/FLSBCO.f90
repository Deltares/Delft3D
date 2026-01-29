subroutine FLSBCO(nnode  ,node   ,nbran  ,branch ,qtyp   ,&
&ngrid  ,h1     ,h      ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&hstat  ,hbdpar ,qstat  ,qbdpar ,&
&rfv1   ,rfv2   ,mat    ,rhs    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLSBCO (FLow Substitute Boundary COnditions)
!
! Module description: Subroutine FLSBCO substitutes the boundary condi-
!                     tions to create a Nodal Administration Matrix.
!
!                     For the assembly of the nodal administration
!                     matrix and the right hand side next two type of
!                     coefficients are needed:
!
!                     -   Double sweeped ABCDE coefficients r1,f1,v1 and
!                         r2,f2,v2. (from FLDSCO)
!                     -   Boundary coefficients alfa, beta and gamma.
!
!                     On the basis of these coefficients the nodal
!                     administration matrix and right hand side will be
!                     computed in subroutine ASNDMX.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 h1                P  -
!  7 h                 P  -
! 15 hbdpar            P  -
! 14 hstat             P  -
! 20 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
!                         at node i of the set of node equations.
! 10 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
!  5 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  2 node              P  -
! 12 ntab              P  -
! 11 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  8 q1                P  -
!  9 q                 P  -
! 17 qbdpar            P  -
! 16 qstat             P  -
! 18 rfv1              P  -
! 19 rfv2              P  -
! 21 rhs(nnode)        O  (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
! 13 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! asndmx  ASsembly NoDal administration MatriX
! flbnco  FLow BouNdary COefficients
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flsbco.pf,v $
! Revision 1.5  1999/03/15  15:50:44  kuipe_j
! tabs removed
!
! Revision 1.4  1997/05/26  07:41:32  kuipe_j
! dicretization Q(H), H(Q) boundaries improved
!
! Revision 1.3  1995/05/30  09:55:27  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:27  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:07  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode, ngrid, maxtab, ntabm, nbran
   integer node(4,nnode), ntab(4,maxtab), branch(4,nbran)
   integer hbdpar(3,*), qbdpar(3,*)
   real    qtyp
   double precision    h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
   real    hstat(*), qstat(*)
   real    table(ntabm)
!
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
!
!     Declaration of local variables
!
   integer          bnode, i, ibr, ix, i1, i2, j, n1, n2
   real             s
   double precision alfa, beta, gamma
!
!     Zeroes to nodal administration matrix mat and right hand side
!     vector rhs
!
   do 20 i = 1, nnode
      do 10 j = 1, nnode
         mat(i,j) = 0.d0
10    continue
      rhs(i) = 0.d0
20 continue
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

      bnode = n1
      ix    = i1
      s     = 1.0
!
      do 30 j = 1, 2
!
         call FLBNCO (bnode  ,ix     ,s      ,nnode  ,node   ,&
         &ngrid  ,h1     ,h      ,q1     ,q      ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &hstat  ,hbdpar ,qstat  ,qbdpar ,&
         &qtyp   ,alfa   ,beta   ,gamma  )
!
         call ASNDMX (bnode  ,nnode  ,ngrid  ,&
         &n1     ,n2     ,i1     ,i2     ,&
         &alfa   ,beta   ,gamma  ,&
         &rfv1(1,1)      ,rfv1(1,2)      ,rfv1(1,3) ,&
         &rfv2(1,1)      ,rfv2(1,2)      ,rfv2(1,3) ,&
         &mat    ,rhs    )
         bnode = n2
         ix    = i2
         s     = -1.0
30    continue
40 continue
end
