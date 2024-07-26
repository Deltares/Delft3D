      subroutine FLSBCO(nnode  ,node   ,nbran  ,branch ,qtyp   ,
     +                  ngrid  ,h1     ,h      ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  hstat  ,hbdpar ,qstat  ,qbdpar ,
     +                  rfv1   ,rfv2   ,mat    ,rhs    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLSBCO (FLow Substitute Boundary COnditions)
c
c Module description: Subroutine FLSBCO substitutes the boundary condi-
c                     tions to create a Nodal Administration Matrix.
c
c                     For the assembly of the nodal administration
c                     matrix and the right hand side next two type of
c                     coefficients are needed:
c
c                     -   Double sweeped ABCDE coefficients r1,f1,v1 and
c                         r2,f2,v2. (from FLDSCO)
c                     -   Boundary coefficients alfa, beta and gamma.
c
c                     On the basis of these coefficients the nodal
c                     administration matrix and right hand side will be
c                     computed in subroutine ASNDMX.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 h1                P  -
c  7 h                 P  -
c 15 hbdpar            P  -
c 14 hstat             P  -
c 20 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
c                         at node i of the set of node equations.
c 10 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c  5 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c  2 node              P  -
c 12 ntab              P  -
c 11 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  8 q1                P  -
c  9 q                 P  -
c 17 qbdpar            P  -
c 16 qstat             P  -
c 18 rfv1              P  -
c 19 rfv2              P  -
c 21 rhs(nnode)        O  (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c 13 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c asndmx  ASsembly NoDal administration MatriX
c flbnco  FLow BouNdary COefficients
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flsbco.pf,v $
c Revision 1.5  1999/03/15  15:50:44  kuipe_j
c tabs removed
c
c Revision 1.4  1997/05/26  07:41:32  kuipe_j
c dicretization Q(H), H(Q) boundaries improved
c
c Revision 1.3  1995/05/30  09:55:27  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:27  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:07  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode, ngrid, maxtab, ntabm, nbran
      integer node(4,nnode), ntab(4,maxtab), branch(4,nbran)
      integer hbdpar(3,*), qbdpar(3,*)
      real    qtyp
      double precision    h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
      real    hstat(*), qstat(*)
      real    table(ntabm)
c
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
c
c     Declaration of local variables
c
      integer          bnode, i, ibr, ix, i1, i2, j, n1, n2
      real             s
      double precision alfa, beta, gamma
c
c     Zeroes to nodal administration matrix mat and right hand side
c     vector rhs
c
      do 20 i = 1, nnode
         do 10 j = 1, nnode
            mat(i,j) = 0.d0
   10    continue
         rhs(i) = 0.d0
   20 continue
c
c     Loop over branches
c
      do 40 ibr = 1, nbran
c
c        n1 = node number at begin of branch
c        n2 = node number at end of branch
c
         n1 = branch (1,ibr)
         n2 = branch (2,ibr)
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)

         bnode = n1
         ix    = i1
         s     = 1.0
c
         do 30 j = 1, 2
c
            call FLBNCO (bnode  ,ix     ,s      ,nnode  ,node   ,
     +                   ngrid  ,h1     ,h      ,q1     ,q      ,
     +                   maxtab ,ntabm  ,ntab   ,table  ,
     +                   hstat  ,hbdpar ,qstat  ,qbdpar ,
     +                   qtyp   ,alfa   ,beta   ,gamma  )
c
            call ASNDMX (bnode  ,nnode  ,ngrid  ,
     +                   n1     ,n2     ,i1     ,i2     ,
     +                   alfa   ,beta   ,gamma  ,
     +                   rfv1(1,1)      ,rfv1(1,2)      ,rfv1(1,3) ,
     +                   rfv2(1,1)      ,rfv2(1,2)      ,rfv2(1,3) ,
     +                   mat    ,rhs    )
            bnode = n2
            ix    = i2
            s     = -1.0
   30    continue
   40 continue
      end
