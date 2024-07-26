      subroutine KASBCO(nnode  ,node   ,nbran  ,branch ,
     +                  ngrid  ,h2     ,q2     ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  hbdpar ,qbdpar ,kbeta  ,
     +                  rfv1   ,rfv2   ,mat    ,rhs    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASBCO (KALman Substitute Boundary COefficients)
c
c Module description: Build coefficient matrix of the nodal administration
c                     system of equations.
c
c                     Store also all coefficients BETA in the coefficients
c                     array KBETA. These coefficients are used to
c                     calculate the right hand side in subroutine KASBNR.
c
c                     Fill Arrays v1 and v2 with zero's, so a dummy right
c                     hand side will be obtained that can be used by
c                     ASNDMX.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 h2                P  -
c 12 hbdpar            P  -
c 14 kbeta(2,nbran)    O  (1,i) contains coefficient Beta at begin of
c                         branch i.
c                         (2,i) contains coefficient Beta at end of branch
c                         i.
c                         Beta is used to calcualte the right hind side od
c                         the nodal administation matrix.
c 17 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
c                         at node i of the set of node equations.
c  8 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c  5 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c  2 node              P  -
c 10 ntab              P  -
c  9 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  7 q2                P  -
c 13 qbdpar            P  -
c 15 rfv1(ngrid,3)     O  Packed array for coefficients for all grid
c                         points (odd rows):
c                         (i,1) = r1(i)
c                         (i,2) = f1(i)
c                         (i,3) = v1(i)
c 16 rfv2(ngrid,3)     O  Packed array for coefficients for all grid
c                         points (even rows):
c                         (i,1) = r2(i)
c                         (i,2) = f2(i)
c                         (i,3) = v2(i)
c 18 rhs(nnode)        O  (i) contains at:
c                               input:  Right-hand-side of the equation
c                                       at node i of the set of node
c                                       equations.
c                               output: Solution in node i.
c 11 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c asndmx  ASsembly NoDal administration MatriX
c kasbnc  KALman Substitute BouNdary Coefficients
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasbco.pf,v $
c Revision 1.3  1999/03/15  15:52:14  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:25  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:57  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode, ngrid, maxtab, ntabm, nbran
      integer node(4,nnode), ntab(4,maxtab), branch(4,nbran)
      integer hbdpar(3,*), qbdpar(3,*)
      real    table(ntabm)
c
      double precision h2(ngrid), q2(ngrid)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
      double precision kbeta(2,nbran)
c
c     Declaration of local variables
c
      integer          bnode, i, ibr, i1, i2, j, n1, n2
      real             s
      double precision alfa, beta, gamma
c
c     Zeroes to nodal administration matrix mat and right hand side
c     vector rhs and
c
      do 20 i = 1, nnode
         do 10 j = 1, nnode
            mat(i,j) = 0.d0
   10    continue
         rhs(i) = 0.d0
   20 continue
c
      do 25 i = 1,ngrid
         rfv1(i,3) = 0.d0
         rfv2(i,3) = 0.d0
   25 continue
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
         s     = 1.0
c
         do 30 j = 1, 2
c
            call KASBNC (bnode  ,s      ,nnode  ,node   ,
     +                   ngrid  ,h2     ,q2     ,
     +                   maxtab ,ntabm  ,ntab   ,table  ,
     +                   hbdpar ,qbdpar ,
     +                   alfa   ,beta   ,gamma  )
            kbeta(j,ibr) = beta
c
            call ASNDMX (bnode  ,nnode  ,ngrid  ,
     +                   n1     ,n2     ,i1     ,i2     ,
     +                   alfa   ,beta   ,gamma  ,
     +                   rfv1(1,1)      ,rfv1(1,2)      ,rfv1(1,3) ,
     +                   rfv2(1,1)      ,rfv2(1,2)      ,rfv2(1,3) ,
     +                   mat    ,rhs    )
            bnode = n2
            s     = -1.0
   30    continue
   40 continue
      end
