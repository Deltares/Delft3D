subroutine KASBCO(nnode  ,node   ,nbran  ,branch ,&
&ngrid  ,h2     ,q2     ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&hbdpar ,qbdpar ,kbeta  ,&
&rfv1   ,rfv2   ,mat    ,rhs    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASBCO (KALman Substitute Boundary COefficients)
!
! Module description: Build coefficient matrix of the nodal administration
!                     system of equations.
!
!                     Store also all coefficients BETA in the coefficients
!                     array KBETA. These coefficients are used to
!                     calculate the right hand side in subroutine KASBNR.
!
!                     Fill Arrays v1 and v2 with zero's, so a dummy right
!                     hand side will be obtained that can be used by
!                     ASNDMX.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 h2                P  -
! 12 hbdpar            P  -
! 14 kbeta(2,nbran)    O  (1,i) contains coefficient Beta at begin of
!                         branch i.
!                         (2,i) contains coefficient Beta at end of branch
!                         i.
!                         Beta is used to calcualte the right hind side od
!                         the nodal administation matrix.
! 17 mat(nnode,nnode)  O  (i,j) contains coefficient j of the equation
!                         at node i of the set of node equations.
!  8 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
!  5 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  2 node              P  -
! 10 ntab              P  -
!  9 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  7 q2                P  -
! 13 qbdpar            P  -
! 15 rfv1(ngrid,3)     O  Packed array for coefficients for all grid
!                         points (odd rows):
!                         (i,1) = r1(i)
!                         (i,2) = f1(i)
!                         (i,3) = v1(i)
! 16 rfv2(ngrid,3)     O  Packed array for coefficients for all grid
!                         points (even rows):
!                         (i,1) = r2(i)
!                         (i,2) = f2(i)
!                         (i,3) = v2(i)
! 18 rhs(nnode)        O  (i) contains at:
!                               input:  Right-hand-side of the equation
!                                       at node i of the set of node
!                                       equations.
!                               output: Solution in node i.
! 11 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! asndmx  ASsembly NoDal administration MatriX
! kasbnc  KALman Substitute BouNdary Coefficients
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kasbco.pf,v $
! Revision 1.3  1999/03/15  15:52:14  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:25  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:57  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nnode, ngrid, maxtab, ntabm, nbran
   integer node(4,nnode), ntab(4,maxtab), branch(4,nbran)
   integer hbdpar(3,*), qbdpar(3,*)
   real    table(ntabm)
!
   double precision h2(ngrid), q2(ngrid)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
   double precision kbeta(2,nbran)
!
!     Declaration of local variables
!
   integer          bnode, i, ibr, i1, i2, j, n1, n2
   real             s
   double precision alfa, beta, gamma
!
!     Zeroes to nodal administration matrix mat and right hand side
!     vector rhs and
!
   do 20 i = 1, nnode
      do 10 j = 1, nnode
         mat(i,j) = 0.d0
10    continue
      rhs(i) = 0.d0
20 continue
!
   do 25 i = 1,ngrid
      rfv1(i,3) = 0.d0
      rfv2(i,3) = 0.d0
25 continue
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
      s     = 1.0
!
      do 30 j = 1, 2
!
         call KASBNC (bnode  ,s      ,nnode  ,node   ,&
         &ngrid  ,h2     ,q2     ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &hbdpar ,qbdpar ,&
         &alfa   ,beta   ,gamma  )
         kbeta(j,ibr) = beta
!
         call ASNDMX (bnode  ,nnode  ,ngrid  ,&
         &n1     ,n2     ,i1     ,i2     ,&
         &alfa   ,beta   ,gamma  ,&
         &rfv1(1,1)      ,rfv1(1,2)      ,rfv1(1,3) ,&
         &rfv2(1,1)      ,rfv2(1,2)      ,rfv2(1,3) ,&
         &mat    ,rhs    )
         bnode = n2
         s     = -1.0
30    continue
40 continue
end
