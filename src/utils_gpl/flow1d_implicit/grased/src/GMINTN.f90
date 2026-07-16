subroutine GMINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,&
&ntabm  ,nfrac  ,time   ,dtm    ,alphac,alphad ,&
&alphae ,branch ,brnode ,bgout  ,itot  ,stot   ,&
&node   ,mbdpar ,ntab   ,table  ,disgse,intgr  ,&
&sedtr  ,celer  ,celert ,source ,dfrac ,ds     ,&
&spredc ,cela1  ,flwdir ,x      ,juer  ,jugralg,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             GMINTN (Graded Morphology INTegrals for Nodes)
!
! Module description: Calculate integrals for nodes and boundaries
!
!                     The integral for an outflowing branch (from node
!                     to branch) will be a fraction of the total inte-
!                     grals flowing to the node. Therefore first for all
!                     nodes the inflowing and outflowing branches will
!                     be determined and stored in a separate structure.
!                     If the node contains only one branch (boundary),
!                     routine MOIBOU will be called to calculate either
!                     the boundary condition or the outflowing integral
!                     value.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 alphac            P  -
! 14 bgout             P  -
! 11 branch            P  -
! 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 23 celer             P  -
! 19 disgse            P  -
!  9 dtm               P  -
! 15 grid              P  -
! 20 intgr             P  -
! 25 juer              P  -
! 26 ker               P  -
!  6 maxtab            I  Maximum number of defined tables.
! 17 mbdpar            P  -
!  5 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
! 16 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 18 ntab              P  -
!  7 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 24 sedtr             P  -
! 22 table             P  -
!  8 time              P  -
! 13 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 21 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moibou  MOrphology Integral BOUndary
! moinod  MOrphology Integral for a NODe
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmintn.F,v $
! Revision 1.2  1995/09/27  10:11:39  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nnode  ,nbran  ,nbrnod ,ngrid  ,nboun  ,maxtab ,&
   &ntabm  ,nfrac  ,juer   ,jugralg,ker
   integer    branch (4,nbran)       ,&
   &brnode (nbrnod+1,nnode),&
   &bgout  (3,nbrnod)      ,&
   &node   (4,nnode)       ,&
   &mbdpar (5,nboun)       ,&
   &flwdir (ngrid)         ,&
   &ntab   (4,maxtab)
   real       alphac ,alphad  ,alphae
   real       disgse (nfrac,2,nbran) ,intgr  (nfrac,2,nbran) ,&
   &table  (ntabm)         ,&
   &celer  (ngrid,nfrac,5) ,sedtr (ngrid,nfrac+2),&
   &source (ngrid,nfrac+2) ,celert(ngrid)        ,&
   &x      (ngrid)         ,cela1 (nfrac,nfrac)  ,&
   &ds     (nfrac)         ,spredc(nfrac)        ,&
   &dfrac  (nfrac)
   double precision  time   ,dtm

!
!     Declaration of local parameters
!
   integer    inode        ,ibr         , igp
   real       itot(nfrac)  ,stot(nfrac)
!
!     Include sobek constants
!
!     Do for each node
!
   do 10 inode = 1, nnode
!
!        Test number of connected branches
!
      if (brnode(1,inode) .gt. 1) then
!
!           Internal node : call GMINOD
!
         call GMINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,nfrac  ,&
         &dtm    ,alphac ,alphad ,alphae ,branch,brnode ,&
         &bgout  ,disgse ,intgr  ,x      ,celer ,celert ,&
         &source ,dfrac  ,ds     ,spredc ,cela1 ,flwdir ,&
         &sedtr  ,itot   ,stot   ,jugralg)
!
      else
!
!           Boundary      : call GMIBOU
!
         ibr  = brnode(2,inode)
         igp  = node  (2,inode)
!
!           Call processing routine
!
         call GMIBOU ( ibr    ,igp    ,juer   ,time   ,dtm    ,&
         &ngrid  ,nbran  ,nboun  ,nnode  ,nfrac  ,&
         &maxtab ,ntabm  ,branch ,node   ,mbdpar ,&
         &ntab   ,table  ,alphac ,alphad ,alphae ,&
         &x      ,celer  ,celert ,sedtr  ,disgse ,&
         &source ,dfrac  ,ds     ,spredc ,cela1  ,&
         &flwdir ,intgr(1,1,ibr) ,jugralg,ker    )
      endif
!
10 continue
!
end
