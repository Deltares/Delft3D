      subroutine GMINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,
     +                   ntabm  ,nfrac  ,time   ,dtm    ,alphac,alphad ,
     +                   alphae ,branch ,brnode ,bgout  ,itot  ,stot   ,
     +                   node   ,mbdpar ,ntab   ,table  ,disgse,intgr  ,
     +                   sedtr  ,celer  ,celert ,source ,dfrac ,ds     ,
     +                   spredc ,cela1  ,flwdir ,x      ,juer  ,jugralg,
     +                   ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             GMINTN (Graded Morphology INTegrals for Nodes)
c
c Module description: Calculate integrals for nodes and boundaries
c
c                     The integral for an outflowing branch (from node
c                     to branch) will be a fraction of the total inte-
c                     grals flowing to the node. Therefore first for all
c                     nodes the inflowing and outflowing branches will
c                     be determined and stored in a separate structure.
c                     If the node contains only one branch (boundary),
c                     routine MOIBOU will be called to calculate either
c                     the boundary condition or the outflowing integral
c                     value.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 alphac            P  -
c 14 bgout             P  -
c 11 branch            P  -
c 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 23 celer             P  -
c 19 disgse            P  -
c  9 dtm               P  -
c 15 grid              P  -
c 20 intgr             P  -
c 25 juer              P  -
c 26 ker               P  -
c  6 maxtab            I  Maximum number of defined tables.
c 17 mbdpar            P  -
c  5 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  3 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c 16 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c 18 ntab              P  -
c  7 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 24 sedtr             P  -
c 22 table             P  -
c  8 time              P  -
c 13 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 21 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moibou  MOrphology Integral BOUndary
c moinod  MOrphology Integral for a NODe
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmintn.F,v $
c Revision 1.2  1995/09/27  10:11:39  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nnode  ,nbran  ,nbrnod ,ngrid  ,nboun  ,maxtab ,
     +           ntabm  ,nfrac  ,juer   ,jugralg,ker
      integer    branch (4,nbran)       ,
     +           brnode (nbrnod+1,nnode),
     +           bgout  (3,nbrnod)      ,
     +           node   (4,nnode)       ,
     +           mbdpar (5,nboun)       ,
     +           flwdir (ngrid)         ,
     +           ntab   (4,maxtab)
      real       alphac ,alphad  ,alphae
      real       disgse (nfrac,2,nbran) ,intgr  (nfrac,2,nbran) ,
     +           table  (ntabm)         ,
     +           celer  (ngrid,nfrac,5) ,sedtr (ngrid,nfrac+2),
     +           source (ngrid,nfrac+2) ,celert(ngrid)        ,
     +           x      (ngrid)         ,cela1 (nfrac,nfrac)  ,
     +           ds     (nfrac)         ,spredc(nfrac)        ,
     +           dfrac  (nfrac)
      double precision  time   ,dtm

c
c     Declaration of local parameters
c
      integer    inode        ,ibr         , igp
      real       itot(nfrac)  ,stot(nfrac)
c
c     Include sobek constants
c
c     Do for each node
c
      do 10 inode = 1, nnode
c
c        Test number of connected branches
c
         if (brnode(1,inode) .gt. 1) then
c
c           Internal node : call GMINOD
c
            call GMINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,nfrac  ,
     +                   dtm    ,alphac ,alphad ,alphae ,branch,brnode ,
     +                   bgout  ,disgse ,intgr  ,x      ,celer ,celert ,
     +                   source ,dfrac  ,ds     ,spredc ,cela1 ,flwdir ,
     +                   sedtr  ,itot   ,stot   ,jugralg)
c
         else
c
c           Boundary      : call GMIBOU
c
            ibr  = brnode(2,inode)
            igp  = node  (2,inode)
c
c           Call processing routine
c
            call GMIBOU ( ibr    ,igp    ,juer   ,time   ,dtm    ,
     +                    ngrid  ,nbran  ,nboun  ,nnode  ,nfrac  ,
     +                    maxtab ,ntabm  ,branch ,node   ,mbdpar ,
     +                    ntab   ,table  ,alphac ,alphad ,alphae ,
     +                    x      ,celer  ,celert ,sedtr  ,disgse ,
     +                    source ,dfrac  ,ds     ,spredc ,cela1  ,
     +                    flwdir ,intgr(1,1,ibr) ,jugralg,ker    )
         endif
c
 10   continue
c
      end
