      subroutine MOINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,
     +                   ntabm  ,time   ,dtm    ,alphac ,branch,brnode ,
     +                   typcr  ,bgout  ,grid   ,node   ,mbdpar,ntab   ,
     +                   dissed ,intgr  ,x      ,table  ,celer ,sedtr  ,
     +                   mopta  ,moptb  ,moptc  ,moptd  ,alphad,flwdir ,
     +                   juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOINTN (MOrphology INTegrals for Nodes)
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
c 29 alphad            P  -
c 14 bgout             P  -
c 11 branch            P  -
c 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 23 celer             P  -
c 19 dissed            P  -
c  9 dtm               P  -
c 30 flwdir            P  -
c 15 grid              P  -
c 20 intgr             P  -
c 31 juer              P  -
c 32 ker               P  -
c  6 maxtab            I  Maximum number of defined tables.
c 17 mbdpar            P  -
c 25 mopta             P  -
c 26 moptb             P  -
c 27 moptc             P  -
c 28 moptd             P  -
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
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mointn.pf,v $
c Revision 1.7  1998/06/11  11:47:12  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1996/12/04  12:01:06  kuipe_j
c declarations / undefined vars
c
c Revision 1.5  1996/03/08  09:39:08  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.4  1996/03/07  10:44:17  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/05/30  09:55:55  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:50  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:18  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:38  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:47  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nnode  ,nbran  ,nbrnod ,ngrid  ,nboun  ,maxtab ,
     +           ntabm  ,juer   ,ker

      real       alphac ,alphad

      integer    branch (4,nbran)       ,
     +           flwdir (ngrid)         ,
     +           brnode (nbrnod+1,nnode),
     +           typcr  (nbran)         ,
     +           bgout  (3,nbrnod)      ,
     +           grid   (ngrid)         ,
     +           node   (4,nnode)       ,
     +           mbdpar (5,nboun)       ,
     +           ntab   (4,maxtab)

      real       dissed (4,nbran),
     +           intgr  (ngrid,2,*),
     +           x      (ngrid)  ,
     +           table  (ntabm)  ,
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*)

      double     precision  time ,dtm

      logical    mopta, moptb, moptc, moptd
c
c     Declaration of local parameters
c
      integer    inode, ibr, igp, nsec, isec
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Do for each node
c
      do 20 inode = 1, nnode
c
c        Test number of connected branches
c
         if (brnode(1,inode) .gt. 1) then
c
c           Internal node : call MOINOD
c
            call MOINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,dtm   ,
     +                   alphac,branch  ,brnode ,typcr  ,bgout ,grid  ,
     +                   dissed ,intgr  ,x      ,celer  ,
     +                   mopta  ,moptb  ,moptc  ,moptd  ,flwdir,alphad,
     +                   sedtr  ,juer   ,ker    )

         else
c
c           Boundary      : call MOIBOU
c
            ibr  = brnode(2,inode)
            igp  = node  (2,inode)
c
c           Check for sedredge branch
c
            if (typcr(ibr) .eq. ccrsed) then
               nsec = 2
            else
               nsec = 1
            endif
c
            do 10 isec = 1, nsec
c
c              Call processing routine
c
               call MOIBOU ( ibr    ,igp    ,isec   ,
     +                       ngrid  ,nbran  ,nboun  ,nnode  ,
     +                       grid   ,branch ,node   ,
     +                       mbdpar ,x      ,
     +                       maxtab ,ntabm  ,ntab   ,table  ,
     +                       time   ,dtm    ,alphac ,
     +                       celer  ,sedtr  ,dissed ,
     +                       mopta  ,moptb  ,moptc  ,moptd  ,
     +                       alphad ,flwdir ,
     +                       juer   ,ker    ,intgr(igp,1,isec)
     +                     )
 10         continue
         endif
c
 20   continue
c
      end
