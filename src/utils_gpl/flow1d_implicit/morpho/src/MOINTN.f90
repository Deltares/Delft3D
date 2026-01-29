subroutine MOINTN (nnode  ,nbran  ,nbrnod ,ngrid  ,nboun ,maxtab ,&
&ntabm  ,time   ,dtm    ,alphac ,branch,brnode ,&
&typcr  ,bgout  ,grid   ,node   ,mbdpar,ntab   ,&
&dissed ,intgr  ,x      ,table  ,celer ,sedtr  ,&
&mopta  ,moptb  ,moptc  ,moptd  ,alphad,flwdir ,&
&juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOINTN (MOrphology INTegrals for Nodes)
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
! 29 alphad            P  -
! 14 bgout             P  -
! 11 branch            P  -
! 12 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 23 celer             P  -
! 19 dissed            P  -
!  9 dtm               P  -
! 30 flwdir            P  -
! 15 grid              P  -
! 20 intgr             P  -
! 31 juer              P  -
! 32 ker               P  -
!  6 maxtab            I  Maximum number of defined tables.
! 17 mbdpar            P  -
! 25 mopta             P  -
! 26 moptb             P  -
! 27 moptc             P  -
! 28 moptd             P  -
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
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mointn.pf,v $
! Revision 1.7  1998/06/11  11:47:12  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1996/12/04  12:01:06  kuipe_j
! declarations / undefined vars
!
! Revision 1.5  1996/03/08  09:39:08  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.4  1996/03/07  10:44:17  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/05/30  09:55:55  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:50  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:18  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:38  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:47  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nnode  ,nbran  ,nbrnod ,ngrid  ,nboun  ,maxtab ,&
   &ntabm  ,juer   ,ker

   real       alphac ,alphad

   integer    branch (4,nbran)       ,&
   &flwdir (ngrid)         ,&
   &brnode (nbrnod+1,nnode),&
   &typcr  (nbran)         ,&
   &bgout  (3,nbrnod)      ,&
   &grid   (ngrid)         ,&
   &node   (4,nnode)       ,&
   &mbdpar (5,nboun)       ,&
   &ntab   (4,maxtab)

   real       dissed (4,nbran),&
   &intgr  (ngrid,2,*),&
   &x      (ngrid)  ,&
   &table  (ntabm)  ,&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*)

   double     precision  time ,dtm

   logical    mopta, moptb, moptc, moptd
!
!     Declaration of local parameters
!
   integer    inode, ibr, igp, nsec, isec
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Do for each node
!
   do 20 inode = 1, nnode
!
!        Test number of connected branches
!
      if (brnode(1,inode) .gt. 1) then
!
!           Internal node : call MOINOD
!
         call MOINOD (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,dtm   ,&
         &alphac,branch  ,brnode ,typcr  ,bgout ,grid  ,&
         &dissed ,intgr  ,x      ,celer  ,&
         &mopta  ,moptb  ,moptc  ,moptd  ,flwdir,alphad,&
         &sedtr  ,juer   ,ker    )

      else
!
!           Boundary      : call MOIBOU
!
         ibr  = brnode(2,inode)
         igp  = node  (2,inode)
!
!           Check for sedredge branch
!
         if (typcr(ibr) .eq. ccrsed) then
            nsec = 2
         else
            nsec = 1
         endif
!
         do 10 isec = 1, nsec
!
!              Call processing routine
!
            call MOIBOU ( ibr    ,igp    ,isec   ,&
            &ngrid  ,nbran  ,nboun  ,nnode  ,&
            &grid   ,branch ,node   ,&
            &mbdpar ,x      ,&
            &maxtab ,ntabm  ,ntab   ,table  ,&
            &time   ,dtm    ,alphac ,&
            &celer  ,sedtr  ,dissed ,&
            &mopta  ,moptb  ,moptc  ,moptd  ,&
            &alphad ,flwdir ,&
            &juer   ,ker    ,intgr(igp,1,isec)&
            &)
10       continue
      endif
!
20 continue
!
end
