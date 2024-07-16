      subroutine senode (nnode  ,nbran  ,nbrnod ,ngrid ,maxtab ,ntabm  ,
     &                   branch ,brnode ,bgout  ,sedinf,sdrdbf ,seddb  ,
     &                   ntab   ,morcon ,q2     ,ws    ,table  ,sedtr  ,
     &                   dissed ,trform )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SENODE (SEdiment transports in NODEs)
c
c Module description: Distribute sediment transport from the inflowing
c                     branches to the outflowing branches all connected
c                     to a node.
c
c                     When the morphology module is included the sedi-
c                     ment transport at the nodes must be redistributed.
c                     This module will distribute the sediment transport
c                     in nodes by using a distribution function. First
c                     the number of outflowing branches (i.e. that are
c                     branches that recieve transport from the node) is
c                     counted by routine SECBIO. In case only one out-
c                     flowing branch exists the sediment transport to
c                     this branch will be the total inflowing transport.
c                     In case of two outflowing branches three different
c                     distribution functions are possible. In case no
c                     function has been defined by the user the default
c                     proportional method will be taken. The proportio-
c                     nal method will always be used in case more than
c                     two outflowing branches are counted.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 bgout             P  -
c  7 branch            P  -
c  8 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 18 dissed            P  -
c  5 maxtab            I  Maximum number of defined tables.
c 14 morcon            P  -
c  2 nbran             I  Number of branches.
c  3 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c 13 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 q2                P  -
c 11 sdrdbf            P  -
c 12 seddb(2,nnode)    I  Sediment distribution as function of the di-
c                         scharge:
c                         (1,j) = Type of distribution function:
c                                 cdbpro (1) : Proportional
c                                 cdblin (2) : Linear
c                                 cdbrat (3) : Ratio
c                         Options 2 and 3 are only available for nodes
c                         with three connected branches.
c                         (2,j) = Starting index in morcon for distribu-
c                                 tion functions 2 and 3. If a starting
c                                 index has been specified a seddn(3,j)
c                                 pairs of branches will be found in
c                                 array morcon on position index, in-
c                                 dex+1 , index+2 etc.
c                         (3,j) = Number of pairs with defined linear or
c                                 ratio distribution function.
c 10 sedinf            P  -
c 17 sedtr             P  -
c 16 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c secbio  SEdiment Count Branches In/Out
c seds1b  SEdiment Distribute Sediment forBranch
c sedsli  SEdiment Distribute Sediment LInear
c sedspr  SEdiment Distribe Sediment PRoportional
c sedsra  SEdiment Distribute Sediment RAtio
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: senode.pf,v $
c Revision 1.5  1996/12/03  08:24:16  kuipe_j
c calibration factor added in power distribution
c
c Revision 1.4  1996/09/03  14:46:57  kuipe_j
c frequency time hist,Power distribution added
c
c Revision 1.3  1995/05/30  09:56:30  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:28  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:28  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:59  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nnode ,nbran    ,nbrnod  ,ngrid   ,maxtab, ntabm
      integer    sedinf(2,nbran) ,sdrdbf(2,*)      ,seddb(3,nnode) ,
     &           brnode(nbrnod+1,nnode)            ,bgout(3,nbrnod),
     &           branch(4,nbran) ,ntab  (4,maxtab)
      real       morcon(4,*)     ,ws   (ngrid)   ,
     &           sedtr(ngrid,*)  ,table (ntabm)    ,dissed(4,nbran)
      real       trform(3,nbran) 
      double precision q2(ngrid) 
c
c     Declaration of local parameters
c
      integer    inode ,nrout
ci2
      real       qtot
      real       stotfr(1)
cu    real       stot  ,qtot
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      do 10 inode = 1, nnode
         if (brnode(1,inode) .gt. 1) then
c
c           Determine: 1. Number of inflowing / Outflowing branches.
c                      2. Stot and Qtot.
c                      3. Info of branches with inflow.
c           Copy for outflowing branches sediment transport to array
c           Dissed.
c
            call secbio (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,branch ,
     &                   brnode ,sedinf ,q2     ,sedtr  ,nrout ,qtot   ,
ci1
     &                   stotfr ,bgout  ,dissed ,1      ,2     )
cu   &                   stot   ,bgout  ,dissed )

            if (nrout .eq. 1) then
c
c              In case of one outflowing and one inflowing branch:
c              Outgoing sediment from node equals incoming sediment.
c
               call seds1b(nbran  ,ngrid  ,maxtab ,ntabm ,stotfr ,
     &                     bgout  ,sedinf ,sdrdbf ,ntab   ,q2    ,
     &                     table  ,dissed ,1      ,2      )
cu             call seds1b(nbran  ,ngrid  ,maxtab ,ntabm ,stot ,bgout ,
cu   &                     sedinf ,sdrdbf ,ntab   ,q2    ,table,dissed) 
c
            else if (seddb(1,inode) .eq. cdbpow) then
c     
c              Distribution of from node outflowing sediment
c              according to power function (Wang)
c
               call sedspo(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,
ci1
     &                     nrout ,stotfr,bgout ,sedinf,sdrdbf,seddb ,
cu   &                     nrout ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
     &                     ntab  ,morcon,q2    ,ws    ,table ,dissed,
ci1
     &                     trform,1     ,2     )                     
cu   &                     trform                                    )
c
            else if (nrout .eq. 2) then
c
c              Two branches with ouflow from node:
c              Selection of distribution functions (not power).
c
               if      (seddb(1,inode) .eq. cdblin) then
c
c                 Linear distribution function.
c
                  call sedsli(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,
ci3
     &                        qtot  ,stotfr,bgout ,sedinf,sdrdbf,seddb ,
     &                        ntab  ,morcon,q2    ,table ,dissed,1     ,
     &                        2     )
cu   &                        qtot  ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
cu   &                        ntab  ,morcon,q2    ,table ,dissed)
c
               else if (seddb(1,inode) .eq. cdbrat) then
c
c                 Distribution defined by ratio table.
c
                  call sedsra(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,
ci3
     &                        qtot  ,stotfr,bgout ,sedinf,sdrdbf,seddb ,
     &                        ntab  ,morcon,q2    ,table ,dissed,1     ,
     &                        2     )
cu   &                        qtot  ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
cu   &                        ntab  ,morcon,q2    ,table ,dissed)
c
               else
c
c                 DEFAULT: Distribution proportional to discharge.
c
                  call sedspr(nrout ,nbran ,ngrid ,maxtab,ntabm ,qtot  ,
ci2
     &                        stotfr,bgout ,sedinf,sdrdbf,ntab  ,q2    ,
     &                        table ,dissed,1     ,2     )
cu   &                        stot  ,bgout ,sedinf,sdrdbf,ntab  ,q2    ,
cu   &                        table ,dissed)
               endif

            else if (nrout .gt. 2) then
c
c              More than two branches with ouflow from node:
c              Always proportional distribution.
c
               call sedspr(nrout ,nbran ,ngrid ,maxtab ,ntabm ,qtot   ,
ci2
     &                     stotfr,bgout ,sedinf,sdrdbf ,ntab  ,q2     ,
     &                     table ,dissed,1     ,2      )
cu   &                     stot  ,bgout ,sedinf,sdrdbf ,ntab  ,q2     ,
cu   &                     table ,dissed)
            endif
         endif
c
   10 continue
c
      end
