subroutine gsnode (nnode  ,nbran  ,nbrnod ,ngrid  ,maxtab ,ntabm ,&
&nfrac  ,branch ,brnode ,bgout  ,sedinf,sdrdbf ,&
&seddb  ,ntab   ,morcon ,q2     ,ws    ,table  ,&
&sedtr  ,stotfr ,disgse ,trform )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSNODE (Graded Sediment transports in NODEs)
!
! Module description: Distribute sediment transport from the inflowing
!                     branches to the outflowing branches all connected
!                     to a node.
!
!                     When the morphology module is included the sedi-
!                     ment transport at the nodes must be redistributed.
!                     This module will distribute the sediment transport
!                     in nodes by using a distribution function. First
!                     the number of outflowing branches (i.e. that are
!                     branches that recieve transport from the node) is
!                     counted by routine SECBIO. In case only one out-
!                     flowing branch exists the sediment transport to
!                     this branch will be the total inflowing transport.
!                     In case of two outflowing branches three different
!                     distribution functions are possible. In case no
!                     function has been defined by the user the default
!                     proportional method will be taken. The proportio-
!                     nal method will always be used in case more than
!                     two outflowing branches are counted.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 bgout             P  -
!  7 branch            P  -
!  8 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 18 dissed            P  -
!  5 maxtab            I  Maximum number of defined tables.
! 14 morcon            P  -
!  2 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
! 13 ntab              P  -
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 q2                P  -
! 11 sdrdbf            P  -
! 12 seddb(2,nnode)    I  Sediment distribution as function of the di-
!                         scharge:
!                         (1,j) = Type of distribution function:
!                                 cdbpro (1) : Proportional
!                                 cdblin (2) : Linear
!                                 cdbrat (3) : Ratio
!                         Options 2 and 3 are only available for nodes
!                         with three connected branches.
!                         (2,j) = Starting index in morcon for distribu-
!                                 tion functions 2 and 3. If a starting
!                                 index has been specified a seddn(3,j)
!                                 pairs of branches will be found in
!                                 array morcon on position index, in-
!                                 dex+1 , index+2 etc.
!                         (3,j) = Number of pairs with defined linear or
!                                 ratio distribution function.
! 10 sedinf            P  -
! 17 sedtr             P  -
! 16 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! secbio  SEdiment Count Branches In/Out
! seds1b  SEdiment Distribute Sediment forBranch
! sedsli  SEdiment Distribute Sediment LInear
! sedspr  SEdiment Distribe Sediment PRoportional
! sedsra  SEdiment Distribute Sediment RAtio
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsnode.F,v $
! Revision 1.2  1995/09/27  10:12:39  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nnode ,nbran    ,nbrnod  ,ngrid   ,maxtab, ntabm  ,&
   &nfrac
   integer    sedinf(2,nbran) ,sdrdbf(2,*)      ,seddb(3,nnode) ,&
   &brnode(nbrnod+1,nnode)            ,bgout(3,nbrnod),&
   &branch(4,nbran) ,ntab  (4,maxtab)
!i1
   real       morcon(2+2*nfrac,*), ws   (ngrid)   ,&
!u    real       morcon(4,*)     ,q2    (ngrid)    ,ws   (ngrid)   ,
   &sedtr (ngrid,nfrac+2)             ,stotfr (nfrac) ,&
   &table (ntabm)   ,disgse(nfrac,2,nbran)            ,&
   &trform(3,nbran)

   double precision q2( ngrid )
!
!     Declaration of local parameters
!
   integer    inode ,nrout
!i1
   real       qtot
!u    real       stot  ,qtot
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   do 10 inode = 1, nnode
      if (brnode(1,inode) .gt. 1) then
!
!           Determine: 1. Number of inflowing / Outflowing branches.
!                      2. Stot and Qtot.
!                      3. Info of branches with inflow.
!           Copy for outflowing branches sediment transport to array
!           Dissed.
!
!           Er wordt aangenomen dat de verdeling van het sediment over
!           het knooppunt voor alle fracties hetzelfde is daarom wordt
!           routine SECBIO met het totaaltransport anngeroepen.
!
         call secbio (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,branch,&
!i3
         &brnode ,sedinf ,q2     ,sedtr                 ,&
         &nrout  ,qtot   ,stotfr ,bgout  ,disgse ,nfrac ,&
         &nfrac  )
!u   &                   brnode ,sedinf ,q2     ,sedtr(1,nfrac+1)      ,
!u   &                   nrout  ,qtot   ,stot   ,bgout  ,disgse )

         if (nrout .eq. 1) then
!
!              In case of one outflowing and one inflowing branch:
!              Outgoing sediment from node equals incoming sediment.
!
!i3
            call seds1b(nbran  ,ngrid  ,maxtab ,ntabm ,stotfr,bgout ,&
            &sedinf ,sdrdbf ,ntab   ,q2    ,table ,disgse,&
            &nfrac  ,nfrac  )
!u             call seds1b(nbran  ,ngrid  ,maxtab ,ntabm ,stot ,bgout  ,
!u   &                     sedinf ,sdrdbf ,ntab   ,q2    ,table,disgse )
!
         else if (seddb(1,inode) .eq. cdbpow) then
!
!              Distribution of from node outflowing sediment
!              according to power function (Wang)
!
            call sedspo(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,&
!i1
            &nrout ,stotfr,bgout ,sedinf,sdrdbf,seddb ,&
!u   &                     nrout ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
            &ntab  ,morcon,q2    ,ws    ,table ,disgse,&
!i1
            &trform,nfrac ,nfrac )
!u   &                     trform)
!
         else if (nrout .eq. 2) then
!
!              Two branches with ouflow from node:
!              Selection of distribution functions.
!
            if      (seddb(1,inode) .eq. cdblin) then
!
!                 Linear distribution function.
!
               call sedsli(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,&
!i3
               &qtot  ,stotfr,bgout ,sedinf,sdrdbf,seddb ,&
               &ntab  ,morcon,q2    ,table ,disgse,nfrac ,&
               &nfrac )
!u   &                        qtot  ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
!u   &                        ntab  ,morcon,q2    ,table ,disgse)
!
            else if (seddb(1,inode) .eq. cdbrat) then
!
!                 Distribution defined by ratio table.
!
               call sedsra(inode ,nnode ,nbran ,ngrid ,maxtab,ntabm ,&
!i3
               &qtot  ,stotfr,bgout ,sedinf,sdrdbf,seddb ,&
               &ntab  ,morcon,q2    ,table ,disgse,nfrac ,&
               &nfrac )
!u   &                        qtot  ,stot  ,bgout ,sedinf,sdrdbf,seddb ,
!u   &                        ntab  ,morcon,q2    ,table ,disgse)
!
            else
!
!                 DEFAULT: Distribution proportional to discharge.
!
               call sedspr(nrout ,nbran ,ngrid ,maxtab,ntabm ,qtot  ,&
!i2
               &stotfr,bgout ,sedinf,sdrdbf,ntab  ,q2    ,&
               &table ,disgse,nfrac ,nfrac )
!u   &                        stot  ,bgout ,sedinf,sdrdbf,ntab  ,q2    ,
!u   &                        table ,disgse)
            endif

         else if (nrout .gt. 2) then
!
!              More than two branches with ouflow from node:
!              Always proportional distribution.
!
            call sedspr(nrout ,nbran ,ngrid ,maxtab ,ntabm ,qtot   ,&
!i2
            &stotfr,bgout ,sedinf,sdrdbf ,ntab  ,q2     ,&
            &table ,disgse,nfrac ,nfrac  )
!u   &                     stot  ,bgout ,sedinf,sdrdbf ,ntab  ,q2     ,
!u   &                     table ,disgse)
         endif
      endif
!
10 continue
!
!     The total transports are distributed. Copy to final place
!     according to declaration DISGSE.
!
!u    call  gsdscp (nfrac  ,nbran  ,disgse )
!
!     Distribute transports in each fraction
!
!u    do 20 inode = 1, nnode
!u       if (brnode(1,inode) .gt. 1) then
!u          call gsdsfr (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,
!u   &                   branch ,brnode ,q2     ,sedtr  ,stotfr ,bgout ,
!u   &                   disgse )
!u       endif
!u 20 continue
!
end
