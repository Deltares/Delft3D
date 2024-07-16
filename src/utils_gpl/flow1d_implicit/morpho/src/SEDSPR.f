      subroutine sedspr (nrout ,nbran  ,ngrid ,maxtab ,ntabm ,qtot  ,
ci2
     &                   stotfr,bgout  ,sedinf,sdrdbf ,ntab  ,q2    ,
     &                   table ,dissed ,nfrac ,nfracb )
cu   &                   stot  ,bgout  ,sedinf,sdrdbf ,ntab  ,q2    ,
cu   &                   table ,dissed )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDSPR (SEdiment Distribe Sediment PRoportional)
c
c Module description: Calculate sediment transport according to a pro-
c                     portional distribution function.
c
c                     The sediment transport will be divided over all
c                     outflowing branches (i.e. that are branches that
c                     recieve transport from the node) proportional to
c                     the discharge. After this routine SEDISE is called
c                     in case one or more branches are sedredge bran-
c                     ches.
c                     [ S-FO-002.2KV / Eq. 4.8 ]
c                     Routine SEDISE distributes the sediment transport
c                     over the left and right channel.
c
c Precondition:       The total incoming discharge and transport in the
c                     node are positive.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 bgout(3,nrout)    I  Contains info of every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch:
c                                 +1 : First grid point is in node.
c                                 -1 : Last grid point is in node.
c 14 dissed(4,nbran)   O  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c  4 maxtab            I  Maximum number of defined tables.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  1 nrout             I  Number of outflowing branches from a node
c 11 ntab              P  -
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 12 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  6 qtot              I  Total discharge flowing towards node.
c 10 sdrdbf            P  -
c  9 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  7 stot              I  Total sediment transport going towards node.
c 13 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c sedise  SEdiment DIstribute SEdredge
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedspr.pf,v $
c Revision 1.2  1995/05/30  07:07:22  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:23  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:50  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
ci1
      integer    nrout ,nbran    ,ngrid   ,maxtab ,ntabm ,nfrac, nfracb
cu    integer    nrout ,nbran    ,ngrid   ,maxtab ,ntabm
      integer    sedinf(2,nbran) ,sdrdbf(2,*),
     &           ntab  (4,maxtab),bgout (3,nrout)
ci3
      real       qtot, table (ntabm)  ,stotfr (nfrac) ,
     &           dissed(nfracb,2,nbran)
cu    real       qtot  ,stot
cu    real       q2    (ngrid)    ,table (ntabm)  ,
cu   &           dissed(4,nbran)
      double precision q2(ngrid)
c
c     Declaration of local parameters
c
ci1
      integer    ibr    ,dir     ,igr   ,sedbr   ,i     ,iside, ifrac
cu    integer    ibr    ,dir     ,igr   ,sedbr   ,i
      real       ratio  ,s
      real       dissdb (2)
c
      do 10 i = 1,nrout
c
         ibr = bgout(1,i)
         igr = bgout(2,i)
         dir = bgout(3,i)
c
         if (qtot .gt. 1.e-10) then
            ratio = abs(q2(igr)) / qtot
         else
            ratio = 1. / real(i)
         endif
ci5
         iside = (3-dir)/2
         do 5 ifrac=1,nfrac
            s                 = ratio * stotfr(ifrac) * real(dir)
            dissed(ifrac,iside,ibr) = s
    5    continue
cu       s                 = ratio * stot * real(dir)
cu       dissed(2-dir,ibr) = s
c
c        Get sedredge branch number
c
         sedbr = sedinf(1,ibr)
         if (sedbr .ne. 0) then
c
c           Get total sediment and total discharge of the sedredge
c           branch.
c
            call sedise (sedbr  ,maxtab ,ntabm  ,s   ,q2(igr),
     &                   sdrdbf ,ntab   ,table  ,dissdb )
c
ci2
            dissed(1,iside,ibr) = dissdb(1)
            dissed(2,iside,ibr) = dissdb(2)
cu          dissed(2-dir,ibr) = dissdb(1)
cu          dissed(3-dir,ibr) = dissdb(2)
c
         endif
   10 continue
c
      end
