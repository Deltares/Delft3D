      subroutine secbio (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,branch ,
     &                   brnode ,sedinf ,q2     ,sedtr  ,nrout ,qtot   ,
ci1
     &                   stotfr ,bgout  ,dissed ,nfrac  ,nfracb)
cu   &                   stot   ,bgout  ,dissed )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SECBIO (SEdiment Count Branches In/Out)
c
c Module description: Count number of outflowing branches (i.e. that are
c                     branches that recieve transport from the node) and
c                     calculate total inflowing discharge and sediment
c                     transport in the node.
c
c                     For each connected branch to the node is determi-
c                     ned whether the discharge is inflowing or outflo-
c                     wing.
c
c                     When an inflowing gridpoint has been detected the
c                     discharge and transport from this gridpoint are
c                     added to the total inflowing discharge and trans-
c                     port.
c
c                     The numbers of the outflowing branches and their
c                     connected gridpoints as well as the branch direc-
c                     tionsare returned to the calling routine for usage
c                     in the linear, ratio or proportional distribution
c                     function.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 bgout(3,nrout)    O  Contains info of every outflowing branch i
c                         from the current node:
c                         (1,i) = Branch number
c                         (2,i) = Grid point number
c                         (3,i) = Direction in branch:
c                                 +1 : First grid point is in node.
c                                 -1 : Last grid point is in node.
c  6 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  7 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 15 dissed(4,nbran)   O  Redistributed sediment transport at begin and
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
c  1 inode             I  Number of actual node.
c  3 nbran             I  Number of branches.
c  4 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  5 ngrid             I  Number of grid points in network.
c  2 nnode             I  Number of nodes.
c 11 nrout             IO Number of outflowing branches from a node
c  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 12 qtot              IO Total discharge flowing towards node.
c  8 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 10 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 13 stot              IO Total sediment transport going towards node.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: secbio.pf,v $
c Revision 1.2  1995/05/30  07:07:11  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:14  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:33  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
ci1
      integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nrout ,nfrac ,
     &           nfracb
cu    integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nrout
      integer    branch(4,nbran),brnode(nbrnod+1,nnode),sedinf(2,nbran),
     &           bgout (3,nbrnod)
ci3
      real       qtot
      real       sedtr(ngrid,*) ,
     &           dissed(nfracb,2,nbran),stotfr(nfrac)
	double precision q2(ngrid)
cu    real       stot  ,qtot
cu    real       q2    (ngrid)  ,sedtr(ngrid,*) ,dissed(4,nbran)
c
c     Declaration of local parameters
c
ci1
      integer    ibr   ,nbn     ,igr  ,dir  ,i    ,iside,  ifrac
cu    integer    ibr   ,nbn     ,igr  ,dir  ,i
      real       s
c
      nrout = 0
      qtot  = 0.
cu    stot  = 0.
ci3
      do 1 ifrac=1,nfrac
         stotfr(ifrac) = 0.
    1 continue
c
      nbn = brnode(1,inode)
c
      do 10 i =1,nbn
c
         ibr = brnode(i+1,inode)
c
c        Determine if node is at the beginning or end of the branch.
c
         if (branch (1,ibr) .eq. inode) then
c           Begin of branch
            igr = branch (3,ibr)
            dir = 1
         else
c           End of branch
            igr = branch (4,ibr)
            dir = -1
         endif
         s = real(dir)
c
         if (q2(igr)*s .lt. 0D0) then
c
c           Inflow in node.
c           Add inflowing sediment transport to total inflowing.
c           The calculated sediment transport will not be
c           redistributed.
c
ci4
            iside = (3-dir)/2
            do 2 ifrac=1,nfrac
               dissed(ifrac,iside,ibr) = sedtr(igr,ifrac)
    2       continue
cu          dissed(2-dir,ibr) = sedtr(igr,1)
            if (sedinf(1,ibr) .ne. 0) then
               stotfr(1) = stotfr(1) - (sedtr(igr,1) + sedtr(igr,2))*s
               dissed(2,iside,ibr) = sedtr(igr,2)
cu             stot = stot - (sedtr(igr,1) + sedtr(igr,2))*s
cu             dissed(3-dir,ibr) = sedtr(igr,2)
            else
ci3
               do 5 ifrac=1,nfrac
                  stotfr(ifrac) = stotfr(ifrac) - sedtr(igr,ifrac)*s
    5          continue
cu             stot = stot - sedtr(igr,1)*s
            endif
c
c           Add inflowing discharge to total discharge.
c
            qtot = qtot - q2(igr)*s
c
         else
c
c           Outflow from node.
c
            nrout = nrout + 1
            bgout(1,nrout) = ibr
            bgout(2,nrout) = igr
            bgout(3,nrout) = dir
c
        endif
   10 continue
c
      end
