      subroutine gsdsfr (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,
     &                   branch ,brnode ,q2     ,sedtr  ,stotfr ,bgout ,
     &                   disgse )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSDSFR (Graded Sediment DiStribute over FRactions)
c
c Module description: Fill array DISGSE with (distributed) transports
c                     per fraction at the branch end of a node.
c
c                     For each connected branch to the node is determi-
c                     ned whether the discharge is inflowing or outflo-
c                     wing.
c                     Count number of outflowing branches (i.e. that are
c                     branches that recieve transport from the node) and
c                     calculate total inflowing sediment transport in
c                     in the node per fraction. Store these transports
c                     in DISGSE. Then distribute the sediment transports
c                     per fraction.
c
c Assumption:         All fractions will get the same distribution.
c
c Precondition:       The total distribution is stored in array DISGSE
c                     on the index of fraction 1.
c Postcondition:      DISGSE contains the (distributed) transports per
c                     fraction at all branch end of this node.
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
c $Log: $
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nfrac
      integer    branch(4,nbran),brnode(nbrnod+1,nnode),
     &           bgout (3,nbrnod)
      real       q2    (ngrid)  ,sedtr(ngrid,*) ,disgse(nfrac,2,nbran),
     &           stotfr(nfrac)
c
c     Declaration of local parameters
c
      integer    ibr   ,nbn     ,igr  ,dir  ,i  ,jf  ,nrout
      real       s     ,stot
c
      nrout = 0
      stot  = 0.
      do 10 jf=1,nfrac
         stotfr(jf) = 0.
   10 continue
c
      nbn = brnode(1,inode)
c
      do 30 i =1,nbn
c
         ibr = brnode(i+1,inode)
c
c        Determine if node is at the beginning or end of the branch.
c
         if (branch (1,ibr) .eq. inode) then
c           Begin of branch
            igr = branch (3,ibr)
            dir = 1
	    s   = 1.
         else
c           End of branch
            igr = branch (4,ibr)
            dir = 2  
	    s   = -1.
         endif
c
         if (q2(igr)*s .lt. 0.) then
c
c           Inflow in node.
c           Add inflowing sediment transport to total inflowing.
c           First total transport and then transports per fraction.
c           Store from branch outflowing transports in DISGSE
c
            stot = stot - sedtr(igr,nfrac+1)*s
c
	    do 20 jf=1,nfrac
	       stotfr(jf) = stotfr(jf) - sedtr(igr,jf)*s
               disgse(jf,dir,ibr) = sedtr(igr,jf)
   20       continue
c
         else
c
c           Outflow from node.
c
            nrout = nrout + 1
            bgout(1,nrout) = ibr
            bgout(3,nrout) = dir
c
        endif
c
   30 continue
c
c     Store in branch inflowing transports in DISGSE
c
      do 50 i = 1,nrout
         ibr = bgout(1,i)
	 dir = bgout(3,i)
	 do 40 jf=nfrac,1,-1
           if (stot.eq.0.) then
             disgse(jf,dir,ibr) = 0.0
           else
            disgse(jf,dir,ibr) = disgse(1,dir,ibr) * stotfr(jf) / stot
           endif

   40    continue
   50 continue
c
      end
