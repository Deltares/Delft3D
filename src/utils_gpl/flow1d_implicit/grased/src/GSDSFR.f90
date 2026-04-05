subroutine gsdsfr (inode  ,nnode  ,nbran  ,nbrnod ,ngrid  ,nfrac ,&
&branch ,brnode ,q2     ,sedtr  ,stotfr ,bgout ,&
&disgse )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSDSFR (Graded Sediment DiStribute over FRactions)
!
! Module description: Fill array DISGSE with (distributed) transports
!                     per fraction at the branch end of a node.
!
!                     For each connected branch to the node is determi-
!                     ned whether the discharge is inflowing or outflo-
!                     wing.
!                     Count number of outflowing branches (i.e. that are
!                     branches that recieve transport from the node) and
!                     calculate total inflowing sediment transport in
!                     in the node per fraction. Store these transports
!                     in DISGSE. Then distribute the sediment transports
!                     per fraction.
!
! Assumption:         All fractions will get the same distribution.
!
! Precondition:       The total distribution is stored in array DISGSE
!                     on the index of fraction 1.
! Postcondition:      DISGSE contains the (distributed) transports per
!                     fraction at all branch end of this node.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 bgout(3,nrout)    O  Contains info of every outflowing branch i
!                         from the current node:
!                         (1,i) = Branch number
!                         (2,i) = Grid point number
!                         (3,i) = Direction in branch:
!                                 +1 : First grid point is in node.
!                                 -1 : Last grid point is in node.
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  7 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 15 dissed(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  1 inode             I  Number of actual node.
!  3 nbran             I  Number of branches.
!  4 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  5 ngrid             I  Number of grid points in network.
!  2 nnode             I  Number of nodes.
! 11 nrout             IO Number of outflowing branches from a node
!  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 12 qtot              IO Total discharge flowing towards node.
! 10 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 13 stot              IO Total sediment transport going towards node.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: $
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nfrac
   integer    branch(4,nbran),brnode(nbrnod+1,nnode),&
   &bgout (3,nbrnod)
   real       q2    (ngrid)  ,sedtr(ngrid,*) ,disgse(nfrac,2,nbran),&
   &stotfr(nfrac)
!
!     Declaration of local parameters
!
   integer    ibr   ,nbn     ,igr  ,dir  ,i  ,jf  ,nrout
   real       s     ,stot
!
   nrout = 0
   stot  = 0.
   do 10 jf=1,nfrac
      stotfr(jf) = 0.
10 continue
!
   nbn = brnode(1,inode)
!
   do 30 i =1,nbn
!
      ibr = brnode(i+1,inode)
!
!        Determine if node is at the beginning or end of the branch.
!
      if (branch (1,ibr) .eq. inode) then
!           Begin of branch
         igr = branch (3,ibr)
         dir = 1
         s   = 1.
      else
!           End of branch
         igr = branch (4,ibr)
         dir = 2
         s   = -1.
      endif
!
      if (q2(igr)*s .lt. 0.) then
!
!           Inflow in node.
!           Add inflowing sediment transport to total inflowing.
!           First total transport and then transports per fraction.
!           Store from branch outflowing transports in DISGSE
!
         stot = stot - sedtr(igr,nfrac+1)*s
!
         do 20 jf=1,nfrac
            stotfr(jf) = stotfr(jf) - sedtr(igr,jf)*s
            disgse(jf,dir,ibr) = sedtr(igr,jf)
20       continue
!
      else
!
!           Outflow from node.
!
         nrout = nrout + 1
         bgout(1,nrout) = ibr
         bgout(3,nrout) = dir
!
      endif
!
30 continue
!
!     Store in branch inflowing transports in DISGSE
!
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
!
end
