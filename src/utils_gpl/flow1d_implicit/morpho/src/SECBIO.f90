subroutine secbio (inode  ,nnode  ,nbran  ,nbrnod ,ngrid ,branch ,&
&brnode ,sedinf ,q2     ,sedtr  ,nrout ,qtot   ,&
!i1
&stotfr ,bgout  ,dissed ,nfrac  ,nfracb)
!u   &                   stot   ,bgout  ,dissed )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SECBIO (SEdiment Count Branches In/Out)
!
! Module description: Count number of outflowing branches (i.e. that are
!                     branches that recieve transport from the node) and
!                     calculate total inflowing discharge and sediment
!                     transport in the node.
!
!                     For each connected branch to the node is determi-
!                     ned whether the discharge is inflowing or outflo-
!                     wing.
!
!                     When an inflowing gridpoint has been detected the
!                     discharge and transport from this gridpoint are
!                     added to the total inflowing discharge and trans-
!                     port.
!
!                     The numbers of the outflowing branches and their
!                     connected gridpoints as well as the branch direc-
!                     tionsare returned to the calling routine for usage
!                     in the linear, ratio or proportional distribution
!                     function.
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
!  8 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
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
! $Log: secbio.pf,v $
! Revision 1.2  1995/05/30  07:07:11  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:14  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:33  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
!i1
   integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nrout ,nfrac ,&
   &nfracb
!u    integer    inode ,nnode   ,nbran ,nbrnod  ,ngrid ,nrout
   integer    branch(4,nbran),brnode(nbrnod+1,nnode),sedinf(2,nbran),&
   &bgout (3,nbrnod)
!i3
   real       qtot
   real       sedtr(ngrid,*) ,&
   &dissed(nfracb,2,nbran),stotfr(nfrac)
   double precision q2(ngrid)
!u    real       stot  ,qtot
!u    real       q2    (ngrid)  ,sedtr(ngrid,*) ,dissed(4,nbran)
!
!     Declaration of local parameters
!
!i1
   integer    ibr   ,nbn     ,igr  ,dir  ,i    ,iside,  ifrac
!u    integer    ibr   ,nbn     ,igr  ,dir  ,i
   real       s
!
   nrout = 0
   qtot  = 0.
!u    stot  = 0.
!i3
   do 1 ifrac=1,nfrac
      stotfr(ifrac) = 0.
1  continue
!
   nbn = brnode(1,inode)
!
   do 10 i =1,nbn
!
      ibr = brnode(i+1,inode)
!
!        Determine if node is at the beginning or end of the branch.
!
      if (branch (1,ibr) .eq. inode) then
!           Begin of branch
         igr = branch (3,ibr)
         dir = 1
      else
!           End of branch
         igr = branch (4,ibr)
         dir = -1
      endif
      s = real(dir)
!
      if (q2(igr)*s .lt. 0D0) then
!
!           Inflow in node.
!           Add inflowing sediment transport to total inflowing.
!           The calculated sediment transport will not be
!           redistributed.
!
!i4
         iside = (3-dir)/2
         do 2 ifrac=1,nfrac
            dissed(ifrac,iside,ibr) = sedtr(igr,ifrac)
2        continue
!u          dissed(2-dir,ibr) = sedtr(igr,1)
         if (sedinf(1,ibr) .ne. 0) then
            stotfr(1) = stotfr(1) - (sedtr(igr,1) + sedtr(igr,2))*s
            dissed(2,iside,ibr) = sedtr(igr,2)
!u             stot = stot - (sedtr(igr,1) + sedtr(igr,2))*s
!u             dissed(3-dir,ibr) = sedtr(igr,2)
         else
!i3
            do 5 ifrac=1,nfrac
               stotfr(ifrac) = stotfr(ifrac) - sedtr(igr,ifrac)*s
5           continue
!u             stot = stot - sedtr(igr,1)*s
         endif
!
!           Add inflowing discharge to total discharge.
!
         qtot = qtot - q2(igr)*s
!
      else
!
!           Outflow from node.
!
         nrout = nrout + 1
         bgout(1,nrout) = ibr
         bgout(2,nrout) = igr
         bgout(3,nrout) = dir
!
      endif
10 continue
!
end
