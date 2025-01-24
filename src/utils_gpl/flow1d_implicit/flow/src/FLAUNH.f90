subroutine flaunh (nnode  ,nbran ,nbrnod ,ngrid ,branch ,&
&brnode ,h     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAUNH (FLow AUtostart Node H adaptation)
!
! Module description: Water levels at nodes should be made equal
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
!  7 h(ngrid)          IO Water level in every grid point at the latest
!                         iteration.
!  2 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flaunh.pf,v $
! Revision 1.3  1995/10/18  08:59:13  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/09/29  10:36:10  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.1  1995/09/22  10:00:50  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nnode ,nbran    ,nbrnod         ,ngrid
   integer    branch(4,nbran) ,brnode(nbrnod+1,nnode)
   double precision h     (ngrid)
!
!     Declaration of local variables
!
   integer    i  ,ibr  ,igr  ,nn  ,nbn
   double precision hsum, hmax
!
   do 70 nn = 1,nnode
!
!        Determine water levels and discharges at nodes.
!
!        Loop over connected branches to obtain filtered value.
!
      hsum  = 0D0
      hmax  = -9999D0
      nbn   = brnode(1,nn)
      do 50 i = 1,nbn
         ibr = brnode(i+1,nn)
         if (branch(1,ibr) .eq. nn) then
!
!              Begin of branch in node
            igr = branch(3,ibr)
         else
!
!              End of branch in node
            igr = branch(4,ibr)
         endif
         hsum = hsum + h(igr)
         hmax = max(hmax,h(igr))
50    continue
!
      hsum = hsum / nbn
!
!        Copy filtered value to grid points in connected branches.
!
      do 60 i = 1,nbn
         ibr = brnode(i+1,nn)
         if (branch(1,ibr) .eq. nn) then
!
!              Begin of branch in node
            igr = branch(3,ibr)
         else
!
!              End of branch in node
            igr = branch(4,ibr)
         endif
!JK         h(igr) = hsum
!           Maximum is probably better then average.
         h(igr) = hmax
60    continue
70 continue
!
end
