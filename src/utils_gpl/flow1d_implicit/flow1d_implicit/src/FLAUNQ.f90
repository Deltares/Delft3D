subroutine flaunq (nnode ,nbran ,nbrnod ,ngrid ,branch ,&
&brnode,q     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAUNQ (FLow AUtostart Node Q adaptation)
!
! Module description: Discharges at nodes are adapted in such a way
!                     that they satisfy the continuity equation
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
!  2 nbran             I  Number of branches.
!  3 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  4 ngrid             I  Number of grid points in network.
!  1 nnode             I  Number of nodes.
!  7 q(ngrid)          IO Discharge in every grid point at the latest
!                         iteration.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flaunq.pf,v $
! Revision 1.2  1999/03/15  15:49:27  kuipe_j
! tabs removed
!
! Revision 1.1  1995/09/22  10:00:51  kuipe_j
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
   double precision  q(ngrid)
!
!     Declaration of local variables
!
   integer    i     ,ibr  ,igr  ,nn  ,nbn
   double precision  qsum  ,s
!
   do 70 nn = 1,nnode
!
!        Determine water levels and discharges at nodes.
!        Loop over connected branches to obtain filtered value.
!
      qsum  = 0D0
      nbn   = brnode(1,nn)
      if (nbn.gt.1) then
         do 50 i = 1,nbn
            ibr = brnode(i+1,nn)
            if (branch(1,ibr) .eq. nn) then
!
!              Begin of branch in node
               igr = branch(3,ibr)
               s   = 1D0
            else
!
!              End of branch in node
               igr = branch(4,ibr)
               s   = -1D0
            endif
            qsum = qsum + q(igr) *  s
50       continue
!
         qsum = qsum / nbn
!
!        Copy filtered value to grid points in connected branches.
!
         do 60 i = 1,nbn
            ibr = brnode(i+1,nn)
            if (branch(1,ibr) .eq. nn) then
!
!              Begin of branch in node
               igr = branch(3,ibr)
               s   = 1D0
            else
!
!              End of branch in node
               igr = branch(4,ibr)
               s   = -1D0
            endif
            q(igr) = q(igr) - qsum * s
60       continue
      endif
70 continue
!
end
