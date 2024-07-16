      subroutine flaunh (nnode  ,nbran ,nbrnod ,ngrid ,branch ,
     &                   brnode ,h     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAUNH (FLow AUtostart Node H adaptation)
c
c Module description: Water levels at nodes should be made equal
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c  7 h(ngrid)          IO Water level in every grid point at the latest
c                         iteration.
c  2 nbran             I  Number of branches.
c  3 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  4 ngrid             I  Number of grid points in network.
c  1 nnode             I  Number of nodes.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flaunh.pf,v $
c Revision 1.3  1995/10/18  08:59:13  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/09/29  10:36:10  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.1  1995/09/22  10:00:50  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nnode ,nbran    ,nbrnod         ,ngrid
      integer    branch(4,nbran) ,brnode(nbrnod+1,nnode)
      double precision h     (ngrid)
c
c     Declaration of local variables
c
      integer    i  ,ibr  ,igr  ,nn  ,nbn
      double precision hsum, hmax
c
      do 70 nn = 1,nnode
c
c        Determine water levels and discharges at nodes.
c
c        Loop over connected branches to obtain filtered value.
c
         hsum  = 0D0
         hmax  = -9999D0
         nbn   = brnode(1,nn)
         do 50 i = 1,nbn
            ibr = brnode(i+1,nn)
            if (branch(1,ibr) .eq. nn) then
c
c              Begin of branch in node
               igr = branch(3,ibr)
            else
c
c              End of branch in node
               igr = branch(4,ibr)
            endif
            hsum = hsum + h(igr)
            hmax = max(hmax,h(igr))
   50    continue
c
         hsum = hsum / nbn
c
c        Copy filtered value to grid points in connected branches.
c
         do 60 i = 1,nbn
            ibr = brnode(i+1,nn)
            if (branch(1,ibr) .eq. nn) then
c
c              Begin of branch in node
               igr = branch(3,ibr)
            else
c
c              End of branch in node
               igr = branch(4,ibr)
            endif
CJK         h(igr) = hsum
C           Maximum is probably better then average.
            h(igr) = hmax
   60    continue
   70 continue
c
      end
