      subroutine wqtnod ( igrid , inode , branch , nbran )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers      
c
c Module:             WQTNOD (Water Quality geT Node number)
c
c Module description: Get node number for a specified gridpoint.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  1 igrid             I  gridpoint number
c  2 inode             O  node number
c  4 nbran             I  nr of branches
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqtnod.pf,v $
c Revision 1.1  1996/10/31  09:51:49  kuipe_j
c Calculation of exchanges added
c
c
c***********************************************************************
c
      integer      igrid ,
     j             inode ,
     j             nbran
      integer      branch(4,nbran)

      integer      ibran

      inode = 0
      do 100 ibran = 1,nbran
         if ( igrid .eq. branch(3,ibran) ) then
             inode = branch(1,ibran)
             return
         endif
         if ( igrid .eq. branch(4,ibran) ) then
             inode = branch(2,ibran)
             return
         endif
  100 continue

      end
