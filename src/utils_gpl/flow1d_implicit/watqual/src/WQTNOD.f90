subroutine wqtnod ( igrid , inode , branch , nbran )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQTNOD (Water Quality geT Node number)
!
! Module description: Get node number for a specified gridpoint.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  1 igrid             I  gridpoint number
!  2 inode             O  node number
!  4 nbran             I  nr of branches
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqtnod.pf,v $
! Revision 1.1  1996/10/31  09:51:49  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!
   integer      igrid ,&
   &inode ,&
   &nbran
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
