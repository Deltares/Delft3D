subroutine flausm (nbran  ,ngrid  ,branch  ,hq1  ,hq2, alfa)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAUSM (FLow AUtostart SMoothing)
!
! Module description: Smoothing of water levels or discharges
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  4 hq1(ngrid)        IO (to be) smoothed array
!  5 hq2(ngrid)        IO scratch array
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flausm.pf,v $
! Revision 1.2  1995/10/18  08:59:14  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.1  1995/09/22  10:00:52  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer    nbran ,ngrid
   integer    branch(4,nbran)
   real       alfa
   double precision hq1(ngrid)     ,hq2(ngrid)
!
!     Declaration of local variables
!
   integer    i1  ,i2  ,ibr   ,igr
   real       alfa1

   alfa1 = (1.-alfa)*.5
   do 30 ibr = 1, nbran
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
      do 10 igr = i1+1, i2-1
         hq2(igr) = alfa1*hq1(igr-1) + alfa*hq1(igr)&
         &+ alfa1*hq1(igr+1)
10    continue
      do 20 igr = i1+1, i2-1
         hq1(igr) = hq2(igr)
20    continue
30 continue

end

