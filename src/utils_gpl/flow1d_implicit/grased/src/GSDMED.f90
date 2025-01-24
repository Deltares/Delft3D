subroutine gsdmed (ngrid ,nfrac ,igr   ,dfrac ,pfrac ,dm)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdmed.F,v $
! Revision 1.2  1995/09/27  10:12:10  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Graded Sediment calculate CHAracteristic grain Diameter

!
!     Declaration of parameters
!
   integer    nfrac  ,ngrid  ,igr
   real       dm
   real       dfrac  (nfrac)     ,pfrac (ngrid,nfrac)
!
!     Declaration of local parameters
!
   integer    if
!
   dm = 0.

   do 10 if=1,nfrac
      dm = dm + pfrac(igr,if) * dfrac(if)
10 continue

end
