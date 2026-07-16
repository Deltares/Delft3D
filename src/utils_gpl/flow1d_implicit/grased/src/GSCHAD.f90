subroutine gschad (ngrid ,nfrac ,igr   ,preq ,ddis ,pfrac ,dreq)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gschad.F,v $
! Revision 1.2  1995/09/27  10:11:52  kuipe_j
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
   real       preq   ,dreq
   real       ddis   (nfrac+1)     ,pfrac (ngrid,nfrac)
!
!     Declaration of local parameters
!
   integer    if   ,if1
   real       sp   ,theta
!
!     Find fraction that contains requested probability.
!     Summation of probabilty over fractions.
!
   sp = 0.
   if = 0

10 if (sp .lt. preq .and. if .lt. nfrac) then
      if = if +1
      sp = sp + pfrac(igr,if)
      goto 10
   endif
!
!     Calculate requested grain size by interpolation in found
!     fraction. Interpolation in grain size is logarithmic and
!     in probability linear.
!
   theta = 1. + (preq - sp) / pfrac(igr,if)
   if1   = if + 1
   dreq  = (ddis(if1)/ddis(if))**theta * ddis(if)

end
