      subroutine gschad (ngrid ,nfrac ,igr   ,preq ,ddis ,pfrac ,dreq)

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gschad.F,v $
c Revision 1.2  1995/09/27  10:11:52  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Graded Sediment calculate CHAracteristic grain Diameter

c
c     Declaration of parameters
c
      integer    nfrac  ,ngrid  ,igr
      real       preq   ,dreq
      real       ddis   (nfrac+1)     ,pfrac (ngrid,nfrac)
c
c     Declaration of local parameters
c
      integer    if   ,if1
      real       sp   ,theta
c
c     Find fraction that contains requested probability.
c     Summation of probabilty over fractions.
c
      sp = 0.
      if = 0

  10  if (sp .lt. preq .and. if .lt. nfrac) then
         if = if +1
         sp = sp + pfrac(igr,if)
         goto 10
      endif
c
c     Calculate requested grain size by interpolation in found
c     fraction. Interpolation in grain size is logarithmic and
c     in probability linear.
c
      theta = 1. + (preq - sp) / pfrac(igr,if)
      if1   = if + 1
      dreq  = (ddis(if1)/ddis(if))**theta * ddis(if)

      end
