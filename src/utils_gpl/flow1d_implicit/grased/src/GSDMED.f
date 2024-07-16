      subroutine gsdmed (ngrid ,nfrac ,igr   ,dfrac ,pfrac ,dm)

c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdmed.F,v $
c Revision 1.2  1995/09/27  10:12:10  kuipe_j
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
      real       dm
      real       dfrac  (nfrac)     ,pfrac (ngrid,nfrac)
c
c     Declaration of local parameters
c
      integer    if
c
      dm = 0.

      do 10 if=1,nfrac
         dm = dm + pfrac(igr,if) * dfrac(if)
   10 continue

      end
