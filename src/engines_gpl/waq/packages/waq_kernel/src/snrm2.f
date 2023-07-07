!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_snrm2

      implicit none

      contains


      real(8) function snrm2 ( n, sx, incx)
      integer i, incx, ix, j, n, next
      real(8)sx(1),  cutlo, cuthi, hitest, sum, xmax, zero, one
      data   zero, one /0.0e0, 1.0e0/
!
!     euclidean norm of the n-vector stored in sx() with storage
!     increment incx .
!     if    n .le. 0 return with result = 0.
!     if n .ge. 1 then incx must be .ge. 1
!
!           c.l.lawson, 1978 jan 08
!     modified to correct failure to update ix, 1/25/92.
!     modified 3/93 to return if incx .le. 0.
!
!     four phase method     using two built-in constants that are
!     hopefully applicable to all machines.
!         cutlo = maximum of  sqrt(u/eps)  over all known machines.
!         cuthi = minimum of  sqrt(v)      over all known machines.
!     where
!         eps = smallest no. such that eps + 1. .gt. 1.
!         u   = smallest positive no.   (underflow limit)
!         v   = largest  no.            (overflow  limit)
!
!     brief outline of algorithm..
!
!     phase 1    scans zero components.
!     move to phase 2 when a component is nonzero and .le. cutlo
!     move to phase 3 when a component is .gt. cutlo
!     move to phase 4 when a component is .ge. cuthi/m
!     where m = n for x() real and m = 2*n for complex.
!
!     values for cutlo and cuthi..
!     from the environmental parameters listed in the imsl converter
!     document the limiting values are as follows..
!     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
!                   univac and dec at 2**(-103)
!                   thus cutlo = 2**(-51) = 4.44089e-16
!     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
!                   thus cuthi = 2**(63.5) = 1.30438e19
!     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
!                   thus cutlo = 2**(-33.5) = 8.23181d-11
!     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
!     data cutlo, cuthi / 8.232d-11,  1.304d19 /
!     data cutlo, cuthi / 4.441e-16,  1.304e19 /
      data cutlo, cuthi / 4.441e-16,  1.304e19 /
!
      if(n .gt. 0 .and. incx.gt.0) go to 10
         snrm2  = zero
         go to 300
!
   10 assign 30 to next
      sum = zero
      i = 1
      ix = 1
!                                                 begin main loop
   20    go to next,(30, 50, 70, 110)
   30 if( abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
!
!                        phase 1.  sum is zero
!
   50 if( sx(i) .eq. zero) go to 200
      if( abs(sx(i)) .gt. cutlo) go to 85
!
!                                prepare for phase 2.
      assign 70 to next
      go to 105
!
!                                prepare for phase 4.
!
  100 continue
      ix = j
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115
!
!                   phase 2.  sum is small.
!                             scale to avoid destructive underflow.
!
   70 if( abs(sx(i)) .gt. cutlo ) go to 75
!
!                     common code for phases 2 and 4.
!                     in phase 4 sum is large.  scale to avoid overflow.
!
  110 if( abs(sx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200
!
  115 sum = sum + (sx(i)/xmax)**2
      go to 200
!
!
!                  prepare for phase 3.
!
   75 sum = (sum * xmax) * xmax
!
!
!     for real or d.p. set hitest = cuthi/n
!     for complex      set hitest = cuthi/(2*n)
!
   85 hitest = cuthi/float( n )
!
!                   phase 3.  sum is mid-range.  no scaling.
!
      do 95 j = ix, n
         if(abs(sx(i)) .ge. hitest) go to 100
         sum = sum + sx(i)**2
         i = i + incx
   95 continue
      snrm2 = sqrt( sum )
      go to 300
!
  200 continue
      ix = ix + 1
      i = i + incx
      if( ix .le. n ) go to 20
!
!              end of main loop.
!
!              compute square root and adjust for scaling.
!
      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end

      end module m_snrm2
