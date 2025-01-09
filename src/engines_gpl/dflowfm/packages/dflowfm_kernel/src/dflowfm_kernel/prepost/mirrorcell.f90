!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_mirrorcell

   implicit none

   private

   public :: mirrorcell

contains

   !> Prepares a mirror cell as candidate for an open boundary cell, and a probe point 'uitsteker' for use in selectelset().
 !! This is done by determining a representative half cell size:
 !! dis = MAX(.5*sqrt(cell area), distance from circumcenter to edge)
 !! The mirrored circumcenter is obtained by projecting cell circumcenter onto edge, and from there extend by dis (i.e., half approximate cell 'width').
 !! The probe point is also obtained by projecting cell circumcenter onto edge, and from there extend by rrtol*dis (i.e., rrtol*apprimate cell 'width')
   subroutine mirrorcell(n, x3_, y3, x4_, y4, xci, yci, xcb, ycb, xmir, ymir, xx, yy) !
      use precision, only: dp
      use m_netw ! bounday segment   !intern circumcentre, boundary circumcentre, mirrorpoint, cell corners
      use m_flowgeom
      use m_sferic
      use m_missing, only: dmiss, dxymis
      use geometry_module, only: normaloutchk, duitpl, dprodout, half, spher2locvec, xpav, dlinedis

      integer, intent(in) :: n !< cell number (in 1:nump)
      real(kind=dp), intent(in) :: x3_, y3, x4_, y4 !< net node coordinates of a boundary edge of this cell
      real(kind=dp), intent(out) :: xci, yci !< cell circumcenter coordinates for this cell (i.e., xz(n), yz(n))
      real(kind=dp), intent(out) :: xcb, ycb !< cell circumcenter coordinates for the 'mirror' cell
      real(kind=dp), intent(out) :: xmir, ymir !< 'Uitsteker' probe point, used for testing for open boundary in selectelset
      real(kind=dp), intent(out) :: xx(4), yy(4) !< Coordinates for mirrored cell contour (counter clockwise), contains x3,y3 and also x4,y4.

      real(kind=dp) :: dis, dis2, rx, ry, xd, yd
      real(kind=dp) :: x3, x4
      integer :: ja, jadismxbnd = 0

      real(kind=dp), dimension(1) :: rxloc, ryloc
      real(kind=dp) :: xref, yref
      real(kind=dp) :: x5, y5, x6, y6
      real(kind=dp) :: dout

      ! call getcellWEIGHTEDcenter( n, xci, yci, zci )
      ! edge = dbdistance(x3,y3,x4,y4)
      ! dis  = ar/edge

!fix for spherical, periodic coordinates: get external boundary points close to internal in (lon,lat)
      x3 = x3_
      x4 = x4_
      if (jsferic == 1) then
         !   x3 = x3 + floor( (180d0+xz(n)-x3)/360d0 ) * 360d0
         !   x4 = x4 + floor( (180d0+xz(n)-x4)/360d0 ) * 360d0

         x3 = x3 + nint((xz(n) - x3) / 360d0) * 360d0
         x4 = x4 + nint((xz(n) - x4) / 360d0) * 360d0
      end if

      call DLINEDIS(Xzw(n), Yzw(n), X3, Y3, X4, Y4, JA, DIS, Xd, Yd, jsferic, jasfer3D, dmiss) ! dis is half cell size in boundary normal dir
      if (jadismxbnd == 1) dis = max(dis, 0.5d0 * sqrt(ba(n)))
      ! dis = max(dis,0.5d0*sqrt(ba(n)))

! (rx,ry) outward normal in reference frame of half(x3,y3,x4,y4)
      call normaloutchk(x3, y3, x4, y4, xzw(n), yzw(n), rx, ry, ja, jsferic, jasfer3D, dmiss, dxymis)

      xci = xz(n)
      yci = yz(n)

      if (jsferic == 1 .and. jasfer3D == 1) then
         call DLINEDIS(Xci, Yci, X3, Y3, X4, Y4, JA, DIS2, Xd, Yd, jsferic, jasfer3D, dmiss) ! dis is half cell size in boundary normal dir

!   compute reference point coordinates
         call half(x3, y3, x4, y4, xref, yref, jsferic, jasfer3D)

!   get outward normal in frame reference point
         call spher2locvec(xd, yd, 1, (/xref/), (/yref/), (/rx/), (/ry/), rxloc, ryloc, jsferic, jasfer3D, dmiss)

!   xcb = xd + dis*rx
         call xpav(xd, yd, dis, rxloc(1), ryloc(1), xcb, ycb, jsferic, jasfer3D)
!   xmir = xd + 2*rrtol*dis*rx
         call xpav(xd, yd, 2d0 * rrtol * dis, rxloc(1), ryloc(1), xmir, ymir, jsferic, jasfer3D)

!   x5 = x3 + dis*rx
         call spher2locvec(x3, y3, 1, (/xref/), (/yref/), (/rx/), (/ry/), rxloc, ryloc, jsferic, jasfer3D, dmiss)
         call xpav(x3, y3, dis, rxloc(1), ryloc(1), x5, y5, jsferic, jasfer3D)

!   x6 = x4 + dis*rx
         call spher2locvec(x4, y4, 1, (/xref/), (/yref/), (/rx/), (/ry/), rxloc, ryloc, jsferic, jasfer3D, dmiss)
         call xpav(x4, y4, dis, rxloc(1), ryloc(1), x6, y6, jsferic, jasfer3D)

!   dout = rx*(y4-y3) - ry*(x4-x3)
         dout = dprodout(xd, yd, xcb, ycb, x3, y3, x4, y4, jsferic, jasfer3D) / dis
      else

         ! Convert back to sferic, if necessary
         if (jsferic == 1) then
            dis = dis * rd2dg / ra
         end if

         call DLINEDIS(Xci, Yci, X3, Y3, X4, Y4, JA, DIS2, Xd, Yd, jsferic, jasfer3D, dmiss) ! dis is half cell size in boundary normal dir

         xcb = xd + rx * dis
         ycb = yd + ry * dis
         xmir = xd + rx * 2d0 * rrtol * dis
         ymir = yd + ry * 2d0 * rrtol * dis

         x5 = x3 + dis * rx
         y5 = y3 + dis * ry

         x6 = x4 + dis * rx
         y6 = y4 + dis * ry

         dout = rx * (y4 - y3) - ry * (x4 - x3)
      end if

      !call movabs(xci, yci)
      !call lnabs(xmir, ymir)
      !call toemaar()

! store ordered contour of cell
      if (dout > 0d0) then
         xx(1) = x3; yy(1) = y3
         xx(2) = x5; yy(2) = y5
         xx(3) = x6; yy(3) = y6
         xx(4) = x4; yy(4) = y4
      else
         xx(4) = x3; yy(4) = y3
         xx(3) = x5; yy(3) = y5
         xx(2) = x6; yy(2) = y6
         xx(1) = x4; yy(1) = y4
      end if
   end subroutine mirrorcell

end module m_mirrorcell
