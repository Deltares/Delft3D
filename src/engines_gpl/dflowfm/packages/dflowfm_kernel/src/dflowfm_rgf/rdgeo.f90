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

module m_rdgeo

   implicit none

   private

   public :: rdgeo

contains

   subroutine RDGEO(xrd, yrd, xgeo, ygeo, JAPARIJS)
      use m_bessel2wgs84, only: bessel2wgs84
      use precision, only: dp
      use m_sferic

      integer :: japarijs
!
! -----------------------------------------------------------------------------
!
!     Conversion of RD-coordinates into Geographical coordinates (Bessel)
!
! -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 29 january 1991
! -----------------------------------------------------------------------------
!
!     arguments:
!     xrd    [ I ]   east-coordinate in RD system
!     yrd    [ I ]   north-coordinate in RD system
!     xgeo   [ O ]   geographical east-coordinate (degrees; decimal)
!     ygeo   [ O ]   geographical north-coordinate (degrees; decimal)
!
      real(kind=dp) :: xrd, yrd
      real(kind=dp) :: xgeo, ygeo
!
!     local variables:
!     urd    : linearly transformed xrd
!     vrd    : linearly transformed yrd
!     ugeo   : linearly transformed xgeo
!     vgeo   : linearly transformed ygeo
!
      real(kind=dp) :: urd, vrd
      real(kind=dp) :: ugeo, vgeo

      real(kind=dp) :: a01, a20, a02, a21, a03, a22, a40, a23, a41, a04, a42, a24
      real(kind=dp) :: b10, b11, b12, b30, b31, b13, b32, b14, b50, b33, b51, b15
      real(kind=dp) :: dx, dx2, dx3, dx4, dx5, xd, x0
      real(kind=dp) :: dy, dy2, dy3, dy4, dy5, yd, y0

      real(kind=dp) :: a, e, ya, xa, b0, dl0, gn, gm, rr, dk, r, sa, ca, psi, spsi
      real(kind=dp) :: cb, sb, b, sdl, dl, rl, w, q, psia, dq, phi

      integer :: k, jazitman = 1

      x0 = 155000d0
      y0 = 463000d0

      ya = 52.156160556d0
      xa = 5.387638889d0

      if (JAPARIJS == 1) then
         XRD = XRD - x0
         YRD = YRD - y0
      end if
      urd = 0.00001d0 * xrd
      vrd = 0.00001d0 * yrd

      if (jazitman == 1) then
         vgeo = 187762.178d0 + 3236.033d0 * vrd - 32.592d0 * (urd**2) - &
                0.247d0 * (vrd**2) - 0.850d0 * vrd * (urd**2) - 0.065d0 * (vrd**3) + &
                0.005d0 * (urd**4) - 0.017d0 * (urd**2) * (vrd**2)
         ugeo = 19395.500d0 + 5261.305d0 * urd + 105.979d0 * urd * vrd + &
                2.458d0 * urd * (vrd**2) - 0.819d0 * (urd**3) + &
                0.056d0 * urd * (vrd**3) - 0.056d0 * vrd * (urd**3)
         !xgeo = ugeo/3600d0
         !ygeo = vgeo/3600d0
         call bessel2wgs84(vgeo / 3600d0, ugeo / 3600d0, ygeo, xgeo)
      else if (jazitman == 2) then
         a01 = 3236.0331637d0
         a20 = -32.5915821d0
         a02 = -0.2472814d0
         a21 = -0.8501341d0
         a03 = -0.0655238d0
         a22 = -0.0171137d0
         a40 = 0.0052771d0
         a23 = -0.0003859d0
         a41 = 0.0003314d0
         a04 = 0.0000371d0
         a42 = 0.0000143d0
         a24 = -0.0000090d0

         b10 = 5261.3028966d0
         b11 = 105.9780241d0
         b12 = 2.4576469d0
         b30 = -0.8192156d0
         b31 = -0.0560092d0
         b13 = 0.0560089d0
         b32 = -0.0025614d0
         b14 = 0.0012770d0
         b50 = 0.0002574d0
         b33 = -0.0000973d0
         b51 = 0.0000293d0
         b15 = 0.0000291d0

         dx = urd; dx2 = dx * dx; dx3 = dx * dx2; dx4 = dx * dx3; dx5 = dx * dx4
         dy = vrd; dy2 = dy * dy; dy3 = dy * dy2; dy4 = dy * dy3; dy5 = dy * dy4

         yd = a01 * dy + a20 * dx2 + a02 * dy2 + a21 * dx2 * dy + a03 * dy3 + &
              a40 * dx4 + a22 * dx2 * dy2 + a04 * dy4 + a41 * dx4 * dy + &
              a23 * dx2 * dy3 + a42 * dx4 * dy2 + a24 * dx2 * dy4

         xd = b10 * dx + b11 * dx * dy + b30 * dx3 + b12 * dx * dy2 + &
              b31 * dx3 * dy + b13 * dx * dy3 + b50 * dx5 + b32 * dx3 * dy2 + &
              b14 * dx * dy4 + b51 * dx5 * dy + b33 * dx3 * dy3 + b15 * dx * dy5

         xgeo = xa + xd / 3600d0
         ygeo = ya + yd / 3600d0

      else ! SPvdP: may not be accurate

         a = 6377397.155d0
         e = 0.081696831222d0
         b0 = 52.121097249d0
         dl0 = xa
         gn = 1.00047585668d0
         gm = 0.003773953832d0
         rr = 6382644.571d0
         dk = 0.999079d0

         r = sqrt(xrd * xrd + yrd * yrd)
         sa = xrd / r
         ca = yrd / r
         psi = 2d0 * atan2(r, 2d0 * dk * rr)
         spsi = sin(psi)
         cb = cos(dg2rd * b0)
         sb = ca * cb * spsi + sin(dg2rd * b0) * cos(psi)
         b = asin(sb)

         sdl = sa * spsi / cb
         dl = asin(sdl)
         rl = rd2dg * dl / gn + xa
         w = atanh(sb)
         do k = 1, 4
            q = (w - gm) / gn
            psia = 2d0 * atan(exp(q)) - 0.5d0 * pi
            dq = e * atanh(e * sin(psia))
            phi = asin(tanh(q + dq))
         end do

         ygeo = phi * rd2dg
         xgeo = rl

      end if

!

      continue
      return
   end subroutine RDGEO

end module m_rdgeo
