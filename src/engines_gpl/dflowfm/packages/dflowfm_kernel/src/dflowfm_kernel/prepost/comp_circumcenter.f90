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

!> compute the "best-fit" circumcenter of a polygon
module m_comp_circumcenter

implicit none

private

public :: comp_circumcenter

contains

subroutine comp_circumcenter(N, xp, yp, xf, yf, xc, yc)
   use m_sferic
   use m_missing
   use geometry_module, only: pinpok, getdxdy, getdx, getdy, cross, normalin
   use m_qnerror

   integer, intent(in) :: N !< polygon dimension
   double precision, dimension(N), intent(in) :: xp, yp !< polygon node coordinates
   double precision, dimension(N), intent(in) :: xf, yf !< face coordinates
   double precision, intent(out) :: xc, yc !< circumcenter coordinates

   double precision, dimension(N) :: tx, ty
   double precision, dimension(2, 2) :: T
   double precision, dimension(2) :: rhs

   double precision :: xhalf, yhalf, dfac, x0, y0, det

   double precision :: xzw, yzw, SL, SM, XCR, YCR, CRP
   double precision :: xref, yref

   integer :: i, in, ip1

   integer :: JACROS, m, m2

   integer, parameter :: jacenterinside = 1

   if (jsferic == 1 .and. jasfer3D == 1) then
      call qnerror('comp_circumcenter not supported for jasfer3D=1', ' ', ' ')
   end if

   x0 = DMISS
   y0 = DMISS

   if (N < 2) goto 1234

   x0 = minval(xp(1:N))
   y0 = minval(yp(1:N))

!  compute the tangent vectors
   do i = 1, N
      ip1 = i + 1; if (ip1 > N) ip1 = ip1 - N
      xref = 0d0 ! not ok for jasfer3D=1
      yref = 0d0 ! not ok for jasfer3D=1
      call normalin(xp(i), yp(i), xp(ip1), yp(ip1), tx(i), ty(i), xref, yref, jsferic, jasfer3D, dxymis)
   end do

!  make the matrix
   T = 0d0
   do i = 1, N
      T(1, 1) = T(1, 1) + tx(i) * tx(i)
      T(1, 2) = T(1, 2) + tx(i) * ty(i)
      T(2, 1) = T(2, 1) + ty(i) * tx(i)
      T(2, 2) = T(2, 2) + ty(i) * ty(i)
   end do

!  make the right-hand side
   rhs = 0d0
   do i = 1, N
      ip1 = i + 1; if (ip1 > N) ip1 = ip1 - N
!      xhalf = 0.5d0*(getdx(x0,y0,xp(i),yp(i)) + getdx(x0,y0,xp(ip1),yp(ip1)))
!      yhalf = 0.5d0*(getdy(x0,y0,xp(i),yp(i)) + getdy(x0,y0,xp(ip1),yp(ip1)))

      call getdxdy(x0, y0, xf(i), yf(i), xhalf, yhalf, jsferic)
      !xhalf = getdx(x0,y0,xf(i),yf(i))
      !yhalf = getdy(x0,y0,xf(i),yf(i))

      dfac = tx(i) * xhalf + ty(i) * yhalf
      rhs(1) = rhs(1) + tx(i) * dfac
      rhs(2) = rhs(2) + ty(i) * dfac
   end do

!  solve the system
   det = T(1, 1) * T(2, 2) - T(1, 2) * T(2, 1)
   if (abs(det) < 1d-8) goto 1234

   xc = (T(2, 2) * rhs(1) - T(1, 2) * rhs(2)) / det
   yc = (-T(2, 1) * rhs(1) + T(1, 1) * rhs(2)) / det

   if (JSFERIC /= 0) then
      yc = yc / (Ra * dg2rd)
      xc = xc / (Ra * dg2rd * cos((yc + y0) * dg2rd))
   end if

   xc = xc + x0
   yc = yc + y0

   if (jacenterinside == 1) then
      call pinpok(xc, yc, N, xp, yp, in, jins, dmiss) ! circumcentre may not lie outside cell
      if (in == 0) then
         xzw = sum(xp(1:N)) / dble(N)
         yzw = sum(yp(1:N)) / dble(N)
         do m = 1, N
            m2 = m + 1; if (m == N) m2 = 1
            call CROSS(xzw, yzw, xc, yc, xp(m), yp(m), xp(m2), yp(m2), &
                       JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
            if (jacros == 1) then
               xc = 0.5d0 * (xp(m) + xp(m2)) ! xcr
               yc = 0.5d0 * (yp(m) + yp(m2)) ! ycr
               exit
            end if
         end do
      end if
   end if

1234 continue

   return
end subroutine comp_circumcenter

end module m_comp_circumcenter
