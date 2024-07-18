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

 !> split polygon with line through two points (xa,ya) and (xb,yb)
 subroutine split_pol_with_line(xa, ya, xb, yb, idir)
    use m_polygon
    use m_alloc
    use m_missing
    use m_tpoly
    implicit none

    double precision, intent(in) :: xa, ya, xb, yb !< of two point on line
    integer, intent(in) :: idir !< left (1), or right (2)

    double precision :: sx, sy ! vector perpendicular to line

    integer :: imask
    integer :: i, ip1, ipol, numpols, num, numadd

    logical :: L, Lprev

    type(tpoly), dimension(:), allocatable :: pli

    if (NPL < 3) return

!   copy to tpoly-type polygons
    call pol_to_tpoly(numpols, pli, keepExisting=.false.)

!   compute a vector perpendicular to the line through (xa,ya) and (xb,yb)
    sx = -(yb - ya)
    sy = xb - xa

    if (idir == 2) then
       sx = -sx
       sy = -sy
    end if

    NPL = 0
    do ipol = 1, numpols
       num = pli(ipol)%len
       L = isleft(pli(ipol)%x(1), pli(ipol)%y(1))
       numadd = 0
       do i = 1, num
          Lprev = L
          ip1 = i + 1; if (ip1 > num) ip1 = ip1 - num
          L = isleft(pli(ipol)%x(ip1), pli(ipol)%y(ip1))
          if (L .and. Lprev) then
!            segment is internal
             imask = 1
          else if (L) then
!            segment is crossed, ingoing
             imask = 2
          else if (Lprev) then
!            segment is crossed, outgoing
             imask = 3
          else
             imask = 0
          end if

          if (imask /= 0) then
!            active segment, add start point
             if (numadd == 0 .and. NPL > 0) then
!               add seperator
                NPL = NPL + 1
                call increasepol(NPL, 1)
                xpl(NPL) = DMISS
                ypl(NPL) = DMISS
                zpl(NPL) = DMISS
             end if

             numadd = numadd + 1
             NPL = NPL + 1
             call increasepol(NPL, 1)
             if (imask == 2) then
!               ingoing, add intersection
                call intersect(pli(ipol)%x(i), pli(ipol)%y(i), pli(ipol)%x(ip1), pli(ipol)%y(ip1), xpl(NPL), ypl(NPL))
                zpl(NPL) = pli(ipol)%z(i)
             else
!               internal, add start point
                xpl(NPL) = pli(ipol)%x(i)
                ypl(NPL) = pli(ipol)%y(i)
                zpl(NPL) = pli(ipol)%z(i)
             end if
          end if

          if (imask == 3) then
!            outgoing, add intersection
             NPL = NPL + 1
             call increasepol(NPL, 1)
             call intersect(pli(ipol)%x(i), pli(ipol)%y(i), pli(ipol)%x(ip1), pli(ipol)%y(ip1), xpl(NPL), ypl(NPL))
             zpl(NPL) = pli(ipol)%z(ip1)
          end if
       end do
    end do

!   deallocate
    call dealloc_tpoly(pli)

    return

 contains

    logical function isleft(x, y) !< is left-hand side of line (given some orientation)
       double precision, intent(in) :: x, y

       isleft = .false.

       if (x /= DMISS .and. y /= DMISS) then
          if ((x - xa) * sx + (y - ya) * sy >= 0d0) then
             isleft = .true.
          end if
       end if

       return
    end function isleft

!>  intersect line segment 1-2 with line through points a and b
    subroutine intersect(x1, y1, x2, y2, xi, yi)
       implicit none

       double precision, intent(in) :: x1, y1, x2, y2 !< line start and end coordinates
       double precision, intent(out) :: xi, yi !< intersection coordinates

       double precision :: alpha, dx12, dy12, dxab, dyab

       dx12 = x2 - x1
       dy12 = y2 - y1

       dxab = xb - xa
       dyab = yb - ya

       alpha = ((xa - x1) * dyab - (ya - y1) * dxab) / (dx12 * dyab - dy12 * dxab)

       xi = x1 + alpha * dx12
       yi = y1 + alpha * dy12

       return
    end subroutine
 end subroutine split_pol_with_line
