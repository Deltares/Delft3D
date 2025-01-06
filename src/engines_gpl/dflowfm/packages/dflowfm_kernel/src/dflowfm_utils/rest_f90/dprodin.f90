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

module m_dprodin

implicit none

private

public :: dprodin

contains

 real(kind=dp) function dprodin(x1, y1, x2, y2, x3, y3, x4, y4) ! inner product of two segments
    use precision, only: dp
    use m_missing
    use m_sferic
    use geometry_module, only: getdx, getdy, sphertoCart3D

    real(kind=dp) :: x1, y1, x2, y2, x3, y3, x4, y4
    real(kind=dp) :: dx1, dy1, dx2, dy2

    real(kind=dp), dimension(4) :: xx, yy, zz
    real(kind=dp) :: dz1, dz2

    if (jsferic == 1 .and. jasfer3D == 1) then
       call sphertocart3D(x1, y1, xx(1), yy(1), zz(1))
       call sphertocart3D(x2, y2, xx(2), yy(2), zz(2))
       call sphertocart3D(x3, y3, xx(3), yy(3), zz(3))
       call sphertocart3D(x4, y4, xx(4), yy(4), zz(4))

       dx1 = xx(2) - xx(1)
       dy1 = yy(2) - yy(1)
       dz1 = zz(2) - zz(1)

       dx2 = xx(4) - xx(3)
       dy2 = yy(4) - yy(3)
       dz2 = zz(4) - zz(3)

       dprodin = dx1 * dx2 + dy1 * dy2 + dz1 * dz2
    else

       dx1 = getdx(x1, y1, x2, y2, jsferic)
       dx2 = getdx(x3, y3, x4, y4, jsferic)

       dy1 = getdy(x1, y1, x2, y2, jsferic)
       dy2 = getdy(x3, y3, x4, y4, jsferic)

       dprodin = (dx1 * dx2 + dy1 * dy2)
    end if

    return
 end function dprodin

end module m_dprodin
