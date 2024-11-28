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

module m_adjacent
use m_closeenough, only: closeenough

implicit none

private

public :: adjacent

contains

  subroutine adjacent(x1, y1, x2, y2, x3, y3, x4, y4, ja, k1k, k2k)

     use m_missing, only: dmiss
     use m_sferic, only: jsferic, jasfer3D
     use geometry_module, only: dbdistance, dlinedis

     integer :: jac
     double precision :: x1, y1, x2, y2, x3, y3, x4, y4
     integer :: ja, k1k, k2k

     double precision :: r1, r2, rm, xd, yd, xm, ym, dis1
     integer :: ja1

     k1k = 0
     k2k = 0
     ja = 0

     if (x1 == x2 .and. y1 == y2 .or. &
         x3 == x4 .and. y3 == y4) return

     r1 = dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss)
     r2 = dbdistance(x3, y3, x4, y4, jsferic, jasfer3D, dmiss)
     rm = 0.4d0 * min(r1, r2)
     if (r1 <= r2) then
        xm = 0.5d0 * (x1 + x2); ym = 0.5d0 * (y1 + y2)
        call DLINEDIS(Xm, Ym, X3, Y3, X4, Y4, JA1, DIS1, Xd, Yd, jsferic, jasfer3D, dmiss)
        if (ja1 == 1 .and. dis1 < rm) then
           ja = 1
        end if
     else
        xm = 0.5d0 * (x3 + x4); ym = 0.5d0 * (y3 + y4)
        call DLINEDIS(Xm, Ym, X1, Y1, X2, Y2, JA1, DIS1, Xd, Yd, jsferic, jasfer3D, dmiss)
        if (ja1 == 1 .and. dis1 < rm) then
           ja = 1
        end if
     end if

     if (ja == 1) then
        call closeenough(x1, y1, x3, y3, rm, jac)
        if (jac == 1) then
           k1k = 1
        else
           call closeenough(x1, y1, x4, y4, rm, jac)
           if (jac == 1) then
              k1k = 2
           end if
        end if
        call closeenough(x2, y2, x3, y3, rm, jac)
        if (jac == 1) then
           k2k = 1
        else
           call closeenough(x2, y2, x4, y4, rm, jac)
           if (jac == 1) then
              k2k = 2
           end if
        end if
     end if

  end subroutine adjacent

end module m_adjacent
