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

!> create pillar grid in polygon
module m_pillargrid

   implicit none

   private

   public :: pillargrid

contains

   subroutine pillargrid(ierror)
      use precision, only: dp
      use m_grid
      use m_gridsettings
      use m_polygon
      use m_missing
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dbdistance, get_startend
      use m_increase_grid

      integer, intent(out) :: ierror ! error (1) or not (0)

      integer :: i, j, jstart, jend, num, ipol

      real(kind=dp) :: R0, R1, x0, y0, x1, y1, alpha, beta

      ierror = 1

      if (NPL < 3) goto 1234

!  get the first polygon
      call get_startend(NPL, XPL, YPL, jstart, jend, dmiss)

!  number of points in the polygon
      num = jend - jstart + 1
      if (num < 2) goto 1234 ! we need at least two points in the polygon

!  set the grid sizes
      mc = num + 1
      nc = nfac + 1

      call increasegrid(mc, nc)

      xc = DMISS
      yc = DMISS

!  construct the grid
      R0 = pil_rad
      x0 = pil_x
      y0 = pil_y

      do i = 1, mc
!     get the coordinates of the point on the polyline
         ipol = jstart + i - 1
         if (ipol > jend) ipol = ipol - num
         x1 = xpl(ipol)
         y1 = ypl(ipol)

!     make the gridline from the pillar to the polygon point
         do j = 1, nc
            R1 = dbdistance(x0, y0, x1, y1, jsferic, jasfer3D, dmiss)
!        determine relative position on the gridline
!        uniform:
            alpha = dble(j - 1) / dble(nc - 1)
            beta = (1d0 - alpha) * R0 / R1 + alpha

            xc(i, j) = x0 + beta * (x1 - x0)
            yc(i, j) = y0 + beta * (y1 - y0)
         end do ! do j=1,nc
      end do ! do i=1,mc

      ierror = 0

!  error handling
1234  continue

      return
   end subroutine pillargrid

end module m_pillargrid
