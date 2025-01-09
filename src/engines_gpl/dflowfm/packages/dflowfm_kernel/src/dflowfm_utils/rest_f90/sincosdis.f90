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

module m_sincosdis

   implicit none

   private

   public :: sincosdis

contains

   subroutine sincosdis(x1, y1, x2, y2, s, c, d) ! get sin, cos, length of a line segment
      use precision, only: dp
      use m_missing
      use m_sferic, only: jsferic
      use geometry_module, only: getdx, getdy

      real(kind=dp) :: x1, y1, x2, y2, s, c, d
      real(kind=dp) :: dx1, dy1

      dx1 = getdx(x1, y1, x2, y2, jsferic)
      dy1 = getdy(x1, y1, x2, y2, jsferic)
      d = sqrt(dx1 * dx1 + dy1 * dy1)
      if (d > 0d0) then
         s = dy1 / d
         c = dx1 / d
      else
         s = 0d0
         c = 0d0
      end if
   end subroutine sincosdis

end module m_sincosdis
