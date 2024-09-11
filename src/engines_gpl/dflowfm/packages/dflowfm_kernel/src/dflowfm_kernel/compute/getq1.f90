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
module m_getq1
   implicit none
contains
   subroutine getq1(hg, d, h1, h2, q) ! energiebehoud bovenstrooms
      double precision :: hg, d, h1, h2, q
      double precision :: g, t, r, tr

      g = 9.81d0
      t = 2d0 * g * (h1 - h2)
      r = 1d0 / hg**2 - 1d0 / (h1 + d)**2
      tr = t / r
      if (tr > 0) then
         q = sqrt(tr)
      else
         q = h1 * sqrt(g * h1)
      end if
   end subroutine getq1
end module m_getq1
