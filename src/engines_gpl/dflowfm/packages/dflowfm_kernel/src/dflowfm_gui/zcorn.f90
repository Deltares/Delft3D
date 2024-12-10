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

module m_zcorn

   implicit none

contains

   real(kind=dp) function zcorn(k) ! get various values at flow cell corners
      use precision, only: dp
      use m_flowgeom
      use m_drawthis
      implicit none

      integer :: k, nodval

      nodval = ndraw(31)
      if (nodval == 2) then
         zcorn = ucnx(k)
      else if (nodval == 3) then
         zcorn = ucny(k)
      else if (nodval == 4) then
         zcorn = sqrt(ucnx(k) * ucnx(k) + ucny(k) * ucny(k))
      end if
   end function zcorn

end module m_zcorn
