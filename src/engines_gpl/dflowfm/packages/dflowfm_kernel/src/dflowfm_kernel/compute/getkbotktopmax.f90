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

module m_getkbotktopmax

   implicit none

contains

   subroutine getkbotktopmax(n, kb, kt, ktx)
! Variation on getkbotktop. Always returns the maximum possible layer range instead of the actual range.
      use m_flow
      use m_flowgeom
      implicit none
      integer :: n, kb, kt, ktx
      if (kmx == 0) then
         kb = n; kt = n; ktx = n
      else
         kb = kbot(n); kt = ktop(n); ktx = kb + kmxn(n) - 1
      end if
   end subroutine getkbotktopmax

end module m_getkbotktopmax
