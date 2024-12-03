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

!>    return y-component in link coordinate frame of vector in "klnup"-node coordinate frame
      real(kind=dp) function nodup2liny(L, ib, ux, uy)
         use precision, only: dp
         use m_flowgeom, only: csbup, snbup
         use m_sferic
         implicit none

         integer, intent(in) :: L !< flowlink number
         integer, intent(in) :: ib !< stencil index (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
         real(kind=dp), intent(in) :: ux, uy !< vector components in flownode coordinate frame

         if (jsferic /= 1 .or. jasfer3D /= 1) then
            nodup2liny = uy
         else
            nodup2liny = -snbup(ib, L) * ux + csbup(ib, L) * uy
         end if

         return
      end function nodup2liny
