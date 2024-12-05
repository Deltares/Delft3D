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

!>    return y-component in link coordinate frame of a vector in node coordinate frame
      real(kind=dp) function nod2liny(L, i12, ux, uy)
         use precision, only: dp
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none

         integer, intent(in) :: L !< flowlink number
         integer, intent(in) :: i12 !< left (1) or right (2) neighboring cell
         real(kind=dp), intent(in) :: ux, uy !< vector components in flownode coordinate frame

         if (jsferic /= 1 .or. jasfer3D /= 1) then
            nod2liny = uy
         else
            nod2liny = -snb(i12, L) * ux + csb(i12, L) * uy
         end if

         return
      end function nod2liny
