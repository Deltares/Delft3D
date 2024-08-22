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

subroutine Swart(Tsig, uorbu, z00, fw, ustw2)

   implicit none
   double precision :: Tsig, uorbu, z00, fw, ustw2
   double precision :: astar

   if (uorbu == 0d0) then
      fw = 0d0; ustw2 = 0d0; return
   end if

   astar = Tsig * uorbu / z00
   if (astar > 296.088d0) then ! 30pipi
      fw = 0.00251d0 * exp(14.1d0 / (astar**0.19d0)) ! astar=Tuorb/z00
   else
      fw = 0.3d0
   end if

   ustw2 = 0.5d0 * fw * uorbu * uorbu

end subroutine Swart
