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

      subroutine CLOSEdefinedflownode(XP1, YP1, N1) !

         use m_flowgeom
         use m_flow
         use geometry_module, only: dbdistance
         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D

         implicit none

         integer :: n1
         double precision :: XP1, YP1
         double precision :: dismin, dis
         integer :: n

         N1 = 0
         DISMIN = 9d33
         do n = 1, ndxi
            if (laydefnr(n) > 0) then
               dis = dbdistance(XP1, YP1, XZ(n), YZ(n), jsferic, jasfer3D, dmiss)
               if (dis < dismin) then
                  n1 = n; dismin = dis
               end if
            end if
         end do
      end subroutine CLOSEdefinedflownode
