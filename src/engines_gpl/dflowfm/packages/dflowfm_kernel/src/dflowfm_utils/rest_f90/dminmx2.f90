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

      subroutine DMINMX2(X, XMIN, XMAX, MC, NC, MMAX, NMAX)
         use M_MISSING
         implicit none
         integer :: i
         integer :: j
         integer :: mc
         integer :: mmax
         integer :: nc
         integer :: nmax
         double precision :: xmax
         double precision :: xmin
         double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN TWEEDIMENSIONALE ARRAY
         double precision :: X(MMAX, NMAX)
         if (MC == 0 .or. NC == 0) then
            XMIN = 0
            XMAX = 0
            return
         end if
         XMIN = 10d20
         XMAX = -10d20
         do I = 1, MC
            do J = 1, NC
               XX = X(I, J)
               if (XX /= DXYMIS) then
                  XMIN = min(XX, XMIN)
                  XMAX = max(XX, XMAX)
               end if
            end do
         end do
         if (XMIN == 10d20) XMIN = 0
         if (XMAX == -10d20) XMAX = 0
         return
      end
