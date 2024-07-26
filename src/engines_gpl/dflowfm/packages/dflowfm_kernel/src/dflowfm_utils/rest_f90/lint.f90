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

      subroutine LINT(X, Y, N, TV, XV, YV)
         implicit none
         integer :: n
         integer :: n1
         integer :: n2
         integer :: ntv
         double precision :: t
         double precision :: tv
         double precision :: xv
         double precision :: yv
!     Lineaire interpolatie op TV in lijn
         double precision :: X(N), Y(N)
         NTV = int(TV)
         T = TV - NTV
         N1 = NTV + 1
         N2 = N1 + 1
         XV = (1 - T) * X(N1) + T * X(N2)
         YV = (1 - T) * Y(N1) + T * Y(N2)
         return
      end subroutine LINT
