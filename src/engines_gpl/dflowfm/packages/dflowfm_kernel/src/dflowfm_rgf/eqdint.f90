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

      subroutine EQDINT(YH2, imax, TJ, Y2)
         use precision, only: dp
         implicit none
         integer :: imax
         real(kind=dp) :: YH2(imax)
         real(kind=dp) :: TJ, Y2
         integer :: j1, j2
         real(kind=dp) :: T1, T2
         J1 = int(TJ) + 1
         J2 = J1 + 1
         T1 = TJ - int(TJ)
         T2 = 1 - T1
         Y2 = T2 * YH2(J1) + T1 * YH2(J2)
         return
      end subroutine EQDINT
