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

module m_d1arrows

   implicit none

contains

   subroutine D1ARROWS(X, Y, Z, U, V, W, PSI0, VFAC)
      use precision, only: dp
      use m_arrows
      use m_three_two

      real(kind=dp) :: psi0
      real(kind=dp) :: vfac
      real(kind=dp) :: X, Y, Z, U, V, W
      real(kind=dp) XD, YD, ZD, XP, YP, ZP, &
         UD, VD, WD, UR, VR, WR
      XD = X
      YD = Y
      ZD = Z
      UD = U
      VD = V
      WD = W
      call DRIETWEE(XD, YD, ZD, XP, YP, ZP)
      call DRIETWEE(UD, VD, WD, UR, VR, WR)
      call ARROWS(XP, YP, UR, VR, PSI0, VFAC)
      return
   end subroutine D1ARROWS

end module m_d1arrows
