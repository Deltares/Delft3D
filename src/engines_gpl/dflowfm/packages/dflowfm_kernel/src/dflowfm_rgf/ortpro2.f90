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

module m_ortpro2

   implicit none

   private

   public :: ortpro2

contains

   subroutine ORTPRO2(X1, Y1, X2, Y2, X3, Y3, X4, Y4, TV, JA)
      use precision, only: dp

      real(kind=dp) :: X1, Y1, X2, Y2, X3, Y3, X4, Y4, TV
      integer :: JA

      real(kind=dp) :: DX, DY, R2

      JA = -1
      DX = X2 - X1
      DY = Y2 - Y1
      R2 = (DX * DX + DY * DY)
      TV = (X3 * DX + Y3 * DY - X1 * DX - Y1 * DY) / R2
      X4 = X1 + TV * DX
      Y4 = Y1 + TV * DY
      if (0d0 <= TV .and. TV <= 1d0) JA = 1
      TV = TV * sqrt(R2)
      return
   end subroutine ORTPRO2

end module m_ortpro2
