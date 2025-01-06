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

module m_makey

implicit none

private

public :: makey

contains

      subroutine MAKEY(XR, YR, MMAX, NMAX) ! terug naar graden SUBROUTINE MAKEY
         use precision, only: dp
         use M_SFERIC
         use M_MISSING

         integer :: mmax, nmax

         real(kind=dp) :: XR(MMAX, NMAX), YR(MMAX, NMAX), FI2
         integer :: i, j

         do I = 1, MMAX
            do J = 1, NMAX
               if (XR(I, J) /= DXYMIS) then
                  FI2 = atan(sinh(YR(I, J)))
                  YR(I, J) = RD2DG * FI2
                  XR(I, J) = RD2DG * XR(I, J)
               end if
            end do
         end do
         return
      end subroutine MAKEY

end module m_makey
