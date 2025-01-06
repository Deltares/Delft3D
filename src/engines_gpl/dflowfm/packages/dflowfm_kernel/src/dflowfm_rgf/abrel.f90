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

module m_abrel

implicit none

private

public :: abrel

contains

      subroutine ABREL(X1, Y1, B1R, NFAC)
         use precision, only: dp

         integer :: nfac
         real(kind=dp) :: X1(NFAC + 1), Y1(NFAC + 1), B1R(NFAC + 1)
         integer :: J
         real(kind=dp) :: B1
         B1 = 0
         do J = 2, NFAC + 1
            B1 = B1 + sqrt((X1(J) - X1(J - 1))**2 + (Y1(J) - Y1(J - 1))**2)
            B1R(J) = B1
         end do

         do j = 2, NFAC + 1
            B1R(J) = B1R(J) / B1R(NFAC + 1)
         end do
         return
      end subroutine abrel

end module m_abrel
