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

module m_chacol

implicit none

private

public :: chacol

contains

      subroutine CHACOL(X, mmax, nmax, J1, J2, NUMSPL)
         use precision, only: dp

         integer :: mmax, nmax, j1, j2, numspl
         real(kind=dp) :: X(MMAX, NMAX)
         integer :: i
         real(kind=dp) :: xh

         do I = 1, NUMSPL
            XH = X(I, J1)
            X(I, J1) = X(I, J2)
            X(I, J2) = XH
         end do
         return
      end subroutine chacol

end module m_chacol
