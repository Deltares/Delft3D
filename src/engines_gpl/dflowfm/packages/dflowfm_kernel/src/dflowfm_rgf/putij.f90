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
module m_put_ij
   implicit none
contains
      subroutine PUTIJ(X, XH, MMAX, NMAX, MNMAX, I1, I2, J1, J2)
         integer :: i
         integer :: i1
         integer :: i2
         integer :: j
         integer :: j1
         integer :: j2
         integer :: k
         integer :: mmax
         integer :: mnmax
         integer :: nmax
!     EN ZET HEM WEER TERUG

         double precision :: X(MMAX, NMAX), XH(MNMAX)
         K = 0
         do J = J1, J2
            do I = I1, I2
               K = K + 1
               X(I, J) = XH(K)
            end do
         end do
         return
      end subroutine PUTIJ
end module m_put_ij
