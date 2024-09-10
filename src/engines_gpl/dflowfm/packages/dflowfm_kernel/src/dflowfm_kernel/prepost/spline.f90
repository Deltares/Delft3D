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
module m_spline
   implicit none
contains

   subroutine SPLINE(Y, N, Y2)
      integer :: i
      integer :: k
      integer :: n
      double precision :: Y(N), Y2(N)
      double precision, allocatable :: U(:)
      double precision :: P

      allocate (U(N))

      Y2(1) = 0.d0
      U(1) = 0.d0

      do I = 2, N - 1
         P = 0.5d0 * Y2(I - 1) + 2d0
         Y2(I) = -0.5d0 / P
         U(I) = (6d0 * ((Y(I + 1) - Y(I)) - (Y(I) - Y(I - 1))) / 2d0 - 0.5d0 * U(I - 1)) / P
      end do

      Y2(N) = 0.d0

      do K = N - 1, 1, -1
         Y2(K) = Y2(K) * Y2(K + 1) + U(K)
      end do

      deallocate (U)
      return
   end subroutine SPLINE
end module m_spline
