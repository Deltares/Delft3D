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

module m_rsort3new

implicit none

private

public :: rsort3new

contains

      subroutine RSORT3new(X, Y, Z, N)
         use precision, only: dp
         use stdlib_sorting, only: sort_index

         integer :: k, n
         real(kind=dp) :: X(N), Y(N), Z(N)
         integer, allocatable :: ind(:)
         real(kind=dp), allocatable :: h(:)

         allocate (ind(n), h(n))

         call sort_index(x, ind)

         h = y
         do k = 1, n
            y(k) = h(ind(k))
         end do

         h = z
         do k = 1, n
            z(k) = h(ind(k))
         end do

         deallocate (ind, h)

      end subroutine RSORT3new

end module m_rsort3new
