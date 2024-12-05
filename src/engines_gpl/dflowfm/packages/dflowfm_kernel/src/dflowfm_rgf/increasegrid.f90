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
module m_increase_grid
   implicit none
contains
   subroutine INCREASEGRID(M, N)
      use M_GRID
      use M_MISSING, only: dxymis
      use m_alloc

      integer :: m, n

      integer, dimension(2) :: ibounds, iboundsp1

      !if (m <= mmax .and. n <= nmax) return
      !Freshly allocate arrays, so that size fits exactly (e.g., for passing as 2D arrays to ecrrea)
!      if (allocated(xc)) deallocate (xc,yc,zc,ijc,ijyes)

!      mmax = m ; nmax = n ; MNMAX = MAX(M,N)
!      ibounds   = (/ mmax, nmax /)
!      iboundsp1 = (/ mmax+1, nmax+1 /)

      mmax = m + 1; nmax = n + 1; MNMAX = max(M, N)
      ibounds = (/mmax, nmax/)
      iboundsp1 = ibounds

      call realloc(xc, ibounds, fill=dxymis)
      call realloc(yc, ibounds, fill=dxymis)
      call realloc(zc, iboundsp1, fill=dxymis)
      call realloc(ijc, ibounds, fill=0)
      call realloc(ijyes, ibounds, fill=0)

   end subroutine INCREASEGRID
end module m_increase_grid
