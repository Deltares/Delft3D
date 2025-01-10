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

module m_nums

   implicit none

   private

   public :: nums

contains

   subroutine NUMS(X, mmax, nmax, MC, NC)
      use precision, only: dp
!     GEEF AANTAL SPLINES MC EN MAXIMUM AANTAL PUNTEN OP SPLINE NC
!      USE DIMENS
      use m_numpold

      integer :: mc, nc, mmax, nmax
      real(kind=dp) :: X(MMAX, NMAX)
      integer :: i, numpi
      MC = 0
      NC = 0
      do I = 1, MMAX
         call NUMPold(X, mmax, nmax, I, NUMPI)
         if (NUMPI /= 0) then
            MC = I
            NC = max(NC, NUMPI)
         end if
      end do
      return
   end subroutine nums

end module m_nums
