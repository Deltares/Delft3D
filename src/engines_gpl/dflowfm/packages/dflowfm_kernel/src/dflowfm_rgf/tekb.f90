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

module m_tekb

implicit none

private

public :: tekb

contains

      subroutine TEKB(X, Y, MMAX, NMAX, NCOL)
         use precision, only: dp
         use m_grid_block
         use m_cirr
         use m_tekln2

         integer :: mmax, nmax, ncol
         real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX)
         integer :: i

         if (ITYPE == 1) then
            call TEKLN2(X, Y, mmax, nmax, MB(1), NB(1), MB(2), NB(2), NCOL)
         end if
         do I = 1, 6
            if (MB(I) /= 0) then
               call CIRR(X(MB(I), NB(I)), Y(MB(I), NB(I)), NCOL)
            end if
         end do
         return
      end subroutine tekb

end module m_tekb
