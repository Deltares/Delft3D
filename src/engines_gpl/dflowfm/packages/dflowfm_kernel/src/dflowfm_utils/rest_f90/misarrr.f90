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

module m_misarrr

implicit none

private

public :: misarrr

contains

      subroutine MISARRR(H, NUMQ, MMAX, NMAX)
         use precision, only: dp
         use M_MISSING

         integer :: i
         integer :: j
         integer :: k
         integer :: mmax
         integer :: nmax
         integer :: numq
         real(kind=dp) :: H(NUMQ, MMAX, NMAX)

         do K = 1, NUMQ
            do I = 1, MMAX
               do J = 1, NMAX
                  H(K, I, J) = dmiss
               end do
            end do
         end do
         return
      end

end module m_misarrr
