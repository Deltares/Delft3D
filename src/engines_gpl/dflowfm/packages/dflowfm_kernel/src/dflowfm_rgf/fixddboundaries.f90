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

module m_fixddboundaries

   implicit none

   private

   public :: fixddboundaries

contains

   subroutine FIXDDBOUNDARIES()
      use m_grid

      integer :: i
      integer :: m
      integer :: m1
      integer :: m2
      integer :: mb(80)
      integer :: md
      integer :: n
      integer :: n1
      integer :: n2
      integer :: nb(80)
      integer :: nd
      integer :: npt

      do I = 1, NPT - 1, 2
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I + 1)
         N2 = NB(I + 1)
         if (M1 > M2 .or. N1 > N2) then
            MB(I) = M2
            NB(I) = N2
            MB(I + 1) = M1
            NB(I + 1) = N1
         end if
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I + 1)
         N2 = NB(I + 1)
         MD = M2 - M1
         ND = N2 - N1
         if (MD > 0) then
            N = N1
            do m = M1, M2
               IJC(M, N) = -IJC(M, N)
            end do
         else if (ND > 0) then
            M = M1
            do N = N1, N2
               IJC(M, N) = -IJC(M, N)
            end do
         end if
      end do
      return
   end subroutine FIXDDBOUNDARIES

end module m_fixddboundaries
