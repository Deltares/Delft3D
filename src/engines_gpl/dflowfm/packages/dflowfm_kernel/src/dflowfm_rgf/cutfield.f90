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

      subroutine CUTFIELD(X, Y, mmax, nmax, MC, NC)
         use precision, only: dp
         use m_missing
         use m_grid_block
         implicit none
         integer :: mmax, nmax, mc, nc
         real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX)
         integer :: i, j

         do I = 1, MC
            do J = 1, NC
               if (I >= MB(3) .and. I <= MB(4) .and. J >= NB(3) .and. J <= NB(4)) then
!               mooi houwen zo
               else
                  X(I, J) = XYMIS
                  Y(I, J) = 0d0
               end if
            end do
         end do
         return
      end subroutine cutfield
