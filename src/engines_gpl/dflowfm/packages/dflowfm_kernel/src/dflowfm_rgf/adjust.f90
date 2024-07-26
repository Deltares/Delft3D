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

      subroutine ADJUST(X, Y, mmax, nmax, MC, NC)
         use m_missing
         implicit none
         integer :: mmax, nmax, mc, nc
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX)
! TODO: Z not present, no filling with dmiss [AvD]
!     schuif data naar links en of beneden en geef nieuwe MC,NC

         integer :: i, j, ifirst, jfirst
         double precision, allocatable :: XH(:, :), YH(:, :)
         allocate (xh(MMAX, NMAX), YH(MMAX, NMAX))

         xh = x
         yh = y
         x = xymis
         y = xymis

         IFIRST = 0
         do I = 1, MC
            do J = 1, NC
               if (XH(I, J) /= XYMIS .and. IFIRST == 0) IFIRST = I
            end do
         end do

         JFIRST = 0
         do J = 1, NC
            do I = 1, MC
               if (XH(I, J) /= XYMIS .and. JFIRST == 0) JFIRST = J
            end do
         end do

         if (IFIRST == 0 .or. JFIRST == 0) then
            MC = 0
            NC = 0
         else
            IFIRST = IFIRST - 1
            JFIRST = JFIRST - 1
            do I = 1, MC - IFIRST
               do J = 1, NC - JFIRST
                  X(I, J) = XH(I + IFIRST, J + JFIRST)
                  Y(I, J) = YH(I + IFIRST, J + JFIRST)
               end do
            end do
            call NUMS(X, mmax, nmax, MC, NC)
         end if

         deallocate (xh, yh)
         return
      end
