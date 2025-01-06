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

module m_linemirror
use m_shifxy, only: shifxy

implicit none

private

public :: linemirror

contains

      subroutine LINEMIRROR() !X, Y, mmax, nmax, MC, NC, IJC,IJYES)
         use precision, only: dp
         use m_missing
         use m_grid
         use m_gridsettings
         use unstruc_colors
         use m_grid_block
         use m_qnerror
         use m_okay
         use m_isitu

!      integer :: mmax, nmax, mc, nc
!      real(kind=dp) :: X(MMAX,NMAX), Y(MMAX,NMAX)
!      INTEGER IJC(MMAX,NMAX), IJYES(MMAX,NMAX)

         integer :: M1, M2, N1, N2, MD, ND, M, N
         real(kind=dp) :: A, B

         call ISITU()

         M1 = MB(1)
         N1 = NB(1)
         M2 = MB(2)
         N2 = NB(2)
         MD = M2 - M1
         ND = N2 - N1
         A = 1 + FACMIR
         B = -FACMIR

         if (MD == 0) then
            if (M1 == MC) then
               if (M1 >= MMAX - 1) then
                  call OKAY(0)
                  call QNERROR('TOO MANY GRIDLINES IN M-DIRECTION', ' ', ' ')
                  return
               end if
               MC = MC + 1
            else
               if (M1 == 1) then
                  call SHIFXY(1, 0, M1, N1) ! X, Y, mmax, nmax, MC, NC,
               end if
            end if
            M = M1
            do N = N1, N2
               if (Xc(M, N) /= XYMIS) then
                  if (Xc(M + 1, N) == XYMIS) then
                     if (Xc(M - 1, N) /= XYMIS) then
                        Xc(M + 1, N) = A * Xc(M, N) + B * Xc(M - 1, N)
                        Yc(M + 1, N) = A * Yc(M, N) + B * Yc(M - 1, N)
                     end if
                  else if (Xc(M - 1, N) == XYMIS) then
                     if (Xc(M + 1, N) /= XYMIS) then
                        Xc(M - 1, N) = A * Xc(M, N) + B * Xc(M + 1, N)
                        Yc(M - 1, N) = A * Yc(M, N) + B * Yc(M + 1, N)
                     end if
                  end if
               end if
            end do
         else if (ND == 0) then
            if (N1 == NC) then
               if (N1 >= NMAX - 1) then
                  call OKAY(0)
                  call QNERROR('TOO MANY GRIDLINES IN N-DIRECTION', ' ', ' ')
                  return
               end if
               NC = NC + 1
            else
               if (N1 == 1) then
                  call SHIFXY(0, 1, M1, N1) ! X, Y, mmax, nmax, MC, NC,
               end if
            end if
            N = N1
            do M = M1, M2
               if (Xc(M, N) /= XYMIS) then
                  if (Xc(M, N + 1) == XYMIS) then
                     if (Xc(M, N - 1) /= XYMIS) then
                        Xc(M, N + 1) = A * Xc(M, N) + B * Xc(M, N - 1)
                        Yc(M, N + 1) = A * Yc(M, N) + B * Yc(M, N - 1)
                     end if
                  else if (Xc(M, N - 1) == XYMIS) then
                     if (Xc(M, N + 1) /= XYMIS) then
                        Xc(M, N - 1) = A * Xc(M, N) + B * Xc(M, N + 1)
                        Yc(M, N - 1) = A * Yc(M, N) + B * Yc(M, N + 1)
                     end if
                  end if
               end if
            end do
         end if
         return
      end subroutine linemirror

end module m_linemirror
