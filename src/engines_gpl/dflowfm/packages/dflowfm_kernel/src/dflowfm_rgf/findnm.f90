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

module m_findnm

   implicit none

   private

   public :: findnm

contains

   subroutine FINDNM(XL, YL, X, Y, mmax, nmax, MC, NC, INSIDE, MV, NV, IN, JN, wf)
      use precision, only: dp
      use m_missing
      use geometry_module, only: pinpok

      integer :: mmax, nmax, mc, nc, inside, mv, nv, in, jn
      real(kind=dp) :: X(MMAX, NMAX), Y(MMAX, NMAX), XX(4), YY(4), XK(3), YK(3)
      real(kind=dp) :: xl, yl, wf(4)

      integer :: ishot, i, j, mz, nz, m1, m2, n1, n2, insidet, mvol, nvol, i1, i2, ier
      real(kind=dp) :: dx, dy, r, rmin, xxc, yyc

      data MVOL/0/, NVOL/0/
      if (MC == 0 .or. NC == 0) return
      ISHOT = 0
      RMIN = 99d+20

5     continue
      MV = 0
      NV = 0
      if (MVOL /= 0) then
         MZ = MVOL
         NZ = NVOL
      else
         do I = 1, MC
            do J = 1, NC
               if (X(I, J) /= XYMIS) then
                  DX = XL - X(I, J)
                  DY = YL - Y(I, J)
                  R = DX * DX + DY * DY
                  if (R < RMIN) then
                     RMIN = R
                     MZ = I
                     NZ = J
                  end if
               end if
            end do
         end do
      end if

      M1 = max(1, MZ - 2)
      N1 = max(1, NZ - 2)
      M2 = min(MC - 1, MZ + 1)
      N2 = min(NC - 1, NZ + 1)
      INSIDE = 0
      MVOL = 0
      NVOL = 0
      do I = M1, M2
         do J = N1, N2
            XX(1) = X(I, J)
            XX(2) = X(I + 1, J)
            XX(3) = X(I + 1, J + 1)
            XX(4) = X(I, J + 1)
            YY(1) = Y(I, J)
            YY(2) = Y(I + 1, J)
            YY(3) = Y(I + 1, J + 1)
            YY(4) = Y(I, J + 1)
            if (XX(1) /= XYMIS .and. XX(2) /= XYMIS .and. &
                XX(3) /= XYMIS .and. XX(4) /= XYMIS) then
               call PINPOK(XL, YL, 4, XX, YY, INSIDE, jins, dmiss)
               if (INSIDE == 1) then

                  call bilin5(xx, yy, xL, yL, wf, ier)

                  MVOL = I
                  NVOL = J
                  MV = I
                  NV = J
!                 Bepaal kwadrant
                  XXC = (XX(1) + XX(2) + XX(3) + XX(4)) / 4
                  YYC = (YY(1) + YY(2) + YY(3) + YY(4)) / 4
                  IN = 0
                  JN = 0
                  do I1 = 1, 4
                     I2 = mod(I1, 4) + 1
                     XK(1) = XX(I1)
                     YK(1) = YY(I1)
                     XK(2) = XX(I2)
                     YK(2) = YY(I2)
                     XK(3) = XXC
                     YK(3) = YYC
                     call PINPOK(XL, YL, 3, XK, YK, INSIDET, jins, dmiss)
                     if (INSIDET == 1) then
                        if (I1 == 1) JN = -1
                        if (I1 == 2) IN = 1
                        if (I1 == 3) JN = 1
                        if (I1 == 4) IN = -1
                        return
                     else if (I1 == 4) then
!                       WRITE(MDIA,*) 'NO KWADRANT'
                        return
                     end if
                  end do
               end if
            end if
         end do
      end do

!     WRITE(MDIA,*) 'ISHOT', ISHOT, MVOL, NVOL
      if (ISHOT == 1) return
      ISHOT = 1
      goto 5

      return
   end subroutine findnm

end module m_findnm
