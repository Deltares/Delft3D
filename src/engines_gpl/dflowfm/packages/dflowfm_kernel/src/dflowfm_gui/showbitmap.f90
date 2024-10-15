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

      subroutine SHOWBITMAP(jainterpolate)
         use m_pixcount
         use M_WEARELT
         use M_BITMAP
         use m_drawthis
         use m_halt2
         use m_krec5
         use m_bilinxy
         use m_set_col
         implicit none
         integer :: i
         integer :: ini
         integer :: j
         integer :: k
         integer :: key
         integer :: nko
         double precision :: xd
         double precision :: xs
         double precision :: xx
         double precision :: xx2
         double precision :: yd
         double precision :: ys
         double precision :: yy
         double precision :: yy2
         double precision :: zs
         integer :: jainterpolate

         call IGRCOLOURMODEL(24)

         INI = 1
         XX = 2
         YY = 2
         call BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)
         if (INI == -1) return
         INI = 0

         XD = (XP(2) - XP(1)) / (XB(2) - XB(1))
         YD = (YP(3) - YP(1)) / (YB(3) - YB(1))
         XD = XD / 2
         YD = YD / 2

         do J = NXP, 1, -1
            call HALT2(KEY)
            if (KEY == 1) then
               call IGRCOLOURMODEL(8)
               return
            end if
            NKO = -1
            do I = 1, MXP
               K = (NXP - J) * MXP + I
               XX = dble(I - 1)
               YY = dble(J - 1)
               call BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)

               if (jainterpolate == 1) then
                  xs = xx2
                  ys = yy2
                  zs = 1e-6 * ipix(k)
                  call pixcount(xs, ys, zs, 1)
               end if

               if (XX2 > X1 .and. XX2 < X2 .and. YY2 > Y1 .and. YY2 < Y2) then
                  if (NKO /= IPIX(K)) then
                     call SETCOL(IPIX(K))
                     NKO = IPIX(K)
                  end if
                  if (NDRAW(10) == 0) then
                     call RECTANGLE(real(XX2 - XD), real(YY2 - YD), real(XX2 + XD), real(YY2 + YD))
                     ! CALL IGRMOVETO(XX2-XD,YY2-YD)
                     ! CALL IGrRECTANGLEREL(XD*2,YD*2)
                  else
                     call KREC5(XX2, YY2, XD, YD)
                  end if
               end if
            end do
         end do
         call IGRCOLOURMODEL(8)

         if (jainterpolate == 1) then
            call pixcount(xs, ys, zs, 2)
         end if

         return
      end
