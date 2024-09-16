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
module m_zoomin
   implicit none
contains
   subroutine ZOOMIN(KEY, NPUT)
      use unstruc_colors
      use m_wearelt
      use m_sferic
      use m_sferzoom
      use m_locatora
      use m_dproject
      use m_botlin
      use m_readlocator
      use m_box_nop
      use m_set_col

      double precision :: aspect, dx, dy, xln, yln, xl, yl, X1B, Y1B, X2B, Y2B, xl2, yl2
      integer :: k, nlevel, jadraw, nput, nnn, ja, key

      character(len=40) :: WRDKEY
      integer, parameter :: MAXZOOM = 4
      real XYWOLD(MAXZOOM, 4)
      save XYWOLD
      integer, save :: NUMZOOM = 0
      if (NUMZOOM == 0) then
         do K = 1, MAXZOOM
            XYWOLD(K, 1) = (XMIN + XMAX) / 2
            XYWOLD(K, 2) = (YMIN + YMAX) / 2
            XYWOLD(K, 3) = (YMAX - YMIN)
         end do
         NUMZOOM = 1
      end if
!     geen entry ALS NET BEZIG PUNT TE ZETTEN
!     BIJ VERLATEN MET KEY = 3, TEKEN OPNIEUW
      WRDKEY = 'Z   = ZOOMIN ;'
      NLEVEL = 3
      JADRAW = 1
!
      if (NPUT == 1) return

      call IGRLINEWIDTH(2, -1)
      call SETCOL(KLZM)
      call SETXOR(1)
      call BOTLIN(0, 5, NNN)
      call INQASP(ASPECT)
      XL = XLC
      YL = YLC
      dy = dyh / 3d0
      DX = DY / ASPECT
      if (JSFERTEK >= 1) then
         call dPROJECT(XLC, YLC, XL, YL, 1)
         !   c   = max(1d-4, cos(dg2rd*min(90d0, abs(yl) ) ) )
         !   dx = dx/c
      end if
      X1B = XL - DX / 2
      X2B = XL + DX / 2
      Y1B = YL - DY / 2
      Y2B = YL + DY / 2

10    continue

      if (JADRAW == 1) then
         call BOXnop(X1B, Y1B, X2B, Y2B)
         JADRAW = 0
      end if
      JA = 0
      KEY = 999
      call READLOCATOR(XL, YL, KEY)
      if (JSFERTEK >= 1) call dPROJECT(XLC, YLC, XL, YL, 1)

      if (X2B > X2 .or. X1B < X1 .or. Y2B > Y2 .or. Y1B < Y1) then
         dy = dyh
         JA = 1
      else if (KEY == 21) then
         JA = 1
      else if (KEY == 22) then
         JA = 3
      else if (KEY == 90 .or. KEY == 90 + 32) then
         DY = 3d0 * dyh
         if (JSFERTEK >= 1) DY = min(DY, 179d0)
         JA = 1
      else if (KEY == 23) then
         KEY = 3
         call SETXOR(0)
         call IMOUSECURSORHIDE()
         call IGRLINEWIDTH(1, -1)
         return
      else if (KEY == 24) then
!        F1
         NLEVEL = 3
         call HELP(WRDKEY, NLEVEL)
      else if (KEY == 25) then
!        F2
         call HISTOR()
      else if (KEY == 162 .or. KEY == 160 .or. KEY == 45 .or. KEY == 43 .or. KEY < 0) then
         call BOXnop(X1B, Y1B, X2B, Y2B)
         JADRAW = 1
         if (KEY == 162 .or. KEY == 43) then
            DY = DY * 1.01; if (JSFERTEK >= 1) DY = min(DY, 179d0)
         else if (KEY == 160 .or. KEY == 45) then
            DY = DY / 1.01
         end if
         DX = DY / ASPECT
         X1B = XL - DX / 2
         X2B = XL + DX / 2
         Y1B = YL - DY / 2
         Y2B = YL + DY / 2
      else if (KEY == 143) then
         NUMZOOM = NUMZOOM - 1
         if (NUMZOOM == 0) NUMZOOM = MAXZOOM
         XL = XYWOLD(NUMZOOM, 1)
         YL = XYWOLD(NUMZOOM, 2)
         DY = XYWOLD(NUMZOOM, 3)
         JA = 2
      end if

      if (JA >= 1) then
         call IMOUSECURSORHIDE()
         if (JA /= 3) then
            if (JSFERTEK >= 1) then
               call dPROJECT(XL, YL, XL2, YL2, 2); xl = xl2; yl = yl2
            end if
            call SETWYnew(XL, YL, DY)
         else
            call WEAREL()
         end if
         if (JA /= 2) then
!           alleen opslaan als in of uitgezoomd, niet als teruggezoomd
            NUMZOOM = NUMZOOM + 1
            if (NUMZOOM == MAXZOOM + 1) NUMZOOM = 1
            XYWOLD(NUMZOOM, 1) = XL
            XYWOLD(NUMZOOM, 2) = YL
            XYWOLD(NUMZOOM, 3) = DY
         end if
         XLN = 0.0
         YLN = 0.0
         call ORGLOCATOR(XLN, YLN)
         KEY = 3
         call SETXOR(0)
         call IGRLINEWIDTH(1, -1)
         return
      end if
      goto 10

   end
end module m_zoomin
