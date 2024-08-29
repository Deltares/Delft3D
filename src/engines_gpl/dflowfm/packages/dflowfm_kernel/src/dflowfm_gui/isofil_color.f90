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
module m_isofil_color
    implicit none
contains
    
    subroutine isofil_color(X, Y, Z, n4, NCOLR, VAL, NCOLS, NV)
      use m_topix
      use m_drawthis
      integer :: n4, ncolr
      double precision :: X(n4), Y(n4), Z(n4)

      double precision :: dzn, frac
      integer :: i, ih, j, j1, j2
      integer :: ncol
      integer :: npics
      integer :: num
      integer :: nx1
      integer :: nx3
      integer :: ny1
      integer :: ny3
      double precision :: zmax
      double precision :: zmin
      double precision :: znex
      double precision :: znow
      double precision :: DX(12), DY(12), DZ(12), XH(12), YH(12)
      double precision :: VAL(256)
      integer :: NCOLS(256), NV

      do I = 1, n4
         J = I + 1
         if (I == n4) J = 1
         DX(I) = X(J) - X(I)
         DY(I) = Y(J) - Y(I)
         DZ(I) = Z(J) - Z(I)
      end do

      ZMAX = Z(1)
      ZMIN = Z(1)
      do I = 2, n4
         ZMAX = max(ZMAX, Z(I))
         ZMIN = min(ZMIN, Z(I))
      end do

      if (ZMAX <= VAL(1)) then
         NCOL = NCOLS(1)
         call PFILLER(X, Y, n4, NCOL, NCOL)
      else if (ZMIN >= VAL(NV)) then
         NCOL = NCOLS(NV + 1)
         call PFILLER(X, Y, n4, NCOL, NCOL)
      else
         do i = 0, NV
            if (I == 0) then
               ZNOW = -1e+30
            else
               ZNOW = VAL(I)
            end if
            if (I == NV) then
               ZNEX = 1e+30
            else
               ZNEX = VAL(I + 1)
            end if
            NCOL = NCOLS(I + 1)
            if (ZMIN <= ZNOW .and. ZMAX >= ZNOW .or. &
                ZMIN <= ZNEX .and. ZMAX >= ZNEX) then
               IH = 1
               do J1 = 1, n4
                  J2 = J1 + 1
                  if (J1 == n4) J2 = 1
                  if (Z(J1) < ZNOW) then
                     if (Z(J2) > ZNOW) then
                        DZN = ZNOW - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     end if
                     if (Z(J2) > ZNEX) then
                        DZN = ZNEX - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     end if
                  else if (Z(J1) > ZNEX) then
                     if (Z(J2) < ZNEX) then
                        DZN = ZNEX - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     end if
                     if (Z(J2) < ZNOW) then
                        DZN = ZNOW - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     end if
                  else
                     XH(IH) = X(J1)
                     YH(IH) = Y(J1)
                     IH = IH + 1
                     if (Z(J2) < ZNOW) then
                        DZN = ZNOW - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     else if (Z(J2) > ZNEX) then
                        DZN = ZNEX - Z(J1)
                        FRAC = DZN / DZ(J1)
                        if (FRAC > 0d0 .and. FRAC <= 1d0) then
                           XH(IH) = X(J1) + FRAC * DX(J1)
                           YH(IH) = Y(J1) + FRAC * DY(J1)
                           IH = IH + 1
                        end if
                     end if
                  end if
               end do

               NUM = IH - 1
               if (NUM >= 3) then
                  call PFILLER(XH, YH, NUM, NCOL, NCOL)
               else if (NUM /= 0) then
!              CALL OKAY(1)
               end if
            else if (ZMIN >= ZNOW .and. ZMAX <= ZNEX) then
               call PFILLER(X, Y, n4, NCOL, NCOL)
            end if
         end do
      end if

      if (NDRAW(2) == -1) then ! .GE. 1) THEN ! vintage
         call TOPIX(X(1), Y(1), NX1, NY1)
         call TOPIX(X(3), Y(3), NX3, NY3)
         NPICS = abs(NX1 - NX3) + abs(NY1 - NY3)
         if (NCOLR == 0) then
            if (NPICS >= 5) then
               call SETCOL(NCOLR)
               call PTABS(X(1), Y(1))
            end if
         else
            if (NPICS >= 5) then
               NUM = n4
               call POLYGON(X, Y, NUM, NCOLR)
            else
               call SETCOL(NCOLR)
               call PTABS(X(1), Y(1))
            end if
         end if
      end if
      return
   end subroutine isofil_color

end module m_isofil_color
