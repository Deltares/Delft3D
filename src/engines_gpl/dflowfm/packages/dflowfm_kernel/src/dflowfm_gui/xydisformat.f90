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

      subroutine XYDISFORMAT()
         use m_sferic
         use m_wearelt
         implicit none

         double precision :: dv
         integer :: ix
         integer :: ixmax
         integer :: ixmin
         integer :: ixy
         integer :: iy
         integer :: iymax
         integer :: iymin
         integer :: izmax
         integer :: izmin
         integer :: jaauto, JMOUSE, JASHOW
         integer :: ncols
         integer :: ndec
         integer :: nie
         integer :: nis
         integer :: nv
         integer :: nxy
         integer :: nz
         double precision :: val
         double precision :: vmax, XLC, YLC, XA, YA
         double precision :: vmin

         common / DEPMAX / VMAX, VMIN, DV, VAL(256), NCOLS(256), NV, NIS, NIE, JAAUTO

         common / DISPFOR / XYFORM, ZFORM, DISFORM
         character * 7 XYFORM, ZFORM, DISFORM

         common / LOCATORA / XLC, YLC, XA, YA, JMOUSE, JASHOW

         ZFORM = '(F7.1)'

         xlc = max(x1, min(x2, xlc))
         ylc = max(y1, min(y2, ylc))

         IXMIN = int(log10(max(1d-6, abs(X1))))
         IXMAX = int(log10(max(1d-6, abs(X2))))
         IYMIN = int(log10(max(1d-6, abs(Y1))))
         IYMAX = int(log10(max(1d-6, abs(Y2))))
         IZMIN = int(log10(max(1d0, abs(VMIN))))
         IZMAX = int(log10(max(1d0, abs(VMAX))))

         IX = max(IXMIN, IXMAX)
         IY = max(IYMIN, IYMAX)
         IXY = max(IX, IY)

!     -------------------
!     1 VOOR +-
!     1 VOOR .
!     1 VOOR LOG(100) = 2
!     -------------------

         NXY = IXY + 4
         NDEC = 10 - NXY
         if (NDEC >= 0) then
            XYFORM = '(F10.1)'
            write (XYFORM(6:6), '(I1)') NDEC
         else
            XYFORM = '(E10.3)'
         end if

         DISFORM = 'F17.5'

         NZ = IZMAX + 3
         write (ZFORM(5:5), '(I1)') max(0, 9 - NZ)

         return
      end
