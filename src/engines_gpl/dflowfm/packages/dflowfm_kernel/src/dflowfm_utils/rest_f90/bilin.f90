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

module m_bilin

implicit none

private

public :: bilin

contains

      subroutine BILIN(X, Y, Z, XP, YP, ZP)
         use precision, only: dp

         real(kind=dp) :: r1
         real(kind=dp) :: r2
         real(kind=dp) :: x1
         real(kind=dp) :: x2
         real(kind=dp) :: xa
         real(kind=dp) :: xp
         real(kind=dp) :: xr
         real(kind=dp) :: xrm
         real(kind=dp) :: y1
         real(kind=dp) :: y2
         real(kind=dp) :: ya
         real(kind=dp) :: yp
         real(kind=dp) :: yr
         real(kind=dp) :: yrm
         real(kind=dp) :: zp
         real(kind=dp) :: X(4), Y(4), Z(4)
!     Bepaal relatieve ligging in cel.
!     Twee coordinaten (xr,yr) van 0 tot 1.
!     Bilineaire interpolatie
         X1 = X(2) - X(1)
         Y1 = Y(2) - Y(1)
         R1 = X1 * X1 + Y1 * Y1
         X2 = X(4) - X(1)
         Y2 = Y(4) - Y(1)
         R2 = X2 * X2 + Y2 * Y2
         XA = XP - X(1)
         YA = YP - Y(1)
         XR = (X1 * XA + Y1 * YA) / R1
         YR = (X2 * XA + Y2 * YA) / R2

         XRM = 1 - XR
         YRM = 1 - YR
         ZP = XRM * YRM * Z(1) + XR * YRM * Z(2) + XR * YR * Z(3) + XRM * YR * Z(4)
         return
      end

end module m_bilin
