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

module m_allcolours

   implicit none

contains

   subroutine ALLCOLOURS()
      use precision, only: dp
      use m_wearelt
      use m_box_nop
      use m_fbox_nop
      use m_set_col
      implicit none
      real(kind=dp) :: dx
      real(kind=dp) :: dxc
      real(kind=dp) :: dy
      real(kind=dp) :: dyc
      integer :: i
      integer :: j
      integer :: ncol
      real(kind=dp) :: x
      real(kind=dp) :: xc
      real(kind=dp) :: xl
      real(kind=dp) :: xu
      real(kind=dp) :: y
      real(kind=dp) :: yc
      real(kind=dp) :: yl
      real(kind=dp) :: yu
      NCOL = 0
      XL = X2 - 0.66d0 * DSIX - RCIR * 4
      XU = XL + 0.66d0 * DSIX
      YL = Y1 + DSIX
      YU = Y2 - DSIX
      DX = XU - XL
      DY = YU - YL
      DXC = DX / 20
      DYC = DY / 20
      do J = 1, 16
         do I = 1, 16
            X = dble(I - 1) / 15d0
            Y = dble(J - 1) / 15d0
            XC = XL + X * DX
            YC = YL + Y * DY
            call SETCOL(NCOL)
            NCOL = NCOL + 1
            call FBOXnop(XC - DXC, YC - DYC, XC + DXC, YC + DYC)
            call SETCOL(0)
            call BOXnop(XC - DXC, YC - DYC, XC + DXC, YC + DYC)
         end do
      end do
      return
   end

end module m_allcolours
