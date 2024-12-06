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
module m_dproject
   implicit none
contains
   subroutine dPROJECT(X8, Y8, XX4, YY4, MODE)
      use precision, only: dp
      use m_sferic
      use m_wearelt
      use m_sferzoom

      real(kind=dp) :: x8, y8, xx4, yy4
      integer :: mode
      real(kind=dp) :: X, Y, XX, YY, SX, CX, SY, CY, SY0, CY0, RR, C, SC, CC, RN
      real(kind=dp), save :: EPS = 1.d-20
      X = X8
      Y = Y8
      if (JSFERTEK == 0) then ! Just Transfer
         XX = X
         YY = Y
      else if (JSFERTEK == 1) then ! Stereographic
         SY0 = sin(DG2RD * Y0)
         CY0 = cos(DG2RD * Y0)
         if (MODE == 1) then ! LON,LAT to X,Y
            SX = sin(DG2RD * (X - X0))
            CX = cos(DG2RD * (X - X0))
            SY = sin(DG2RD * (Y))
            CY = cos(DG2RD * (Y))
            RN = 1.d0 + SY0 * SY + CY0 * CY * CX
            if (abs(RN) < EPS) then
               RN = sign(1.d0, RN) * EPS
            end if
            RR = FAC * 2.d0 * RD2DG / RN ! FAC om naar X1,Y1,X2,Y2 te schalen
            XX = RR * CY * SX ! Stereographic to Degrees
            YY = RR * (CY0 * SY - SY0 * CY * CX)
         else if (MODE == 2) then ! X,Y to LON,LAT
            XX = X / FAC
            YY = Y / FAC
            RR = sqrt(XX * XX + YY * YY)
            if (RR > EPS) then
               SX = sin(DG2RD * (XX - X0))
               CX = cos(DG2RD * (XX - X0))
               SY = sin(DG2RD * (YY))
               CY = cos(DG2RD * (YY))
               C = 2.d0 * atan2(RR, 2.d0 * RD2DG)
               SC = sin(C)
               CC = cos(C)
               XX = X0 * DG2RD + atan2(XX * SC, RR * CY0 * CC - YY * SY0 * SC)
               YY = asin(CC * SY0 + YY * SC * CY0 / RR)
               XX = XX * RD2DG
               YY = YY * RD2DG
            else
               XX = X
               YY = Y
            end if
            call inworld(xx)
         end if

      else if (JSFERTEK == 2) then ! MERCATOR
         if (MODE == 1) then
            if (Y >= 89d0) Y = 89.d0
            if (Y <= -89d0) Y = -89.d0
            YY = DG2RD * Y
            YY = log(1d0 + sin(YY)) / cos(YY)
            XX = DG2RD * X
         else if (MODE == 2) then
            YY = atan(sinh(Y))
            YY = RD2DG * YY
            XX = RD2DG * X
         end if
      end if

      XX4 = XX
      YY4 = YY
      return
   end subroutine dPROJECT
end module m_dproject
