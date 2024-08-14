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

subroutine yzprofile(hpr, ka, itp, area, width, japerim, frcn, ifrctyp, perim, cfhi)
   use m_profiles
   use m_physcoef, only: ag
   implicit none
   integer :: ka, japerim, itp
   double precision :: hpr ! hoogte in profiel
   double precision :: area ! wet cross sectional area
   double precision :: width ! width at water surface
   double precision :: perim ! wet perimeter
   double precision :: cfhi ! cfuhi(L)

   double precision :: wid ! wid of segment
   double precision :: ar ! ar of segment
   double precision :: conv, convall ! (sum of) conv
   double precision :: hpr2 ! height in segment under consideration
   double precision :: frcn ! user defined friction coefficient
   double precision :: bl1, bl2, b21 ! bottom levels segment, b21, diff of bl1,bl2, always > 0
   double precision :: wu2, ai, aconv, per
   integer :: ifrctyp ! user defined frcition type
   integer :: k, numseg, jac

   numseg = size(profiles1D(ka)%y) - 1

   area = 0d0; width = 0d0; convall = 0d0; perim = 0d0

   jac = 0
   if (japerim == 1) then
      if (itp == 100) then
         jac = 1 ! lumped
      else
         jac = 2 ! 1D conveyance
      end if
   end if

   do k = 1, numseg

      if (profiles1D(ka)%z(k) < profiles1D(ka)%z(k + 1)) then
         BL1 = profiles1D(ka)%z(k); BL2 = profiles1D(ka)%z(k + 1)
      else
         BL2 = profiles1D(ka)%z(k); BL1 = profiles1D(ka)%z(k + 1)
      end if
      hpr2 = hpr - bl1 ! TODO: LUMBRICUS: HK: is hpr niet een hu en bl een echte/nep BL?

      if (hpr2 > 0d0) then
         b21 = BL2 - BL1
         wu2 = abs(profiles1D(ka)%y(k) - profiles1D(ka)%y(k + 1))
         ai = b21 / wu2
         call getseg1D(hpr2, wu2, b21, ai, frcn, ifrctyp, wid, ar, conv, per, jac)
         width = width + wid
         area = area + ar
         if (jac == 2) then
            convall = convall + conv
         else if (jac == 1) then
            perim = perim + per
         end if
      end if

   end do

   if (jac == 2) then
      if (convall > 0) then
         aconv = (area / convall)**2
         cfhi = ag * aconv
      else
         cfhi = 0d0
      end if
   end if

end subroutine yzprofile
