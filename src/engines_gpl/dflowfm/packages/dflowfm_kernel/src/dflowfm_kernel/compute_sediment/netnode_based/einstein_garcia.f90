!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_einstein_garcia_sub

   implicit none

   private

   public :: einstein_garcia

contains

   subroutine einstein_garcia(da, rs, dj1, dj2)
      use precision, only: dp
      use m_einstein_garcia, only: d, c1, c2

      real(kind=dp) :: da, rs, dj1, dj2

      real(kind=dp) :: aa, rsk, dj12, dj22
      integer :: i1, i2, k

      if (da < 0.001_dp) then
         i1 = 1; i2 = 1
      else if (da < 0.005_dp) then
         i1 = 1; i2 = 2
      else if (da < 0.01_dp) then
         i1 = 2; i2 = 3
      else if (da < 0.05_dp) then
         i1 = 3; i2 = 4
      else if (da < 0.1_dp) then
         i1 = 4; i2 = 5
      else
         i1 = 5; i2 = 5
      end if
      if (i1 == i2) then
         aa = 0.0_dp
      else
         aa = (da - d(i1)) / (d(i2) - d(i1))
      end if

      dj1 = 0.0_dp
      dj2 = 0.0_dp
      dj12 = 0.0_dp
      dj22 = 0.0_dp

      do k = 0, 6
         rsk = rs**k
         !cck = (1d0-aa)*c1(i1,k) + aa*c1(i2,k)
         !dj1 = dj1 + cck*rsk
         !cck = (1d0-aa)*c2(i1,k) + aa*c2(i2,k)
         !dj2 = dj2 + cck*rsk

         dj1 = dj1 + c1(i1, k) * rsk
         dj12 = dj12 + c1(i2, k) * rsk
         dj2 = dj2 + c2(i1, k) * rsk
         dj22 = dj22 + c2(i2, k) * rsk

      end do

      dj1 = (1.0_dp - aa) * dj1 + aa * dj12
      dj2 = (1.0_dp - aa) * dj2 + aa * dj22

      if (dj1 /= 0.0_dp) then
         dj1 = 1.0_dp / dj1
      end if

      if (dj2 /= 0.0_dp) then
         dj2 = -1.0_dp / dj2
      end if

   end subroutine einstein_garcia

end module m_einstein_garcia_sub
