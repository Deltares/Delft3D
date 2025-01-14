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

!> network field move
!!   Is is assumed that there is a backup copy of the grid.
module m_netmodfld

   implicit none

   private

   public :: netmodfld

contains

   subroutine netmodfld(xp, yp, kp)
      use precision, only: dp
      use m_netw
      use m_grid
      use m_alloc
      use m_missing
      use m_wearelt
      use m_sferic

      real(kind=dp) :: xp, yp !< coordinates that determine the influenced region

      integer :: kp !< center point index

      real(kind=dp) :: Dx0, Dy0, rsx, xn, yn, dist, frac
      real(kind=dp) :: xcen, ycen

      integer :: i

      xcen = xk(kp)
      ycen = yk(kp)

      Dx0 = xp - xcen
      Dy0 = yp - ycen

      rsx = max(dsix, sqrt(Dx0 * Dx0 + Dy0 * Dy0))

      do i = 1, numk
         xn = xk(i)
         yn = yk(i)
! intentional not in sferical coordinates
         dist = sqrt((xn - xcen)**2 + (yn - ycen)**2)
         frac = 0.5 * (1 + cos(min(max(dist / rsx, -1d0), 1d0) * pi))

         xk(i) = xk(i) + Dx0 * frac
         yk(i) = yk(i) + Dy0 * frac
      end do

   end subroutine netmodfld

end module m_netmodfld
