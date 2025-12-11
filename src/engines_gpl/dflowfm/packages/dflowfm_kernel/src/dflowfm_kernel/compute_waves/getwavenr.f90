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

module m_getwavenr

   implicit none

   private

   public :: getwavenr

contains

   subroutine getwavenr(h, T, k)
      use precision, only: dp
      use m_sferic, only: twopi
      use m_physcoef, only: ag
      implicit none
      ! get wavenr from waterdepth and period, see d3d doc

      real(kind=dp), parameter :: a1 = 5.060219360721177e-01_dp, a2 = 2.663457535068147e-01_dp, &
                                  a3 = 1.108728659243231e-01_dp, a4 = 4.197392043833136e-02_dp, &
                                  a5 = 8.670877524768146e-03_dp, a6 = 4.890806291366061e-03_dp, &
                                  b1 = 1.727544632667079e-01_dp, b2 = 1.191224998569728e-01_dp, &
                                  b3 = 4.165097693766726e-02_dp, b4 = 8.674993032204639e-03_dp

      real(kind=dp), intent(in) :: h !  Waterheight
      real(kind=dp), intent(in) :: t !  Period
      real(kind=dp), intent(out) :: k !  Approximation of wave lenght

      real(kind=dp) :: den ! Denominator
      real(kind=dp) :: num ! Numerator
      real(kind=dp) :: ome2

      ome2 = ((twopi / T)**2) * h / ag
      num = 1.0_dp + ome2 * (a1 + ome2 * (a2 + ome2 * (a3 + ome2 * (a4 + ome2 * (a5 + ome2 * a6)))))
      den = 1.0_dp + ome2 * (b1 + ome2 * (b2 + ome2 * (b3 + ome2 * (b4 + ome2 * a6))))
      k = sqrt(ome2 * num / den) / h

   end subroutine getwavenr

end module m_getwavenr
