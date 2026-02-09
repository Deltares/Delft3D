!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_setellipse
   implicit none
   private
   public :: setellipse

contains

   subroutine setellipse(iell)
      use m_ellipse, only: semi_major_axis, eccentricity
      use precision, only: dp

      integer, intent(in) :: iell

      semi_major_axis = 6378137_dp
      eccentricity = 0.081819_dp

      if (iell == 1) then ! hayford
         semi_major_axis = 6378388_dp
         eccentricity = 0.081992_dp
      elseif (iell == 2) then ! bessel
         semi_major_axis = 6377397_dp
         eccentricity = 0.081690_dp
      elseif (iell == 3) then ! wgs 84
         semi_major_axis = 6378137_dp
         eccentricity = 0.081819_dp
      elseif (iell == 4) then ! clarke 1880
         semi_major_axis = 6378249_dp
         eccentricity = 0.082478_dp
      elseif (iell == 5) then ! india 1830
         semi_major_axis = 6377276.345_dp
         eccentricity = 0.081473_dp
      end if
   end subroutine setellipse
end module m_setellipse
