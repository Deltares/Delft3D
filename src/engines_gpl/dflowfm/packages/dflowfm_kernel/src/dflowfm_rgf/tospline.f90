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

!
!

module m_tospline

   implicit none

   private

   public :: tospline

contains

   subroutine TOSPLINE(XX, YY, XV, YV)
      use m_dismin, only: dismin
      use precision, only: dp
      use M_SPLINES, only: points_per_spline_capacity, nump, xsp, spline_capacity, ysp
      use m_spline, only: spline
      use m_get_ij, only: get_ij

      real(kind=dp) :: XX, YY, XV, YV

      real(kind=dp), dimension(points_per_spline_capacity) :: XI, XI2, YI, YI2
      real(kind=dp) :: TV, DIS
      integer :: IN, NUMPI

      IN = 1 ! Pick first spline
      call NUMP(IN, NUMPI)
      TV = NUMPI / 2.0_dp
      call get_ij(XSP, XI, spline_capacity, points_per_spline_capacity, points_per_spline_capacity, IN, IN, 1, NUMPI)
      call get_ij(YSP, YI, spline_capacity, points_per_spline_capacity, points_per_spline_capacity, IN, IN, 1, NUMPI)
      call SPLINE(XI, NUMPI, XI2)
      call SPLINE(YI, NUMPI, YI2)
      call DISMIN(XI, XI2, YI, YI2, XX, YY, NUMPI, DIS, TV, XV, YV)
      return
   end subroutine tospline

end module m_tospline
