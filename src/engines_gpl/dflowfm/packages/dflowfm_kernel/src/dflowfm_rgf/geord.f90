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

module m_geord

   implicit none

   private

   public :: geord

contains

   subroutine GEORD(xgeo, ygeo, xrd, yrd, JAPARIJS)
      use precision, only: dp
      use m_wgs842bessel, only: wgs842bessel

      integer :: japarijs

! -----------------------------------------------------------------------------
!     Conversion of Geographical coordinates (Bessel) into RD-coordinates
! -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 29 january 1991
! -----------------------------------------------------------------------------
!
!     arguments:
!     xgeo   [ I ]   geographical east-coordinate (degrees; decimal)
!     ygeo   [ I ]   geographical north-coordinate (degrees; decimal)
!     xrd    [ O ]   east-coordinate in RD system
!     yrd    [ O ]   north-coordinate in RD system
!
      real(kind=dp) :: xgeo, ygeo
      real(kind=dp) :: xrd, yrd
      real(kind=dp) :: xx, yy
!
!     local variables:
!     ugeo   : linearly transformed xgeo
!     vgeo   : linearly transformed ygeo
!
      real(kind=dp) :: ugeo, vgeo
!
!     externals:
!     none
!
! -----------------------------------------------------------------------------
!
!     compute linear tramsformation of Geographical coordinates

      call wgs842bessel(ygeo, xgeo, yy, xx)
!!
      ugeo = 0.3600_dp * xx - 1.9395500_dp
      vgeo = 0.3600_dp * yy - 18.7762178_dp

!
!     perform conversion
!
      xrd = 190066.91_dp * ugeo - 11831.0_dp * ugeo * vgeo - &
            114.2_dp * ugeo * (vgeo**2) - 32.39_dp * (ugeo**3) - &
            2.33_dp * ugeo * (vgeo**3) - 0.61_dp * vgeo * (ugeo**3)
      yrd = 309020.34_dp * vgeo + 3638.36_dp * (ugeo**2) + &
            72.92_dp * (vgeo**2) - 157.97_dp * vgeo * (ugeo**2) + &
            59.77_dp * (vgeo**3) + 0.09_dp * (ugeo**4) - &
            6.45_dp * (vgeo**2) * (ugeo**2) + 0.07_dp * (vgeo**4)
!

      if (JAPARIJS == 1) then
         XRD = XRD + 155000.
         YRD = YRD + 463000.
      end if

      return
   end subroutine GEORD

end module m_geord
