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

      subroutine GEORD(xgeo, ygeo, xrd, yrd, JAPARIJS)
         implicit none
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
         double precision :: xgeo, ygeo
         double precision :: xrd, yrd
         double precision :: xx, yy
!
!     local variables:
!     ugeo   : linearly transformed xgeo
!     vgeo   : linearly transformed ygeo
!
         double precision :: ugeo, vgeo
!
!     externals:
!     none
!
! -----------------------------------------------------------------------------
!
!     compute linear tramsformation of Geographical coordinates

         call wgs842bessel(ygeo, xgeo, yy, xx)
!!
         ugeo = 0.3600d0 * xx - 1.9395500d0
         vgeo = 0.3600d0 * yy - 18.7762178d0

!
!     perform conversion
!
         xrd = 190066.91d0 * ugeo - 11831d0 * ugeo * vgeo - &
               114.2d0 * ugeo * (vgeo**2) - 32.39d0 * (ugeo**3) - &
               2.33d0 * ugeo * (vgeo**3) - 0.61d0 * vgeo * (ugeo**3)
         yrd = 309020.34d0 * vgeo + 3638.36d0 * (ugeo**2) + &
               72.92d0 * (vgeo**2) - 157.97d0 * vgeo * (ugeo**2) + &
               59.77d0 * (vgeo**3) + 0.09d0 * (ugeo**4) - &
               6.45d0 * (vgeo**2) * (ugeo**2) + 0.07d0 * (vgeo**4)
!

         if (JAPARIJS == 1) then
            XRD = XRD + 155000.
            YRD = YRD + 463000.
         end if

         return
      end subroutine GEORD
