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

  subroutine ADDELEM(K1, K2, JA)
     use M_AFMETING
     implicit none
     integer :: K1, K2, JA

     double precision :: a0
     double precision :: ag
     double precision :: cdflow
     double precision :: cfl
     double precision :: cfric
     double precision :: e0
     double precision :: eps
     double precision :: fbouy
     double precision :: fdyn
     integer :: janet
     integer :: moments
     double precision :: pi
     double precision :: r0
     double precision :: rho
     double precision :: rhow

     double precision DLENGTH
     common / SETTINGS / FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET
     common / CONSTANTS / E0, RHO, RHOW, CFL, EPS, AG, PI
     if (JANET == 1) then
        A0 = PI * RDIAM * RDIAM / 4
     else
        A0 = 1e6 * RWIDTH * RTHICK
     end if
     R0 = DLENGTH(K1, K2)
     call CONNECT(K1, K2, 1, A0, R0)
     return
  end subroutine ADDELEM
