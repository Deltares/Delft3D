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

 subroutine setvelocityfield()
    use m_setcornervelocities, only: setcornervelocities
    use m_flow
    use m_flowgeom
    implicit none
    integer :: k, k1, k2, L
    double precision :: xx, yy, ux, uy, yyy, uuu, ykmx

    uy = -0.5d0
    ux = 0.5d0 * sqrt(3d0)

    ykmx = 100d0 ! 0d0

    do k = 1, ndx
       xx = xz(k)
       yy = yz(k) ! ykmx - yz(k)

       if (iuvfield == 1) then ! kwadratic horizontal
          ucx(k) = yy * yy
          ucy(k) = 0
       else if (iuvfield == 2) then ! kwadratic 30 degrees
          yyy = -uy * xx + ux * yy
          uuu = yyy * yyy
          ucx(k) = uuu * ux
          ucy(k) = uuu * uy
       else if (iuvfield == 3) then ! circular
          ucx(k) = -yy
          ucy(k) = xx
       else if (iuvfield == 4) then ! linear horizontal
          ucx(k) = yy
          ucy(k) = 0
       else if (iuvfield == 5) then ! linear 30 degrees
          yyy = -uy * xx + ux * yy
          uuu = yyy
          ucx(k) = uuu * ux
          ucy(k) = uuu * uy
       else if (iuvfield == 6) then ! random
          ucx(k) = 2 + sin(0.1d0 * k)
          ucy(k) = cos(1.5d0 * k)
       end if

    end do
    do L = 1, lnx
       k1 = ln(1, L); k2 = ln(2, L)
       u1(L) = ((1d0 - acl(L)) * ucx(k1) + acl(L) * ucx(k2)) * csu(L) + & ! reversed acl weighting
               ((1d0 - acl(L)) * ucy(k1) + acl(L) * ucy(k2)) * snu(L)
    end do

    u0 = u1
    s0 = s1

    call setcornervelocities()

 end subroutine setvelocityfield
