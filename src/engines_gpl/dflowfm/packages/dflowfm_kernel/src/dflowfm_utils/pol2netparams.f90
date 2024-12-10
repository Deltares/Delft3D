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

 !> get uniform curvilinear grid parameters in "makenet" from polygon
 subroutine pol2netparams()
    use precision, only: dp
    use m_makenet
    use m_polygon
    use m_sferic
    use m_missing
    use geometry_module
    implicit none

    real(kind=dp) :: ximin, ximax
    real(kind=dp) :: etamin, etamax
    real(kind=dp) :: xref, yref, Dx, Dy
    real(kind=dp) :: xi, eta, csa, sna

    integer :: i

    integer :: ierror ! error (1) or not (0)

    ierror = 0

!  check if polygon exists
    if (NPL < 3) return

    ierror = 1

!  get reference point: first non-missing
    i = 1; 
    do while (i <= NPL .and. (xpl(i) == DMISS .or. ypl(i) == DMISS))
       i = i + 1
    end do

!  check if point was found
    if (i > NPL) goto 1234

    xref = xpl(i)
    yref = ypl(i)

    csa = cos(dg2rd * ANGLE)
    sna = sin(dg2rd * ANGLE)

!  get polygon min/max in rotated (xi,eta) coordinaes
    ximin = huge(1d0)
    ximax = -ximin
    etamin = huge(1d0)
    etamax = -etamin
    do i = 1, NPL
       if (xpl(i) /= DMISS) then
          call getdxdy(xref, yref, xpl(i), ypl(i), Dx, Dy, jsferic)
          xi = Dx * csa + Dy * sna
          eta = -Dx * sna + Dy * csa
          ximin = min(ximin, xi)
          ximax = max(ximax, xi)
          etamin = min(etamin, eta)
          etamax = max(etamax, eta)
       end if
    end do

!  get x0, y0, NRX, NRY
    Dx = ximin * csa - etamin * sna
    Dy = ximin * sna + etamin * csa
    if (jsferic == 1) then
       Dx = Dx / Ra * rd2dg
       Dy = Dy / (Ra * cos(yref * dg2rd)) * rd2dg
    end if
    x0 = xref + Dx
    y0 = yref + Dy

    NRX = ceiling((ximax - ximin) / DX0)
    NRY = ceiling((etamax - etamin) / DY0)

    ierror = 0
1234 continue

    return
 end subroutine pol2netparams
