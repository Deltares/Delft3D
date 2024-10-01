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

module m_cosphiu

implicit none

contains

 double precision function cosphiu(L) ! get link cos

    use m_flowgeom
    use m_netw, only: xk, yk
    use geometry_module, only: normalin
    use m_missing, only: dxymis
    use m_sferic, only: jsferic, jasfer3D

    integer :: L ! for link L,

    ! locals
    integer :: k1, k2, k3, k4
    double precision :: rn, rt, rnl, rtl
    k1 = ln(1, L)
    k2 = ln(2, L)
    k3 = lncn(1, L)
    k4 = lncn(2, L)
    call normalin(xz(k1), yz(k1), xz(k2), yz(k2), rnl, rtl, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! in pos LL direction
    call normalin(xk(k3), yk(k3), xk(k4), yk(k4), rn, rt, xu(L), yu(L), jsferic, jasfer3D, dxymis) ! = normalin (k1,k2)
    cosphiu = rnl * rn + rtl * rt

 end function cosphiu

end module m_cosphiu
