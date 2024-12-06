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

 subroutine getdxofconnectedkcu1(Lf, wuL) ! width of connection link has lenght of connected 1D links
    use precision, only: dp
    use geometry_module
    use m_sferic
    use m_missing
    use m_flowgeom
    use m_flow
    use m_netw
    implicit none
    integer :: Lf, L, LL, k, kk, n, k1, k2, k3, k4, n1, n2, n2d
    real(kind=dp) :: wu1, wu2, wuL
    wu1 = 0d0; n = 0

    !if (kcs(ln(1,L) ) == 21)  k = ln(2,L)
    !if (kcs(ln(2,L) ) == 21)  k = ln(1,L)
    !do kk = 1,nd(k)%lnx
    !   if (kcu(L) == 1) then
    !      n   = n + 1
    !      LL  = abs( nd(k)%ln(kk) )
    !      wu1 = wu1 + dx(L)
    !   endif
    ! enddo

    L = ln2lne(Lf)
    k1 = kn(1, L)
    k2 = kn(2, L)
    if (nmk(k1) == 1) k = k2
    if (nmk(k2) == 1) k = k1
    do kk = 1, nmk(k)
       LL = abs(nod(k)%lin(kk))
       if (kn(3, LL) == 1 .or. kn(3, LL) == 6) then ! on second thought, only true 1D links should influence lateral inflow width ! .or. kn(3,LL) == 4) then
          n = n + 1
          wu1 = wu1 + dx(lne2ln(LL))
       end if
    end do

    if (n > 0) then
       wu1 = wu1 / n
    end if

    n1 = ln(1, Lf); n2 = ln(2, Lf)
    if (kcs(n1) == 21) then
       n2d = n1
    else if (kcs(n2) == 21) then
       n2d = n2
    end if
    L = 0
    call which2Dnetlinkwascrossed(n2d, k1, k2, L)
    if (L == 0) then
       ! 1d2d link is inside the 2d grid cell, use the square root of the surface area instead.
       wu2 = sqrt(ba(n2d))
    else
       k3 = kn(1, L)
       k4 = kn(2, L)
       wu2 = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
    end if

    wuL = min(wu1, wu2) ! both 1D sides flood at the same moment, no division by 2
 end subroutine getdxofconnectedkcu1
