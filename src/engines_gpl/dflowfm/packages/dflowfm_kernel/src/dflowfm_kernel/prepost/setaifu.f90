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

 subroutine setaifu() ! set bed skewness array for roughness
    use precision, only: dp
    use m_flow
    use m_flowgeom
    use m_netw
    use m_missing
    implicit none
    integer :: k, L, m, n, k1, k2
    real(kind=dp) :: zu, ai, bi, zkk
    aifu = 1d0
    bz = 0d0
    do m = 1, mxban ! bz based on netnodes area
       k = nban(1, m)
       n = nban(2, m)
       if (zk(k) /= dmiss) then
          zkk = zk(k)
       else
          zkk = zkuni + xk(k) * bedslope
       end if
       bz(n) = bz(n) + banf(m) * zkk
    end do
    bz = bz / ba

    do L = 1, Lnx ! next, link bed slope  bi in link, ai cross link
       k1 = Ln(1, L); k2 = Ln(2, L)
       ai = (bob(2, L) - bob(1, L)) * wui(L)
       if (L <= lnx1D) then
          bi = 0d0
       else if (L > Lnxi) then
          zu = 0.5d0 * (bob(1, L) + bob(2, L))
          bi = (bz(k2) - zu) * dxi(L) / max(eps4, 1d0 - acL(L))
       else
          bi = (bz(k2) - bz(k1)) * dxi(L)
       end if
       if (jaconveyance2D == 1) then
          aifu(L) = 1d0 + bi * bi
       else
          aifu(L) = 1d0 + ai * ai + bi * bi
       end if
       aifu(L) = sqrt(aifu(L))
    end do

 end subroutine setaifu
