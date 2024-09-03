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

! =================================================================================================
! =================================================================================================
  subroutine getucxucybarrierzero(Lf, ku, ucxku, ucyku)
     use m_flow
     use m_flowgeom
     implicit none

     integer :: ku, L, LL, Ls, n12, Lf
     double precision :: ucxku, ucyku, ww, ac1, cs, sn
     double precision, external :: lin2nodx, lin2nody

     ucxku = 0d0; ucyku = 0d0

     do LL = 1, nd(ku)%lnx
        Ls = nd(ku)%ln(LL); L = abs(Ls)
        if (Ls < 0) then
           ac1 = acL(L)
           n12 = 1
        else
           ac1 = 1d0 - acL(L)
           n12 = 2
        end if
        ww = ac1 * dx(L) * wu(L)
        cs = ww * csu(L); sn = ww * snu(L)
        if (L /= Lf) then
           ucxku = ucxku + lin2nodx(L, n12, cs, sn) * u1(L)
           ucyku = ucyku + lin2nody(L, n12, cs, sn) * u1(L)
        end if
     end do
     ucxku = ucxku / ba(ku)
     ucyku = ucyku / ba(ku)

  end subroutine getucxucybarrierzero
