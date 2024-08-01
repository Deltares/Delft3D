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
 subroutine getucxucyweironly(ku, ucxku, ucyku, ischeme)
    use m_flow
    use m_flowgeom
    use m_sferic, only: jasfer3D
    implicit none

    integer :: ku, LL, L, Ls, ischeme, n12

    double precision :: ucxku, ucyku, ww, ac1, huweir, hunoweir, wl, wlno, at, cs, sn, fac

    double precision, external :: lin2nodx, lin2nody

    ucxku = 0d0; ucyku = 0d0
    huweir = 0d0; hunoweir = 0d0; wl = 0d0; wlno = 0d0; at = 0d0

    do LL = 1, nd(ku)%lnx
       Ls = nd(ku)%ln(LL); L = iabs(Ls)
       if (iadv(L) >= 21 .and. iadv(L) <= 29) then
          huweir = huweir + wu(L) * hu(L)
          wl = wl + wu(L)
       end if
    end do
    huweir = huweir / wl

    do LL = 1, nd(ku)%lnx
       Ls = nd(ku)%ln(LL); L = iabs(Ls)
       if (Ls < 0) then
          ac1 = acL(L)
          n12 = 1
       else
          ac1 = 1d0 - acL(L)
          n12 = 2
       end if
       ww = ac1 * dx(L) * wu(L)
       cs = ww * csu(L); sn = ww * snu(L)

       if (iadv(L) >= 21 .and. iadv(L) <= 29) then
          if (jasfer3D == 0) then
             ucxku = ucxku + cs * u0(L)
             ucyku = ucyku + sn * u0(L)
          else
             ucxku = ucxku + lin2nodx(L, n12, cs, sn) * u0(L)
             ucyku = ucyku + lin2nody(L, n12, cs, sn) * u0(L)
          end if
       else
          fac = 1d0
          if (huweir > 0d0) fac = max(1d0, hu(L) / huweir)
          if (jasfer3D == 0) then
             ucxku = ucxku + cs * u0(L) * fac
             ucyku = ucyku + sn * u0(L) * fac
          else
             ucxku = ucxku + lin2nodx(L, n12, cs, sn) * u0(L) * fac
             ucyku = ucyku + lin2nody(L, n12, cs, sn) * u0(L) * fac
          end if
       end if
    end do
    ucxku = ucxku / ba(ku)
    ucyku = ucyku / ba(ku)

 end subroutine getucxucyweironly
