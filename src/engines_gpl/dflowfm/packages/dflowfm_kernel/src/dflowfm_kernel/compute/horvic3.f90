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

 double precision function horvic3(n12, L) ! horizontal viscosity term, out of face normal and tang comp's
    use m_flow
    use m_flowgeom
    use m_missing
    implicit none

    integer :: L ! in direction of link L
    integer :: n12 ! find hor visc term for cell 1 or 2

    ! locals
    integer :: LL, LLL, LLLL ! for links LL,
    integer :: k12, k1, k2, k3, k4 ! relevant node, 1 or 2
    double precision :: cs, sn, csL, snL, vicl
    double precision :: txx, tyy
    double precision :: uuk1, vvk1, uuk2, vvk2, uuk3, uuk4, dux, duy, dvx, tuu, tvv

    double precision :: txx_k12, tyy_k12

    integer :: ik1, ik2, in3, in4

    double precision, external :: nod2linx, nod2liny, lin2nodx, lin2nody, cor2linx, cor2liny

    horvic3 = 0d0
    csL = csu(L); snL = snu(L)
    k12 = ln(n12, L)
    vicL = vicouv

    do LL = 1, nd(k12)%lnx ! loop over all attached links

       LLL = nd(k12)%ln(LL)
       LLLL = abs(LLL)

       if (LLLL /= L) then

          if (LLL < 0) then
             cs = csu(LLLL)
             sn = snu(LLLL)
             k1 = ln(1, LLLL); k2 = ln(2, LLLL)
             k3 = lncn(1, LLLL); k4 = lncn(2, LLLL)
             ik1 = 1
             ik2 = 2
             in3 = 1
             in4 = 2
          else
             cs = -csu(LLLL)
             sn = -snu(LLLL)
             k1 = ln(2, LLLL); k2 = ln(1, LLLL)
             k3 = lncn(2, LLLL); k4 = lncn(1, LLLL)
             ik1 = 2
             ik2 = 1
             in3 = 2
             in4 = 1
          end if

!       uuk1  =  cs* ucx(k1) + sn* ucy(k1)
!       vvk1  = -sn* ucx(k1) + cs* ucy(k1)
!       uuk2  =  cs* ucx(k2) + sn* ucy(k2)
!       vvk2  = -sn* ucx(k2) + cs* ucy(k2)

          uuk1 = cs * nod2linx(LLLL, ik1, ucx(k1), ucy(k1)) + sn * nod2liny(LLLL, ik1, ucx(k1), ucy(k1))
          vvk1 = -sn * nod2linx(LLLL, ik1, ucx(k1), ucy(k1)) + cs * nod2liny(LLLL, ik1, ucx(k1), ucy(k1))
          uuk2 = cs * nod2linx(LLLL, ik2, ucx(k2), ucy(k2)) + sn * nod2liny(LLLL, ik2, ucx(k2), ucy(k2))
          vvk2 = -sn * nod2linx(LLLL, ik2, ucx(k2), ucy(k2)) + cs * nod2liny(LLLL, ik2, ucx(k2), ucy(k2))

          dux = (uuk2 - uuk1) * dxi(LLLL)
          dvx = (vvk2 - vvk1) * dxi(LLLL)

!       uuk3  =  cs*ucnx(k3) + sn*ucny(k3)
!       uuk4  =  cs*ucnx(k4) + sn*ucny(k4)
          uuk3 = cs * cor2linx(LLLL, in3, ucnx(k3), ucny(k3)) + sn * cor2liny(LLLL, in3, ucnx(k3), ucny(k3))
          uuk4 = cs * cor2linx(LLLL, in4, ucnx(k4), ucny(k4)) + sn * cor2liny(LLLL, in4, ucnx(k4), ucny(k4))

          duy = (uuk4 - uuk3) * wui(LLLL)

          tuu = dux + dux
          tvv = duy + dvx

          txx = tuu * cs - tvv * sn
          tyy = tuu * sn + tvv * cs

          if (javiusp == 1) then
             vicl = viusp(LLLL)
          else
             vicl = vicouv
          end if

          txx_k12 = lin2nodx(LLLL, ik1, txx, tyy)
          tyy_k12 = lin2nody(LLLL, ik1, txx, tyy)

!       horvic3 = horvic3 + ( txx*csl + tyy*snl )*wu(LLLL)*vicL
          horvic3 = horvic3 + (nod2linx(L, n12, txx_k12, tyy_k12) * csl + nod2liny(L, n12, txx_k12, tyy_k12) * snl) * wu(LLLL) * vicL

       end if

    end do
    horvic3 = horvic3 * bai(k12)

 end function Horvic3
