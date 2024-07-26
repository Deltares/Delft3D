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

 subroutine linkstocentercartcomp(knod, vlin, vnod)
    use m_flow
    use m_netw
    use m_flowgeom

    implicit none

    integer, intent(in) :: knod
    double precision, intent(in) :: vlin(lnkx)
    double precision, intent(out) :: vnod(2, max(kmx, 1))

    integer :: L, k1, k2, k3, LL, LLL, Lb, Lt

    vnod = 0d0
    if (kmx == 0) then
       do L = 1, nd(knod)%lnx
          LL = iabs(L)
          k1 = ln(1, LL); k2 = ln(2, LL)
          if (k1 == knod) then
             vnod(1, 1) = vnod(1, 1) + vlin(LL) * wcx1(LL)
             vnod(2, 1) = vnod(2, 1) + vlin(LL) * wcy1(LL)
          end if
          if (k2 == knod) then
             vnod(1, 1) = vnod(1, 1) + vlin(LL) * wcx2(LL)
             vnod(2, 1) = vnod(2, 1) + vlin(LL) * wcy2(LL)
          end if
       end do

    else
       do L = 1, nd(knod)%lnx
          LL = iabs(nd(knod)%ln(L))
          k1 = ln(1, LL); k2 = ln(2, LL)
          if (k1 == knod) then
             call getLbotLtop(LL, Lb, Lt)
             if (Lt < Lb) cycle
             do LLL = Lb, Lt
                k3 = LLL - Lb + 1
                vnod(1, k3) = vnod(1, k3) + vlin(LLL) * wcx1(LL)
                vnod(2, k3) = vnod(2, k3) + vlin(LLL) * wcy1(LL)
             end do
          end if
          !
          if (k2 == knod) then
             call getLbotLtop(LL, Lb, Lt)
             if (Lt < Lb) cycle
             do LLL = Lb, Lt
                k3 = LLL - Lb + 1
                vnod(1, k3) = vnod(1, k3) + vlin(LLL) * wcx2(LL)
                vnod(2, k3) = vnod(2, k3) + vlin(LLL) * wcy2(LL)
             end do
          end if
       end do
    end if

 end subroutine linkstocentercartcomp
