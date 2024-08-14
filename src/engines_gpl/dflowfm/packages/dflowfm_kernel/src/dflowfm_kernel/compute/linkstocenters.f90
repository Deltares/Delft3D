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

 subroutine linkstocenters(vnod, vlin) ! set flow node value based on flow link values scalar

    use m_flow
    use m_netw
    use m_flowgeom

    implicit none

    double precision :: vlin(lnkx)
    real :: vnod(ndkx)
    integer :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

    vnod = 0d0

    if (kmx == 0) then
       do L = 1, lnx
          k1 = ln(1, L); k2 = ln(2, L)
          vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
          vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
       end do
    else
       do LL = 1, lnx
          call getLbotLtop(LL, Lb, Lt)
          do L = Lb, Lt
             k1 = ln(1, L); k2 = ln(2, L)
             vnod(k1) = vnod(k1) + vlin(L) * wcL(1, LL)
             vnod(k2) = vnod(k2) + vlin(L) * wcL(2, LL)
          end do
       end do

       !$OMP PARALLEL DO                                          &
       !$OMP PRIVATE(kk,kb,kt,k)
       do kk = 1, ndx
          call getkbotktop(kk, kb, kt)
          do k = kt + 1, kb + kmxn(kk) - 1
             vnod(k) = vnod(kt)
          end do
       end do
       !$OMP END PARALLEL DO
    end if
 end subroutine linkstocenters
