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

  subroutine NETNODEVALS(MET)
     use M_FLOW
     use M_FLOWGEOM
     use m_netw
     use m_sediment
     use M_MISSING
     use m_ship

     implicit none
     integer :: MET

     integer :: k, L, j, K1, K2, K3, K4
     double precision :: x, y, z, uar
     double precision :: xn, yn, dis, rL ! for smallest distance to land boundary (method=7)

     if (MET == 1) return

     if (MET == 9 .and. allocated(ban)) then
        RNOD = 0d0
        do L = 1, LNXi
           K3 = LNCN(1, L)
           K4 = LNCN(2, L)
           K1 = LN(1, L)
           K2 = LN(2, L)
           UAR = CSU(L) * (ACL(L) * UCX(K1) + (1d0 - ACL(L)) * UCX(K2)) + &
                 SNU(L) * (ACL(L) * UCY(K1) + (1d0 - ACL(L)) * UCY(K2))
           UAR = UAR * DX(L)
           RNOD(K3) = RNOD(K3) - UAR
           RNOD(K4) = RNOD(K4) + UAR
        end do
        do L = LNXi + 1, lnx
           K3 = LNCN(1, L)
           K4 = LNCN(2, L)
           UAR = DX(L) * (1d0 - acl(L)) * U1(L)
           RNOD(K3) = RNOD(K3) - UAR
           RNOD(K4) = RNOD(K4) + UAR
        end do

        do k = 1, mxwalls
           k3 = walls(2, k)
           k4 = walls(3, k)
           if (irov == 0 .or. irov == 1) then ! free slip or partial slip hardly show vorticity
              RNOD(K3) = 0d0
              RNOD(K4) = 0d0
           else
              !if (irov == 1) then         ! partial slip
              !   uar = walls(16,k)
              !else if (irov == 2) then    ! no slip
              !   uar = 0.d0 ! walls(16,k)               ! *(1d0/walls(6,k) - 1d0/vonkar)
              !endif
              uar = walls(16, k)
              UAR = 0.5d0 * UAR * WALLS(9, K)
              RNOD(K3) = RNOD(K3) + UAR
              RNOD(K4) = RNOD(K4) + UAR
           end if
        end do

        do K = 1, NUMK
           if (BAN(K) > 0d0) then
              RNOD(K) = RNOD(K) / BAN(K)
           end if
        end do

     else
        do K = 1, NUMK
           X = XK(K)
           Y = YK(K)
           Z = ZK(K)

           if (MET == 2) then
              RNOD(K) = K
           else if (MET == 3) then
              RNOD(K) = NMK(K)
           else if (MET == 5) then
              if (allocated(NB)) then
                 if (size(NB) /= NUMK) then
                    exit
                 else
                    RNOD(K) = NB(K)
                 end if
              else
                 RNOD(K) = 0
              end if
           else if (MET == 6) then
              RNOD(K) = ZK(K)
           else if (MET == 7) then
              call toland(x, y, 1, MXLAN, 1, xn, yn, dis, j, rL)
              rnod(k) = dis
           else if (MET == 8 .and. jased > 0 .and. jaceneqtr > 1) then
              RNOD(K) = grainlay(jgrtek, k) ! erodable layer
           else if (MET == 10) then
              RNOD(k) = BAN(K)
           else if (MET == 11) then
              RNOD(k) = zspc(k)
           end if
        end do
     end if
     return
  end subroutine NETNODEVALS
