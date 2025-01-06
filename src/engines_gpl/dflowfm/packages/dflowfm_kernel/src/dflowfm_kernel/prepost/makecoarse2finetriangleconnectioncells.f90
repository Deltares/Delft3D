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

module m_makecoarse2finetriangleconnectioncells

implicit none

private

public :: makecoarse2finetriangleconnectioncells

contains

  subroutine MAKECOARSE2FINETRIANGLECONNECTIONCELLS()
     use precision, only: dp
     use m_netw
     use m_sferic, only: jsferic, jasfer3D
     use m_missing, only: dxymis
     use geometry_module, only: dcosphi
     use gridoperations
     use m_set_nod_adm
     use m_new_link

     integer :: N3(6)
     real(kind=dp) :: ARN, XCN, YCN
     integer :: N, NN, K3, K, K0, NR, KA, KB, K1, K2, L1, L2, LNU, K01, KP, K7, KK3, K03, NN3, KK, L, K23

     call FINDCELLS(0)

     do N = 1, NUMP
        NN = netcell(N)%N
        if (NN == 5 .or. NN == 6) then
           K3 = 0; N3 = 0
           do K = 1, NN
              K0 = netcell(N)%NOD(K)
              NR = NMK(K0)
              if (NR == 3) then
                 KA = K + 1; if (KA > NN) KA = KA - NN; K1 = netcell(N)%NOD(KA) ! L2  L1  K0  K1  K2
                 KA = K + 2; if (KA > NN) KA = KA - NN; K2 = netcell(N)%NOD(KA)
                 KA = K - 1; if (KA < 1) KA = KA + NN; L1 = netcell(N)%NOD(KA)
                 KA = K - 2; if (KA < 1) KA = KA + NN; L2 = netcell(N)%NOD(KA)

                 if (abs(dcosphi(XK(L2), YK(L2), XK(L1), YK(L1), XK(L1), YK(L1), XK(K0), YK(K0), jsferic, jasfer3D, dxymis)) < 0.3 .and. &
                     abs(dcosphi(XK(K0), YK(K0), XK(K1), YK(K1), XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dxymis)) < 0.3) then

                    if (abs(dcosphi(XK(L1), YK(L1), XK(K0), YK(K0), XK(K0), YK(K0), XK(K1), YK(K1), jsferic, jasfer3D, dxymis)) > 0.7) then
                       ! PROBABLY THE SMALL SIDES AROUND THE CENTRAL POINT
                       K3 = K3 + 1
                       N3(K3) = K
                    end if

                 end if

              end if
           end do
           if (K3 == 3) then
              K3 = 1d0 * K3
           end if

           if (K3 == 1 .and. NN == 5) then
              K0 = netcell(N)%NOD(N3(K3))
              KA = N3(K3) + 2; if (KA > NN) KA = KA - NN; K1 = netcell(N)%NOD(KA)
              KB = N3(K3) - 2; if (KB < 1) KB = KB + NN; K2 = netcell(N)%NOD(KB)
              call NEWLINK(K0, K1, LNU)
              call NEWLINK(K0, K2, LNU)
           else if (K3 == 2 .and. NN == 6) then
              K3 = 1
              K0 = netcell(N)%NOD(N3(K3)); K01 = K0
              KA = N3(K3) + 2; if (KA > NN) KA = KA - NN; K1 = netcell(N)%NOD(KA)
              KB = N3(K3) - 2; if (KB < 1) KB = KB + NN; K2 = netcell(N)%NOD(KB)
              call NEWLINK(K0, K1, LNU)
              call NEWLINK(K0, K2, LNU)
              K3 = 2
              K0 = netcell(N)%NOD(N3(K3))
              KA = N3(K3) + 2; if (KA > 6) KA = KA - 6; K1 = netcell(N)%NOD(KA)
              KB = N3(K3) - 2; if (KB < 1) KB = KB + 6; K2 = netcell(N)%NOD(KB)
              if (K1 /= K01) call NEWLINK(K0, K1, LNU)
              if (K2 /= K01) call NEWLINK(K0, K2, LNU)
           else if (K3 == -3 .and. NN == 7) then ! NAAR CENTRAAL POINT MET VIJF LINKJES, TWEE VANUIT HOEKEN, DIRE VANUIT K3 = 3
              call getcellsurface(N, ARN, XCN, YCN) ! SORRY, 7 IS EILAND EN WORDT DUS SOWIESO NIET HERKEND, EVEN VERGETEN
              call dSETNEWPOINT(XCN, YCN, KP)

              do K3 = 1, 3
                 K0 = netcell(N)%NOD(N3(K3))
                 call NEWLINK(K0, KP, LNU)
              end do

              do K7 = 1, 7
                 K = NETCELL(N)%NOD(K7)
                 KK3 = 0 ! VOOR PUNTEN DIE GEEN K3 ZIJN
                 do K3 = 1, 3
                    K03 = N3(K3)
                    KK3 = K03
                 end do
                 if (KK3 == 0) then

                    NN3 = 0
                    do KK = 1, NMK(K)
                       L = NETCELL(N)%LIN(K7)
                       call OTHERNODE(K, L, K2)
                       do K3 = 1, 3
                          K23 = N3(K3)
                          if (K23 == K2) then
                             NN3 = NN3 + 1
                          end if
                       end do
                       if (NN3 /= 2) then
                          call NEWLINK(K0, KP, LNU)
                       end if
                    end do
                 end if
              end do

           end if

        end if

     end do

     call SETNODADM(0)

  end subroutine MAKECOARSE2FINETRIANGLECONNECTIONCELLS

end module m_makecoarse2finetriangleconnectioncells
