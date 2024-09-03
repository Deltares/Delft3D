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

  subroutine CUTCELLS(n12)
     use m_netw
     use gridoperations
     use m_readyy
     implicit none
     integer, intent(in) :: N12
     integer :: ja, KMOD
     integer :: K, KM, K1, K2, L, LL, LNU, N, NN
     integer, allocatable :: KNP(:), KNEW(:)
     integer :: KK(4)

     double precision :: XM, YM

     call READYY('CUTCELLS', 0d0)

     call FINDCELLS(0) ! ALL FACES INSIDE LANDBOUNDARY PIECE

     allocate (KNP(NUMP)); KNP = 0
     allocate (KNEW(NUML)); KNEW = 0

     do N = 1, NUMP
        NN = netcell(N)%N
        if (NN >= 4) then
           K1 = NETCELL(N)%NOD(1)
           KNP(N) = KC(K1)
           do K = 2, NN
              K1 = NETCELL(N)%NOD(K)
              KNP(N) = KNP(N) * KC(K1) ! COMPLETELY INSIDE = 1
           end do
        end if
     end do

     KMOD = max(1, NUMP / 100)
     do N = 1, NUMP

        if (mod(n, KMOD) == 0) call READYY('CUTCELLS', dble(n) / dble(nump))

        if (KNP(N) == 0) then ! AT LEAST 1 POINT OUTSIDE POLYGON

           NN = netcell(N)%N

           do LL = 1, NN

              L = netcell(N)%LIN(LL)

              if (KNEW(L) == 0) then

                 call CROSSLINKPOLY(L, 0, 0, (/0/), (/0/), XM, YM, JA)

                 if (JA == 1) then
                    call DSETNEWPOINT(XM, YM, KM)
                    KNEW(L) = KM
                 end if

              end if

           end do

        end if

     end do

     do N = 1, NUMP

        K = 0
        NN = netcell(N)%N
        do LL = 1, NN

           L = netcell(N)%LIN(LL)
           K1 = KN(1, L); K2 = KN(2, L)

           if (KNP(N) == 0) then ! SHOULD BE HANDLED

              if (KNEW(L) /= 0) then ! NIEUW PUNT KOPPELEN

                 if (KNEW(L) > 0) then
                    if (KC(K1) == 1) then
                       call NEWLINK(KNEW(L), K2, LNU)
                    else
                       call NEWLINK(KNEW(L), K1, LNU)
                    end if
                    KNEW(L) = -1 * KNEW(L)
                 end if
                 K = K + 1
                 KK(K) = abs(KNEW(L))

              end if

           end if

        end do

        if (K >= 2) then
           call NEWLINK(KK(1), KK(2), LNU)
        end if

        if (K >= 3) then
           call NEWLINK(KK(2), KK(3), LNU)
        end if

        if (K >= 4) then
           call NEWLINK(KK(3), KK(4), LNU)
        end if
     end do

     if (N12 /= 4) then
        do L = 1, NUML
           K1 = KN(1, L); K2 = KN(2, L) ! NETPUNTEN DIE NIET IN NUMP VOORKWAMEN OOK MAAR GELIJK WEG
           if (K1 /= 0 .and. K2 /= 0) then
              if (KC(K1) == 1 .or. KC(K2) == 1) then
                 KN(1, L) = 0; KN(2, L) = 0
              end if
           end if
        end do
     end if

     deallocate (KNP, KNEW)

     call SETNODADM(0)

     call READYY('CUTCELLS', -1d0)

  end subroutine CUTCELLS
