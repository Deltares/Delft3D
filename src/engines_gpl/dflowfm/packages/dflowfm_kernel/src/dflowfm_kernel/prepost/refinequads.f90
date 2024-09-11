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

  subroutine REFINEQUADS()
     use m_netw
     use M_AFMETING
     use gridoperations
     use m_readyy
     use m_delpol
     use m_copynetboundstopol
     implicit none
     integer :: jaddrand
     integer :: k, KMOD
     integer :: k1
     integer :: k12
     integer :: k2
     integer :: k23
     integer :: k3
     integer :: k34
     integer :: k4
     integer :: k41
     integer :: ki
     integer :: kk
     integer :: km
     integer :: l
     integer :: l12
     integer :: l12o
     integer :: l23
     integer :: l23o
     integer :: l34
     integer :: l34o
     integer :: l41
     integer :: l41o
     integer :: lfa
     integer :: ll
     integer :: ll2
     integer :: lnu
     integer :: n
     integer :: nf
     integer :: numkorg
     double precision :: XM, YM
     integer, allocatable :: KNP(:)
     integer KKI(5)

     integer :: numk_old, jatolan ! for generating polygon
     integer, allocatable, dimension(:) :: kc_old ! for generating polygon

     LFA = 2
     JADDRAND = 1

     call INCREASENETW(4 * NUMK, 6 * NUML)

     NUMKORG = NUMK
     numk_old = numk

     call FINDCELLS(4); LC = 0

     call READYY('Refine quads', 0d0)

     allocate (KNP(NUMP)); KNP = 0
     do N = 1, NUMP
        if (netcell(N)%N == 4) then
           K1 = netcell(N)%NOD(1)
           K2 = netcell(N)%NOD(2)
           K3 = netcell(N)%NOD(3)
           K4 = netcell(N)%NOD(4)
           KNP(N) = KC(K1) * KC(K2) * KC(K3) * KC(K4)
        end if
     end do

     KMOD = max(1, NUMK / 100)
     do N = 1, NUMP

        if (mod(n, KMOD) == 0) call READYY('Refine quads', dble(n) / dble(nump))

        if (KNP(N) == 1) then

           K1 = netcell(N)%NOD(1)
           K2 = netcell(N)%NOD(2)
           K3 = netcell(N)%NOD(3)
           K4 = netcell(N)%NOD(4)

           L12 = netcell(N)%LIN(1); L12O = L12
           L23 = netcell(N)%LIN(2); L23O = L23
           L34 = netcell(N)%LIN(3); L34O = L34
           L41 = netcell(N)%LIN(4); L41O = L41

           K12 = 0
           if (KN(1, L12) /= 0 .and. M13QUAD >= 0) then
              call REFINELINK2(L12, K12); LC(L12O) = -K12; if (LNN(L12O) == 2) KC(K12) = 3
           end if

           K23 = 0
           if (KN(1, L23) /= 0 .and. M13QUAD <= 0) then
              call REFINELINK2(L23, K23); LC(L23O) = -K23; if (LNN(L23O) == 2) KC(K23) = 3
           end if

           K34 = 0
           if (KN(1, L34) /= 0 .and. M13QUAD >= 0) then
              call REFINELINK2(L34, K34); LC(L34O) = -K34; if (LNN(L34O) == 2) KC(K34) = 3
           end if

           K41 = 0
           if (KN(1, L41) /= 0 .and. M13QUAD <= 0) then
              call REFINELINK2(L41, K41); LC(L41O) = -K41; if (LNN(L41O) == 2) KC(K41) = 3
           end if

           if (M13QUAD == 0) then

              XM = 0.25d0 * (XK(K1) + XK(K2) + XK(K3) + XK(K4))
              YM = 0.25d0 * (YK(K1) + YK(K2) + YK(K3) + YK(K4))

              call DSETNEWPOINT(XM, YM, KM); KC(KM) = 2

           end if

           if (M13QUAD == 0) then
              if (K12 /= 0) then
                 call NEWLINK(KM, K12, lnu)
              else if (LC(L12) < 0) then
                 call NEWLINK(KM, -LC(L12), lnu)
              end if

              if (K34 /= 0) then
                 call NEWLINK(KM, K34, lnu)
              else if (LC(L34) < 0) then
                 call NEWLINK(KM, -LC(L34), lnu)
              end if
           else if (M13QUAD > 0) then

              if (K12 == 0) K12 = -LC(L12)
              if (K34 == 0) K34 = -LC(L34)
              call NEWLINK(K12, K34, lnu)
           end if

           if (M13QUAD == 0) then
              if (K23 /= 0) then
                 call NEWLINK(KM, K23, lnu)
              else if (LC(L23) < 0) then
                 call NEWLINK(KM, -LC(L23), lnu)
              end if

              if (K41 /= 0) then
                 call NEWLINK(KM, K41, lnu)
              else if (LC(L41) < 0) then
                 call NEWLINK(KM, -LC(L41), lnu)
              end if

           else if (M13QUAD < 0) then

              if (K23 == 0) K23 = -LC(L23)
              if (K41 == 0) K41 = -LC(L41)
              call NEWLINK(K23, K41, lnu)

           end if

        end if

     end do

     KMOD = max(1, NUMP / 100)
     do N = 1, NUMP
        if (mod(n, KMOD) == 0) call READYY('Refine quads', dble(n) / dble(nump))
        if (KNP(N) == 0) then
           NF = netcell(N)%N
           if (NF == 4) then

              KI = 0
              do KK = 1, NF
                 K = netcell(N)%NOD(KK)
                 L = netcell(N)%LIN(KK)
                 if (LC(L) < 0) then
                    KI = KI + 1; KKI(KI) = -LC(L); LL = KK
                 end if
              end do

              if (KI == 1) then
                 K0 = KKI(1)
                 LL2 = LL + 2
                 if (LL2 > 4) LL2 = LL2 - 4
                 LL2 = netcell(N)%LIN(LL2)
                 K1 = KN(1, LL2); K2 = KN(2, LL2)

                 if (K1 /= 0 .and. K2 /= 0) then
                    call NEWLINK(K0, K1, LNU) ! ; KC(K1) = 6
                    call NEWLINK(K0, K2, LNU) ! ; KC(K2) = 6
                 end if

              else if (KI == 2) then
                 call NEWLINK(KKI(1), KKI(2), LNU)
                 do KK = 1, NF
                    K = netcell(N)%NOD(KK)
                    if (KC(K) == 0) then
                       call NEWLINK(KKI(1), K, LNU) !; KC(KKI(1)) = 3
                       call NEWLINK(KKI(2), K, LNU) !; KC(KKI(2)) = 3
                       exit
                    end if
                 end do

              end if

           end if

        end if
     end do

     call READYY('Refine quads', -1d0)

     call SETNODADM(0)

     deallocate (KNP)

     jatolan = 1
     call confrm('Copy refinement border to polygon?', jatolan)
     if (jatolan == 1) then
!     store original node mask
        allocate (kc_old(numk))
        kc_old = min(kc, 1) ! see admin_mask

!     deative polygon
        call savepol()
        call delpol()
        call findcells(100)
!     reactivate polygon
        call restorepol()
!     mark cells crossed by polygon, by setting lnn of their links appropriately
        kc_old(numk_old + 1:numk) = 1
        call mark_cells_crossed_by_poly(numk, kc_old)
        call delpol()
        call copynetboundstopol(0, 0, 0, 1)

        deallocate (kc_old)
     end if

     return
  end subroutine REFINEQUADS
