!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
module m_set_kbot_ktop
   implicit none
   private
   public :: set_kbot_ktop
   public :: update_vertical_coordinates_boundary

contains

   !> Initialise vertical coordinates, calculate layer volumes, set layer interfaces
   subroutine set_kbot_ktop(jazws0)
      use precision, only: dp
      use m_flowgeom, only: ndx, ba, bl, ln, lnx, nd
      use m_flow, only: kmx, zws0, zws, ktop0, ktop, vol1, layertype, kbot, jased, kmxn, &
                        zslay, toplayminthick, numtopsig, keepzlayeringatbed, dkx, rho, s1, sdkx, tsigma, epshu, laydefnr, laytyp, &
                        LAYTP_SIGMA, LAYTP_Z, LAYTP_POLYGON_MIXED, LAYTP_DENS_SIGMA
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_zlayer_indices, only: getzlayerindices
      use m_flowtimes, only: dts

      integer, intent(in) :: jazws0 !< Whether to store zws in zws0 at initialisation

      integer :: kb, k, n, kk, nlayb, nrlay, ktx
      integer :: kt, Ldn, kt1, k1, k2
      real(kind=dp) :: zkk, h0, dtopsi
      logical :: ktop_changed

      integer :: numbd, numtp, j, L
      real(kind=dp) :: drhok, a, aa, h00, zsl, aaa, sig, dsig, dsig0

      if (kmx == 0) then
         return
      end if

      zws0 = zws
      ktop0 = ktop
      vol1 = 0.0_dp

      if (Layertype == LAYTP_SIGMA) then ! sigma-layers
         do n = 1, ndx
            kb = kbot(n)

            if (jased > 0) then
               zws(kb - 1) = bl(n)
            end if
            h0 = s1(n) - zws(kb - 1)
            do k = 1, kmxn(n)
               kk = kb + k - 1
               zws(kk) = zws(kb - 1) + h0 * zslay(k, 1)
               vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1)) ! just for now here
               vol1(n) = vol1(n) + vol1(kk)
            end do
            ktop(n) = kb - 1 + kmxn(n)

         end do
         return

      else if (Layertype == LAYTP_Z) then ! z- or z-sigma-layers
         do n = 1, ndx
            kb = kbot(n)

            ktx = kb + kmxn(n) - 1
            call getzlayerindices(n, nlayb, nrlay)

            do k = kb, ktx
               kk = k - kb + nlayb
               zkk = zslay(kk, 1)
               if (zkk < s1(n) - toplayminthick .and. k < ktx) then
                  zws(k) = zkk
               else
                  zws(k) = s1(n)
                  ktop(n) = k
                  if (ktx > k) then
                     zws(k + 1:ktx) = zws(k)
                  end if
                  exit
               end if
            end do

            if (numtopsig > 0) then
               kt1 = max(kb, ktx - numtopsig + 1)
               if (ktop(n) >= kt1) then
                  h0 = s1(n) - zws(kt1 - 1)
                  dtopsi = 1.0_dp / real(ktx - kt1 + 1, kind=dp)
                  do k = kt1, ktx
                     kk = k - kt1 + 1
                     zws(k) = zws(kt1 - 1) + h0 * real(kk, kind=dp) * dtopsi
                  end do
                  zws(ktx) = s1(n)
                  ktop(n) = ktx
               end if
            end if

            if (keepzlayeringatbed >= 2) then
               if (ktop(n) > kb) then
                  if (keepzlayeringatbed <= 3) then ! fifty/fifty btween bed and top of second layer
                     zws(kb) = 0.5_dp * (zws(kb + 1) + zws(kb - 1))
                  else if (keepzlayeringatbed <= 4) then ! idem, but never below current z-layer level of bed layer.
                     zws(kb) = max(zws(kb), 0.5_dp * (zws(kb + 1) + zws(kb - 1)))
                  else
                     zws(kb) = max(zws(kb), 0.1_dp * zws(kb + 1) + 0.9_dp * zws(kb - 1)) ! not important
                  end if
               end if
            end if
         end do

      else if (Layertype == LAYTP_POLYGON_MIXED) then ! polygon defined z-layers
         do n = 1, ndx
            kb = kbot(n)

            Ldn = laydefnr(n)
            if (Ldn > 0) then
               if (Laytyp(Ldn) == 1) then ! sigma
                  h0 = s1(n) - zws(kb - 1)
                  do k = 1, kmxn(n) - 1
                     zws(kb + k - 1) = zws(kb - 1) + h0 * zslay(k, Ldn)
                  end do
                  ktop(n) = kb + kmxn(n) - 1
                  zws(ktop(n)) = s1(n)
               else if (Laytyp(Ldn) == 2) then ! z

                  ktx = kb + kmxn(n) - 1
                  call getzlayerindices(n, nlayb, nrlay)
                  do k = kb, ktx
                     kk = k - kb + nlayb
                     zkk = zslay(kk, Ldn)
                     if (zkk < s1(n) - toplayminthick .and. k < ktx) then
                        zws(k) = zkk
                     else
                        zws(k) = s1(n)
                        ktop(n) = k
                        if (ktx > k) then
                           zws(k + 1:ktx) = zws(k)
                        end if
                        exit
                     end if
                  end do
               end if
            end if
         end do

      else if (Layertype == LAYTP_DENS_SIGMA) then ! density controlled sigma-layers
         dkx = 0.5_dp
         do n = 1, ndx
            drhok = 0.01_dp
            kb = kbot(n)
            kt = kb - 1 + kmxn(n)
            ktop(n) = kt
            do k = kb + 1, kt
               if (abs(rho(k) - rho(k - 1)) > drhok) then
                  drhok = abs(rho(k) - rho(k - 1))
                  dkx(n) = real(k - kb, kind=dp) / real(kt - kb + 1, kind=dp)
                  dkx(n) = min(0.8_dp, dkx(n))
                  dkx(n) = max(0.2_dp, dkx(n))
               end if
            end do
         end do

         do j = 1, 10
            sdkx = 0.0_dp
            do L = 1, Lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               sdkx(k1) = sdkx(k1) + dkx(k2)
               sdkx(k2) = sdkx(k2) + dkx(k1)
            end do

            a = 0.25_dp
            do n = 1, ndx
               dkx(n) = a * dkx(n) + (1.0_dp - a) * sdkx(n) / real(nd(n)%lnx, kind=dp)
            end do
         end do

         numbd = 0.5_dp * kmx
         numtp = kmx - numbd
         aaa = 1.05_dp
         aa = min(1.0_dp, exp(-dts / Tsigma))

         dkx = 0.5_dp

         do n = 1, ndx

            call getkbotktop(n, kb, kt)

            h0 = s1(n) - zws(kb - 1)
            h00 = max(epshu, zws0(kt) - zws0(kb - 1))
            sig = 0.0_dp
            dsig0 = 0.1_dp / real(numtp, kind=dp)

            do k = 1, kmxn(n)
               if (k == 1) then
                  dsig = dkx(n) * (1.0_dp - aaa) / (1.0_dp - aaa**numbd)
                  dsig = dsig * aaa**(numbd - 1)
               else if (k <= numbd) then
                  dsig = dsig / aaa
               else if (k == numbd + 1) then
                  dsig = (1.0_dp - sig) * (1.0_dp - aaa) / (1.0_dp - aaa**numtp)
               else
                  dsig = dsig * aaa
               end if

               sig = sig + dsig

               kk = kb + k - 1
               if (k == kmxn(n)) then
                  zws(kk) = s1(n)
               else
                  if (jazws0 == 1) then
                     zsl = zslay(k, 1)
                  else
                     zsl = (1.0_dp - aa) * sig + aa * (zws0(kk) - zws0(kb - 1)) / h00
                  end if
                  zws(kk) = zws(kb - 1) + h0 * zsl
               end if

               vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1)) ! just for now here
               vol1(n) = vol1(n) + vol1(kk)
            end do
         end do
         return ! sigma only: quick exit
      end if

      do n = 1, ndx

         kb = kbot(n)
         ktx = kb - 1 + kmxn(n)

         if (laydefnr(n) == 0) then ! overlap zone
            call process_overlap_zone(n, kb, ktx, s1, ktop_changed)
         end if

         kt = ktop(n)
         call calculate_node_volumes(n, kb, kt, ktx)
      end do

      if (jazws0 == 1) then ! at initialise, store zws in zws0
         zws0 = zws
      end if

      if (layertype /= LAYTP_SIGMA) then ! ln does not change in sigma only
         call update_link_connectivity()
      end if
   end subroutine set_kbot_ktop

   !> Update vertical coordinates for boundary nodes only after an update of s0, avoiding global side effects
   !! but including link connectivity updates when dry-to-wet transitions occur.
   !! Is a special version of the set_kbot_ktop subroutine for after s0 updates on the boundary nodes.
   subroutine update_vertical_coordinates_boundary()
      use precision, only: dp
      use m_flowgeom, only: ndx, ba, bl
      use m_flow, only: kmx, zws, zws0, ktop, vol1, layertype, kbot, jased, kmxn, &
                        zslay, toplayminthick, numtopsig, keepzlayeringatbed, s0, tsigma, epshu, laydefnr, laytyp, &
                        LAYTP_SIGMA, LAYTP_Z, LAYTP_POLYGON_MIXED, LAYTP_DENS_SIGMA, &
                        nbndz, kbndz
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_zlayer_indices, only: getzlayerindices
      use m_flowtimes, only: dts

      integer :: kb, k, n, kk, nlayb, nrlay, ktx
      integer :: kt0, kt1, kt, Ldn
      real(kind=dp) :: zkk, h0, dtopsi
      integer :: i_bnd
      logical :: ktop_changed

      integer :: numbd, numtp
      real(kind=dp) :: aa, h00, zsl, aaa, sig, dsig

      logical :: need_link_update

      need_link_update = .false.

      if (kmx == 0 .or. nbndz == 0) then
         return
      end if

      if (Layertype == LAYTP_SIGMA) then ! sigma only
         do i_bnd = 1, nbndz
            n = kbndz(1, i_bnd)
            if (n <= 0 .or. n > ndx) then
               cycle
            end if

            vol1(n) = 0.0_dp

            kb = kbot(n)

            if (jased > 0) then
               zws(kb - 1) = bl(n)
            end if
            h0 = s0(n) - zws(kb - 1)
            do k = 1, kmxn(n)
               kk = kb + k - 1
               zws(kk) = zws(kb - 1) + h0 * zslay(k, 1)
               vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1))
               vol1(n) = vol1(n) + vol1(kk)
            end do
            ktop(n) = kb - 1 + kmxn(n)
         end do
         return ! Early exit - no link updates needed for sigma layers, volumes already calculated

      else if (Layertype == LAYTP_Z) then ! z or z-sigma
         do i_bnd = 1, nbndz
            n = kbndz(1, i_bnd)
            if (n <= 0 .or. n > ndx) then
               cycle
            end if

            kb = kbot(n)
            kt0 = ktop(n) ! Store original ktop for comparison

            ktx = kb + kmxn(n) - 1
            call getzlayerindices(n, nlayb, nrlay)

            do k = kb, ktx
               kk = k - kb + nlayb
               zkk = zslay(kk, 1)
               if (zkk < s0(n) - toplayminthick .and. k < ktx) then
                  zws(k) = zkk
               else
                  zws(k) = s0(n)
                  ktop(n) = k
                  if (ktx > k) then
                     zws(k + 1:ktx) = zws(k)
                  end if
                  exit
               end if
            end do

            if (numtopsig > 0) then
               kt1 = max(kb, ktx - numtopsig + 1)
               if (ktop(n) >= kt1) then
                  h0 = s0(n) - zws(kt1 - 1)
                  dtopsi = 1.0_dp / real(ktx - kt1 + 1, kind=dp)
                  do k = kt1, ktx
                     kk = k - kt1 + 1
                     zws(k) = zws(kt1 - 1) + h0 * real(kk, kind=dp) * dtopsi
                  end do
                  zws(ktx) = s0(n)
                  ktop(n) = ktx
               end if
            end if

            if (keepzlayeringatbed >= 2) then
               if (ktop(n) > kb) then
                  if (keepzlayeringatbed <= 3) then
                     zws(kb) = 0.5_dp * (zws(kb + 1) + zws(kb - 1))
                  else if (keepzlayeringatbed <= 4) then
                     zws(kb) = max(zws(kb), 0.5_dp * (zws(kb + 1) + zws(kb - 1)))
                  else
                     zws(kb) = max(zws(kb), 0.1_dp * zws(kb + 1) + 0.9_dp * zws(kb - 1)) ! not important
                  end if
               end if
            end if

            ! Check if this node experienced significant layer changes
            if (ktop(n) /= kt0) then
               need_link_update = .true.
            end if
         end do

      else if (Layertype == LAYTP_POLYGON_MIXED) then ! polygon defined z-layers
         do i_bnd = 1, nbndz
            n = kbndz(1, i_bnd)
            if (n <= 0 .or. n > ndx) then
               cycle
            end if

            kb = kbot(n)
            kt0 = ktop(n) ! Store original ktop for comparison

            Ldn = laydefnr(n)
            if (Ldn > 0) then
               if (Laytyp(Ldn) == 1) then ! sigma
                  h0 = s0(n) - zws(kb - 1)
                  do k = 1, kmxn(n) - 1
                     zws(kb + k - 1) = zws(kb - 1) + h0 * zslay(k, Ldn)
                  end do
                  ktop(n) = kb + kmxn(n) - 1
                  zws(ktop(n)) = s0(n)
               else if (Laytyp(Ldn) == 2) then ! z
                  ktx = kb + kmxn(n) - 1
                  call getzlayerindices(n, nlayb, nrlay)
                  do k = kb, ktx
                     kk = k - kb + nlayb
                     zkk = zslay(kk, Ldn)
                     if (zkk < s0(n) - toplayminthick .and. k < ktx) then
                        zws(k) = zkk
                     else
                        zws(k) = s0(n)
                        ktop(n) = k
                        if (ktx > k) then
                           zws(k + 1:ktx) = zws(k)
                        end if
                        exit
                     end if
                  end do
               end if
            end if

            ! Check if this node experienced significant layer changes
            if (ktop(n) /= kt0) then
               need_link_update = .true.
            end if
         end do

      else if (Layertype == LAYTP_DENS_SIGMA) then ! density controlled sigma-layers
         ! Simplified version for boundary nodes only - skip the global smoothing,
         ! because looping over all nodes would be too expensive for updating boundary nodes only.
         numbd = 0.5_dp * kmx
         numtp = kmx - numbd
         aaa = 1.05_dp
         aa = min(1.0_dp, exp(-dts / Tsigma))

         do i_bnd = 1, nbndz
            n = kbndz(1, i_bnd)
            if (n <= 0 .or. n > ndx) then
               cycle
            end if

            call getkbotktop(n, kb, kt)

            h0 = s0(n) - zws(kb - 1)
            h00 = max(epshu, zws0(kt) - zws0(kb - 1))
            sig = 0.0_dp
            vol1(n) = 0.0_dp

            do k = 1, kmxn(n)
               if (k == 1) then
                  dsig = 0.5_dp * (1.0_dp - aaa) / (1.0_dp - aaa**numbd) ! Use default dkx = 0.5
                  dsig = dsig * aaa**(numbd - 1)
               else if (k <= numbd) then
                  dsig = dsig / aaa
               else if (k == numbd + 1) then
                  dsig = (1.0_dp - sig) * (1.0_dp - aaa) / (1.0_dp - aaa**numtp)
               else
                  dsig = dsig * aaa
               end if

               sig = sig + dsig
               kk = kb + k - 1

               if (k == kmxn(n)) then
                  zws(kk) = s0(n)
               else
                  zsl = (1.0_dp - aa) * sig + aa * (zws0(kk) - zws0(kb - 1)) / h00
                  zws(kk) = zws(kb - 1) + h0 * zsl
               end if

               vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1))
               vol1(n) = vol1(n) + vol1(kk)
            end do
         end do
         return ! Early exit - no link updates needed for density sigma layers, volumes already calculated
      end if

      ! Handle overlap zones for all boundary nodes
      do i_bnd = 1, nbndz
         n = kbndz(1, i_bnd)
         if (n <= 0 .or. n > ndx) then
            cycle
         end if

         kb = kbot(n)
         ktx = kb - 1 + kmxn(n)

         if (laydefnr(n) == 0) then ! overlap zone
            call process_overlap_zone(n, kb, ktx, s0, ktop_changed)

            ! Check if this node experienced significant layer changes
            if (ktop_changed) then
               need_link_update = .true.
            end if
         end if
      end do

      ! Finalize volumes for all boundary nodes
      do i_bnd = 1, nbndz
         n = kbndz(1, i_bnd)
         if (n <= 0 .or. n > ndx) then
            cycle
         end if

         kb = kbot(n)
         kt = ktop(n)
         ktx = kb - 1 + kmxn(n)
         call calculate_node_volumes(n, kb, kt, ktx)
      end do

      ! Update link connectivity only if boundary nodes experienced wetting or significant layer changes
      if (need_link_update .and. layertype /= LAYTP_SIGMA) then
         call update_link_connectivity()
      end if
   end subroutine update_vertical_coordinates_boundary

   !> Calculate layer volumes for a single node
   subroutine calculate_node_volumes(n, kb, kt, ktx)
      use precision, only: dp
      use m_flowgeom, only: ba, bl
      use m_flow, only: vol1, zws, keepzlay1bedvol

      integer, intent(in) :: n !< Node index
      integer, intent(in) :: kb !< Bottom layer index for the node
      integer, intent(in) :: kt !< Top layer index for the node
      integer, intent(in) :: ktx !< Maximum layer index for the node

      integer :: kb1, kk

      vol1(n) = 0.0_dp ! Reset before accumulation

      if (keepzlay1bedvol == 1) then ! inconsistent control volumes in baroclinic terms
         vol1(kb) = ba(n) * (zws(kb) - bl(n)) ! transport and momentum volumes not too big anymore
         vol1(n) = vol1(n) + vol1(kb)
         kb1 = kb + 1
      else ! Default, transport and momentum volumes too big
         kb1 = kb ! consistent with control volumes in baroclinic terms
      end if

      do kk = kb1, kt
         vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1))
         vol1(n) = vol1(n) + vol1(kk)
      end do

      call handle_constituent_conservation(n, kt, ktx)
   end subroutine calculate_node_volumes

   !> Process overlap zone interpolation for a node
   subroutine process_overlap_zone(n, kb, ktx, water_levels, ktop_changed)
      use precision, only: dp
      use m_flow, only: wflaynod, indlaynod, kbot, ktop, zws, kmxn

      integer, intent(in) :: n !< Node index
      integer, intent(in) :: kb !< Bottom layer index for the node
      integer, intent(in) :: ktx !< Maximum layer index for the node
      real(kind=dp), dimension(:), intent(in) :: water_levels !< Water levels at the node (s0 or s1)
      logical, intent(out) :: ktop_changed !< Flag indicating if ktop has changed

      real(kind=dp) :: w1, w2, w3, h1, h2, h3, zw1, zw2, zw3, bL1, bL2, bL3
      integer :: k1, k2, k3, kb1, kb2, kb3, kk1, kk2, kk3
      integer :: kt1, kt2, kt3, k, kk, kt0
      real(kind=dp) :: h0, zkk, toplaymint

      kt0 = ktop(n) ! Store original ktop for comparison
      ktop_changed = .false.

      w1 = wflaynod(1, n)
      w2 = wflaynod(2, n)
      w3 = wflaynod(3, n)
      k1 = indlaynod(1, n)
      k2 = indlaynod(2, n)
      k3 = indlaynod(3, n)
      kb1 = kbot(k1)
      kb2 = kbot(k2)
      kb3 = kbot(k3)
      bL1 = zws(kb1 - 1)
      bL2 = zws(kb2 - 1)
      bL3 = zws(kb3 - 1)
      h1 = water_levels(k1) - bL1 ! For k1, but use water_level at current node
      h2 = water_levels(k2) - bL2 ! For k2, but use water_level at current node
      h3 = water_levels(k3) - bL3 ! For k3, but use water_level at current node
      h0 = water_levels(n) - zws(kb - 1)
      kt1 = ktop(k1)
      kt2 = ktop(k2)
      kt3 = ktop(k3)

      toplaymint = 0.1_dp

      do k = 1, kmxn(n)
         kk = kb + k - 1

         kk1 = kb1 + k - 1
         zw1 = 1.0_dp
         zw2 = 1.0_dp
         zw3 = 1.0_dp
         if (kk1 > kt1) then
            zw1 = zw1 + min(zw2, zw3)
         else
            zw1 = (zws(kk1) - bL1) / h1
         end if

         kk2 = kb2 + k - 1
         if (kk2 > kt2) then
            zw2 = zw2 + min(zw1, zw3)
         else
            zw2 = (zws(kk2) - bL2) / h2
         end if

         kk3 = kb3 + k - 1
         if (kk3 > kt3) then
            zw3 = zw3 + min(zw1, zw2)
         else
            zw3 = (zws(kk3) - bL3) / h3
         end if

         zkk = zws(kb - 1) + (w1 * zw1 + w2 * zw2 + w3 * zw3) * h0

         if (zkk < water_levels(n) - toplaymint .and. k < kmxn(n)) then
            zws(kk) = zkk
         else
            zws(kk) = water_levels(n)
            ktop(n) = kk
            if (ktx > kk) then
               zws(kk + 1:ktx) = zws(kk)
            end if
            exit
         end if
      end do

      ! Check if this node experienced significant layer changes
      if (ktop(n) /= kt0) then
         ktop_changed = .true.
      end if
   end subroutine process_overlap_zone

   !> Handle constituent conservation when layers are removed
   subroutine handle_constituent_conservation(n, kt, ktx)
      use precision, only: dp
      use m_flow, only: ktop0, vol0, jasal, temperature_model, TEMPERATURE_MODEL_NONE, qwwaq
      use m_flowtimes, only: ti_waq
      use m_transport, only: Constituents, ISALT, ITEMP

      integer, intent(in) :: n !< Node index
      integer, intent(in) :: kt !< Current layer index for the node
      integer, intent(in) :: ktx !< Maximum layer index for the node

      integer :: kt0, kkk, kwaq
      real(kind=dp) :: volkt, savolkt, tevolkt

      kt0 = ktop0(n)
      if (kt0 > kt) then
         volkt = vol0(kt)

         if (jasal > 0) then
            savolkt = volkt * constituents(isalt, kt)
         end if
         if (temperature_model /= TEMPERATURE_MODEL_NONE) then
            tevolkt = volkt * constituents(itemp, kt)
         end if

         do kkk = kt0, kt + 1, -1 ! old volumes above present ktop are lumped in ktop
            volkt = volkt + vol0(kkk)
            vol0(kt) = volkt
            if (jasal > 0) then
               savolkt = savolkt + vol0(kkk) * constituents(isalt, kkk)
            end if
            if (temperature_model /= TEMPERATURE_MODEL_NONE) then
               tevolkt = tevolkt + vol0(kkk) * constituents(itemp, kkk)
            end if
            if (ti_waq > 0) then
               do kwaq = kkk, kt + 1, -1
                  qwwaq(kwaq - 1) = qwwaq(kwaq - 1) - vol0(kkk)
               end do
            end if
            vol0(kkk) = 0.0_dp
         end do
         if (volkt > 0) then
            if (jasal > 0) then
               constituents(isalt, kt) = savolkt / volkt
               if (ktx > kt) then
                  constituents(isalt, kt + 1:ktx) = constituents(isalt, kt)
               end if
            end if
            if (temperature_model /= TEMPERATURE_MODEL_NONE) then
               constituents(itemp, kt) = tevolkt / volkt
               if (ktx > kt) then
                  constituents(itemp, kt + 1:ktx) = constituents(itemp, kt)
               end if
            end if
         end if
      end if
   end subroutine handle_constituent_conservation

   !> Update link connectivity for layer changes
   subroutine update_link_connectivity()
      use m_flowgeom, only: ln, lnx
      use m_flow, only: ktop, ln0
      use m_get_Lbot_Ltop, only: getlbotltop

      integer :: LL, L, Lb, Lt, n1, n2, kt1, kt2

      do LL = 1, Lnx
         n1 = ln(1, LL)
         n2 = ln(2, LL)
         kt1 = ktop(n1)
         kt2 = ktop(n2)
         call getLbotLtop(LL, Lb, Lt)
         do L = Lb, Lt
            ln(1, L) = min(ln0(1, L), kt1)
            ln(2, L) = min(ln0(2, L), kt2)
         end do
      end do
   end subroutine update_link_connectivity
end module m_set_kbot_ktop
