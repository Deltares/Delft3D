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

 subroutine setkbotktop(jazws0) ! initialise vertical coordinates
    use m_netw
    use m_flowgeom
    use m_flow
    use m_flowtimes
    use m_transport, only: Constituents, ISALT, ITEMP

    implicit none

    integer :: jazws0

    integer :: k2, kb, k, n, kk, nL, nR, nlayb, nrlay, ktx
    integer :: kt0, kt1, kt2, kt3, LL, L, Lb, Lt, n1, n2, kb1, kb2, kt, kkk, kwaq, Ldn
    double precision :: zkk, h0, toplaymint, volkt, savolkt, tevolkt, dtopsi
    double precision :: w1, w2, w3, h1, h2, h3, zw1, zw2, zw3, bL1, bL2, bL3, ht1, ht2, ht3
    integer :: k1, k3, kb3, kk1, kk2, kk3

    integer :: numbd, numtp, j
    double precision :: drhok, a, aa, h00, zsl, aaa, sig, dsig, dsig0

    if (kmx == 0) return

    zws0 = zws
    ktop0 = ktop
    vol1 = 0d0

    nL = 1
    nR = 2

    if (Layertype == 1) then ! sigma only

       do n = 1, ndx

          kb = kbot(n)

          if (jased > 0) zws(kb - 1) = bl(n)
          h0 = s1(n) - zws(kb - 1) ! bl(n)
          !if (h0 < epshs) then
          !    ktop(n) = 1 ; cycle
          !endif
          do k = 1, kmxn(n)
             kk = kb + k - 1
             zws(kk) = zws(kb - 1) + h0 * zslay(k, 1)
             vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1)) ! just for now here
             vol1(n) = vol1(n) + vol1(kk)
          end do
          ktop(n) = kb - 1 + kmxn(n)

       end do
       return ! sigma only: quick exit

    else if (Layertype == 2) then ! z only

       ! toplayminthick = 0.5d0*( zslay(1,1) - zslay(0,1) )

       ! toplayminthick = 0d0

       do n = 1, ndx

          kb = kbot(n)

          !if (jased>0)  zws(kb-1) = bl(n)
          !h0        = s1(n) - zws(kb-1) ! bl(n)
          !if (h0 < epshs) then
          ! ktop(n) = 1 ; cycle
          !endif

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
                dtopsi = 1d0 / dble(ktx - kt1 + 1)
                do k = kt1, ktx
                   kk = k - kt1 + 1
                   zws(k) = zws(kt1 - 1) + h0 * dble(kk) * dtopsi
                end do
                zws(ktx) = s1(n)
                ktop(n) = ktx
             end if
          end if

          if (keepzlayeringatbed >= 2) then
             if (ktop(n) > kb) then
                if (keepzlayeringatbed <= 3) then ! fifty/fifty btween bed and top of second layer
                   zws(kb) = 0.5d0 * (zws(kb + 1) + zws(kb - 1))
                else if (keepzlayeringatbed <= 4) then ! idem, but never below current z-layer level of bed layer.
                   zws(kb) = max(zws(kb), 0.5d0 * (zws(kb + 1) + zws(kb - 1)))
                else
                   zws(kb) = max(zws(kb), 0.1d0 * zws(kb + 1) + 0.9d0 * zws(kb - 1)) ! not important
                end if
             end if
          end if

       end do

    else if (Layertype == 4) then ! density controlled sigma

       dkx = 0.5d0
       do n = 1, ndx
          drhok = 0.01d0
          kb = kbot(n); kt = kb - 1 + kmxn(n); ktop(n) = kt
          do k = kb + 1, kt
             if (abs(rho(k) - rho(k - 1)) > drhok) then
                drhok = abs(rho(k) - rho(k - 1))
                dkx(n) = dble(k - kb) / dble(kt - kb + 1)
                dkx(n) = min(0.8d0, dkx(n))
                dkx(n) = max(0.2d0, dkx(n))
             end if
          end do
       end do

       do j = 1, 10
          sdkx = 0d0
          do L = 1, Lnx
             k1 = ln(1, L); k2 = ln(2, L)
             sdkx(k1) = sdkx(k1) + dkx(k2)
             sdkx(k2) = sdkx(k2) + dkx(k1)
          end do

          a = 0.25d0
          do n = 1, ndx
             dkx(n) = a * dkx(n) + (1d0 - a) * sdkx(n) / dble(nd(n)%lnx)
          end do
       end do

       numbd = 0.5d0 * kmx; numtp = kmx - numbd; aaa = 1.05d0; aa = min(1d0, exp(-dts / Tsigma))

       dkx = 0.5d0

       do n = 1, ndx

          call getkbotktop(n, kb, kt)

          h0 = s1(n) - zws(kb - 1); h00 = max(epshu, zws0(kt) - zws0(kb - 1)); sig = 0d0
          dsig0 = 0.1d0 / dble(numtp)

          do k = 1, kmxn(n)

             if (k == 1) then
                dsig = dkx(n) * (1d0 - aaa) / (1d0 - aaa**numbd)
                dsig = dsig * aaa**(numbd - 1)
             else if (k <= numbd) then
                dsig = dsig / aaa
             else if (k == numbd + 1) then
                dsig = (1d0 - sig) * (1d0 - aaa) / (1d0 - aaa**numtp)
             else
                dsig = dsig * aaa
             end if

             !if (k == 1) then
             !   dsig = dkx(n) / numbd
             !else if ( k <= numbd ) then
             !
             !else if (k == numbd + 1) then
             !   aaa  = ( (1d0-dkx(n))**(1d0/dble(numbd)) - dsig0 ) / dsig0
             !   dsig = dsig0
             !else
             !   dsig = dsig*(1d0 + a)
             !endif

             sig = sig + dsig

             kk = kb + k - 1
             if (k == kmxn(n)) then
                zws(kk) = s1(n)
             else
                if (jazws0 == 1) then
                   zsl = zslay(k, 1)
                else
                   zsl = (1d0 - aa) * sig + aa * (zws0(kk) - zws0(kb - 1)) / h00
                end if
                zws(kk) = zws(kb - 1) + h0 * zsl
             end if

             vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1)) ! just for now here
             vol1(n) = vol1(n) + vol1(kk)
          end do

       end do
       return ! sigma only: quick exit

    else if (Layertype == 3) then ! mix : first do sigma and z

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
                ! toplayminthick = 0.5d0*( zslay(2,1) - zslay(1,1) )
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

    end if

    do n = 1, ndx

       kb = kbot(n)
       ktx = kb - 1 + kmxn(n)

       if (laydefnr(n) == 0) then ! overlap zone
          w1 = wflaynod(1, n); w2 = wflaynod(2, n); w3 = wflaynod(3, n)
          k1 = indlaynod(1, n); k2 = indlaynod(2, n); k3 = indlaynod(3, n)
          kb1 = kbot(k1); kb2 = kbot(k2); kb3 = kbot(k3)
          bL1 = zws(kb1 - 1); bL2 = zws(kb2 - 1); bL3 = zws(kb3 - 1)
          h1 = s1(k1) - bL1; h2 = s1(k2) - bL2; h3 = s1(k3) - bL3; h0 = s1(n) - zws(kb - 1)
          kt1 = ktop(k1); kt2 = ktop(k2); kt3 = ktop(k3)
          ht1 = zws(kt1) - zws(kt1 - 1)
          ht2 = zws(kt2) - zws(kt2 - 1)
          ht3 = zws(kt3) - zws(kt3 - 1)
          !Ld1 = laydefnr(k1)   ; Ld2 = laydefnr(k2)   ; Ld3 = laydefnr(k3)
          !dz1 = 0d0            ; dz2 = 0d0            ; dz3 = 0d0
          !if (laytyp(Ld1) == 2) dz1 = 0.5d0*zslay(2,Ld1) - zslay(1,Ld1)
          !if (laytyp(Ld2) == 2) dz2 = 0.5d0*zslay(2,Ld2) - zslay(1,Ld2)
          !if (laytyp(Ld3) == 2) dz3 = 0.5d0*zslay(2,Ld3) - zslay(1,Ld3)
          !toplaymint = w1*dz1 + w2*dz2 + w3*dz3

          toplaymint = 0.1d0 ! 0.5d0*min(ht1,ht2,ht3)

          do k = 1, kmxn(n)
             kk = kb + k - 1

             kk1 = kb1 + k - 1
             if (kk1 > kt1) then
                ! zw1 =  2d0*zws(kt1) - zws(kt1-1)
                ! zw1 = zw1 + 0.5d0*(ht2 + ht3)
                zw1 = zw1 + min(zw2, zw3)
             else
                zw1 = (zws(kk1) - bL1) / h1
             end if

             kk2 = kb2 + k - 1
             if (kk2 > kt2) then
                ! zw2 =  2d0*zws(kt2) - zws(kt2-1)
                ! zw2 = zw2 + 0.5d0*(ht1 + ht3)
                zw2 = zw2 + min(zw1, zw3)
             else
                zw2 = (zws(kk2) - bL2) / h2
             end if

             kk3 = kb3 + k - 1
             if (kk3 > kt3) then
                ! zw3 = 2d0*zws(kt3) - zws(kt3-1)
                ! zw3 = zw3 + 0.5d0*(ht1 + ht2)
                zw3 = zw3 + min(zw1, zw2)
             else
                zw3 = (zws(kk3) - bL3) / h3
             end if

             zkk = zws(kb - 1) + (w1 * zw1 + w2 * zw2 + w3 * zw3) * h0

             !sigm = dble(k) / dble( kmxn(n) )
             !zkk  = bl(n) + h0*sigm
             if (zkk < s1(n) - toplaymint .and. k < kmxn(n)) then
                zws(kk) = zkk
             else
                zws(kk) = s1(n)
                ktop(n) = kk
                if (ktx > kk) then
                   zws(kk + 1:ktx) = zws(kk)
                end if
                exit
             end if
          end do
       end if

       kt = ktop(n)
       kkk = kt - kb + 1 ! nr of layers
       if (kkk >= 2 .and. sigmagrowthfactor > 0) then ! bedlayers equal thickness
          ! zws(kb) = 0.5d0*(zws(kb+1) + zws(kb-1))
       end if
       if (kkk >= 3) then
          ! zws(kt-1) = 0.5d0*(zws(kt) + zws(kt-2))   ! toplayers equal thickness
       end if

       if (keepzlay1bedvol == 1) then ! inconsistent control volumes in baroclinic terms
          vol1(kb) = ba(n) * (zws(kb) - bl(n)) ! transport and momentum volumes not too big anymore
          vol1(n) = vol1(n) + vol1(kb)
          kb1 = kb + 1
       else ! Default, transport and momentum volumes too big
          kb1 = kb ! consistent with control volumes in baroclinic terms
       end if

       do kk = kb1, kt ! x
          vol1(kk) = ba(n) * (zws(kk) - zws(kk - 1)) ! just for now here
          vol1(n) = vol1(n) + vol1(kk)
       end do

       kt0 = ktop0(n)
       if (kt0 > kt) then
          volkt = vol0(kt)

          if (jasal > 0) savolkt = volkt * constituents(isalt, kt)
          if (jatem > 0) tevolkt = volkt * constituents(itemp, kt)

          do kkk = kt0, kt + 1, -1 ! old volumes above present ktop are lumped in ktop
             volkt = volkt + vol0(kkk)
             vol0(kt) = volkt
             if (jasal > 0) savolkt = savolkt + vol0(kkk) * constituents(isalt, kkk)
             if (jatem > 0) tevolkt = tevolkt + vol0(kkk) * constituents(itemp, kkk)
             if (ti_waq > 0) then
                do kwaq = kkk, kt + 1, -1
                   qwwaq(kwaq - 1) = qwwaq(kwaq - 1) - vol0(kkk)
                end do
             end if
             vol0(kkk) = 0d0
          end do
          if (volkt > 0) then
             if (jasal > 0) then
                constituents(isalt, kt) = savolkt / volkt
                if (ktx > kt) then
                   constituents(isalt, kt + 1:ktx) = constituents(isalt, kt)
                end if
             end if
             if (jatem > 0) then
                constituents(itemp, kt) = tevolkt / volkt
                if (ktx > kt) then
                   constituents(itemp, kt + 1:ktx) = constituents(itemp, kt)
                end if
             end if
          end if
       end if

    end do

    if (jazws0 == 1) then ! at initialise, store zws in zws0
       zws0 = zws
    end if

    if (layertype > 1) then ! ln does not change in sigma only

       do LL = 1, Lnx
          n1 = ln(1, LL); n2 = ln(2, LL)
          kt1 = ktop(n1); kt2 = ktop(n2)
          call getLbotLtop(LL, Lb, Lt)
          do L = Lb, Lt
             ln(1, L) = min(ln0(1, L), kt1)
             ln(2, L) = min(ln0(2, L), kt2)
          end do
       end do

    end if

 end subroutine setkbotktop
