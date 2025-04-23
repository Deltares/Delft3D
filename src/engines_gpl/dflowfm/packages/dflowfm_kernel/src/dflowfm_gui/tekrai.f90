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

module m_tekrai
   use m_wetbed, only: wetbed
   use m_thacker1d, only: thacker1d
   use m_model_specific, only: equatorial, poiseuille
   use m_drybed, only: drybed
   use m_coriolistilt, only: coriolistilt
   use m_corioliskelvin, only: corioliskelvin
   use m_carrier, only: carrier
   use m_belanger, only: belanger
   use m_tektransport1d
   use m_tekrailinesbathy
   use m_tekrailines

   implicit none

contains

   subroutine tekrai(nsiz)
      use precision, only: dp

      use m_setwor_rai
      use m_setwor
      use m_linewidth
      use m_isofil
      use m_isocol2
      use m_isocol
      use m_htext_rai
      use m_getktoplot
      use m_dhtext
      use m_arrowsxyzfac
      use m_netw
      use m_flow
      use m_flowgeom
      use m_flowtimes
      use unstruc_model
      use unstruc_display
      use m_raaitek
      use m_missing
      use m_sediment
      use m_strucs
      use fm_external_forcings_data
      use kdtree2Factory
      use m_depmax
      use m_vfac
      use m_drawthis
      use m_fbox
      use m_get_Lbot_Ltop
      use m_find_crossed_links_kdtree2
      use m_cirr
      use m_pfiller
      use m_set_col
      use m_inview
      use m_view_port
      use m_movabs
      use m_lnabs
      use m_zlin
      use m_znod

      implicit none

      integer :: nsiz

      real(kind=dp) :: xx2, zz
      integer :: k1, k2, n1, n2
      real(kind=dp) :: uu, ww
      real(kind=dp) :: zfac, zgaten
      integer :: l, k, kk, j, kplotorg, n, ncol
      real(kind=dp) :: doorh
      integer :: ng
      real(kind=dp) :: zz1, zz2, xz1, xz2
      real(kind=dp) :: xmn, xmx, ymx, zmx, zmx2, bot, top, xx, yy, bup, xxu, zzu
      real(kind=dp) :: xp(4), yp(4), zp(4), xxmn, xxmx, zn, dlay, dl, xp1, yp1, qsrck
      integer :: mx, kb, kt, Lb, Lt, LL, kplotfrombedorsurfacesav, ierror, intersection_count, japol = 0

      real(kind=dp), allocatable :: plotlin2(:)
      integer, allocatable :: ip(:), ip2(:)

      integer, allocatable :: crossed_links(:), polygon_nodes(:)
      real(kind=dp), allocatable :: polygon_segment_weights(:)

      if (ndx < 1) return

      if (npl > 1) then
         if (japol == 0) then
            kc = 0
            allocate (crossed_links(Lnx), polygon_nodes(Lnx), polygon_segment_weights(Lnx))
            call find_crossed_links_kdtree2(treeglob, NPL, XPL, YPL, ITYPE_FLOWLINK_1D_DUAL, Lnx, BOUNDARY_ALL, intersection_count, crossed_links, polygon_nodes, polygon_segment_weights, ierror)
            do LL = 1, intersection_count
               L = crossed_links(LL)
               kc(ln(1, L)) = 1; kc(ln(2, L)) = 1
            end do
            deallocate (crossed_links, polygon_nodes, polygon_segment_weights)
            japol = 1
         end if
      else
         japol = 0
      end if

      kplotfrombedorsurfacesav = kplotfrombedorsurface
      kplotfrombedorsurface = 1

      if (nsiz > 3) then
         call poiseuille(0)
         return
      end if

      xmn = 1e10
      xmx = -xmn
      ymn = 1e10
      ymx = -ymn
      zmn = 1e4
      zmx = -zmn
      do k = 1, ndx
         if (japol == 1) then
            if (kc(k) == 0) cycle
         end if

         xx = xz(k)
         yy = yz(k)
         if (inview(xx, yy)) then
            if (xz(k) < xmn) xmn = xz(k)
            if (xz(k) > xmx) xmx = xz(k)

            if (yz(k) < ymn) ymn = yz(k)
            if (yz(k) > ymx) ymx = yz(k)

            bot = bl(k)

            top = s1(k)

            zmn = min(zmn, min(bot, top))
            zmx = max(zmx, max(bot, top))

         end if
      end do

      if (jased == 1 .or. jased == 2 .and. zminrai == dmiss) then
         dlay = 0d0
         if (jaceneqtr == 1) then
            mx = ndxi
         else
            mx = size(grainlay, 2)
         end if
         do k = 1, mx
            if (jaceneqtr == 1) then
               xx = xz(k)
               yy = yz(k)
            else
               xx = xk(k)
               yy = yk(k)
            end if
            if (inview(xx, yy)) then
               dL = 0
               do j = 1, mxgr
                  dL = dL + grainlay(j, k)
               end do
               dlay = max(dlay, dL)
            end if
         end do

         zmn = zmn - dlay

      end if

      if (zmn == zmx) then
         zmn = zmn - 1d-3
         zmx = zmx + 1d-3
      else
         zmn = zmn - 1d-2 * (zmx - zmn)
      end if

      if (xmn == xmx) then
         xmn = xmn - 1d-3
         xmx = xmx + 1d-3
      end if

      if (nsiz == 1) then
         zmx = zmn + 1.2d0 * (zmx - zmn)
      else
         zmx = zmn + 1.5d0 * (zmx - zmn)
      end if

      if (zminrai /= -999) then
         zmn = zminrai
         zmx = max(zmn + 1d-2, zmaxrai)
      end if

      if (md_ident == 'transport1d') then
         zmn = 0
         zmx = 30
      end if

      if (YFAC > 0) then
         if (ymx /= ymn) then
            yfac = (zmx - zmn) / (ymx - ymn)
         else
            yfac = 0d-4
         end if
      end if

      zmx2 = zmx + yfac * (ymx - ymn)
      if (zmx2 == zmn) zmx2 = zmn + 1

      if (nsiz == 1) then
         call setwor_rai(0.0, 0.77, 1.0, 0.92, x1, zmn, x2, zmx2)
      else if (nsiz == 2) then
         call setwor_rai(0.0, 0.56, 1.0, 0.92, x1, zmn, x2, zmx2)
      else if (nsiz == 3) then
         call setwor_rai(0.0, 0.15, 1.0, 0.92, x1, zmn, x2, zmx2)
      end if

      if (kmx > 0) then
         kplotorg = kplot
         kplot = max(kplot, 1)
         kplot = min(kplot, kmxn(nplot))

         if (ndraw(28) > 3) then ! show node values

            if (ndraw(19) == 3) then

               if (.not. allocated(plotlin2)) then
                  allocate (plotlin2(lnkx), ip(lnkx), ip2(lnkx))
               end if
               plotlin = 0d0
               plotlin2 = 0d0
               ip = 0
               ip2 = 0
               do LL = 1, lnx
                  k1 = ln(1, LL)
                  k2 = ln(2, LL)
                  if (japol == 1) then
                     if (kc(k1) * kc(k2) == 0) cycle
                  end if

                  call getLbotLtop(LL, Lb, Lt)
                  do L = Lb, Lt
                     kplot = L - Lb + 1
                     plotlin(L) = plotlin(L) + znod(k1)
                     ip(L) = ip(L) + 1

                     kplot = max(kplot - 1, 1)
                     plotlin(L - 1) = plotlin(L - 1) + znod(k1)
                     ip(L - 1) = ip(L - 1) + 1

                     plotlin2(L - 1) = plotlin2(L - 1) + znod(k2)
                     ip2(L - 1) = ip2(L - 1) + 1

                     kplot = L - Lb + 1
                     plotlin2(L) = plotlin2(L) + znod(k2)
                     ip2(L) = ip2(L) + 1
                  end do
               end do

               do LL = 1, lnx
                  if (japol == 1) then
                     k1 = ln(1, LL); k2 = ln(2, LL)
                     if (kc(k1) * kc(k2) == 0) cycle
                  end if

                  call getLbotLtop(LL, Lb, Lt)
                  do L = Lb - 1, Lt
                     if (ip(L) > 0) then
                        plotlin(L) = plotlin(L) / ip(L)
                     end if
                     if (ip2(L) > 0) then
                        plotlin2(L) = plotlin2(L) / ip2(L)
                     end if
                  end do
               end do

               do LL = 1, lnx
                  k1 = ln(1, LL)
                  k2 = ln(2, LL)
                  if (japol == 1) then
                     if (kc(k1) * kc(k2) == 0) cycle
                  end if

                  xp(1) = xz(k1)
                  xp(2) = xp(1)
                  xp(3) = xz(k2)
                  xp(4) = xp(3)
                  call getLbotLtop(LL, Lb, Lt)
                  do L = Lb, Lt
                     k1 = ln(1, L)
                     k2 = ln(2, L)
                     yp(1) = zws(k1)
                     yp(2) = zws(k1 - 1)
                     yp(3) = zws(k2 - 1)
                     yp(4) = zws(k2)

                     zp(1) = plotlin(L)
                     zp(2) = plotlin(L - 1)
                     zp(3) = plotlin2(L - 1)
                     zp(4) = plotlin2(L)

                     call isofil(xp, yp, zp, 4, 0)
                  end do
               end do

            else

               do n = 1, ndxi
                  if (japol == 1) then
                     if (kc(n) == 0) cycle
                  end if
                  xxmn = minval(nd(n)%x)
                  xxmx = maxval(nd(n)%x)
                  xp(1) = xxmn
                  xp(2) = xxmx
                  xp(3) = xxmx
                  xp(4) = xxmn
                  kb = kbot(n)
                  kt = ktop(n)
                  do k = kb, kt

                     yp(1) = zws(k - 1)
                     yp(2) = yp(1)
                     yp(3) = zws(k)
                     yp(4) = yp(3)

                     kplot = k - kb + 1
                     zn = znod(n)
                     call isocol(zn, ncol)

                     if (ndraw(19) == 2) then
                        call dhtext(zn, xz(N), 0.5d0 * (YP(1) + YP(3)), 0.5d0 * (YP(1) + YP(3)))
                     else
                        call PFILLER(xp, yp, 4, ncol, ncol)
                     end if

                  end do
               end do

            end if
         end if

         if (ndraw(29) > 1) then ! show link values
            do LL = 1, Lnx
               n1 = ln(1, LL)
               n2 = ln(2, LL)
               if (japol == 1) then
                  if (kc(n1) * kc(n2) == 0) cycle
               end if

               xp(1) = xz(n1)
               xp(4) = xp(1)
               xp(2) = xz(n2)
               xp(3) = xp(2)

               Lb = Lbot(LL)
               do L = Lb, Ltop(LL)
                  k1 = ln(1, L)
                  k2 = ln(2, L)

                  yp(1) = zws(k1 - 1)
                  yp(2) = zws(k2 - 1)
                  yp(3) = zws(k2)
                  yp(4) = zws(k1)
                  kplot = L - Lb + 1
                  zn = zlin(LL)

                  call isocol2(zn, ncol)

                  if (ndraw(11) == 2) then
                     xp1 = 0.25d0 * (xp(1) + xp(2) + xp(3) + xp(4))
                     yp1 = 0.25d0 * (yp(1) + yp(2) + yp(3) + yp(4))
                     call dhtext(zn, xp1, yp1, yp1)
                  else
                     call PFILLER(xp, yp, 4, ncol, ncol)
                  end if
               end do
            end do
         end if

         call setcol(ncolwhite)
         if (NDRAW(2) > 0) then ! draw layer interface lines in white

            do LL = 1, lnxi
               n1 = ln(1, LL)
               n2 = ln(2, LL)
               if (japol == 1) then
                  if (kc(n1) * kc(n2) == 0) cycle
               end if

               xx = xz(n1)
               xx2 = xz(n2)
               xxu = xu(LL)

               if (hu(LL) > 0) then
                  Lb = Lbot(LL)
                  Lt = Ltop(LL)
                  do L = Lb, Lt
                     if (hu(L) > 0) then
                        k1 = ln(1, L)
                        k2 = ln(2, L)
                        call movabs(xx, zws(k1))
                        if (ndraw(42) > 0) then
                           zzu = min(bob(1, LL), bob(2, LL)) + hu(L)
                           call lnabs(xxu, zzu)
                        end if
                        call lnabs(xx2, zws(k2))
                     end if
                  end do
               end if

            end do

         end if

         n = nplot ! markerpoint
         kplot = kplotorg
         kplotfrombedorsurface = kplotfrombedorsurfacesav
         call getktoplot(n, k)
         k = max(k, kbot(n))
         xxmn = minval(nd(n)%x)
         xxmx = maxval(nd(n)%x)
         xp(1) = 0.5d0 * (xxmx + xxmn)
         yp(1) = 0.5d0 * (zws(k) + zws(k - 1))
         if (kmx == 0) then
            call cirr(xp(1), yp(1), ncolblack)
         else
            xp(1) = xxmn; xp(2) = xxmx; xp(3) = xxmx; xp(4) = xxmn
            yp(1) = zws(k - 1); yp(2) = yp(1)
            yp(3) = zws(k); yp(4) = yp(3)
            call PFILLER(xp, yp, 4, ncolblack, ncolblack)
         end if

         if (jaanalytic == 0) then

            call setcol(ncolblack)
            call LINEWIDTH(2)
            do LL = 1, lnxi
               if (hu(LL) > 0) then
                  n1 = ln(1, LL)
                  n2 = ln(2, LL)
                  if (japol == 1) then
                     if (kc(n1) * kc(n2) == 0) cycle
                  end if

                  xx = xz(n1)
                  xx2 = xz(n2)
                  call movabs(xx, s1(n1))
                  call lnabs(xx2, s1(n2))
               end if
            end do
         end if

         if (NDRAW(2) >= 1) then
            call setcol(31)
            do LL = 1, lnxi
               n1 = ln(1, LL)
               n2 = ln(2, LL)
               if (japol == 1) then
                  if (kc(n1) * kc(n2) == 0) cycle
               end if

               xx = xz(n1)
               xx2 = xz(n2)
               call movabs(xx, bl(n1))
               call lnabs(xx2, bl(n2))
            end do
         end if

         call LINEWIDTH(1)

         if (NDRAW(13) >= 2) then
            call setcol(klvec)
            zfac = (zmx2 - zmn) / (x2 - x1)
            do n = 1, ndxi
               if (japol == 1) then
                  if (kc(n) == 0) cycle
               end if

               xp(1) = xz(n)
               do k = kbot(n), ktop(n)

                  uu = ucx(k)
                  ww = 0.5d0 * (ww1(k) + ww1(k - 1))
                  yp(1) = 0.5d0 * (zws(k) + zws(k - 1))
                  call arrowsxyzfac(xp(1), yp(1), uu, ww, VFAC, zfac)

               end do

            end do

         end if

         kplot = kplotorg
      end if

      if (NDRAW(2) >= 1) then
         call tekrailinesbathy(31, 1) ! bl
      end if

      if (jased > 0 .and. jased < 4) then
         do j = 1, mxgr
            call tekrailinesbathy(15, 1 + j) ! grainlay 1,2 etc
         end do
      end if

      if (jagrw >= 1) then
         call tekrailines(ncolln, 1, 4) ! pgrw
         call tekrailines(ncolln, 1, 5) ! pgrw
      end if

      if (md_ident == 'transport1d' .or. jasal == 1 .and. (md_ident == 'wetbed' .or. md_ident == 'wetbed')) then
         call tekrailines(221, 1, 3) ! sa1
      end if

      if (kmx == 0) then
         call tekrailines(221, 1, 1) ! s1
         if (nonlin >= 2) then
            call tekrailines(ncolana, 1, 6) ! s1m
            ! call tekrailines(2,0,2)       ! bob
            ! call tekrailines(2,0,7)       ! bbb
         end if
      end if

      if (ndraw(30) == 5) then
         call setcol(2)
         do LL = 1, lnx
            n1 = ln(1, LL); n2 = ln(2, LL)
            if (japol == 1) then
               if (kc(n1) * kc(n2) == 0) cycle
            end if

            xz1 = xz(n1); xz2 = xz(n2)
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L); k2 = ln(2, L)
               zz1 = 0.5d0 * (zws(k1) + zws(k1 - 1))
               zz2 = 0.5d0 * (zws(k2) + zws(k2 - 1))
               call movabs(xz1, zz1)
               call lnabs(xz2, zz2)
            end do
         end do
      end if

      call setcol(ncolblack) ! NCOLANA)
      ! call LINEWIDTH(2)

      if (md_IDENT == 'transport1d') then
         call tektransport1D(time1 - tstart_user)
         call setcol(3)
         call movabs(xmn, 0d0)
         call lnabs(xmx, 0d0)
         !call htext( 1d0, xmx, 1d0)
      else if (md_IDENT == 'carrier') then
         call carrier(time1 - tstart_user)
      else if (md_IDENT(1:6) == 'drybed') then
         call drybed(time1 - tstart_user)
      else if (md_IDENT(1:6) == 'wetbed') then
         call wetbed(time1 - tstart_user)
      else if (md_IDENT(1:12) == 'coriolistilt') then
         call coriolistilt(time1 - tstart_user)
      else if (md_IDENT(1:14) == 'corioliskelvin') then
         call corioliskelvin(time1 - tstart_user)
      else if (index(md_ident, 'thacker1d') > 0) then
         call thacker1d(0, xz, yz, s1, bl, ndx, time1 - tstart_user)
      else if (md_IDENT == 'equator1d') then
         call equatorial(time1 - tstart_user)
      else if (md_IDENT(1:8) == 'belanger') then
         call belanger()
      end if
      ! call LINEWIDTH(1)

      call setcol(ncolblack)
      do ng = 1, ngatesg ! loop over gate signals, tekrai
         zgaten = zgate(ng)
         do n = L1gatesg(ng), L2gatesg(ng)
            L = kgate(3, n); k1 = ln(1, L); k2 = ln(2, L)
            if (japol == 1) then
               if (kc(k1) * kc(k2) == 0) cycle
            end if

            bup = min(bob(1, L), bob(2, L))
            call fbox(xz(k1), zgaten, xz(k2), zgaten + 20d0)
            ! call fbox(xz(k1),bup   ,xz(k2),bup-10d0)
         end do
      end do

      do ng = 1, ncgensg ! loop over gate signals, tekrai
         zgaten = zcgen(3 * ng - 1)
         do n = L1cgensg(ng), L2cgensg(ng)
            k1 = kcgen(1, n)
            k2 = kcgen(2, n)
            L = kcgen(3, n)
            if (japol == 1) then
               if (kc(k1) * kc(k2) == 0) cycle
            end if

            bup = min(bob(1, L), bob(2, L))
            doorh = 10d0
            if (generalstruc(ng)%gatedoorheight < 1d10 .and. generalstruc(ng)%gatedoorheight < 1d10) then
               if (generalstruc(ng)%gatedoorheight > 0d0) doorh = generalstruc(ng)%gatedoorheight
               call fbox(xz(k1), zgaten, xz(k2), zgaten + doorh)
            end if

            call fbox(xz(k1), bup, xz(k2), zmn) ! bup-10d0
         end do
      end do

      do ng = 1, ncdamsg ! loop over gate signals, tekrai
         do n = L1cdamsg(ng), L2cdamsg(ng)
            L = kcdam(3, n); k1 = ln(1, L); k2 = ln(2, L)
            if (japol == 1) then
               if (kc(k1) * kc(k2) == 0) cycle
            end if

            bup = bob(2, L) ! min( bob(1,L), bob(2,L) )
            call fbox(xz(k1), bup, xz(k2), bup - 10d0)
         end do
      end do

      call setcol(121)
      if (kmx > 0) then
         do n = 1, numsrc ! teksorsin rai
            qsrck = qsrc(n)
            kk = ksrc(1, n) ! 2D pressure cell nr from
            if (japol == 1) then
               if (kc(kk) == 0) cycle
            end if

            if (kk /= 0 .and. ksrc(2, n) > 0) then
               xp(1) = xz(kk)
               bup = 0.1d0 * sqrt(ba(kk))
               do k = ksrc(2, n), ksrc(3, n)
                  yp(1) = 0.5d0 * (zws(k) + zws(k - 1))
                  ! CALL KCIR(XP(1),YP(1),qsrck)
                  call fbox(xz(kk) - bup, zws(k - 1), xz(kk) + bup, zws(k))
               end do
            end if

            kk = ksrc(4, n) ! 2D pressure cell nr to
            if (japol == 1) then
               if (kc(kk) == 0) cycle
            end if

            if (kk /= 0 .and. ksrc(5, n) > 0) then
               xp(1) = xz(kk)
               bup = 0.1d0 * sqrt(ba(kk))
               do k = ksrc(5, n), ksrc(6, n)
                  yp(1) = 0.5d0 * (zws(k) + zws(k - 1))
                  ! CALL KCIR(XP(1),YP(1),qsrck)
                  call fbox(xz(kk) - bup, zws(k - 1), xz(kk) + bup, zws(k))
               end do
            end if
         end do
      end if

      if (javeg > 0 .and. kmx > 0) then
         call setcol(221)
         do k = 1, ndxi
            if (japol == 1) then
               if (kc(kk) == 0) cycle
            end if

            if (stemheight(k) > 0d0) then
               call movabs(xz(k), zws(kbot(k) - 1))
               xx = stemheight(k) * sin(phiv(k))
               yy = stemheight(k) * cos(phiv(k))
               call lnabs(xz(k) + xx, zws(kbot(k) - 1) + yy)
            end if
         end do
      end if

      call viewport(0.0, 0.0, 1.0, 1.0)

      if (nsiz > 1 .and. jtextflow > 0) then
         ! assen in 'gewone' aspect=1 wereld coordinaten, anders wordt de text plat afgedrukt in interacter
         !   CALL IGrUnits (0.0,0.0,1.0,1.0)
         call setwor(0d0, 0d0, 1d0, 1d0)

         call setcol(3) ! zwart
         zz = 0.05 * (zmx - zmn) / nsiz
         call htext_rai(zmn, x1 + 12d0 * rcir, zmn - 2d0 * zz, rcir, zz, 1)
         call htext_rai(zmx, x1 + 12d0 * rcir, zmx, rcir, zz, 1)
         call htext_rai(x1 + 10d0 * rcir, x1 + 12d0 * rcir, zmn - 2d0 * zz, rcir, zz, 2)
         call htext_rai(x2 - 10d0 * rcir, x2 - 12d0 * rcir, zmn - 2d0 * zz, rcir, zz, 2)
      end if
      kplotfrombedorsurface = kplotfrombedorsurfacesav
      call setwor(x1, y1, x2, y2)

      return
   end subroutine tekrai

end module m_tekrai
