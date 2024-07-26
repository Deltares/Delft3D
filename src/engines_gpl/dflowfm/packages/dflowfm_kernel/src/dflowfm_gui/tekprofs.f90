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

  subroutine tekprofs() ! and initialise some turb pars
     use m_flow
     use m_flowgeom
     use m_wearelt
     use M_RAAITEK
     use m_observations
     use m_missing
     use m_polygon
     use m_wind
     use m_flowtimes
     use unstruc_model, only: md_ident
     use UNSTRUC_DISPLAY
     use m_waves, only: ustokes
     use m_sediment, only: jased, sed
     use m_transport !, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents, itrac2const, const_names, const_units

     implicit none

     integer :: ini = 0, kt, jaref
     double precision :: vmin, vmax, ugem, viceld
     integer :: n, kb, km, km1, k, kk, ku, kku, Lb0, Lb, Lt, Lm1, L, LL, La
     double precision :: zmin, zmax
     double precision :: h0, b0, z00, zinc, cz, ustbref, ustwref, zint, z1, dz2, zz
     double precision :: tkebot, tkesur, tkewin
     double precision :: epsbot, epssur, dzkap, sqcf, ulx, sg, drhodz, rhomea, rhop0, prsappr
     double precision, external :: densfm, setrhofixedp

     double precision :: VMAX2, VMIN2, DV2, VAL2
     integer :: NCOLS2, NV2, NIS2, NIE2, JAAUTO2, is, Ls, LLs, Lbs, Lts
     integer :: jabruv

     integer :: ndraw
     common / DRAWTHIS / ndraw(50)
     common / DEPMAX2 / VMAX2, VMIN2, DV2, VAL2(256), NCOLS2(256), NV2, NIS2, NIE2, JAAUTO2

     if (ndx < 1 .or. kmx < 2 .or. ndraw(35) == 0) return

     n = nplot
     kb = kbot(n)
     kt = ktop(n)
     if (kt - kb + 1 < 2) then
        return ! for less than 2 layers
     end if

     uLx = 0d0
     LL = 0
     do kk = 1, nd(n)%lnx
        L = nd(n)%ln(kk)
        La = iabs(L)
        Lb = Lbot(La)
        is = 1; if (L < 0) is = -1
        if (is * u1(Lb) > uLx) then ! search link with highest outflow velocity
           LL = La
           uLx = is * u1(Lb)
        end if
     end do
     if (LL == 0) then
        LL = La
     end if
     if (hu(LL) < epshu) then
        LL = 0
     else
        Lb = Lbot(LL)
        Lb0 = Lb - 1
        Lt = Ltop(LL)
        Lm1 = Lt - Lb0 + 1
     end if

     L = LL

     if (ini == 0) then
        call MAKEPLOTAREAS(2, 4, 1) ! ndraw(35))
        ini = 1
     end if

     b0 = zws(kb - 1)
     h0 = zws(kt) - b0
     if (h0 < epshu) return
     ! h0 = 5d0 ! slope

     zmin = 0d0
     zmax = 1.1d0 * h0 ! + 1d0

     if (zmaxrai /= dmiss .and. zminrai /= dmiss) then
        zmax = zmaxrai - zminrai
     end if

     km = kt - kb + 1
     km1 = km + 1

     ugem = sum(ucx(kb:kt)) / dble(kt - kb + 1)

     hwref(0) = 0d0
     do k = kb, kt
        kk = k - kb + 1
        hcref(kk) = 0.5d0 * (zws(k) + zws(k - 1)) - b0
        hwref(kk) = zws(k) - b0
     end do

     jaref = index(md_ident, 'slope')
     if (LL > 0) then
        if (bedslope == 0d0) then
           zinc = max(1d-20, (s1(ln(2, LL)) - s1(ln(1, LL))) * dxi(LL))
        else
           zinc = bedslope
        end if

        if (zinc > 0) then
           sg = -1d0
        else
           sg = 1d0
        end if
        zinc = abs(zinc)

        if (frcuni > 0 .and. jaref > 0) then
           call getczz0(h0, frcuni, ifrctypuni, cz, z00)
           ugem = Cz * sqrt(h0 * zinc)
           sqcf = sag / Cz
           ustbref = sqcf * ugem ! ustb(LL)
           ustwref = ustw(LL)

           viceld = vonkar * ustbref * h0 / 6d0

           do k = kb, kt
              kk = k - kb + 1
              ucxref(kk) = sg * ustbref * log(c9of1 + hcref(kk) / z00) / vonkar
           end do

           if (iturbulencemodel == 1) then

              vicwref = vicoww

           else if (iturbulencemodel == 2) then

              do k = 1, km - 1
                 zint = hwref(k) / h0
                 z1 = 1d0 - zint
                 zz = h0 * z1 * zint
                 vicwref(k) = zz * ustbref * vonkar
              end do
              vicwref(0) = 0d0
              vicwref(km) = 0d0

           else if (iturbulencemodel >= 3) then

              tkebot = ustbref**2 / sqcmukep
              tkewin = ustwref**2 / sqcmukep
              tkesur = max(tkewin, ustbref**2)
              ! tkesur = 0d0
              epsbot = cewall * tkebot**1.5d0
              epssur = cewall * tkesur**1.5d0

              ! TKE and epsilon at layer interfaces:
              do k = 1, km - 1
                 zint = hwref(k) / h0
                 z1 = 1d0 - zint
                 tkin1ref(k) = tkebot * z1 + tkesur * zint
                 teps1ref(k) = (epsbot / zint + epssur / z1) / h0
                 teps1ref(k) = max(epseps, teps1ref(k))
                 vicwref(k) = cmukep * tkin1ref(k)**2 / abs(teps1ref(k))
              end do

              ! TKE, epsilon and mixing coefficients at free surface:
              tkin1ref(km) = tkesur
              teps1ref(km) = epssur / (hwref(km) - hwref(km - 1))
              ! TKE, epsilon and mixing coefficients at bed:
              tkin1ref(0) = tkebot
              teps1ref(0) = epsbot / (hwref(1) - hwref(0)) !  dzcs(kb)

              dzkap = vonkar * 0.5d0 * (hwref(1) - hwref(0))
              teps1ref(0) = epsbot / dzkap
              vicwref(0) = ustbref * dzkap
              vicwref(km) = 0d0
           end if

           if (dnt == 0) then ! at initialise : copy refprofiles to solution

              do LLs = 1, lnx
                 Lbs = Lbot(LLs); Lts = Ltop(LLs)
                 do Ls = Lbs, Lts
                    k = Ls - Lbs + 1
                    u1(Ls) = csu(LLs) * ucxref(k)
                    turkin1(Ls) = tkin1ref(k)
                    tureps1(Ls) = teps1ref(k)
                 end do
                 turkin1(Lbs - 1) = tkin1ref(0)
                 tureps1(Lbs - 1) = teps1ref(0)

              end do

           end if

        end if

        do kk = 1, km - 1
           kku = kk + 1
           k = kb + kk - 1
           ku = k + 1
           dz2 = (hcref(kku) - hcref(kk))**2
           if (jaref > 0) dijdijref(kk) = ((ucxref(kku) - ucxref(kk))**2) / dz2
           dijdij(kk) = ((ucx(ku) - ucx(k))**2) / dz2
        end do
        dijdijref(0) = 0d0 ! ustbref / max(1d-6,vicwref(0) )
        dijdij(0) = 0d0 !
     end if

     ! TEKFN(NSC,NF,JW,X,Y,N,X1,X2,Y1,Y2,NCOL,TITLE,JAUTO,JP,DAG)
     ! NSC    schermnr
     ! NF     functienr
     ! JW     update assen    1 = ja, niet 1 = nee
     ! JAUTO  zelf schalen    1 = ja, niet 1 = nee
     ! JP     teken profielen 1 = ja, niet 1 = teken isolijnen
     ! in dat geval DAG (nr van de dag) toevoegen

     if (ndraw(35) == 1) then ! turbulence profiles etc

        ucm(1:km) = sqrt(ucx(kb:kt) * ucx(kb:kt) + ucy(kb:kt) * ucy(kb:kt))
        call getvminmax(1, vmin, vmax, ucm, km)
        vmin = 0d0; vmax = 1d0
        if (jaref > 0) then
           call TEKFN(1, 1, 0, ucxref, hcref, km, vmin, vmax, zmin, zmax, 31, 'vel. mag.', 0, 1, 0d0, 0) ! mid-layers
        end if

        call TEKFN(1, 2, 1, ucm(1:km), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'vel. mag.', 0, 2, 0d0, kplot)

        if (LL > 0) then
           vmin = 0d0
           vmax = 0.0d0
           vmax = max(vmax, maxval(vicwwu(Lb0:Lt)), vmin + 1d-5)
           if (jaref > 0) then
              call TEKFN(2, 3, 0, vicwref, hwref, km1, vmin, vmax, zmin, zmax, 31, 'vicww', 0, 1, 0d0, 0) ! mid-layers
           end if

           if (LL > 0) then
              call TEKFN(2, 4, 1, vicwwu(Lb0:), hwref, Lm1, vmin, vmax, zmin, zmax, KLPROF, 'vicww', 0, 2, 0d0, kplot + 1)
           end if
        end if

        ! vmax = 0.1d0  ; vmin = 0d0
        ! if (frcuni > 0 .and. ndraw(35) == 1 ) then
        ! call TEKFN(3, 5, 0, dijdijref(1:), hwref(1:) , km-1, vmin, vmax, zmin, zmax,  31, 'dijdij'    , 0, 1 , 0d0,0)   ! interfaces
        ! endif
        ! call TEKFN(3, 6, 1, dijdij(1:)   , hwref(1:) , km-1, vmin, vmax, zmin, zmax, KLAXS, 'dijdij'    , 0, 2 , 0d0,kplot)

        ! vmin = -0.15d0; vmax = 0.15d0
        ! call TEKFN(3, 6, 1, qw(kb:kt)    , hwref(1:) , km, vmin, vmax, zmin, zmax, KLAXS, 'qw'    , 1, 2 , 0d0,kplot)

        vmax = max(minval(ww1(kb:kt)), maxval(ww1(kb:kt)))
        vmin = -vmax
        call TEKFN(3, 6, 1, ww1(kb:kt), hwref(1:), km, vmin, vmax, zmin, zmax, KLPROF, 'ww1', 1, 2, 0d0, kplot)

        if (iturbulencemodel >= 3 .and. LL > 0) then

           if (frcuni > 0 .and. ndraw(35) == 1) then
              vmin = 0d0; vmax = 0d0; vmax = max(vmax, maxval(turkin1(Lb0:Lt)), vmin + 1d-5)
              if (jaref > 0) call TEKFN(4, 7, 0, tkin1ref, hwref, km1, vmin, vmax, zmin, zmax, 31, 'tkin1', 0, 1, 0d0, 0) ! interfaces
              call TEKFN(4, 8, 1, turkin1(Lb0:Lt), hwref, Lm1, vmin, vmax, zmin, zmax, KLPROF, 'tkin1', 0, 2, 0d0, kplot + 1)
           end if

           if (jasal > 0 .and. jatem > 0 .and. idensform < 0) then
              if (idensform == 13) then
                 do k = kb, kt
                    rhop0 = setrhofixedp(k, 0d0)
                    dijdij(k - kb + 1) = rhop0
                 end do
                 call getvminmax(5, vmin, vmax, dijdij(1:km), km)
                 call TEKFN(5, 10, 1, dijdij(1:km), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'rhopot', 1, 2, 0d0, kplot)
              else
                 call getvminmax(6, vmin, vmax, rho(kb:), kt - kb + 1)
                 call TEKFN(5, 10, 1, rho(kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'rhopot', 1, 2, 0d0, kplot)
              end if
           else
              if (frcuni > 0 .and. ndraw(35) == 1) then
                 vmin = 0d0; vmax = 0.d0; vmax = max(vmax, maxval(tureps1(Lb0:Lt)), vmin + 1d-5)
                 if (jaref > 0) call TEKFN(5, 9, 0, teps1ref, hwref, km1, vmin, vmax, zmin, zmax, 31, 'teps1', 0, 1, 0d0, 0) ! interfaces
                 call TEKFN(5, 10, 1, tureps1(Lb0:Lt), hwref, Lm1, vmin, vmax, zmin, zmax, KLPROF, 'teps1', 0, 2, 0d0, kplot + 1)
              end if
           end if

        end if

        if (iconst_cur > 0 .and. iconst_cur <= NUMCONST) then

           vmin = 1d2
           vmax = -1d2
           vmin = min(vmin, minval(constituents(iconst_cur, kb:kt)))
           vmax = max(vmax, maxval(constituents(iconst_cur, kb:kt)), vmin + 1d-5)

           call TEKFN(6, 11, 1, constituents(iconst_cur, kb:kt), hcref, km, vmin, vmax, zmin, zmax, 221, trim(const_names(iconst_cur)), 1, 2, 0d0, kplot)

           !vmin = 1d2
           !vmax = -1d2
           !do iconst=ISED1,ISEDN
           !   vmin = min(vmin, minval(constituents(iconst,kb:kt)) )
           !   vmax = max(vmax, maxval(constituents(iconst,kb:kt)), vmin+1d-5 )
           !end do
           !
           !do iconst=ISED1,ISEDN
           !   call TEKFN(6,11, 1, constituents(iconst,kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, 221, 'all seds', 1, 2 , 0d0,kplot)
           !end do

        else if (frcuni > 0 .and. ndraw(35) == 1 .and. LL > 0) then
           ! if (jaref > 0) call TEKFN(5, 9, 0, teps1ref    , hwref   , km1, vmin, vmax, zmin, zmax,  31, 'teps1'      , 0, 1 , 0d0,0)   ! interfaces
           dijdij(1:km - 1) = (vicwwu(Lb:Lt - 1) + vicoww) * (u1(Lb + 1:Lt) - u1(Lb:Lt - 1)) * 2d0 / (hu(Lb + 1:Lt) + hu(Lb:Lt - 1))
           dijdij(0) = ustb(L) * ustb(L)
           if (csu(L) * u1(Lb) < 0) dijdij(0) = -dijdij(0)
           dijdij(km) = ustw(L) * ustw(L)
           if (allocated(wdsu)) then
              if (wdsu(L) < 0) dijdij(km) = -dijdij(km)
           end if

           call getvminmax(6, vmin, vmax, dijdij(0:km), km + 1)
           call TEKFN(6, 11, 1, dijdij(0:km), hwref, Lm1, vmin, vmax, zmin, zmax, KLPROF, 'Reyn', 0, 2, 0d0, kplot + 1)
        end if

        jabruv = 1
        if (jabruv > 0) then

           !call TEKFN(6,11, 1, tem1(kt:kt) , hcref(kt-kb+1)  , 1, 0d0, 86400.d0, -200d0, 600d0, KLPROF, '-200 - 600 WATT' , 0, 2 , 0d0,kplot)
           !CALL TEKHEATS( time1)

           do k = kb, kt - 1
              kk = k - kb + 1
              prsappr = ag * rhomean * (zws(kt) - zws(k))
              drhodz = (setrhofixedp(k + 1, prsappr) - setrhofixedp(k, prsappr)) / (0.5d0 * (zws(k + 1) - zws(k - 1)))
              rhomea = 0.5d0 * (rho(k + 1) + rho(k))
              dijdij(kk) = -ag * drhodz / rhomea
           end do
           dijdij(0) = 0d0; dijdij(km) = dijdij(km - 1)
           vmin = minval(dijdij(1:km - 1))
           vmax = maxval(dijdij(1:km - 1))
           if (abs(vmin) < vmax) vmin = -vmax
           if (vmax < abs(vmin)) vmax = -vmin
           if (abs(vmin - vmax) < 1d-20) then
              vmax = vmax + 1d-5; vmin = vmin - 1d-5
           end if
           call TEKFN(7, 13, 1, dijdij(0:km), hwref, Lm1, vmin, vmax, zmin, zmax, KLPROF, 'Bruva', 0, 2, 0d0, kplot + 1)
        else if (jatem > 0) then
           if (jafahrenheit > 0) then
              dijdij(1:km) = 32d0 + constituents(itemp, kb:kt) * 1.8d0
              vmin = 70d0; vmax = 90d0
              call TEKFN(7, 13, 1, dijdij(1:km), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'Temp (F)', 0, 2, 0d0, kplot)
           else
              dijdij(1:km) = constituents(itemp, kb:kt)
              call getvminmax(7, vmin, vmax, dijdij(1:km), km)
              call TEKFN(7, 13, 1, dijdij(1:km), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'Temp (C)', 0, 2, 0d0, kplot)
           end if
        else if (jased > 0 .and. jased < 4) then
           vmin = 1d2; vmax = -1d2
           vmin = min(vmin, minval(sed(1, kb:kt)))
           vmax = max(vmax, maxval(sed(1, kb:kt)), vmin + 1d-5)
           call TEKFN(7, 13, 1, sed(1, kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'sed', 0, 2, 0d0, kplot)
        else
           vmin = 1d2; vmax = -1d2
           vmin = min(vmin, minval(ucy(kb:kt)))
           vmax = max(vmax, maxval(ucy(kb:kt)), vmin + 1d-5)
           call TEKFN(7, 13, 1, ucy(kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'y-velocity', 0, 2, 0d0, kplot)
        end if

        if (jawave > 0 .and. jawaveStokes > 0 .and. .not. flowWithoutWaves) then
           vmin = minval(ucx(kb:kt) - ustokes(Lb:Lt))
           vmax = maxval(ucx(kb:kt) - ustokes(Lb:Lt))
           vmax = max(abs(vmin), abs(vmax)); vmin = -vmax
           dijdij(1:km) = ucx(kb:kt) - ustokes(Lb:Lt)
           call TEKFN(8, 12, 1, dijdij(1:km), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'x-velocity', 0, 2, 0d0, kplot)
        else
           vmin = minval(ucx(kb:kt))
           vmax = maxval(ucx(kb:kt))
           vmax = max(abs(vmin), abs(vmax), 1d-4); vmin = -vmax
           call TEKFN(8, 12, 1, ucx(kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'x-velocity', 0, 2, 0d0, kplot)
        end if

     else if (jasal > 0) then

        if (vmin2 > vmax2) then
           vmin = 0d0
           vmax = 33d0
        else
           vmin = vmin2
           vmax = vmax2
        end if
        if (ndraw(35) == 2) then
           do n = 1, min(8, numobs)
              kk = kobs(n)
              if (kk < 1) cycle
              call getkbotktop(kk, kb, kt)
              if (kt > kb) then
                 call TEKFN(n, 2 * n - 1, 1, constituents(isalt, kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'sal', 0, 2, 0d0, kplot)
              end if
           end do
        else if (ndraw(35) == 3) then
           do n = 1, min(8, npl)
              call in_flowcell(xpl(n), ypl(n), kk)
              if (kk == 0) cycle
              call getkbotktop(kk, kb, kt)
              if (kt > kb) then
                 call TEKFN(n, 2 * n - 1, 1, constituents(isalt, kb:kt), hcref, km, vmin, vmax, zmin, zmax, KLPROF, 'sal', 0, 2, 0d0, kplot)
              end if
           end do
        end if

     end if

     call FULLSCREEN()
     call setwor(x1, y1, x2, y2) ! reset horizontal world coordinates

     call tekprofpoint()

  end subroutine tekprofs
