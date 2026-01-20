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
!

module m_furu
   use m_update_verticalprofiles, only: update_verticalprofiles
   use m_getustbcfuhi
   use m_furu_structures
   use m_furusobekstructures
   use m_waveconst

   implicit none

contains

   subroutine furu() ! set fu, ru and kfs
      use precision, only: dp
      use m_fixedweirfriction2d
      use m_filter
      use m_flow ! substitue u1 and q1
      use m_flowgeom
      use timers
      use m_flowtimes
      use m_alloc
      use m_partitioninfo
      use m_waves, only: cfwavhi, cfhi_vanrijn, uorb
      use m_sediment
      use unstruc_channel_flow
      use m_sferic
      use m_trachy, only: trachy_resistance
      use m_1d2d_fixedweirs, only: compfuru_1d2d_fixedweirs
      use m_flowparameters, only: ifixedWeirScheme1d2d
      use fm_manhole_losses, only: calculate_manhole_losses
      use m_get_Lbot_Ltop
      use m_ispumpon
      use mathconsts, only: ee

      implicit none

      integer :: L, n, k1, k2, kb, LL, itu1, Lb, Lt, itpbn, i
      integer :: kup, kdo, iup

      real(kind=dp) :: bui, cu, du, du0, gdxi, ds
      real(kind=dp) :: slopec, hup, hdo, u1L, v2, frL, u1L0, zbndun, zbndu0n
      real(kind=dp) :: qk0, qk1, dzb, hdzb, z00 !
      real(kind=dp) :: st2
      real(kind=dp) :: twot = 2.0_dp / 3.0_dp, hb, h23, ustbLL, agp, vLL
      real(kind=dp) :: fsqrtt, uorbL

      integer :: np, L1 ! pumpstuff
      real(kind=dp) :: ap, qp, vp ! pumpstuff

      real(kind=dp) :: cfuhi3D ! for bed friction

      integer :: jaustarintsave
      real(kind=dp) :: sqcfi

      fsqrtt = sqrt(0.5_dp)
      call timstrt('Furu', handle_furu)

      if (kmx == 0 .or. ifixedweirscheme > 0) then ! original 2D coding

         call calculate_manhole_losses(network%storS, advi)

         !$OMP PARALLEL DO                       &
         !$OMP PRIVATE(L,k1,k2,slopec,hup,gdxi,cu,du,du0,ds,u1L,v2,itu1,frL,bui,u1L0,st2,agp,uorbL)
         do L = 1, lnx

            if (hu(L) > 0) then

               if (kmx > 0) then
                  if (.not. (iadv(L) == IADV_SUBGRID_WEIR .or. iadv(L) >= IADV_RAJARATNAM_WEIR .and. iadv(L) <= IADV_VILLEMONTE_WEIR)) then ! in 3D, only do this for weir points
                     cycle
                  end if
               end if

               k1 = ln(1, L)
               k2 = ln(2, L)

               slopec = 0.0_dp
               if (L > lnx1D) then
                  if (Slopedrop2D > 0) then ! 2D droplosses at ridge points and at 2D/1D2D couplings
                     if (iadv(L) == IADV_ORIGINAL_LATERAL_OVERFLOW) then
                        hup = s0(k2) - (min(bob(1, L), bob(2, L)) + twot * hu(L))
                        if (hup < 0) then
                           slopec = hup
                        else
                           hup = s0(k1) - (min(bob(1, L), bob(2, L)) + twot * hu(L))
                           if (hup < 0) then
                              slopec = -hup
                           end if
                        end if
                     end if
                  end if
               else if (iadv(L) == IADV_ORIGINAL_LATERAL_OVERFLOW) then ! 1d or 1D2D droplosses, coding to avoid evaluating array iadv as long as possible,
                  hup = s0(k2) - (max(bob(1, L), bob(2, L)) + twot * hu(L))
                  if (hup < 0) then
                     slopec = hup
                  else
                     hup = s0(k1) - (max(bob(1, L), bob(2, L)) + twot * hu(L))
                     if (hup < 0) then
                        slopec = -hup
                     end if
                  end if
               else if (Drop1d) then ! 1d droplosses, coding to avoid evaluating array iadv as long as possible,
                  hup = s0(k2) - bob(2, L)
                  if (hup < 0) then
                     slopec = hup
                  else
                     hup = s0(k1) - bob(1, L)
                     if (hup < 0) then
                        slopec = -hup
                     end if
                  end if
               end if

               agp = ag
               if (jahelmert > 0 .and. jsferic > 0) then
                  st2 = sin(dg2rd * yu(L))**2
                  agp = 9.7803253359 * (1.0_dp + 0.00193185265241 * st2) / sqrt(1.0_dp - 0.00669437999013 * st2)
               end if
               gdxi = agp * dxi(L)
               if (jarhoxu >= 2) then
                  gdxi = gdxi * rhomean / rhou(L)
               end if

               cu = gdxi * teta(L)
               du = dti * u0(L) - adve(L) + gdxi * slopec
               ds = s0(k2) - s0(k1)
               if (teta(L) /= 1.0_dp) then
                  du = du - (1.0_dp - teta(L)) * gdxi * ds
               end if
               du0 = du

               u1L = u0(L)

               if (jaconveyance2D >= 3 .or. L <= lnx1D) then
                  v2 = 0.0_dp
               else
                  v2 = v(L) * v(L)
               end if

               if (jafrculin > 0) then
                  advi(L) = advi(L) + frculin(L) / hu(L)
               end if

               do itu1 = 1, 4 ! furu_loop
                  if (jawave > NO_WAVES .and. .not. flowWithoutWaves) then ! Delft3D-Wave Stokes-drift correction

                     if (modind < 9) then
                        frL = cfwavhi(L) * hypot(u1L - ustokes(L), v(L) - vstokes(L))
                     elseif (modind == 9) then
                        frL = cfhi_vanrijn(L) * hypot(u1L - ustokes(L), v(L) - vstokes(L))
                     elseif (modind == 10) then ! Ruessink 2003
                        uorbL = 0.5_dp * (uorb(k1) + uorb(k2))
                        frL = cfuhi(L) * sqrt((u1L - ustokes(L))**2 + (v(L) - vstokes(L))**2 + (1.16_dp * uorbL * fsqrtt)**2)
                     end if
                     !
                     du = du0 + frL * ustokes(L)
                     !
                     ! and add vegetation stem drag with eulerian velocities, assumes fixed stem
                     if ((jaBaptist >= 2) .or. trachy_resistance) then
                        frL = frL + alfav(L) * hypot(u1L - ustokes(L), v(L) - vstokes(L))
                     end if

                  else if (ifxedweirfrictscheme > 0) then
                     if (iadv(L) == IADV_SUBGRID_WEIR .or. kcu(L) == 3) then
                        call fixedweirfriction2D(L, k1, k2, frL)
                     else
                        frL = cfuhi(L) * sqrt(u1L * u1L + v2) ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
                     end if
                  else if ((jaBaptist >= 2) .or. trachy_resistance) then
                     frL = (cfuhi(L) + alfav(L)) * sqrt(u1L * u1L + v2) ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
                  else
                     frL = cfuhi(L) * sqrt(u1L * u1L + v2) ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu
                  end if

                  bui = 1.0_dp / (dti + advi(L) + frL)
                  fu(L) = cu * bui
                  ru(L) = du * bui
                  u1L0 = u1L
                  u1L = ru(L) - fu(L) * ds

                  if (huvli(L) <= 1.0_dp .or. abs(u1L - u1L0) <= 1.0e-2_dp) then ! less than 1 m deep or small change in velocity: exit
                     exit ! furu_loop
                  end if
               end do

            end if

         end do
         !$OMP END PARALLEL DO

         if (npump > 0) then ! model has at least one pump link
            do np = 1, npumpsg ! loop over pump signals, sethu
               qp = qpump(np)
               ap = 0.0_dp
               vp = 0.0_dp
               do n = L1pumpsg(np), L2pumpsg(np)
                  k1 = kpump(1, n)
                  k2 = kpump(2, n)
                  L1 = kpump(3, n)
                  L = abs(L1)
                  hu(L) = 0.0_dp
                  au(L) = 0.0_dp
                  fu(L) = 0.0_dp
                  ru(L) = 0.0_dp
                  if (qp * L1 >= 0) then
                     kup = k1
                     kdo = k2
                     iup = 1
                  else
                     kup = k2
                     kdo = k1
                     iup = 2
                  end if

                  if (hs(kup) > 1.0e-2_dp .and. ispumpon(np, s1(kup)) == 1) then
                     hup = s1(kup) - bob0(iup, L)
                     hdo = s1(kdo) - bob0(3 - iup, L)
                     hu(L) = max(hup, hdo) ! 1d0
                     au(L) = wu(L) * hu(L) ! 1d0
                     ap = ap + au(L)
                     vp = vp + vol1(k1)
                  end if
               end do
               if (qp > 0.5_dp * vp / dts) then
                  qp = 0.5_dp * vp / dts
               end if

               if (ap > 0.0_dp) then
                  do n = L1pumpsg(np), L2pumpsg(np)
                     L1 = kpump(3, n)
                     L = abs(L1)
                     if (au(L) > 0.0_dp) then
                        if (L1 > 0) then
                           ru(L) = qp / ap
                        else
                           ru(L) = -qp / ap
                        end if
                     end if
                  end do
               end if
            end do
         end if

         call furu_structures()

      end if

      if (kmx > 0) then

         if (jafilter /= 0) then
            call comp_filter_predictor()
         end if

         call update_verticalprofiles()

      end if

      do n = 1, nbndu ! boundaries at u points

         k2 = kbndu(2, n)
         LL = kbndu(3, n)
         itpbn = kbndu(4, n)
         call getLbotLtop(LL, Lb, Lt)

         !Original:  !zbndun = zbndu( (n-1)*kmxd + 1 )
         if (itpbn == 4) then ! dischargebnd
            zbndun = zbndq(n)
         else if (itpbn == 5) then ! absgenbc
            zbndun = u1(LL) ! set in xbeach_absgen_bc
         else ! other types that use alfsmo
            zbndun = zbndu((n - 1) * kmxd + 1)
         end if

         if (alfsmo < 1.0_dp) then
            zbndu0n = u0(LL)
            zbndun = alfsmo * zbndun + (1.0_dp - alfsmo) * zbndu0n ! i.c. smoothing, start from 0
         end if

         if (itpbn == 8) then ! Criticaloutflowbnd
            if (hu(LL) > 0.0_dp) then
               zbndun = -sqrt(ag * (s1(k2) - min(bob(1, LL), bob(2, LL))))
            end if
         else if (itpbn == 9) then ! Weiroutflowbnd 2/3h(sqrt
            if (hu(LL) > 0.0_dp) then
               hb = s1(k2) - min(bob(1, LL), bob(2, LL))
               h23 = twot * hb
               au(LL) = twot * au(LL)
               zbndun = -sqrt(ag * h23)
            end if
         end if

         if (Lt > Lb) then ! true 3D
            u1(LL) = zbndun
            jaustarintsave = jaustarint
            if (jaustarint == 0 .or. jaustarint == 3) then
               jaustarint = 1
            end if
            vLL = v(LL)
            v(LL) = 0.0_dp
            call getustbcfuhi(LL, LL, ustbLL, cfuhi(LL), hdzb, z00, cfuhi3D) ! call with Lb = LL => layer integral profile
            ! JRE with HK, used to be in getustb
            if (jawave > NO_WAVES .and. jawaveStokes >= STOKES_DRIFT_DEPTHUNIFORM) then ! Ustokes correction at bed
               adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
            end if
            v(LL) = vLL
            jaustarint = jaustarintsave
            qk0 = 0.0_dp
         end if

         do L = Lb, Lt
            fu(L) = 0.0_dp
            ru(L) = zbndun

            if (Lt > Lb) then
               if (jaLogprofatubndin /= 1 .and. itpbn == 3) then ! non logprof and vertical profile specified
                  ru(L) = zbndu((n - 1) * kmxd + L - Lb + 1) * min(1.0_dp, alfsmo)
               else if (abs(u1(Lb)) > 1.0e-4_dp .and. z00 > 0.0_dp) then
                  if (jaustarint == 0 .or. jaustarint == 3 .or. jaustarint == 1) then
                     dzb = hu(L) + c9of1 * z00
                     sqcfi = (log(dzb / z00) - 1.0_dp) / vonkar
                  else if (jaustarint == 2) then
                     dzb = hu(L) / ee + c9of1 * z00
                     sqcfi = (log(dzb / z00)) / vonkar
                  else if (jaustarint == 4) then
                     dzb = hu(L) / ee + c9of1 * z00 * 0.66_dp
                     sqcfi = (log(dzb / z00)) / vonkar
                  else if (jaustarint == 5) then
                     dzb = hu(L)
                     sqcfi = ((1.0_dp + c9of1 * z00 / dzb) * log(dzb / z00 + c9of1) - c9of1 * z00 / dzb * log(c9of1) - 1.0_dp) / vonkar
                  end if
                  qk1 = hu(L) * ustbLL * sqcfi ! integral flux till level k
                  ru(L) = (qk1 - qk0) / (hu(L) - hu(L - 1))
                  if (zbndun < 0.0_dp) then
                     ru(L) = -1.0_dp * ru(L)
                  end if
                  qk0 = qk1
               end if
            end if

         end do

      end do

      call furusobekstructures()

      if ((jawave == WAVE_SWAN_ONLINE .or. jawave == WAVE_NC_OFFLINE) .and. .not. flowWithoutWaves) then
         if (kmx == 0) then
            !   add wave-induced mass fluxes on boundaries to convert euler input to GLM
            do L = Lnxi + 1, Lnx
               ru(L) = ru(L) + wavmubnd(L)
            end do
         else ! to check: vertical distribution
            do L = lnxi + 1, lnx
               call getLbotLtop(L, Lb, Lt)
               if (Lt < Lb) then
                  cycle
               end if
               do LL = Lb, Lt
                  ru(LL) = ru(LL) + wavmubnd(LL)
               end do
            end do
         end if
      end if

      do i = 1, nqhbnd
         do n = L1qhbnd(i), L2qhbnd(i)
            kb = kbndz(1, n)
            k2 = kbndz(2, n)
            L = kbndz(3, n)
            if (au(L) > 0.0_dp .and. qh_gamma(i) /= 0.0_dp .and. atqh_all(i) /= 0.0_dp) then
               fu(L) = abs(q1(L) / atqh_all(i)) * qh_gamma(i) / au(L)
               ru(L) = q1(L) / au(L)
               continue
            end if
         end do
      end do

! BEGIN DEBUG
      if (jampi == 1) then
!    call update_ghosts(ITYPE_U,1,Lnx,fu,ierr)
!    call update_ghosts(ITYPE_U,1,Lnx,ru,ierr)

!     call diff_ghosts(ITYPE_U,dxi)
!     call diff_ghosts(ITYPE_Sall,ucx)
      end if
! END DEBUG

      if (ifixedWeirScheme1d2d == 1) then
         call compfuru_1d2d_fixedweirs()
      end if
      call timstop(handle_furu)

   end subroutine furu

end module m_furu
