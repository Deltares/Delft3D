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

module m_advec
   use m_setucxy1d, only: setucxy1d
   use m_sethigherorderadvectionvelocities, only: sethigherorderadvectionvelocities
   use m_qucperipiaczekteta, only: qucperipiaczekteta
   use m_qucperipiaczek, only: qucperipiaczek
   use m_qucper3dsigmapiaczekteta, only: qucper3dsigmapiaczekteta
   use m_qucper3dsigma, only: qucper3dsigma
   use m_getucxucyweironly
   use m_getucxucynoweirs
   use m_getucxucybarrierzero

   implicit none

   private

   public :: advec

contains

   subroutine advec() ! advection, based on u0, q0 24
      use m_flowtimes
      use m_flowgeom
      use m_flow
      use m_partitioninfo
      use m_fixedweirs
      use m_sferic
      use m_dlimiter
      use m_dslim
      use m_get_kbot_ktop
      use m_qucper, only: qucper
      use m_qucpercu, only: qucpercu
      use m_qucperi, only: qucperi
      use m_qucperpure1d, only: qucperpure1d
      use m_qucperq1, only: qucperq1
      use m_qucwen, only: qucwen
      use m_qufper, only: QufPer
      use unstruc_channel_flow, only: network

      ! locals
      integer :: L, k1, k2 ! link, nd1, nd2
      double precision :: v12t
      double precision :: advel ! local adve

      double precision :: qu1 ! Flux times advection velocity node 1 (m4/s2)
      double precision :: qu2 ! idem                          node 2
      double precision :: cs, sn

      integer :: isg, iadvL
      integer :: iad, n, kk, kb
      double precision :: ucxku, ucyku, ai, ae, abh, volu, volui, huvL, baik1, baik2
      double precision :: vol_k1 !< representative volume for node k1
      double precision :: vol_k2 !< representative volume for node k2
      double precision :: ucin, fdx, ql, ac1, ac2, uqn, qn, rhoinsrc, dzss, qnn
      integer :: LL, Lb, Lt, i

      integer :: ku, kd, k, nfw, kt
      integer :: n12

      double precision :: quk1(3, kmxx), quk2(3, kmxx), volukk(kmxx) ! 3D for 1=u, 2=turkin, 3=tureps

      integer :: kt1, kt2, n1, n2, kb1, kb2, Ltx0, ktx01, ktx02, ktx1, ktx2, Ltx, L1, ksb, kst
      double precision :: hs1, hs2, vo1, vo2
      double precision :: tol = 1d-4, sl, dzu, dzk, du1, du2, dux, duy

      double precision :: quuk1(0:kmxx), quuk2(0:kmxx), volk1(0:kmxx), volk2(0:kmxx), sqak1(0:kmxx), sqak2(0:kmxx)
      double precision :: quuL1(0:kmxx), quuL2(0:kmxx), volL1(0:kmxx), volL2(0:kmxx), sqaL1(0:kmxx), sqaL2(0:kmxx)
      double precision :: sigk1(0:kmxx), sigk2(0:kmxx), siguL(0:kmxx)

      double precision, external :: lin2nodx, lin2nody
      double precision, external :: nod2linx, nod2liny

      double precision :: am
      double precision :: qv
      double precision :: u_ene
      double precision :: u_mom

      japiaczek33 = 0

      if (ifixedweirscheme >= 3 .and. ifixedweirscheme <= 5) then
         do L = 1, lnxi
            if (iadv(L) == 21) then
               if (u0(L) > 0) then
                  kd = ln(2, L); ku = ln(1, L)
               else
                  kd = ln(1, L); ku = ln(2, L)
               end if
               call getucxucyweironly(kd, ucx(kd), ucy(kd))
               call getucxucyweironly(ku, ucx(ku), ucy(ku))
            end if
         end do
      end if

      if (jabarrieradvection == 2) then
         do n = 1, ngatesg
            do L = L1gatesg(n), L2gatesg(n)
               LL = kgate(3, L); LL = abs(LL)
               kd = ln(1, LL); ku = ln(2, LL)
               call getucxucybarrierzero(LL, kd, ucx(kd), ucy(kd))
               call getucxucybarrierzero(LL, ku, ucx(ku), ucy(ku))
            end do
         end do
         if (network%sts%numGates > 0) then
            do n = 1, network%sts%numGates
               associate (pstru => network%sts%struct(network%sts%gateIndices(n)))
                  do i = 1, pstru%numlinks
                     L = pstru%linknumbers(i)
                     kd = ln(1, L); ku = ln(2, L)
                     call getucxucybarrierzero(L, kd, ucx(kd), ucy(kd))
                     call getucxucybarrierzero(L, ku, ucx(ku), ucy(ku))
                  end do
               end associate
            end do
         else
            do n = 1, ngategen
               i = gate2cgen(n)
               do L = L1cgensg(i), L2cgensg(i)
                  LL = kcgen(3, L); LL = abs(LL)
                  kd = ln(1, LL); ku = ln(2, LL)
                  call getucxucybarrierzero(LL, kd, ucx(kd), ucy(kd))
                  call getucxucybarrierzero(LL, ku, ucx(ku), ucy(ku))
               end do
            end do
         end if
      end if

      call sethigherorderadvectionvelocities()

      uqcx = 0d0; uqcy = 0d0; sqa = 0d0

      if (kmx == 0) then

         if (jasfer3d == 1) then

            do L = Lnx, 1, -1
               k1 = ln(1, L); k2 = ln(2, L)
               qL = qa(L)
               uqcx(k1) = uqcx(k1) + qL * lin2nodx(L, 1, ucxu(L), ucyu(L))
               uqcx(k2) = uqcx(k2) - qL * lin2nodx(L, 2, ucxu(L), ucyu(L))
               uqcy(k1) = uqcy(k1) + qL * lin2nody(L, 1, ucxu(L), ucyu(L))
               uqcy(k2) = uqcy(k2) - qL * lin2nody(L, 2, ucxu(L), ucyu(L))
               sqa(k1) = sqa(k1) + ql
               sqa(k2) = sqa(k2) - ql
            end do

         else

            do L = Lnx, 1, -1
               k1 = ln(1, L); k2 = ln(2, L)
               qL = qa(L)
               uqcx(k1) = uqcx(k1) + qL * ucxu(L)
               uqcx(k2) = uqcx(k2) - qL * ucxu(L)
               uqcy(k1) = uqcy(k1) + qL * ucyu(L)
               uqcy(k2) = uqcy(k2) - qL * ucyu(L)
               sqa(k1) = sqa(k1) + ql
               sqa(k2) = sqa(k2) - ql
            end do

         end if

      else

         do LL = Lnx, 1, -1
            Lb = lbot(LL); Lt = ltop(LL)
            do L = Lb, Lt
               k1 = ln(1, L) !; k1 = min(k1, ktop(ln(1,LL) ) )
               k2 = ln(2, L) !; k2 = min(k2, ktop(ln(2,LL) ) )
               qL = qa(L)
               if (jasfer3d == 1) then
                  uqcx(k1) = uqcx(k1) + qL * lin2nodx(LL, 1, ucxu(L), ucyu(L))
                  uqcx(k2) = uqcx(k2) - qL * lin2nodx(LL, 2, ucxu(L), ucyu(L))
                  uqcy(k1) = uqcy(k1) + qL * lin2nody(LL, 1, ucxu(L), ucyu(L))
                  uqcy(k2) = uqcy(k2) - qL * lin2nody(LL, 2, ucxu(L), ucyu(L))
               else
                  uqcx(k1) = uqcx(k1) + qL * ucxu(L)
                  uqcx(k2) = uqcx(k2) - qL * ucxu(L)
                  uqcy(k1) = uqcy(k1) + qL * ucyu(L)
                  uqcy(k2) = uqcy(k2) - qL * ucyu(L)
               end if
               sqa(k1) = sqa(k1) + ql
               sqa(k2) = sqa(k2) - ql
            end do
         end do

      end if

      if (javau >= 6) then ! 3D checkerboard pepare explicit node based vertical advection
         if (jarhoxu == 0) then
            do kk = 1, ndxi
               call getkbotktop(kk, kb, kt)
               do k = kb, kt - 1
                  if (qw(k) > 0d0) then
                     uqcx(k + 1) = uqcx(k + 1) - qw(k) * ucx(k)
                     uqcx(k) = uqcx(k) + qw(k) * ucx(k)
                     uqcy(k + 1) = uqcy(k + 1) - qw(k) * ucy(k)
                     uqcy(k) = uqcy(k) + qw(k) * ucy(k)
                     if (javau == 7 .and. k > kb) then
                        dzu = zws(k) - zws(k - 2) ! 2*dz of upwind face
                        if (dzu > tol) then
                           dzk = zws(k + 1) - zws(k - 1) ! 2*dz of this face
                           sl = dzk / dzu
                           du2 = (ucx(k + 1) - ucx(k))
                           du1 = (ucx(k) - ucx(k - 1)) * sl
                           ! dux =  0.5d0*dlimiter(du1,du2,4)
                           dux = 0.5d0 * dslim(du1, du2, 4)
                           du2 = (ucy(k + 1) - ucy(k))
                           du1 = (ucy(k) - ucy(k - 1)) * sl
                           ! duy =  0.5d0*dlimiter(du1,du2,4)
                           duy = 0.5d0 * dslim(du1, du2, 4)
                           uqcx(k + 1) = uqcx(k + 1) - qw(k) * dux
                           uqcx(k) = uqcx(k) + qw(k) * dux
                           uqcy(k + 1) = uqcy(k + 1) - qw(k) * duy
                           uqcy(k) = uqcy(k) + qw(k) * duy
                        end if
                     end if
                  else if (qw(k) < 0d0) then
                     uqcx(k + 1) = uqcx(k + 1) - qw(k) * ucx(k + 1)
                     uqcx(k) = uqcx(k) + qw(k) * ucx(k + 1)
                     uqcy(k + 1) = uqcy(k + 1) - qw(k) * ucy(k + 1)
                     uqcy(k) = uqcy(k) + qw(k) * ucy(k + 1)
                     if (javau == 7 .and. k < kt - 1) then
                        dzu = zws(k + 2) - zws(k) ! 2*dz of upwind face
                        if (dzu > tol) then
                           dzk = zws(k + 1) - zws(k - 1) ! 2*dz of this face
                           sl = dzk / dzu
                           du2 = (ucx(k) - ucx(k + 1))
                           du1 = (ucx(k + 1) - ucx(k + 2)) * sl
                           ! dux =  0.5d0*dlimiter(du1,du2,4)
                           dux = 0.5d0 * dslim(du1, du2, 4)
                           du2 = (ucy(k) - ucy(k + 1))
                           du1 = (ucy(k + 1) - ucy(k + 2)) * sl
                           ! duy =  0.5d0*dlimiter(du1,du2,4)
                           duy = 0.5d0 * dslim(du1, du2, 4)

                           uqcx(k + 1) = uqcx(k + 1) - qw(k) * dux
                           uqcx(k) = uqcx(k) + qw(k) * dux
                           uqcy(k + 1) = uqcy(k + 1) - qw(k) * duy
                           uqcy(k) = uqcy(k) + qw(k) * duy
                        end if
                     end if

                  end if
                  sqa(k + 1) = sqa(k + 1) - qw(k)
                  sqa(k) = sqa(k) + qw(k)
               end do
            end do
         else
            do kk = 1, ndxi
               do k = kbot(kk), ktop(kk) - 1
                  if (qw(k) > 0d0) then
                     uqcx(k + 1) = uqcx(k + 1) - qw(k) * ucx(k) * rho(k)
                     uqcx(k) = uqcx(k) + qw(k) * ucx(k) * rho(k)
                     uqcy(k + 1) = uqcy(k + 1) - qw(k) * ucy(k) * rho(k)
                     uqcy(k) = uqcy(k) + qw(k) * ucy(k) * rho(k)
                  else if (qw(k) < 0d0) then
                     uqcx(k + 1) = uqcx(k + 1) - qw(k) * ucx(k + 1) * rho(k + 1)
                     uqcx(k) = uqcx(k) + qw(k) * ucx(k + 1) * rho(k + 1)
                     uqcy(k + 1) = uqcy(k + 1) - qw(k) * ucy(k + 1) * rho(k + 1)
                     uqcy(k) = uqcy(k) + qw(k) * ucy(k + 1) * rho(k + 1)
                  end if
                  sqa(k + 1) = sqa(k + 1) - qw(k)
                  sqa(k) = sqa(k) + qw(k)
               end do
            end do
         end if
      end if

      if (jarhoxu > 0) then
         sqa = sqa * rho
      end if

      do n = 1, numsrc ! momentum
         if (arsrc(n) > 0) then ! if momentum desired
            if (qsrc(n) > 0) then
               kk = ksrc(4, n) ! 2D pressure cell nr TO
               ksb = ksrc(5, n) ! cell nr
               kst = ksrc(6, n) ! cell nr
            else
               kk = ksrc(1, n) ! 2D pressure cell nr FROM
               ksb = ksrc(2, n) ! cell nr
               kst = ksrc(3, n) ! cell nr
            end if

            if (kk > 0 .and. ksb > 0) then

               qnn = qsrc(n)
               do k = ksb, kst
                  qn = qnn
                  if (kmx > 0) then
                     dzss = zws(kst) - zws(ksb - 1)
                     if (dzss > epshs) then
                        qn = qnn * (zws(k) - zws(k - 1)) / dzss
                     else
                        qn = qnn / (kst - ksb + 1)
                     end if
                  end if
                  uqn = qn * qnn / arsrc(n)

                  if (jarhoxu > 0) then
                     rhoinsrc = rhomean ! just for now
                     qn = qn * rhoinsrc
                     uqn = uqn * rhoinsrc
                  end if

                  if (qsrc(n) > 0) then ! from 1 to 2
                     uqcx(k) = uqcx(k) - uqn * cssrc(2, n)
                     uqcy(k) = uqcy(k) - uqn * snsrc(2, n)
                     sqa(k) = sqa(k) - qn ! sqa : out - in
                  else ! from 2 to 1
                     uqcx(k) = uqcx(k) + uqn * cssrc(1, n)
                     uqcy(k) = uqcy(k) + uqn * snsrc(1, n)
                     sqa(k) = sqa(k) + qn ! sqa : out - in
                  end if

               end do

            end if
         end if
      end do

      nfw = 0
      if (kmx == 0) then

         !$OMP PARALLEL DO                                                                   &
         !$OMP PRIVATE(L, advel,k1,k2,iadvL,qu1,qu2,volu,ai,ae,iad,volui,abh,v12t,ku,kd,isg,n12, ucxku, ucyku, ucin, fdx, vol_k1, vol_k2, u_ene, u_mom, am, qv)

         do L = 1, lnx

            advel = 0 !  advi (1/s), adve (m/s2)

            if (hu(L) > 0) then

               k1 = ln(1, L); k2 = ln(2, L)
               iadvL = iadv(L)

               if (L > lnxi) then
                  if (iadvL == 77) then
                     if (u0(L) < 0) iadvL = 0
                  else if (u0(L) > 0) then
                     iadvL = 0 ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
                  end if
                  !vol1(k1) = 0d0
               end if

               if (iadvL == 33) then !

                  if (jasfer3d == 1) then
                     qu1 = csu(L) * nod2linx(L, 1, uqcx(k1), uqcy(k1)) + snu(L) * nod2liny(L, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                     qu2 = csu(L) * nod2linx(L, 2, uqcx(k2), uqcy(k2)) + snu(L) * nod2liny(L, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                  else
                     qu1 = csu(L) * uqcx(k1) + snu(L) * uqcy(k1) - u1(L) * sqa(k1)
                     qu2 = csu(L) * uqcx(k2) + snu(L) * uqcy(k2) - u1(L) * sqa(k2)
                  end if

                  if (jarhoxu == 0) then
                     if (kcu(L) == 1) then
                        volu = acl(L) * vol1_f(k1) + (1d0 - acl(L)) * vol1_f(k2)
                     else
                        volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                     end if
                  else
                     if (kcu(L) == 1) then
                        volu = acl(L) * vol1_f(k1) * rho(k1) + (1d0 - acl(L)) * vol1_f(k2) * rho(k2)
                     else
                        volu = acl(L) * vol1(k1) * rho(k1) + (1d0 - acl(L)) * vol1(k2) * rho(k2)
                     end if
                  end if

                  if (volu > 0) then
                     advel = (acl(L) * qu1 + (1d0 - acl(L)) * qu2) / volu

                     !if ( japiaczek33 == 1) then
                     !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volu
                     !   if (expl < 0d0) then
                     !      advel   = advel   + expl*u1(L)
                     !      advi(L) = advi(L) - expl
                     !   endif
                     !endif

                  end if

               else if (iadvL == 44) then !

                  if (vol1(k1) > 0) then
                     if (jasfer3D == 1) then
                        qu1 = csu(L) * nod2linx(L, 1, uqcx(k1), uqcy(k1)) + snu(L) * nod2liny(L, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                     else
                        qu1 = csu(L) * uqcx(k1) + snu(L) * uqcy(k1) - u1(L) * sqa(k1)
                     end if
                     advel = advel + acl(L) * qu1 / vol1(k1)
                  end if
                  if (vol1(k2) > 0) then
                     if (jasfer3D == 1) then
                        qu2 = csu(L) * nod2linx(L, 2, uqcx(k2), uqcy(k2)) + snu(L) * nod2liny(L, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                     else
                        qu2 = csu(L) * uqcx(k2) + snu(L) * uqcy(k2) - u1(L) * sqa(k2)
                     end if
                     advel = advel + (1d0 - acl(L)) * qu2 / vol1(k2)
                  end if

               else if (iadvL == 3) then ! explicit first order mom conservative
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QucPer(1, L) ! excess momentum in/out u(L) dir. from k1
                     qu1 = qu1 * acl(L) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPer(2, L) ! excess momentum in/out u(L) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) ! Perot weigthing
                  end if
                  volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                  if (volu > 0) then
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 103) then
                  ! Pure1D consistent with default FM implementation, just less bend losses

                  if (jaPure1D == 1) then
                     vol_k1 = vol1_f(k1)
                     vol_k2 = vol1_f(k2)
                  else ! jaPure1D == 0 .or. jaPure1D == 2
                     vol_k1 = vol1(k1)
                     vol_k2 = vol1(k2)
                  end if

                  if (vol_k1 > 0) then
                     qu1 = QucPerPure1D(1, L) ! excess momentum in/out u(L) dir. from k1
                     qu1 = qu1 * acl(L) ! Perot weigthing
                  else
                     qu1 = 0
                  end if
                  if (vol_k2 > 0) then
                     qu2 = QucPerPure1D(2, L) ! excess momentum in/out u(L) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) ! Perot weigthing
                  else
                     qu2 = 0
                  end if
                  volu = acl(L) * vol_k1 + (1d0 - acl(L)) * vol_k2
                  if (volu > 0) then
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 104) then
                  ! Pure1D implementation SOBEK style

                  advel = 0d0
                  ! weight of momentum versus energy conservation
                  select case (jaPure1D)
                  case (3) ! momentum conserving
                     am = 1d0
                  case (4) ! weighted
                     am = min(au1d(1, L), au1d(2, L)) / max(1d-4, au1d(1, L), au1d(2, L))
                  case (5) ! weighted in contractions, otherwise momentum conserving
                     if ((u1(L) > 0d0 .and. au1D(1, L) > au1D(2, L)) .or. &
                       & (u1(L) < 0d0 .and. au1D(1, L) < au1D(2, L))) then
                        am = min(au1d(1, L), au1d(2, L)) / max(1d-4, au1d(1, L), au1d(2, L))
                     else
                        am = 1d0
                     end if
                  case (6) ! weighted in expansions, otherwise momentum conserving
                     if ((u1(L) > 0d0 .and. au1D(1, L) < au1D(2, L)) .or. &
                       & (u1(L) < 0d0 .and. au1D(1, L) > au1D(2, L))) then
                        am = min(au1d(1, L), au1d(2, L)) / max(1d-4, au1d(1, L), au1d(2, L))
                     else
                        am = 1d0
                     end if
                  case (7) ! energy conserving
                     am = 0d0
                  end select

                  if (q1D(1, L) > 0) then
                     ! flow entering link at node 1
                     qv = q1D(1, L) / max(1d-5, volu1D(L))
                     u_mom = alpha_mom_1D(k1) * q1D(1, L) / au1D(1, L)
                     u_ene = alpha_ene_1D(k1) * q1D(1, L) / au1D(1, L)
                     advel = advel - am * (u_mom - u1(L)) * qv &
                                 & - (1d0 - am) * (u_ene - u1(L)) * qv
                  else
                     ! flow leaving link at node 1
                     ! outflow u = local u, so no contribution
                  end if

                  if (q1D(2, L) < 0) then ! flow entering link at node 2
                     qv = q1D(2, L) / max(1d-5, volu1D(L))
                     u_mom = alpha_mom_1D(k2) * q1D(2, L) / au1D(2, L)
                     u_ene = alpha_ene_1D(k2) * q1D(2, L) / au1D(2, L)
                     advel = advel + am * (u_mom - u1(L)) * qv &
                                 & + (1d0 - am) * (u_ene - u1(L)) * qv
                  else
                     ! flow leaving link at node 2
                     ! outflow u = local u, so no contribution
                  end if

               else if (iadvL == 333) then ! explicit first order mom conservative
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (volau(k1) > 0) then
                     qu1 = QucPer(1, L) ! excess momentum in/out u(L) dir. from k1
                     qu1 = qu1 * acl(L) / volau(k1) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (volau(k2) > 0) then
                     qu2 = QucPer(2, L) ! excess momentum in/out u(L) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) / volau(k2) ! Perot weigthing
                  end if
                  advel = qu1 + qu2 ! dimension: ((m4/s2) / m3) =   (m/s2)

               else if (iadvL == 30) then ! Same as 3, now with alfa = 0.5 in volumes and advection
                  ! based upon cell center excess advection velocity
                  qu1 = 0
                  if (vol1(k1) > 0) then
                     qu1 = QucPer(1, L) ! excess momentum in/out u(L) dir. from k1
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPer(2, L) ! excess momentum in/out u(L) dir. from k2
                  end if
                  volu = vol1(k1) + vol1(k2)
                  if (volu > 0) then
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 31) then ! Thesis Olga 4.8
                  ! based upon cell center excess advection velocity
                  if (jasfer3D == 1) then
                     qu1 = csu(L) * nod2linx(L, 1, uqcx(k1), uqcy(k1)) + snu(L) * nod2liny(L, 1, uqcx(k1), uqcy(k1))
                     qu2 = csu(L) * nod2linx(L, 2, uqcx(k2), uqcy(k2)) + snu(L) * nod2liny(L, 2, uqcx(k2), uqcy(k2))
                  else
                     qu1 = csu(L) * uqcx(k1) + snu(L) * uqcy(k1)
                     qu2 = csu(L) * uqcx(k2) + snu(L) * uqcy(k2)
                  end if
                  advel = acl(L) * qu1 + (1d0 - acl(L)) * qu2

               else if (iadvL == 40) then !

                  if (jasfer3D == 1) then
                     qu1 = csu(L) * nod2linx(L, 1, uqcx(k1), uqcy(k1)) + snu(L) * nod2liny(L, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                     qu2 = csu(L) * nod2linx(L, 2, uqcx(k2), uqcy(k2)) + snu(L) * nod2liny(L, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                  else
                     qu1 = csu(L) * uqcx(k1) + snu(L) * uqcy(k1) - u1(L) * sqa(k1)
                     qu2 = csu(L) * uqcx(k2) + snu(L) * uqcy(k2) - u1(L) * sqa(k2)
                  end if
                  volu = acl(L) * voldhu(k1) + (1d0 - acl(L)) * voldhu(k2)

                  if (volu > 0) then
                     advel = (acl(L) * qu1 + (1d0 - acl(L)) * qu2) / volu
                  end if

               else if (iadvL == 1) then ! explicit first order mom conservative
                  ! based upon cell center advection velocity
                  ! and Wenneker control volume, now with
                  ! uqcx and uqcy arrays instead of function call, (much faster than excess form)

                  volu = vol1(k1) + vol1(k2) ! Wennekers control volume
                  ! qu1     = ( uqcx(k1)*cs + uqcy(k1)*sn )
                  ! qu2     = ( uqcx(k2)*cs + uqcy(k2)*sn )
                  if (volu > 0) then
                     if (jasfer3D == 1) then
                        qu1 = csu(L) * (nod2linx(L, 1, uqcx(k1), uqcy(k1)) + nod2linx(L, 2, uqcx(k2), uqcy(k2)))
                        qu2 = snu(L) * (nod2liny(L, 1, uqcx(k1), uqcy(k1)) + nod2liny(L, 2, uqcx(k2), uqcy(k2)))
                     else
                        qu1 = csu(L) * (uqcx(k1) + uqcx(k2))
                        qu2 = snu(L) * (uqcy(k1) + uqcy(k2))
                     end if
                     v12t = sq(k1) + sq(k2) ! time der. of v12
                     advel = (qu1 + qu2 + u1(L) * v12t) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 2) then ! explicit first order mom conservative
                  ! based upon cell center excess advection velocity
                  volu = vol1(k1) + vol1(k2) ! Wennekers control volume
                  if (volu > 0) then
                     qu1 = QucWen(1, L) ! excess momentum in u(L) dir. out of k1
                     qu2 = QucWen(2, L) ! out of k2
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 4) then ! explicit first order mom conservative

                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QucPeri(1, L) ! excess momentum in u(L) dir. from of k1
                     qu1 = qu1 * acl(L) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPeri(2, L) ! excess momentum in u(L) dir. from of k2
                     qu2 = qu2 * (1d0 - acl(L)) ! Perot weigthing
                  end if

                  volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                  if (volu > 0) then
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 5 .or. iadvL == 6) then ! 5,6 = advection like 3,4, now Piaczek teta

                  if (jarhoxu == 0) then
                     if (kcu(L) == 1) then
                        volu = acl(L) * vol1_f(k1) + (1d0 - acl(L)) * vol1_f(k2)
                     else
                        volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                     end if
                  else
                     if (kcu(L) == 1) then
                        volu = acl(L) * vol1_f(k1) * rho(k1) + (1d0 - acl(L)) * vol1_f(k2) * rho(k2)
                     else
                        volu = acl(L) * vol1(k1) * rho(k1) + (1d0 - acl(L)) * vol1(k2) * rho(k2)
                     end if
                  end if

                  if (volu > 0) then
                     volui = 1d0 / volu
                     if (vol1(k1) > 0) then
                        call QucPeripiaczekteta(1, L, ai, ae, volu, iadvL - 2) ! excess momentum in u(L) dir. out of k1, include own
                        abh = acl(L) * volui
                        adveL = adveL + abh * ae
                        advi(L) = advi(L) + abh * ai
                     end if
                     if (vol1(k2) > 0) then
                        call QucPeripiaczekteta(2, L, ai, ae, volu, iadvL - 2) ! excess momentum in u(L) dir. out of k2
                        abh = (1d0 - acl(L)) * volui
                        adveL = adveL + abh * ae
                        advi(L) = advi(L) + abh * ai
                     end if

                  end if

               else if (iadvL >= 7 .and. iadvL <= 12) then ! Piaczek fully implicit

                  iad = 3
                  if (iadvL == 8 .or. iadvL == 10 .or. iadvL == 12) then
                     iad = 4
                  end if

                  if (kcu(L) == 1) then
                     volu = acl(L) * vol1_f(k1) + (1d0 - acl(L)) * vol1_f(k2)
                  else if (kcu(L) == 3 .and. iadveccorr1D2D == 1) then
                     volu = au(L) * dx(L) ! Use volume weighting based on approximated "lateral volume", to avoid large 1D river volumes.
                  else
                     volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                  end if

                  if (volu > 0) then
                     volui = 1d0 / volu
                     if (hs(k1) > 0) then
                        call QucPeripiaczek(1, L, ai, ae, iad) ! excess momentum in u(L) dir. out of k1, include own
                        abh = acl(L) * volui
                        adveL = adveL + abh * ae
                        advi(L) = advi(L) + abh * ai
                     end if
                     if (hs(k2) > 0) then
                        call QucPeripiaczek(2, L, ai, ae, iad) ! excess momentum in u(L) dir. out of k2
                        abh = (1d0 - acl(L)) * volui
                        adveL = adveL + abh * ae
                        advi(L) = advi(L) + abh * ai
                     end if

                  end if

               else if (iadvL == 21) then ! subgrid weir small stencil, ifixedweirscheme = 3
                  ! upwind center velocity does not feel crest link

                  !advel = 0.5d0*(  u0(L)*u0(L) - u0(L-1)*u0(L-1) ) / dx(L)

                  if (u0(L) > 0d0) then
                     ku = k1; kd = k2; isg = 1; n12 = 1
                  else
                     ku = k2; kd = k1; isg = -1; n12 = 2
                  end if

                  call getucxucynoweirs(ku, ucxku, ucyku)
                  if (jasfer3D == 1) then
                     ucin = nod2linx(L, n12, ucxku, ucyku) * csu(L) + nod2liny(L, n12, ucxku, ucyku) * snu(L)
                  else
                     ucin = ucxku * csu(L) + ucyku * snu(L)
                  end if

                  fdx = 0.5d0 * dxi(L) * isg

                  advi(L) = advi(L) + fdx * u0(L)
                  advel = advel - fdx * ucin * ucin

                  !    advel   = fdx*(u0(L)*u0(L) - ucin*ucin)

               else if (iadvL == 77) then ! supercritical inflow boundary

                  abh = bai(k1) * huvli(L) * acl(L)
                  if (jasfer3D == 1) then
                     adveL = adveL - abh * q1(L) * (nod2linx(L, 1, ucx(k1), ucy(k1)) * csu(L) + nod2liny(L, 1, ucx(k1), ucy(k1)) * snu(L))
                  else
                     adveL = adveL - abh * q1(L) * (ucx(k1) * csu(L) + ucy(k1) * snu(L))
                  end if
                  advi(L) = advi(L) + abh * q1(L)

               else if (iadvL == 38) then ! explicit first order mom conservative olga (17)
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QucPercu(1, L) ! excess momentum in/out uc(k1) dir. from k1
                     qu1 = qu1 * acl(L) / volau(k1) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPercu(2, L) ! excess momentum in/out uc(k2) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) / volau(k2) ! Perot weigthing
                  end if
                  advel = qu1 + qu2 ! dimension: ((m4/s2) / m3) =   (m/s2)

               else if (iadvL == 34) then ! explicit first order mom conservative (stelling kramer)
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QucPer(1, L) ! excess momentum in/out u(L) dir. from k1
                     qu1 = qu1 * acl(L) * bai(k1) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPer(2, L) ! excess momentum in/out u(L) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) * bai(k2) ! Perot weigthing
                  end if
                  advel = (qu1 + qu2) * huvli(L) ! dimension: ((m4/s2) / m3) =   (m/s2)

               else if (iadvL == 35) then ! explicit first order mom conservative (stelling kramer)
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QufPer(1, L) ! excess momentum in/out u(L) dir. from k1
                     qu1 = qu1 * acl(L) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QufPer(2, L) ! excess momentum in/out u(L) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) ! Perot weigthing
                  end if
                  volu = acl(L) * vol1(k1) + (1d0 - acl(L)) * vol1(k2)
                  if (volu > 0) then
                     advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                  end if

               else if (iadvL == 36) then ! explicit first order mom conservative
                  ! based upon cell center excess advection velocity
                  qu1 = 0 ! and Perot control volume
                  if (vol1(k1) > 0) then
                     qu1 = QucPerq1(1, L) ! excess momentum in/out uc(k1) dir. from k1
                     qu1 = qu1 * acl(L) / vol1(k1) ! Perot weigthing
                  end if
                  qu2 = 0
                  if (vol1(k2) > 0) then
                     qu2 = QucPerq1(2, L) ! excess momentum in/out uc(k2) dir. from k2
                     qu2 = qu2 * (1d0 - acl(L)) / vol1(k2) ! Perot weigthing
                  end if
                  advel = qu1 + qu2 ! dimension: ((m4/s2) / m3) =   (m/s2)

               else if (iadvL == 37) then ! Kramer Stelling
                  qu1 = 0d0
                  if (vol1(k1) > 0) then
                     qu1 = acl(L) * QucPerq1(1, L) / ba(k1) ! excess momentum in/out u(L) dir. from k1
                  end if
                  qu2 = 0d0
                  if (vol1(k2) > 0) then
                     qu2 = (1d0 - acl(L)) * QucPerq1(2, L) / ba(k2) ! excess momentum in/out u(L) dir. from k1
                  end if
                  advel = huvli(L) * (qu1 + qu2)

               end if

               adve(L) = adve(L) + advel

            end if

         end do

         !$OMP END PARALLEL DO

      else ! Plus vertical

         do LL = 1, lnx

            if (hu(LL) > 0) then

               iadvL = iadv(LL)
               if (LL > lnxi) then
                  if (iadvL == 77) then
                     if (u0(LL) < 0) cycle
                  else if (u0(LL) > 0) then
                     cycle ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
                  end if
               end if
               cs = csu(LL); sn = snu(LL)
               Lb = Lbot(LL); Lt = Ltop(LL)
               ac1 = acl(LL); ac2 = 1d0 - ac1

               if (iadv(LL) == 3) then
                  call QucPer3Dsigma(1, LL, Lb, Lt, cs, sn, quk1) ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
                  call QucPer3Dsigma(2, LL, Lb, Lt, cs, sn, quk2) ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL

                  do L = Lb, Lt
                     advel = 0d0 ! advi (1/s), adve (m/s2)
                     k1 = ln(1, L); k2 = ln(2, L)
                     qu1 = 0d0
                     if (vol1(k1) > 0) then
                        qu1 = quk1(1, L - Lb + 1) * ac1 ! Perot weigthing
                     end if
                     qu2 = 0d0
                     if (vol1(k2) > 0) then
                        qu2 = quk2(1, L - Lb + 1) * ac2 ! Perot weigthing
                     end if
                     if (jarhoxu > 0) then
                        volu = ac1 * vol1(k1) * rho(k1) + ac2 * vol1(k2) * rho(k2)
                     else
                        volu = ac1 * vol1(k1) + ac2 * vol1(k2)
                     end if
                     if (volu > 0) then
                        advel = (qu1 + qu2) / volu ! dimension: ((m4/s2) / m3) =   (m/s2)
                     end if
                     adve(L) = adve(L) + advel

                  end do

               else if (iadv(LL) == 33 .or. iadv(LL) == 40 .or. iadv(LL) == 6) then !

                  ! qu1   = csu(L)*uqcx(k1) + snu(L)*uqcy(k1) - u1(L)*sqa(k1)
                  ! qu2   = csu(L)*uqcx(k2) + snu(L)*uqcy(k2) - u1(L)*sqa(k2)
                  ! volu  = ac1*vol1(k1)    + ac2*vol1(k2)
                  ! if (volu > 0) then
                  !    advel = (acl(L)*qu1 + (1d0-acl(L))*qu2) / volu
                  ! endif

                  if (layertype == 1) then

                     if (iadv(LL) == -6) then ! .and. newzbndadv == 1 ) then

                        do L = Lb, Lt
                           if (u1(L) > 0) then
                              k = ln(1, L)
                              n12 = 1
                           else
                              k = ln(2, L)
                              n12 = 2
                           end if
                           if (jasfer3D == 1) then
                              advel = 2d0 * (u1(L) - (cs * nod2linx(LL, n12, ucx(k), ucy(k)) + sn * nod2liny(LL, n12, ucx(k), ucy(k)))) * dxi(LL)
                           else
                              advel = 2d0 * (u1(L) - (cs * ucx(k) + sn * ucy(k))) * dxi(LL)
                           end if
                           if (advel > 0d0) then
                              advi(L) = advi(L) + advel
                           else
                              adve(L) = adve(L) - cs * u1(L) * advel
                           end if
                        end do

                     else

                        do L = Lb, Lt
                           k1 = ln(1, L); k2 = ln(2, L)
                           if (jasfer3D == 1) then
                              qu1 = cs * nod2linx(LL, 1, uqcx(k1), uqcy(k1)) + sn * nod2liny(LL, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                              qu2 = cs * nod2linx(LL, 2, uqcx(k2), uqcy(k2)) + sn * nod2liny(LL, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                           else
                              qu1 = cs * uqcx(k1) + sn * uqcy(k1) - u1(L) * sqa(k1)
                              qu2 = cs * uqcx(k2) + sn * uqcy(k2) - u1(L) * sqa(k2)
                           end if

                           if (jarhoxu > 0) then
                              !if (kcu(LL) ==1) then
                              !   volu  = ac1*vol1_f(k1)*rho(k1) + ac2*vol1_f(k2)*rho(k2)
                              !else
                              volu = ac1 * vol1(k1) * rho(k1) + ac2 * vol1(k2) * rho(k2)
                              !endif
                           else
                              !if (kcu(LL) ==1) then
                              !   volu  = ac1*vol1_f(k1)         + ac2*vol1_f(k2)
                              !else
                              volu = ac1 * vol1(k1) + ac2 * vol1(k2)
                              !endif
                           end if

                           if (volu > 0) then

                              adve(L) = adve(L) + (ac1 * qu1 + ac2 * qu2) / volu

                              !if ( japiaczek33 == 1) then
                              !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volu
                              !   if (expl < 0d0) then
                              !      adve(L) = adve(L) + expl*u1(L)
                              !      advi(L) = advi(L) - expl
                              !   endif
                              !endif

                           end if
                        end do

                     end if

                  else if (layertype == 2 .and. jahazlayer == 0) then ! default fixed layers

                     Ltx = Lt - Lb + 1
                     volukk(1:Ltx) = 0d0
                     do L = Lb, Lt
                        k1 = ln(1, L); k2 = ln(2, L)
                        if (jarhoxu > 0) then
                           volukk(L - Lb + 1) = volukk(L - Lb + 1) + ac1 * vol1(k1) * rho(k1) + ac2 * vol1(k2) * rho(k2)
                        else
                           volukk(L - Lb + 1) = volukk(L - Lb + 1) + ac1 * vol1(k1) + ac2 * vol1(k2)
                        end if
                     end do
                     do k = k1 + 1, ktop(ln(1, LL))
                        if (jarhoxu > 0) then
                           volukk(Lt - Lb + 1) = volukk(Lt - Lb + 1) + ac1 * vol1(k) * rho(k)
                        else
                           volukk(Lt - Lb + 1) = volukk(Lt - Lb + 1) + ac1 * vol1(k)
                        end if
                     end do
                     do k = k2 + 1, ktop(ln(2, LL))
                        if (jarhoxu > 0) then
                           volukk(Lt - Lb + 1) = volukk(Lt - Lb + 1) + ac2 * vol1(k) * rho(k)
                        else
                           volukk(Lt - Lb + 1) = volukk(Lt - Lb + 1) + ac2 * vol1(k)
                        end if
                     end do

                     do L = Lb, Lt
                        k1 = ln(1, L); k2 = ln(2, L)
                        if (jasfer3D == 1) then
                           qu1 = cs * nod2linx(LL, 1, uqcx(k1), uqcy(k1)) + sn * nod2liny(LL, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                           qu2 = cs * nod2linx(LL, 2, uqcx(k2), uqcy(k2)) + sn * nod2liny(LL, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                        else
                           qu1 = cs * uqcx(k1) + sn * uqcy(k1) - u1(L) * sqa(k1)
                           qu2 = cs * uqcx(k2) + sn * uqcy(k2) - u1(L) * sqa(k2)
                        end if

                        if (volukk(L - Lb + 1) > 0) then
                           adve(L) = adve(L) + (ac1 * qu1 + ac2 * qu2) / volukk(L - Lb + 1)

                           !if ( japiaczek33 == 1) then
                           !   expl = ( acl(L)*sqa(k1) + (1d0-acl(L))*sqa(k2) ) / volukk(L-Lb+1)
                           !   if (expl < 0d0) then
                           !      adve(L) = adve(L) + expl*u1(L)
                           !      advi(L) = advi(L) - expl
                           !   endif
                           !endif

                        end if

                     end do

                  else if (layertype == 2 .and. jahazlayer == 1) then

                     n1 = ln(1, LL); n2 = ln(2, LL)
                     call getkbotktop(n1, kb1, kt1); ktx1 = kt1 - kb1 + 1
                     call getkbotktop(n2, kb2, kt2); ktx2 = kt2 - kb2 + 1
                     Ltx = Lt - Lb + 1

                     volukk(1:Ltx) = 0d0

                     do L = Lb, Lt
                        k1 = ln(1, L); k2 = ln(2, L); L1 = L - Lb + 1
                        volukk(L1) = volukk(L1) + ac1 * vol1(k1) + ac2 * vol1(k2)
                     end do

                     do k = k1 + 1, kt1
                        volukk(Ltx) = volukk(Ltx) + ac1 * vol1(k)
                     end do

                     do k = k2 + 1, kt2
                        volukk(Ltx) = volukk(Ltx) + ac2 * vol1(k)
                     end do

                     do L = Lb, Lt
                        k1 = ln(1, L); k2 = ln(2, L); L1 = L - Lb + 1
                        if (volukk(L1) > 0) then
                           if (jasfer3D == 1) then
                              qu1 = cs * nod2linx(LL, 1, uqcx(k1), uqcy(k1)) + sn * nod2liny(LL, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                              qu2 = cs * nod2linx(LL, 2, uqcx(k2), uqcy(k2)) + sn * nod2liny(LL, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                           else
                              qu1 = cs * uqcx(k1) + sn * uqcy(k1) - u1(L) * sqa(k1)
                              qu2 = cs * uqcx(k2) + sn * uqcy(k2) - u1(L) * sqa(k2)

                           end if
                           adve(L) = adve(L) + (ac1 * qu1 + ac2 * qu2) / volukk(L - Lb + 1)
                        end if
                     end do

                  else if (layertype == 2 .and. jahazlayer == 2) then ! lineinterp

                     n1 = ln(1, LL); n2 = ln(2, LL)
                     call getkbotktop(n1, kb1, kt1)
                     call getkbotktop(n2, kb2, kt2)
                     hs1 = max(epshs, zws(kt1) - zws(kb1 - 1))
                     hs2 = max(epshs, zws(kt2) - zws(kb2 - 1))

                     ktx01 = kt1 - kb1 + 1
                     ktx02 = kt2 - kb2 + 1

                     volk1(0) = 0d0; quuk1(0) = 0d0; sqak1(0) = 0d0; sigk1(0) = 0d0
                     do k = kb1, kt1
                        volk1(k - kb1 + 1) = volk1(k - kb1) + vol1(k)
                        if (jasfer3D == 1) then
                           quuk1(k - kb1 + 1) = quuk1(k - kb1) + cs * nod2linx(LL, 1, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 1, uqcx(k), uqcy(k))
                        else
                           quuk1(k - kb1 + 1) = quuk1(k - kb1) + cs * uqcx(k) + sn * uqcy(k)
                        end if
                        sqak1(k - kb1 + 1) = sqak1(k - kb1) + sqa(k)
                        sigk1(k - kb1 + 1) = (zws(k) - zws(kb1 - 1)) / hs1
                     end do

                     volk2(0) = 0d0; quuk2(0) = 0d0; sqak2(0) = 0d0; sigk2(0) = 0d0
                     do k = kb2, kt2
                        volk2(k - kb2 + 1) = volk2(k - kb2) + vol1(k)
                        if (jasfer3D == 1) then
                           quuk2(k - kb2 + 1) = quuk2(k - kb2) + cs * nod2linx(LL, 2, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 2, uqcx(k), uqcy(k))
                        else
                           quuk2(k - kb2 + 1) = quuk2(k - kb2) + cs * uqcx(k) + sn * uqcy(k)
                        end if
                        sqak2(k - kb2 + 1) = sqak2(k - kb2) + sqa(k)
                        sigk2(k - kb2 + 1) = (zws(k) - zws(kb2 - 1)) / hs2
                     end do

                     do L = Lb, Lt; Ltx0 = Lt - Lb + 1; siguL(0) = 0d0
                        siguL(L - Lb + 1) = hu(L) / hu(LL)
                     end do

                     call lineinterp3(siguL, quuL1, volL1, sqaL1, Ltx0, sigk1, quuk1, volk1, sqak1, ktx01)
                     call lineinterp3(siguL, quuL2, volL2, sqaL2, Ltx0, sigk2, quuk2, volk2, sqak2, ktx02)

                     do L = Lb, Lt
                        vo1 = volL1(L - Lb + 1) - volL1(L - Lb)
                        vo2 = volL2(L - Lb + 1) - volL2(L - Lb)
                        volu = vo1 * ac1 + vo2 * ac2

                        if (volu > 0) then
                           qu1 = quuL1(L - Lb + 1) - quuL1(L - Lb) - u1(L) * (sqaL1(L - Lb + 1) - sqaL1(L - Lb))
                           qu2 = quuL2(L - Lb + 1) - quuL2(L - Lb) - u1(L) * (sqaL2(L - Lb + 1) - sqaL2(L - Lb))
                           advel = (ac1 * qu1 + ac2 * qu2) / volu
                           adve(L) = adve(L) + advel
                        end if
                     end do

                  else if (layertype == 2 .and. jahazlayer == 4) then

                     n1 = ln(1, LL); n2 = ln(2, LL)
                     call getkbotktop(n1, kb1, kt1); ktx1 = kb1 + kmxn(n1) - 1
                     call getkbotktop(n2, kb2, kt2); ktx2 = kb2 + kmxn(n2) - 1

                     Ltx = Lt - Lb + 1

                     volukk(1:Ltx) = 0d0; quuk1(1:Ltx) = 0d0; sqak1(1:Ltx) = 0d0

                     do k = kb1, ln(1, Lb) - 1 ! below Lb n1
                        volukk(1) = volukk(1) + ac1 * vol1(k)
                        if (jasfer3D == 1) then
                           quuk1(1) = quuk1(1) + ac1 * (cs * nod2linx(LL, 1, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 1, uqcx(k), uqcy(k)))
                        else
                           quuk1(1) = quuk1(1) + ac1 * (cs * uqcx(k) + sn * uqcy(k))
                        end if
                        sqak1(1) = sqak1(1) + ac1 * sqa(k)
                     end do

                     do k = kb2, ln(2, Lb) - 1 ! below Lb n2
                        volukk(1) = volukk(1) + ac2 * vol1(k)
                        if (jasfer3D == 1) then
                           quuk1(1) = quuk1(1) + ac2 * (cs * nod2linx(LL, 2, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 2, uqcx(k), uqcy(k)))
                        else
                           quuk1(1) = quuk1(1) + ac2 * (cs * uqcx(k) + sn * uqcy(k))
                        end if
                        sqak1(1) = sqak1(1) + ac2 * sqa(k)
                     end do

                     do L = Lb, Lt ! intermediate
                        k1 = ln(1, L); k2 = ln(2, L); L1 = L - Lb + 1
                        volukk(L1) = volukk(L1) + ac1 * vol1(k1) + ac2 * vol1(k2)
                        if (jasfer3D == 1) then
                           quuk1(L1) = quuk1(L1) + ac1 * (cs * nod2linx(LL, 1, uqcx(k1), uqcy(k1)) + sn * nod2liny(LL, 1, uqcx(k1), uqcy(k1))) + &
                                       ac2 * (cs * nod2linx(LL, 2, uqcx(k2), uqcy(k2)) + sn * nod2liny(LL, 2, uqcx(k2), uqcy(k2)))
                        else
                           quuk1(L1) = quuk1(L1) + ac1 * (cs * uqcx(k1) + sn * uqcy(k1)) + ac2 * (cs * uqcx(k2) + sn * uqcy(k2))
                        end if
                        sqak1(L1) = sqak1(L1) + ac1 * sqa(k1) + ac2 * sqa(k2)
                     end do

                     do k = k1 + 1, ktx1 ! above Lt n1
                        volukk(Ltx) = volukk(Ltx) + ac1 * vol1(k)
                        if (jasfer3D == 1) then
                           quuk1(Ltx) = quuk1(Ltx) + ac1 * (cs * nod2linx(LL, 1, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 1, uqcx(k), uqcy(k)))
                        else
                           quuk1(Ltx) = quuk1(Ltx) + ac1 * (cs * uqcx(k) + sn * uqcy(k))
                        end if
                        sqak1(Ltx) = sqak1(Ltx) + ac1 * sqa(k)
                     end do

                     do k = k2 + 1, ktx2 ! above Lt n2
                        volukk(Ltx) = volukk(Ltx) + ac2 * vol1(k)
                        if (jasfer3D == 1) then
                           quuk1(Ltx) = quuk1(Ltx) + ac2 * (cs * nod2linx(LL, 2, uqcx(k), uqcy(k)) + sn * nod2liny(LL, 2, uqcx(k), uqcy(k)))
                        else
                           quuk1(Ltx) = quuk1(Ltx) + ac2 * (cs * uqcx(k) + sn * uqcy(k))
                        end if
                        sqak1(Ltx) = sqak1(Ltx) + ac2 * sqa(k)
                     end do

                     do L = Lb, Lt
                        L1 = L - Lb + 1
                        if (volukk(L1) > 0) then
                           adveL = (quuk1(L1) - u1(L) * sqak1(L1)) / volukk(L1)
                           if (abs(advel) > 0.05) then
                              advel = 1d0 * advel
                           end if
                           adve(L) = adve(L) + adveL
                        end if
                     end do

                  end if

               else if (iadv(LL) == 34) then ! Kramer Stelling, ba per cell weighted

                  call QucPer3Dsigma(1, LL, Lb, Lt, cs, sn, quk1) ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
                  call QucPer3Dsigma(2, LL, Lb, Lt, cs, sn, quk2) ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL
                  baik1 = bai(ln(1, LL))
                  baik2 = bai(ln(2, LL))
                  do L = Lb, Lt
                     advel = 0 ! advi (1/s), adve (m/s2)
                     k1 = ln(1, L); k2 = ln(2, L)
                     qu1 = 0d0
                     if (vol1(k1) > 0) then
                        qu1 = quk1(1, L - Lb + 1) * ac1 * baik1
                     end if
                     qu2 = 0
                     if (vol1(k2) > 0) then
                        qu2 = quk2(1, L - Lb + 1) * ac2 * baik2 ! Perot weigthing
                     end if
                     huvL = ac1 * (zws(k1) - zws(k1 - 1)) + ac2 * (zws(k2) - zws(k2 - 1))
                     if (huvL > 0d0) then
                        advel = (qu1 + qu2) / huvL ! dimension: ((m4/s2) / m3) =   (m/s2)
                        adve(L) = adve(L) + advel
                     end if
                  end do

               else if (iadv(LL) == 5) then

                  call QucPer3Dsigmapiaczekteta(LL, Lb, Lt, cs, sn, quk1, quk2)

                  do L = Lb, Lt
                     adve(L) = adve(L) + quk1(1, L - Lb + 1)
                     advi(L) = advi(L) + quk2(1, L - Lb + 1)
                  end do

               else if (iadv(LL) == 44) then

                  do L = Lb, Lt
                     k1 = ln(1, L); k2 = ln(2, L)
                     if (vol1(k1) > 0) then
                        if (jasfer3D == 1) then
                           qu1 = cs * nod2linx(LL, 1, uqcx(k1), uqcy(k1)) + sn * nod2liny(LL, 1, uqcx(k1), uqcy(k1)) - u1(L) * sqa(k1)
                        else
                           qu1 = cs * uqcx(k1) + sn * uqcy(k1) - u1(L) * sqa(k1)
                        end if
                        adve(L) = adve(L) + ac1 * qu1 / vol1(k1)
                     end if
                     if (vol1(k2) > 0) then
                        if (jasfer3D == 1) then
                           qu2 = cs * nod2linx(LL, 2, uqcx(k2), uqcy(k2)) + sn * nod2liny(LL, 2, uqcx(k2), uqcy(k2)) - u1(L) * sqa(k2)
                        else
                           qu2 = cs * uqcx(k2) + sn * uqcy(k2) - u1(L) * sqa(k2)
                        end if
                        adve(L) = adve(L) + ac2 * qu2 / vol1(k2)
                     end if
                  end do

               end if ! advectiontypes

            end if ! (hu)

         end do ! LL

      end if

      if (kmx == 0 .and. lnx1D > 0) then
         call setucxy1D()
      end if

   end subroutine advec

end module m_advec
