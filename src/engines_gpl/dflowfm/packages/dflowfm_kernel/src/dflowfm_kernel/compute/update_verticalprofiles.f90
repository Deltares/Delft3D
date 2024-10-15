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

subroutine update_verticalprofiles()
!c************************************************************************
!c
!c         D e l f t      H y d r a u l i c s   -   Section    M-C-M
!c
!c             Module: based on Subroutine tratur in DPM
!c           Function: Transport solver tke and epsilon plus vertical momentum exchange u0
!c        Method used: Teta method for integration in time.
!c                     Tke and eps computed at layer interfaces.
!c                     SANCTUM
!c               Date: 13:26 dinsdag 4 augustus 1998
!c         Programmer: R.E. Uittenbogaard
!c************************************************************************

   use m_flow
   use m_flowgeom
   use m_waves, only: hwav, gammax, ustokes, vstokes, fbreak, fwavpendep
   use m_partitioninfo
   use m_flowtimes
   use m_ship
   use m_sferic
   use m_missing
   use m_get_kbot_ktop
   use m_get_Lbot_Ltop
   use m_links_to_centers, only: links_to_centers

   implicit none

   double precision :: tetm1, dzc1, dzc2, zb1, zb2, tkedisL
   double precision :: vicu, vicd, difu, difd, dzdz1, dzdz2, sourtu, sinktu, drhodz
   double precision :: zz, z00, ac1, ac2, tkebot, tkesur, epsbot, epssur, volu
   double precision :: hdzb, dtiL, adv, omega1, omega2, omegu, drhodz1, drhodz2
   double precision :: dzu(kmxx), dzw(kmxx), womegu(kmxx), pkwav(kmxx)
   double precision :: gradk, gradt, grad, gradd, gradu, volki, arLL, qqq, faclax, zf
   double precision :: wk, wke, vk, um, tauinv, tauinf, xlveg, rnv, diav, ap1, alf, c2esqcmukep, teps, tkin
   double precision :: cfuhi3D, vicwmax, zint, z1, vicwww, alfaT, tke, eps, tttctot, c3t
   double precision :: rhoLL, pkwmag, hrmsLL, wdep, dzwav, dis1, dis2, surdisLL, prsappr
   integer :: k, ku, kb, kt, n, LL, L, Lb, Lt, kxL, Lu, Lb0, whit
   integer :: k1, k2, k1u, k2u, n1, n2, kup, ierror
   double precision, external :: setrhofixedp

   if (iturbulencemodel <= 0 .or. kmx == 0) return

   if (iadvec == 0) then
      javau = 0
   end if

   if (iturbulencemodel == 1) then ! 1=constant

!    vicwwu = vicoww

      !$OMP PARALLEL DO                                     &
      !$OMP PRIVATE(LL,Lb,Lt,kxL,dzu,L,k,hdzb,z00,ac1,ac2,n1,n2,zb1,zb2,k1,k2,omega1,omega2,volu,womegu,cfuhi3D)

      do LL = 1, lnx

         if (hu(LL) > 0d0) then

            Lb = Lbot(LL) ! bed layer index
            Lt = Ltop(LL) ! surface layer index = surface interface index
            kxL = Lt - Lb + 1 ! nr of layers

            dzu(1) = hu(Lb)
            do L = Lb + 1, Lt ! layer thickness at layer center
               k = L - Lb + 1
               dzu(k) = hu(L) - hu(L - 1)
            end do

            call getustbcfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) !Constant
            advi(Lb) = advi(Lb) + cfuhi3D
            !
            if (jawave > 0 .and. jawaveStokes >= 1 .and. .not. flowWithoutWaves) then ! Ustokes correction at bed
               adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
            end if

            if (javau > 0) then
               ac1 = acL(LL); ac2 = 1d0 - ac1
               n1 = ln(1, LL) !; zb1 = zws(kbot(n1)-1)
               n2 = ln(2, LL) !; zb2 = zws(kbot(n2)-1)
               do L = Lb, Lt - 1 ! vertical omega velocity at layer interface u point
                  k1 = ln(1, L); k2 = ln(2, L)
                  k = L - Lb + 1

                  if (n1 > ndxi) then ! open boundaries
                     if (u1(LL) < 0d0) then
                        womegu(k) = qw(k2) / a1(n2)
                     else
                        womegu(k) = 0d0
                     end if
                  else
                     womegu(k) = (ac1 * qw(k1) + ac2 * qw(k2)) / (ac1 * a1(ln(1, LL)) + ac2 * a1(ln(2, LL)))
                  end if

               end do
               womegu(Lt - Lb + 1) = 0d0 ! top layer : 0
            end if

            call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

         end if

      end do

      !$OMP END PARALLEL DO

   else if (iturbulencemodel == 2) then ! 2=algebraic , just testing 1D flow

      !$xOMP PARALLEL DO                                     &
      !$xOMP PRIVATE(LL,Lb,Lt,kxL,dzu,frcn,L,k,Cz,z00,sqcf,zz,n1,n2,zb1,zb2,volu)
      do LL = 1, lnx

         if (hu(LL) > 0d0) then

            Lb = Lbot(LL) ! bed layer index
            Lt = Ltop(LL) ! surface layer index = surface interface index
            kxL = Lt - Lb + 1 ! nr of layers

            dzu(1) = hu(Lb)
            do L = Lb + 1, Lt ! layer thickness at layer center
               k = L - Lb + 1
               dzu(k) = hu(L) - hu(L - 1)
            end do

            call getustbcfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) ! algebraic
            advi(Lb) = advi(Lb) + cfuhi3D
            !
            if (jawave > 0 .and. jawaveStokes >= 1 .and. .not. flowWithoutWaves) then ! Ustokes correction at bed
               adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
            end if

            if (javau > 0) then
               ac1 = acL(LL); ac2 = 1d0 - ac1
               n1 = ln(1, LL); !zb1 = zws(kbot(n1)-1)
               n2 = ln(2, LL); !zb2 = zws(kbot(n2)-1)
               do L = Lb, Lt - 1 ! vertical omega velocity at layer interface u point
                  k1 = ln(1, L); k2 = ln(2, L)
                  k = L - Lb + 1

                  if (n1 > ndxi) then ! open boundaries
                     if (u1(LL) < 0d0) then
                        womegu(k) = qw(k2) / a1(n2)
                     else
                        womegu(k) = 0d0
                     end if
                  else
                     womegu(k) = (ac1 * qw(k1) + ac2 * qw(k2)) / (ac1 * a1(ln(1, LL)) + ac2 * a1(ln(2, LL)))
                  end if

               end do
               womegu(Lt - Lb + 1) = 0d0 ! top layer : 0
            end if

            vicwwu(Lb - 1) = 0d0

            do L = Lb, Lt
               zz = hu(L) * (1d0 - hu(L) / hu(LL)) ! parabolic
               !           zz       = hu(L)*sqrt( ( 1d0 - hu(L)/ hu(LL) ) )     ! Bakhmetev
               vicwwu(L) = zz * ustb(LL) * vonkar
            end do

            call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

         end if

      end do

      !$xOMP END PARALLEL DO

   else if (iturbulencemodel >= 3) then ! 3=k-epsilon, 4=k-tau

      c2esqcmukep = c2e * sqcmukep

      if (javakeps > 0) then ! transport switched on: prepare horizontal advection k and eps

         call links_to_centers(turkinepsws, turkin1, tureps1)

         if (numsrc > 0 .and. addksources > 0d0) then
            call doaddksources()
         end if

         if (nshiptxy > 0) then
            ! call addkships()
         end if

         if (jampi == 1) then
            call update_ghosts(ITYPE_Sall3D, 2, Ndkx, turkinepsws, ierror)
         end if

         tqcu = 0d0; eqcu = 0d0; sqcu = 0d0

         javatest = 0
         if (javatest == 3) then
            if (.not. allocated(tttu)) then
               allocate (tttu(lnkx), ttqc(ndkx), tttc(ndkx)); tttu = 0d0
               call getLbotLtop(lnx / 2, Lb, Lt)
               do L = Lb, Lt
                  tttu = 1d0
               end do
            end if

            call links_to_centers(tttc, tttu)
            tttctot = 0d0
            do n = 1, ndxi
               call getkbotktop(n, kb, kt)
               do k = kb, kt
                  tttctot = tttctot + tttc(k) * vol1(k)
               end do
            end do
            ttqc = 0d0

         end if

         do LL = 1, lnx
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt - 1
               k1 = ln(1, L); k2 = ln(2, L)
               qqq = 0.5d0 * (q1(L) + q1(L + 1))

               ! k    = L - Lb + 1
               ! dzu(k  ) = hu(L) - hu(L-1)
               ! dzu(k+1) = hu(L+1) - hu(L)
               ! if (dzu(k) < 1d-10 .or. dzu(k+1) < 1d-10) then
               !    call qnerror('dzu(k) < 1d-10',' ',' ')
               ! endif
               ! qqq = dzu(k) * q1(L) + dzu(k+1) * q1(L+1)
               ! qqq = qqq / ( dzu(k) + dzu(k+1) )

               if (qqq > 0) then ! set upwind center values on links
                  tqcu(k2) = tqcu(k2) + qqq * turkinepsws(1, k1)
                  eqcu(k2) = eqcu(k2) + qqq * turkinepsws(2, k1)
                  if (javatest == 3) ttqc(k2) = ttqc(k2) + qqq * tttc(k1)
                  sqcu(k2) = sqcu(k2) + qqq
               else if (qqq < 0) then
                  tqcu(k1) = tqcu(k1) - qqq * turkinepsws(1, k2)
                  eqcu(k1) = eqcu(k1) - qqq * turkinepsws(2, k2)
                  if (javatest == 3) ttqc(k1) = ttqc(k1) - qqq * tttc(k2)
                  sqcu(k1) = sqcu(k1) - qqq
               end if
            end do
         end do
      end if

      tetm1 = 1d0 - tetavkeps
      dtiL = 1d0 / dtprev ! turbulence transport in current velocity field => do not use new timestep but previous step

      !$xOMP PARALLEL DO                                                                  &
      !$xOMP PRIVATE(LL,Lb,Lt,Lb0,kxL,L,k,dzu,dzw,hdzb,z00,tkebot,tkesur)                 &
      !$xOMP PRIVATE(ak,bk,ck,dk,ek,vicu,vicd,dzdz1,dzdz2,difu,difd,Lu,ku)                &
      !$xOMP PRIVATE(k1,k2,k1u,k2u,drhodz,drhodz1,drhodz2,dzc1,dzc2,bruva,buoflu,dijdij)  &
      !$xOMP PRIVATE(sourtu,sinktu,tkedisL,ac1,ac2,n1,n2,womegu,omega1,omega2,adv,omegu ) &
      !$xOMP PRIVATE(gradd,gradu,gradt,gradk,grad,wk,vk,um)

      do LL = 1, lnx ! all this at velocity points

         tkesur = 0d0
         tkebot = 0d0

         Lt = Ltop(LL) ! surface layer index = surface interface index
         Lb = Lbot(LL) ! bed layer index
         if (Lt < Lb) cycle
         Lb0 = Lb - 1 ! bed interface index

         if (hu(LL) > 0d0) then ! epshu?
            kxL = Lt - Lb + 1 ! nr of layers

            do L = Lb, Lt ! layer thickness at layer center (org: Lb + 1)
               k = L - Lb + 1
               !k1   = ln(1,L)  ; k2  = ln(2,L)
               !dzu(k) = acl(LL)*(zws(k1)-zws(k1-1)) + (1d0-acl(LL))*(zws(k2)-zws(k2-1))
               dzu(k) = max(eps4, hu(L) - hu(L - 1))

               ! if (dzu(k) < 1d-10) then
               !   call qnerror('dzu(k) < 1d-10',' ',' ')
               ! endif

            end do

            do L = Lb, Lt - 1 ! layer thickness at layer interface
               k = L - Lb + 1
               dzw(k) = 0.5d0 * (dzu(k) + dzu(k + 1))
            end do

            call getustbcfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) ! K-EPS, K-TAU z00 wave-enhanced roughness for jawave>0

            if (hu(LL) < trsh_u1Lb) then
               advi(Lb:Lt) = advi(Lb:Lt) + cfuhi3D / dble(Lt - Lb + 1)
            else
               advi(Lb) = advi(Lb) + cfuhi3D
            end if

            tkebot = sqcmukepi * ustb(LL)**2 ! this has stokes incorporated when jawave>0
            tkesur = sqcmukepi * ustw(LL)**2 ! only wind+ship contribution

            if (ieps == 3) then ! as Delft3D
               vicwwu(Lb0) = vonkar * ustb(LL) * z00
            end if

            turkin0(Lb0:Lt) = turkin1(Lb0:Lt)
            tureps0(Lb0:Lt) = tureps1(Lb0:Lt)

            ak(0:kxL) = 0.d0 ! Matrix initialisation TKE
            bk(0:kxL) = dtiL
            ck(0:kxL) = 0.d0
            dk(0:kxL) = dtiL * turkin0(Lb0:Lt)

            if (turbulence_lax_factor > 0) then
               if (turbulence_lax_vertical == 1) then
                  do L = Lb, Lt - 1
                     zf = min(1d0, (hu(L) - 0.5 * hu(LL)) / (0.25d0 * hu(LL)))
                     if (zf > 0d0) then ! top half only: 0.5-0.75: zf = linear from 0 to 1,  > 0.75 : zf 1
                        k1 = ln(1, L); k2 = ln(2, L)
                        if (turkinepsws(1, k1) > eps20 .and. turkinepsws(1, k2) > eps20) then
                           if (turbulence_lax_horizontal == 1 .or. (zws(k1) > zws(k2 - 1) .and. zws(k1 - 1) < zws(k2))) then
                              faclax = turbulence_lax_factor * zf
                              faclax = faclax * min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1)) / max(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1))
                              dk(L - Lb + 1) = dtiL * ((1d0 - facLax) * turkin0(L) + 0.5d0 * facLax * (turkinepsws(1, k1) + turkinepsws(1, k2)))
                           end if
                        end if
                     end if
                  end do
               else if (turbulence_lax_vertical == 2) then
                  do L = Lb, Lt - 1
                     k1 = ln(1, L); k2 = ln(2, L)
                     if (turkinepsws(1, k1) > eps20 .and. turkinepsws(1, k2) > eps20) then
                        if (turbulence_lax_horizontal == 1 .or. (zws(k1) > zws(k2 - 1) .and. zws(k1 - 1) < zws(k2))) then
                           faclax = turbulence_lax_factor * min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1)) / max(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1))
                           dk(L - Lb + 1) = dtiL * ((1d0 - facLax) * turkin0(L) + 0.5d0 * facLax * (turkinepsws(1, k1) + turkinepsws(1, k2)))
                        end if
                     end if
                  end do
               end if
            end if

            vicu = viskin + 0.5d0 * (vicwwu(Lb0) + vicwwu(Lb)) * sigtkei !

            ! Calculate turkin source from wave dissipation: preparation
            if (jawave > 0) then
               if (jawaveStokes > 0 .and. .not. flowWithoutWaves) then ! Ustokes correction at bed
                  adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
               end if

               if (jawave > 0 .and. jawavebreakerturbulence > 0) then
                  k1 = ln(1, LL); k2 = ln(2, LL)
                  ac1 = acl(LL); ac2 = 1d0 - ac1
                  hrmsLL = min(max(ac1 * hwav(k1) + ac2 * hwav(k2), 1d-2), gammax * hu(LL))
                  if (hrmsLL > 0.0) then
                     call wave_fillsurdis(k1, dis1)
                     call wave_fillsurdis(k2, dis2)
                     surdisLL = ac1 * dis1 + ac2 * dis2
                     if (surdisLL < 1d-2) surdisLL = 0d0
                     rhoLL = rhomean
                     pkwmag = fbreak * 2d0 * surdisLL / (rhoLL * fwavpendep * hrmsLL)
                     ! tke dirichlet boundary condition at surface
                     tkesur = tkesur + (pkwmag * vonkar * fwavpendep * hrmsLL / (30.d0 * cde))**(2d0 / 3d0)
                  else
                     pkwmag = 0d0
                  end if
                  pkwav = 0d0
                  wdep = hu(LL) - fwavpendep * hrmsLL
                  whit = 0
               end if
            end if

            do L = Lb, Lt - 1 ! Loop over layer interfaces. Doesn't work for kmx==1
               Lu = L + 1

               vicd = vicu
               vicu = viskin + 0.5d0 * (vicwwu(L) + vicwwu(Lu)) * sigtkei

               k = L - Lb + 1; ku = k + 1

               dzdz1 = dzw(k) * dzu(k)
               difd = vicd / dzdz1

               dzdz2 = dzw(k) * dzu(ku)
               difu = vicu / dzdz2

               ak(k) = ak(k) - difd * tetavkeps
               bk(k) = bk(k) + (difd + difu) * tetavkeps
               ck(k) = ck(k) - difu * tetavkeps
               if (tetavkeps /= 1d0) then
                  dk(k) = dk(k) - difu * (turkin0(L) - turkin0(Lu)) * tetm1 &
                          + difd * (turkin0(L - 1) - turkin0(L)) * tetm1
               end if

               !c Source and sink terms                                                                           k turkin
               if (idensform > 0) then
                  k1 = ln(1, L); k2 = ln(2, L)
                  k1u = ln(1, Lu); k2u = ln(2, Lu)

                  drhodz = 0d0; drhodz1 = 0d0; drhodz2 = 0d0

                  dzc1 = 0.5d0 * (zws(k1u) - zws(k1 - 1)) ! vertical distance between cell centers on left side
                  if (dzc1 > 0) then
                     if (.not. density_is_pressure_dependent()) then
                        drhodz1 = (rho(k1u) - rho(k1)) / dzc1
                     else
                        prsappr = ag * rhomean * (zws(ktop(ln(1, LL))) - zws(k1))
                        drhodz1 = (setrhofixedp(k1u, prsappr) - setrhofixedp(k1, prsappr)) / dzc1
                     end if
                  end if

                  dzc2 = 0.5d0 * (zws(k2u) - zws(k2 - 1)) ! vertical distance between cell centers on right side
                  if (dzc2 > 0) then
                     if (.not. density_is_pressure_dependent()) then
                        drhodz2 = (rho(k2u) - rho(k2)) / dzc2
                     else
                        prsappr = ag * rhomean * (zws(ktop(ln(2, LL))) - zws(k2))
                        drhodz2 = (setrhofixedp(k2u, prsappr) - setrhofixedp(k2, prsappr)) / dzc2
                     end if
                  end if

                  if (jadrhodz == 1) then ! averagingif non zero

                     if (drhodz1 == 0) then
                        drhodz = drhodz2
                     else if (drhodz2 == 0) then
                        drhodz = drhodz1
                     else
                        !drhodz  = 0.5d0*( drhodz1 + drhodz2 )
                        drhodz = acl(LL) * drhodz1 + (1.0d0 - acl(LL)) * drhodz2
                     end if

                  else if (jadrhodz == 2) then ! averaging

                     drhodz = acl(LL) * drhodz1 + (1.0d0 - acl(LL)) * drhodz2

                  else if (jadrhodz == 3) then ! upwind

                     if (u1(L) > 0d0) then
                        drhodz = drhodz1
                     else
                        drhodz = drhodz2
                     end if

                  else if (jadrhodz == 4) then ! most stratified, decreases viscosity

                     drhodz = min(drhodz1, drhodz2)

                  else if (jadrhodz == 5) then

                     drhodz = max(drhodz1, drhodz2) ! least stratified, increases viscosity

                  else if (jadrhodz == 6) then ! first average then d/dz

                     if (dzc1 > 0 .and. dzc2 > 0) then

                        if (.not. density_is_pressure_dependent()) then
                           drhodz = (rho(k1u) + rho(k2u) - rho(k1) - rho(k2)) / (dzc1 + dzc2)
                        else
                           prsappr = ag * rhomean * (zws(ktop(ln(1, LL))) - zws(k1))
                           drhodz = (setrhofixedp(k1u, prsappr) + setrhofixedp(k2u, prsappr) - setrhofixedp(k1, prsappr) - setrhofixedp(k1, prsappr)) / (dzc1 + dzc2)
                        end if

                     end if

                  end if
                  !
                  bruva(k) = coefn2 * drhodz ! N.B., bruva = N**2 / sigrho
                  buoflu(k) = max(vicwwu(L), vicwminb) * bruva(k)

                  !c Production, dissipation, and buoyancy term in TKE equation;
                  !c dissipation and positive buoyancy are split by Newton linearization:
                  if (iturbulencemodel == 3) then
                     if (bruva(k) > 0d0) then
                        dk(k) = dk(k) + buoflu(k)
                        bk(k) = bk(k) + 2d0 * buoflu(k) / turkin0(L)
                        ! EdG: make buoyance term in matrix safer
                        !  bk(k) = bk(k) + 2d0*buoflu(k) / max(turkin0(L), 1d-20)
                     elseif (bruva(k) < 0d0) then
                        dk(k) = dk(k) - buoflu(k)
                     end if
                  else if (iturbulencemodel == 4) then
                     if (bruva(k) > 0d0) then
                        bk(k) = bk(k) + buoflu(k) / turkin0(L)
                     else if (bruva(k) < 0d0) then
                        dk(k) = dk(k) - buoflu(k)
                     end if
                  end if
               end if

               !c TKEPRO is the energy transfer flux from Internal Wave energy to
               !c Turbulent Kinetic energy and thus a source for the k-equation.
               !c TKEDIS is the energy transfer flux from Turbulent Kinetic energy to
               !c Internal Wave energy and thus a sink for the k-equation.

               ! Production, dissipation, and buoyancy term in TKE equation;
               ! dissipation and positive buoyancy are split by Newton linearization;
               ! buoyancy only for unstable stratification;
               ! notice: application of TKE at new time level:
               ! Addition of production and of dissipation to matrix ;
               ! observe implicit treatment by Newton linearization.

               if (jawave > 0 .and. jawaveStokes >= 3 .and. .not. flowWithoutWaves) then ! vertical shear based on eulerian velocity field, see turclo,note JvK, Ardhuin 2006
                  dijdij(k) = ((u1(Lu) - ustokes(Lu) - u1(L) + ustokes(L))**2 + (v(Lu) - vstokes(Lu) - v(L) + vstokes(L))**2) / dzw(k)**2
               else
                  dijdij(k) = ((u1(Lu) - u1(L))**2 + (v(Lu) - v(L))**2) / dzw(k)**2
               end if

               if (jarichardsononoutput > 0) then ! save richardson nr to output
                  rich(L) = sigrho * bruva(k) / max(1d-8, dijdij(k)) ! sigrho because bruva premultiplied by 1/sigrho
               end if

               sourtu = max(vicwwu(L), vicwminb) * dijdij(k)

               !
               if (iturbulencemodel == 3) then
                  sinktu = tureps0(L) / turkin0(L) ! + tkedis(L) / turkin0(L)
                  bk(k) = bk(k) + sinktu * 2d0
                  dk(k) = dk(k) + sinktu * turkin0(L) + sourtu ! m2/s3
               else if (iturbulencemodel == 4) then
                  sinktu = 1d0 / tureps0(L) ! + tkedis(L) / turkin0(L)
                  bk(k) = bk(k) + sinktu
                  dk(k) = dk(k) + sourtu
               end if

               ! dk(k)  = dk(k)  + sourtu - sinktu*turkin0(L)

            end do ! Lb, Lt-1

            if (jawave > 0 .and. jawavebreakerturbulence > 0) then
               ! check if first layer is thicker than fwavpendep*wave height
               ! Then use JvK solution
               if (hu(LL) - hu(Lt - 1) >= fwavpendep * hrmsLL) then
                  dk(kxL - 1) = dk(kxL - 1) + pkwmag * fwavpendep * hrmsLL / (hu(LL) - hu(Lt - 1)) ! m2/s3
               else
                  ! distribute over layers
                  do L = Lt - 1, Lb
                     if (hu(L + 1) < wdep) exit
                     k = L - Lb + 1
                     if (hu(L) < wdep .and. hu(L + 1) >= wdep) then
                        ! partial contribution
                        dzwav = hu(LL) - wdep
                        pkwav(k) = pkwmag * (1.0 - dzwav / (fwavpendep * hrmsLL))
                        dk(k) = dk(k) + pkwav(k)
                        exit
                     end if
                     dzwav = hu(LL) - hu(L)
                     pkwav(k) = pkwmag * (1.0 - dzwav / (fwavpendep * hrmsLL))
                     dk(k) = dk(k) + pkwav(k)
                  end do
               end if
            end if
            !
            ! Boundary conditions, dirichlet:
            ! TKE at free surface
            ak(kxL) = 0.d0
            bk(kxL) = 1.d0
            ck(kxL) = 0.d0
            dk(kxL) = tkesur
            ! TKE at the bed:
            ak(0) = 0.d0
            bk(0) = 1.d0
            ck(0) = 0.d0
            dk(0) = tkebot

            if (javau > 0 .or. javakeps > 0) then
               ac1 = acL(LL); ac2 = 1d0 - ac1
               n1 = ln(1, LL); !zb1 = zws(kbot(n1)-1)
               n2 = ln(2, LL); !zb2 = zws(kbot(n2)-1)
               do L = Lb, Lt - 1 ! vertical omega velocity at layer interface u point
                  k1 = ln(1, L); k2 = ln(2, L)
                  k = L - Lb + 1

                  if (n1 > ndxi) then ! open boundaries
                     if (u1(LL) < 0d0) then
                        womegu(k) = qw(k2) / a1(n2)
                     else
                        womegu(k) = 0d0
                     end if
                  else
                     womegu(k) = (ac1 * qw(k1) + ac2 * qw(k2)) / (ac1 * a1(ln(1, LL)) + ac2 * a1(ln(2, LL)))
                  end if

               end do
               womegu(Lt - Lb + 1) = 0d0 ! top layer : 0

               if (javakeps >= 3) then ! Advection of turkin, vertical implicit, horizontal explicit
                  arLL = ac1 * a1(n1) + ac2 * a1(n2)
                  do L = Lb, Lt - 1
                     k = L - Lb + 1
                     omegu = 0.5d0 * womegu(k)
                     if (k > 1) omegu = omegu + 0.5d0 * womegu(k - 1) ! Omega at U-point in between layer interfaces
                     if (omegu > 0d0) then ! omegu(k) lies below interface(k)
                        adv = omegu / dzw(k) ! omegu(k) > 0 contributes to k
                        bk(k) = bk(k) + adv
                        ak(k) = ak(k) - adv
                     else
                        if (k > 1) then
                           adv = -omegu / dzw(k - 1)
                           bk(k - 1) = bk(k - 1) + adv
                           ck(k - 1) = ck(k - 1) - adv
                        end if
                     end if

                     if (javakeps == 3) then ! turkin
                        if (q1(L) + q1(L + 1) > 0) then
                           kup = ln(1, L); arLL = a1(n1)
                        else
                           kup = ln(2, L); arLL = a1(n2)
                        end if
                        volki = 1d0 / (dzw(k) * arLL)
                        dk(k) = dk(k) + tqcu(kup) * volki
                        bk(k) = bk(k) + sqcu(kup) * volki
                     else if (javakeps == 4) then ! turkin
                        k1 = ln(1, L); k2 = ln(2, L)
                        volki = (ac1 * (vol1(k1) + vol1(k1 + 1)) + ac2 * (vol1(k2) + vol1(k2 + 1))) * 0.5d0
                        volki = 1d0 / volki
                        dk(k) = dk(k) + (ac1 * tqcu(k1) + ac2 * tqcu(k2)) * volki
                        bk(k) = bk(k) + (ac1 * sqcu(k1) + ac2 * sqcu(k2)) * volki
                     end if
                  end do
               end if
            end if

            if (javeg > 0) then ! in turbulence model
               dke(1:Lt - Lb + 1) = 0d0; k1 = ln(1, LL); k2 = ln(2, LL)
               rnv = 0.5d0 * (rnveg(ln(1, LL)) + rnveg(ln(2, LL)))
               if (rnv > 0d0) then ! if plants are here
                  do L = Lb, Lt
                     um = sqrt(u1(L) * u1(L) + v(L) * v(L)) ! umod (m2/s2)
                     if (um > 0d0) then ! and if there is flow,
                        k = L - Lb + 1; k1 = ln(1, L); k2 = ln(2, L)
                        rnv = 0.5d0 * (rnveg(k1) + rnveg(k2))
                        if (rnv > 0) then ! if in this layer
                           if (diaveg(k1) > 0 .and. diaveg(k2) > 0) then
                              diav = 0.5d0 * (diaveg(k1) + diaveg(k2))
                           else
                              diav = max(diaveg(k1), diaveg(k2))
                           end if
                           if (jaCdvegsp == 1) then
                              if (Cdvegsp(k1) > 0 .and. Cdvegsp(k2) > 0) then
                                 Cdveg = 0.5d0 * (Cdvegsp(k1) + Cdvegsp(k2))
                              else
                                 Cdveg = max(Cdvegsp(k1), Cdvegsp(k2))
                              end if
                           end if
                           vk = 0.5d0 * Cdveg * rnv * diav * um ! (1/s)
                           advi(L) = advi(L) + vk ! add to diagonal of u1
                           wk = vk * um * um ! work done by this layer m2/s3
                           ap1 = 1.0 - diav * diav * rnv * pi * 0.25 ! Free area
                           xlveg = Clveg * sqrt(ap1 / rnv) ! typical length between plants
                           tauinv = c2esqcmukep * (wk / xlveg**2)**r3
                           teps = 0.5d0 * (tureps0(L) + tureps0(L))
                           tkin = 0.5d0 * (turkin0(L) + turkin0(L))
                           if (iturbulencemodel == 3) then
                              tauinf = c2e * teps / tkin !
                           else if (iturbulencemodel == 4) then
                              tauinf = c2e / teps
                           end if
                           if (tauinf > tauinv) then ! turb damping not governed by plants => free flow damping only
                              tauinv = 0d0 ! tauinv = max(tauinv, tauinf)
                           end if
                           if (iturbulencemodel == 3) then
                              wke = wk * tauinv
                           else if (iturbulencemodel == 4) then
                              wke = wk * (1d0 - tureps1(L) * tauinv) * tureps1(L) / turkin1(L)
                           end if
                           if (L < Lt) then
                              alf = 0.5d0 * dzu(k) / dzw(k)
                              dk(k) = dk(k) + alf * wk ! half is added to top interface to k
                              dke(k) = dke(k) + alf * wke !                                to eps
                           end if
                           if (L > Lb) then
                              alf = 0.5d0 * dzu(k) / dzw(k - 1)
                              dk(k - 1) = dk(k - 1) + alf * wk ! other half added to bed interface to k
                              dke(k - 1) = dke(k - 1) + alf * wke !                                   to eps
                           end if
                        end if
                     end if
                  end do
               end if
            end if

            call tridag(ak, bk, ck, dk, ek, turkin1(Lb0:Lt), kxL + 1) ! solve k
            turkin1(Lb0:Lt) = max(epstke, turkin1(Lb0:Lt))
            do L = Lt + 1, Lb + kmxL(LL) - 1 ! copy to surface for z-layers
               turkin1(L) = turkin1(Lt)
            end do

            !_____________________________________________________________________________________!

            ak(0:kxL) = 0.d0 ! Matrix initialization eps, tau
            bk(0:kxL) = dtiL
            ck(0:kxL) = 0.d0
            dk(0:kxL) = dtiL * tureps0(Lb0:Lt)
            ! Vertical diffusion; Neumann condition on surface;
            ! Dirichlet condition on bed ; teta method:

            if (turbulence_lax_factor > 0) then
               if (turbulence_lax_vertical == 1) then
                  do L = Lb, Lt - 1
                     zf = min(1d0, (hu(L) - 0.5 * hu(LL)) / (0.25d0 * hu(LL)))
                     if (zf > 0d0) then ! top half only: 0.5-0.75: zf = linear from 0 to 1,  > 0.75 : zf 1
                        k1 = ln(1, L); k2 = ln(2, L)
                        if (turkinepsws(2, k1) > eps20 .and. turkinepsws(2, k2) > eps20) then
                           if (turbulence_lax_horizontal == 1 .or. (zws(k1) > zws(k2 - 1) .and. zws(k1 - 1) < zws(k2))) then
                              faclax = turbulence_lax_factor * zf
                              faclax = faclax * dzu(L - Lb + 1) / max(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1))
                              dk(L - Lb + 1) = dtiL * ((1d0 - facLax) * tureps0(L) + 0.5d0 * facLax * (turkinepsws(2, k1) + turkinepsws(2, k2)))
                           end if
                        end if
                     end if
                  end do
               else if (turbulence_lax_vertical == 2) then
                  do L = Lb, Lt - 1
                     k1 = ln(1, L); k2 = ln(2, L)
                     if (turkinepsws(2, k1) > eps20 .and. turkinepsws(2, k2) > eps20) then
                        if (turbulence_lax_horizontal == 1 .or. (zws(k1) > zws(k2 - 1) .and. zws(k1 - 1) < zws(k2))) then
                           faclax = turbulence_lax_factor * dzu(L - Lb + 1) / max(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1))
                           dk(L - Lb + 1) = dtiL * ((1d0 - facLax) * tureps0(L) + 0.5d0 * facLax * (turkinepsws(2, k1) + turkinepsws(2, k2)))
                        end if
                     end if
                  end do
               end if
            end if

            vicu = viskin + 0.5d0 * (vicwwu(Lb0) + vicwwu(Lb)) * sigepsi

            do L = Lb, Lt - 1
               Lu = L + 1

               vicd = vicu
               vicu = viskin + 0.5d0 * (vicwwu(L) + vicwwu(Lu)) * sigepsi

               k = L - Lb + 1; ku = k + 1

               dzdz1 = dzw(k) * dzu(k)
               difd = vicd / dzdz1

               dzdz2 = dzw(k) * dzu(ku)
               difu = vicu / dzdz2

               ak(k) = ak(k) - difd * tetavkeps
               bk(k) = bk(k) + (difd + difu) * tetavkeps
               ck(k) = ck(k) - difu * tetavkeps
               if (tetavkeps /= 1d0) then
                  dk(k) = dk(k) - difu * (tureps0(L) - tureps0(Lu)) * tetm1 &
                          + difd * (tureps0(L - 1) - tureps0(L)) * tetm1
               end if

               if (iturbulencemodel == 3) then !k-eps

                  !c Source and sink terms                                                                epsilon
                  if (bruva(k) < 0.d0) then ! instable, increase rhs
                     dk(k) = dk(k) - cmukep * c1e * bruva(k) * turkin1(L)
                  end if

                  ! Similar to the k-equation, in the eps-equation the net IWE to TKE
                  ! transfer rate (TKEPRO-TKEDIS) is added to the eps-production term, but
                  ! split for implicit treatment for avoiding negative epsilon.

                  sourtu = c1e * cmukep * turkin0(L) * dijdij(k)
                  !
                  ! Add wave dissipation production term
                  if (jawave > 0 .and. jawavebreakerturbulence > 0) then
                     sourtu = sourtu + pkwav(k) * c1e * tureps0(L) / max(turkin0(L), 1d-7)
                     !sourtu = sourtu + c1e*cmukep*turkin0(L)/max(vicwwu(L),vicwminb)*pkwav(k)
                  end if

                  tkedisL = 0d0 ! tkedis(L)
                  sinktu = c2e * (tureps0(L) + tkedisL) / turkin1(L) ! yoeri has here : /turkin0(L)

                  !c Addition of production and of dissipation to matrix ;                               epsilon
                  !c observe implicit treatment by Newton linearization.

                  bk(k) = bk(k) + sinktu * 2d0
                  dk(k) = dk(k) + sinktu * tureps0(L) + sourtu

                  !  bk(k) = bk(k) + sinktu
                  !  dk(k) = dk(k) + sourtu

                  ! dk(k) = dk(k) - sinktu*tureps0(L) + sourtu

               else if (iturbulencemodel == 4) then !                                               k-tau

                  ! buoyancy term, we have in RHS :~ -Bruva*c3t
                  ! c1e = 1.44
                  ! bruva<0, instable, c3e=c1e c3t=1-c3e=-0.44 => -Bruva*c3t < 0 increase diag
                  ! bruva>0    stable, c3e=0   c3t=1-c3e= 1.00 => -Bruva*c3t < 0 increase diag

                  if (bruva(k) < 0d0) then ! instable
                     c3t = c3tuns ! == -0.044   !c3e = c1e ; c3t = (1d0-c3e)*cmukep
                     bk(k) = bk(k) + c3t * bruva(k) * tureps0(L)
                  else if (bruva(k) > 0d0) then ! stable
                     c3t = c3tsta ! == 0.09     !c3e = 0d0 ; c3t = (1d0-c3e)*cmukep
                     bk(k) = bk(k) + c3t * bruva(k) * tureps0(L)
                  end if

                  bk(k) = bk(k) - c1t * dijdij(k) * tureps0(L)
                  dk(k) = dk(k) - c2t

                  gradd = 0.5d0 * (turkin0(L - 1) + turkin0(L)) * (tureps0(L) - tureps0(L - 1)) / dzu(k) ! The D_tt-term:
                  gradu = 0.5d0 * (turkin0(Lu) + turkin0(L)) * (tureps0(Lu) - tureps0(L)) / dzu(ku)
                  gradt = gradd + gradu

                  gradd = 0.5d0 * (tureps0(L - 1) + tureps0(L)) * (turkin0(L) - turkin0(L - 1)) / dzu(k) ! The D_kt-term:
                  gradu = 0.5d0 * (tureps0(Lu) + tureps0(L)) * (turkin0(Lu) - turkin0(L)) / dzu(ku)
                  gradk = gradd + gradu
                  grad = gradk - gradt ! D_kt - D_tt

                  grad = -grad * sigepsi * cmukep ! This is positive advection, dc/dt + wdc/dz
                  grad = grad / dzw(k) ! dzw is receiving volume
                  if (grad > 0d0) then
                     bk(k) = bk(k) + grad
                     ak(k) = ak(k) - grad
                  else if (grad < 0d0) then
                     bk(k) = bk(k) - grad
                     ck(k) = ck(k) + grad
                  end if

               end if

            end do

            if (iturbulencemodel == 3) then ! Boundary conditions EPSILON:

               ak(kxL) = -1.d0 ! Flux at the free surface:
               bk(kxL) = 1.d0
               ck(kxL) = 0.d0
               dk(kxL) = 4d0 * abs(ustw(LL))**3 / (vonkar * dzu(Lt - Lb + 1))
               if (jawave > 0 .and. jawavebreakerturbulence > 0) then ! wave dissipation at surface, neumann bc, dissipation over fwavpendep*Hrms
                  dk(kxL) = dk(kxL) + dzu(Lt - Lb + 1) * pkwmag / (fwavpendep * hrmsLL)
               end if

               ak(0) = 0.d0 ! at the bed:
               bk(0) = 1.d0
               ck(0) = -1.d0
               if (ustb(LL) > 0 .and. kxL > 1) then ! deps/dz = (epsb+1-epsb)/dz = (u*)**3/ ((dz/2+9z0)**2)
                  dk(0) = dzu(1) * abs(ustb(LL))**3 / (vonkar * hdzb * hdzb)
               else
                  dk(0) = 0d0
               end if

            else if (iturbulencemodel == 4) then ! Boundary conditions tau:

               ak(kxL) = 0.d0 ! at the free surface:
               bk(kxL) = 1.d0
               ck(kxL) = 0.d0
               dk(kxL) = 0.d0
               ! dk(kxL) =  vonkar*9d0*z00/(max(ustw(LL),eps6)*0.3d0)  ! 0.3=sqrt(cmu0), cmu0=cmukep

               ak(0) = 0.d0 ! at the bed:
               bk(0) = 1.d0
               ck(0) = 0.d0
               if (ustb(LL) > 0) then
                  dk(0) = vonkar * c9of1 * z00 / (max(ustb(LL), eps6) * 0.3d0) ! 0.3=sqrt(cmu0), cmu0=cmukep
               else
                  dk(0) = 0d0
               end if

            end if

            if (javakeps >= 3) then ! Advection of tureps, vertical implicit, horizontal explicit
               do L = Lb, Lt - 1
                  k = L - Lb + 1
                  omegu = 0.5d0 * womegu(k)
                  if (k > 1) omegu = omegu + 0.5d0 * womegu(k - 1) ! Omega at U-point in between layer interfaces
                  if (omegu > 0d0) then
                     adv = omegu / dzw(k)
                     bk(k) = bk(k) + adv
                     ak(k) = ak(k) - adv
                  else
                     if (k > 1) then
                        adv = -omegu / dzw(k - 1)
                        bk(k - 1) = bk(k - 1) + adv
                        ck(k - 1) = ck(k - 1) - adv
                     end if
                  end if

                  if (javakeps == 3) then ! tureps
                     if (q1(L) + q1(L + 1) > 0) then
                        kup = ln(1, L); arLL = a1(n1)
                     else
                        kup = ln(2, L); arLL = a1(n2)
                     end if
                     volki = 1d0 / (dzw(k) * arLL)
                     dk(k) = dk(k) + eqcu(kup) * volki
                     bk(k) = bk(k) + sqcu(kup) * volki
                  else if (javakeps == 4) then ! tureps
                     k1 = ln(1, L); k2 = ln(2, L)
                     volki = (ac1 * (vol1(k1) + vol1(k1 + 1)) + ac2 * (vol1(k2) + vol1(k2 + 1))) * 0.5d0
                     volki = 1d0 / volki
                     dk(k) = dk(k) + (ac1 * eqcu(k1) + ac2 * eqcu(k2)) * volki
                     bk(k) = bk(k) + (ac1 * sqcu(k1) + ac2 * sqcu(k2)) * volki
                  end if
               end do
            end if

            if (javeg > 0) then ! in turbulence model
               do L = Lb, Lt - 1
                  k = L - Lb + 1
                  dk(k) = dk(k) + dke(k)
               end do
            end if

            call tridag(ak, bk, ck, dk, ek, tureps1(Lb0:Lt), kxL + 1) ! solve eps
            tureps1(Lb0:Lt) = max(epseps, tureps1(Lb0:Lt))
            do L = Lt + 1, Lb + kmxL(LL) - 1 ! copy to surface for z-layers
               tureps1(L) = tureps1(Lt)
            end do

            if (javatest == 3) then ! test advection
               ak(0:kxL) = 0.d0 ! Matrix initialisation
               bk(0:kxL) = dtiL
               ck(0:kxL) = 0.d0
               dk(0:kxL) = dtiL * tttu(Lb0:Lt)

               if (javau > 0 .or. javakeps > 0) then

                  if (javakeps >= 3) then ! Advection of tttu, vertical implicit, horizontal explicit
                     arLL = ac1 * a1(n1) + ac2 * a1(n2)
                     do L = Lb, Lt - 1
                        k = L - Lb + 1
                        omegu = 0.5d0 * womegu(k)
                        if (k > 1) omegu = omegu + 0.5d0 * womegu(k - 1) ! Omega at U-point in between layer interfaces
                        if (omegu > 0d0) then ! omegu(k) lies below interface(k)
                           adv = omegu / dzw(k) ! omegu(k) > 0 contributes to k
                           bk(k) = bk(k) + adv
                           ak(k) = ak(k) - adv
                        else
                           if (k > 1) then
                              adv = -omegu / dzw(k - 1)
                              bk(k - 1) = bk(k - 1) + adv
                              ck(k - 1) = ck(k - 1) - adv
                           end if
                        end if
                        if (q1(L) + q1(L + 1) > 0) then
                           kup = ln(1, L); arLL = a1(n1)
                        else
                           kup = ln(2, L); arLL = a1(n2)
                        end if
                        volki = 1d0 / (dzw(k) * arLL)
                        if (javakeps == 3) then ! in test
                           dk(k) = dk(k) + ttqc(kup) * volki
                           bk(k) = bk(k) + sqcu(kup) * volki
                        else if (javakeps == 4) then ! in test
                           k1 = ln(1, L); k2 = ln(2, L)
                           dk(k) = dk(k) + (ac1 * ttqc(k1) + ac2 * ttqc(k2)) * volki
                           bk(k) = bk(k) + (ac1 * sqcu(k1) + ac2 * sqcu(k2)) * volki
                        end if
                     end do

                  end if

               end if

               call tridag(ak, bk, ck, dk, ek, tttu(Lb0:Lt), kxL + 1) ! solve tttu

            end if ! end test

            ! If it is a restart simulation, spin up function is not allowed
            !if (jarestart == 0) then
            if ((Tspinupturblogprof > 0d0 .and. Time1 < Tstart_user + Tspinupturblogprof) .or. &
                (jaLogprofkepsbndin == 1 .and. LL > lnxi .and. u1(LL) >= 0d0) .or. &
                (jaLogprofkepsbndin == 2 .and. LL > lnxi)) then

               alfaT = dmiss
               if (Tspinupturblogprof > 0d0) then
                  alfaT = (Time1 - Tstart_user) / Tspinupturblogprof
               end if
               if (LL > lnxi) then
                  alfaT = 0d0
               end if

               if (alfaT /= dmiss) then
                  do L = Lb, Lt - 1 ! TKE and epsilon at layer interfaces:
                     zint = hu(L) / hu(LL)
                     z1 = 1d0 - zint
                     k1 = ln(1, L); k2 = ln(1, L)
                     tke = tkebot * z1 + tkesur * zint
                     zz = hu(L) * (1d0 - hu(L) / hu(LL)) ! parabolic visc
                     vicwww = zz * max(0.001d0, ustb(LL)) * vonkar
                     eps = cmukep * tke * tke / vicwww
                     turkin1(L) = tke * (1d0 - alfaT) + alfaT * turkin1(L)
                     tureps1(L) = eps * (1d0 - alfaT) + alfaT * tureps1(L)
                  end do
                  epsbot = tureps1(Lb) + dzu(1) * abs(ustb(LL))**3 / (vonkar * hdzb * hdzb)
                  epssur = tureps1(Lt - 1) - 4d0 * abs(ustw(LL))**3 / (vonkar * dzu(Lt - Lb + 1))
                  if (jawave > 0 .and. jawavebreakerturbulence > 0) then
                     epssur = epssur - dzu(Lt - Lb + 1) * fwavpendep * pkwmag / hrmsLL
                  end if
                  epsbot = max(epsbot, epseps)
                  epssur = max(epssur, epseps)
                  tke = max(epstke, tkesur)
                  turkin1(Lt) = tke * (1d0 - alfaT) + alfaT * turkin1(Lt)
                  eps = epssur / (hu(Lt) - hu(Lt - 1))
                  tureps1(Lt) = eps * (1d0 - alfaT) + alfaT * tureps1(Lt)
                  tke = max(epstke, tkebot)
                  turkin1(Lb - 1) = tke * (1d0 - alfaT) + alfaT * turkin1(Lb - 1)
                  eps = epsbot / (hu(Lb) - hu(Lb - 1))
                  tureps1(Lb - 1) = eps * (1d0 - alfaT) + alfaT * tureps1(Lb - 1)

                  if (jamodelspecific == 1) then
                     call update_turkin_modelspecific(LL) ! will update turkin1 and tureps1 for all layers of flow link LL.
                  end if
               end if
            end if

            vicwmax = 0.1d0 * hu(LL) ! 0.009UH, Elder, uavmax=
            if (iturbulencemodel == 3) then ! k-eps
               vicwwu(Lb0:Lt) = min(vicwmax, cmukep * turkin1(Lb0:Lt) * turkin1(Lb0:Lt) / tureps1(Lb0:Lt))
            else if (iturbulencemodel == 4) then ! k-tau
               vicwwu(Lb0:Lt) = min(vicwmax, cmukep * turkin1(Lb0:Lt) * tureps1(Lb0:Lt))
            end if

            vicwwu(Lt) = min(vicwwu(Lt), vicwwu(Lt - 1) * Eddyviscositysurfacmax)
            vicwwu(Lb0) = min(vicwwu(Lb0), vicwwu(Lb) * Eddyviscositybedfacmax)

            call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

         else ! dry

            tureps1(Lb0:Lb + kmxL(LL) - 1) = epseps
            turkin1(Lb0:Lb + kmxL(LL) - 1) = epstke

         end if ! if (hu(L) > 0) then
      end do ! links loop

      !$xOMP END PARALLEL DO

      turkin0 = turkin1
      tureps0 = tureps1

   end if

   !if ( jampi.eq.1 ) then
   !   call update_ghosts(ITYPE_U3DW, 1, Ndkx, turkin1, ierror)
   !   call update_ghosts(ITYPE_U3DW, 1, Ndkx, tureps1, ierror)
   !end if

   call links_to_centers(vicwws, vicwwu)

end subroutine update_verticalprofiles
