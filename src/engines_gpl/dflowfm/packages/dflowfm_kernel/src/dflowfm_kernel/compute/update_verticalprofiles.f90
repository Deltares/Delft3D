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

module m_update_verticalprofiles
   use precision, only: dp, comparereal

   implicit none(type, external)

   private

   public :: update_verticalprofiles

contains

   !> Compute turbulence quantities and call vertical_profile_u0
   !! When iturbulencemodel>=3 (k-eps, k-tau):
   !!    For each link:
   !!       Build a vertical tri-diagonal system to solve the k-equation. Idem for the the eps-equation.
   !! Update vicww (iturbulencemodel>=2)
   subroutine update_verticalprofiles()
      use m_getustbcfuhi, only: get_ustb_cfuhi
      use m_doaddksources, only: doaddksources
      use m_flow, only: iturbulencemodel, kmx, iadvec, javau, hu, lbot, ltop, ustb, cfuhi, advi, jawave, jawavestokes, flow_without_waves, adve, u1, &
                        a1, vicwwu, vonkar, c2e, ndkx, turbulent_advection_scheme, turbulent_kinetic_energy_ws, turbulent_kinetic_energy_ws_old, turbulent_dissipation_ws, turbulent_dissipation_ws_old, turbulent_kinetic_energy_wu, turbulent_dissipation_wu, sum_q_times_turbulent_kinetic_energy_ws, sum_q_times_turbulent_dissipation_ws, sum_inflowing_discharges_ws, q1, THETA_KEPS_DIFFUSION, &
                        ustbs, ustws, EPS4, trsh_u1lb, ustw, ieps, turbulent_kinetic_energy_wu_old, zws, turbulent_dissipation_wu_old, ak, bk, ck, dk, qw, &
                        jarichardsononoutput, sigrho, vol1, javeg, dke, rnveg, diaveg, jacdvegsp, cdvegsp, cdveg, clveg, r3, ek, tke_min, kmxl, &
                        c1e, c1t, c2t, c9of1, EPS6, eps_min, jalogprofkepsbndin, dmiss, jamodelspecific, eddyviscositybedfacmax, &
                        vicwws, kmxx, tur_time_int_factor, EPS20, tur_time_int_method, TURB_LAX_ALL, viskin, jawavebreakerturbulence, &
                        rhomean, bruva, buoflu, vicwminb, dijdij, v, eddyviscositysurfacmax, use_density, ktop, kbot, hs, epshu, ucx, ucy, kmxn, &
                        TURBULENCE_MODEL_NONE, TURBULENCE_MODEL_CONSTANT, TURBULENCE_MODEL_ALGEBRAIC, TURBULENCE_MODEL_KEPS_FLOW_LINKS, &
                        TURBULENCE_MODEL_KTAU, TURBULENCE_MODEL_KEPS_CELL_CENTER
      use m_source_sink, only: source_sinks
      use m_flowgeom, only: ndx, lnx, acl, ln, lnxi
      use m_waves, only: hwav, gammax, ustokes, vstokes, fbreak, fwavpendep
      use m_partitioninfo, only: jampi, itype_sall3d, update_ghosts
      use m_flowtimes, only: dtprev, t_spinup_turb_log_prof, time1, tstart_tlfsmo_user
      use m_sferic, only: pi
      use m_get_Lbot_Ltop, only: getlbotltop
      use m_links_to_centers, only: links_to_centers
      use m_turbulence, only: cmukep, drhodz, brunt_vaisala_coefficient, rich, richs, c3e_stable, c3e_unstable, sigtkei, sigepsi, cde, &
                  c3t_stable, c3t_unstable
      use m_tridag, only: tridag
      use m_model_specific, only: update_turkin_modelspecific
      use m_wave_fillsurdis, only: wave_fillsurdis
      use m_vertical_profile_u0, only: vertical_profile_u0
      use m_alloc, only: aerr
      use m_waveconst

      real(kind=dp) :: theta_complement
      real(kind=dp) :: vicu, vicd, difu, difd, dzdz1, dzdz2, sourtu, sinktu
      real(kind=dp) :: zz, z00, ac1, ac2, turbulent_kinetic_energy_at_bottom, turbulent_kinetic_energy_at_top, epsbot, epssur
      real(kind=dp) :: hdzb, adv, omegu
      real(kind=dp) :: dt_inverse !< [1/s] Inverse of the previous time step (1/dtprev).
      real(kind=dp), dimension(kmxx) :: dzu !< [m] Layer thickness at flow links.
      real(kind=dp), dimension(kmxx) :: dzw !< [m] Vertical distance between layer centers. (= 0.5 * (dzu(k) + dzu(k+1)))
      real(kind=dp) :: womegu(kmxx), pkwav(kmxx)
      real(kind=dp) :: dzs(kmxx), dzws(kmxx), hdzbs
      real(kind=dp) :: gradk, gradt, grad, gradd, gradu, volki, arLL
      real(kind=dp) :: discharge_at_layer_interface !< [m3/s] Discharge at the layer interface
      real(kind=dp) :: wk, wke, vk, um, tauinv, tauinf, xlveg, rnv, diav, ap1, alf, teps, tkin
      real(kind=dp) :: cfuhi3D, vicwmax, zint, z1, vicwww, alfaT, tke, eps
      real(kind=dp) :: rhoLL, pkwmag, hrmsLL, wdep, dzwav, dis1, dis2, surdisLL
      integer :: k, ku, LL, L, Lb, Lt, kxL, Lu, Lb0, whit
      integer :: k1, k2, n1, n2, kup, ierror
      integer :: kk, kb, kb0, kt

      if (iturbulencemodel <= TURBULENCE_MODEL_NONE .or. kmx == 0) then
         return
      end if

      if (iadvec == 0) then
         javau = 0
      end if
      
      womegu = 0.0_dp

      select case (iturbulencemodel)

      case (TURBULENCE_MODEL_CONSTANT)

         !$OMP PARALLEL DO &
         !$OMP PRIVATE(LL,Lb,Lt,kxL,dzu,L,k,hdzb,z00,ac1,ac2,n1,n2,k1,k2,womegu,cfuhi3D)

         do LL = 1, lnx

            if (hu(LL) > 0.0_dp) then

               Lb = Lbot(LL) ! bed layer index
               Lt = Ltop(LL) ! surface layer index = surface interface index
               kxL = Lt - Lb + 1 ! nr of layers

               dzu(1) = hu(Lb)
               do L = Lb + 1, Lt ! layer thickness at layer center
                  k = L - Lb + 1
                  dzu(k) = hu(L) - hu(L - 1)
               end do

               call get_ustb_cfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) !Constant
               advi(Lb) = advi(Lb) + cfuhi3D
               !
               if (jawave /= NO_WAVES .and. jawaveStokes >= STOKES_DRIFT_DEPTHUNIFORM .and. .not. flow_without_waves) then ! Ustokes correction at bed
                  adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
               end if

               if (javau > 0) then
                  call calculate_womegu(womegu, LL)
               end if

               call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

            end if

         end do

         !$OMP END PARALLEL DO

      case (TURBULENCE_MODEL_ALGEBRAIC) ! just testing 1D flow

         do LL = 1, lnx

            if (hu(LL) > 0.0_dp) then

               Lb = Lbot(LL) ! bed layer index
               Lt = Ltop(LL) ! surface layer index = surface interface index
               kxL = Lt - Lb + 1 ! nr of layers

               dzu(1) = hu(Lb)
               do L = Lb + 1, Lt ! layer thickness at layer center
                  k = L - Lb + 1
                  dzu(k) = hu(L) - hu(L - 1)
               end do

               call get_ustb_cfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) ! algebraic
               advi(Lb) = advi(Lb) + cfuhi3D
               !
               if (jawave /= NO_WAVES .and. jawaveStokes >= STOKES_DRIFT_DEPTHUNIFORM .and. .not. flow_without_waves) then ! Ustokes correction at bed
                  adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
               end if

               if (javau > 0) then
                  call calculate_womegu(womegu, LL)
               end if

               vicwwu(Lb - 1) = 0.0_dp

               do L = Lb, Lt
                  zz = hu(L) * (1.0_dp - hu(L) / hu(LL)) ! parabolic
                  !           zz       = hu(L)*sqrt( ( 1.0_dp - hu(L)/ hu(LL) ) )     ! Bakhmetev
                  vicwwu(L) = zz * ustb(LL) * vonkar
               end do

               call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

            end if

         end do

      case (TURBULENCE_MODEL_KEPS_FLOW_LINKS, TURBULENCE_MODEL_KTAU)

         call calculate_drhodz(zws, drhodz)

         call links_to_centers(turbulent_kinetic_energy_ws, turbulent_kinetic_energy_wu)
         call links_to_centers(turbulent_dissipation_ws, turbulent_dissipation_wu)

         if (turbulent_advection_scheme > 0) then ! transport switched on: prepare horizontal advection k and eps

            if (source_sinks%num_total > 0 .and. source_sinks%add_k_to_turkin) then
               call doaddksources()
            end if

            if (jampi == 1) then
               call update_ghosts(ITYPE_Sall3D, 1, Ndkx, turbulent_kinetic_energy_ws, ierror)
               call update_ghosts(ITYPE_Sall3D, 1, Ndkx, turbulent_dissipation_ws, ierror)
            end if

            sum_q_times_turbulent_kinetic_energy_ws(:) = 0.0_dp
            sum_q_times_turbulent_dissipation_ws(:) = 0.0_dp
            sum_inflowing_discharges_ws(:) = 0.0_dp

            do LL = 1, lnx
               call getLbotLtop(LL, Lb, Lt)
               do L = Lb, Lt - 1
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  discharge_at_layer_interface = 0.5_dp * (q1(L) + q1(L + 1))

                  if (discharge_at_layer_interface > 0) then ! set upwind center values on links

                     sum_q_times_turbulent_kinetic_energy_ws(k2) = &
                        sum_q_times_turbulent_kinetic_energy_ws(k2) &
                        + discharge_at_layer_interface * turbulent_kinetic_energy_ws(k1)
                     
                     sum_q_times_turbulent_dissipation_ws(k2) = &
                        sum_q_times_turbulent_dissipation_ws(k2) &
                        + discharge_at_layer_interface * turbulent_dissipation_ws(k1)
                     
                     sum_inflowing_discharges_ws(k2) = sum_inflowing_discharges_ws(k2) + discharge_at_layer_interface
                  
                  else if (discharge_at_layer_interface < 0) then

                     sum_q_times_turbulent_kinetic_energy_ws(k1) = &
                        sum_q_times_turbulent_kinetic_energy_ws(k1) &
                        - discharge_at_layer_interface * turbulent_kinetic_energy_ws(k2)
                     
                     sum_q_times_turbulent_dissipation_ws(k1) = &
                        sum_q_times_turbulent_dissipation_ws(k1) &
                        - discharge_at_layer_interface * turbulent_dissipation_ws(k2)
                     
                     sum_inflowing_discharges_ws(k1) = sum_inflowing_discharges_ws(k1) - discharge_at_layer_interface
                  
                  end if

               end do
            end do
         end if

         theta_complement = 1.0_dp - THETA_KEPS_DIFFUSION
         dt_inverse = 1.0_dp / dtprev ! turbulence transport in current velocity field => do not use new timestep but previous step

         do LL = 1, lnx ! all this at velocity points

            turbulent_kinetic_energy_at_top = 0.0_dp
            turbulent_kinetic_energy_at_bottom = 0.0_dp

            Lt = Ltop(LL) ! surface layer index = surface interface index
            Lb = Lbot(LL) ! bed layer index
            if (Lt < Lb) then
               cycle
            end if
            Lb0 = Lb - 1 ! bed interface index

            if (hu(LL) > 0.0_dp) then ! epshu?
               kxL = Lt - Lb + 1 ! nr of layers

               do L = Lb, Lt ! layer thickness at layer center (org: Lb + 1)
                  k = L - Lb + 1
                  dzu(k) = max(EPS4, hu(L) - hu(L - 1))
               end do

               do L = Lb, Lt - 1 ! layer thickness at layer interface
                  k = L - Lb + 1
                  dzw(k) = 0.5_dp * (dzu(k) + dzu(k + 1))
               end do

               call get_ustb_cfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) ! K-EPS, K-TAU z00 wave-enhanced roughness for jawave>0

               if (hu(LL) < trsh_u1Lb) then
                  advi(Lb:Lt) = advi(Lb:Lt) + cfuhi3D / real(Lt - Lb + 1, kind=dp)
               else
                  advi(Lb) = advi(Lb) + cfuhi3D
               end if

               turbulent_kinetic_energy_at_bottom = ustb(LL)**2 / sqrt(cmukep) ! this has stokes incorporated when jawave>0
               turbulent_kinetic_energy_at_top = ustw(LL)**2 / sqrt(cmukep) ! only wind+ship contribution

               if (ieps == 3) then ! as Delft3D
                  vicwwu(Lb0) = vonkar * ustb(LL) * z00
               end if

               turbulent_kinetic_energy_wu_old(Lb0:Lt) = turbulent_kinetic_energy_wu(Lb0:Lt)
               turbulent_dissipation_wu_old(Lb0:Lt) = turbulent_dissipation_wu(Lb0:Lt)

               ak(0:kxL) = 0.0_dp ! Matrix initialisation TKE
               bk(0:kxL) = dt_inverse
               ck(0:kxL) = 0.0_dp
               dk(0:kxL) = dt_inverse * turbulent_kinetic_energy_wu_old(Lb0:Lt)

               if (tur_time_int_factor > 0) then
                  call apply_horizontal_coupling(turbulent_kinetic_energy_wu_old, turbulent_kinetic_energy_ws)
               end if

               vicu = viskin + 0.5_dp * (vicwwu(Lb0) + vicwwu(Lb)) * sigtkei

               ! Calculate turkin source from wave dissipation: preparation
               if (jawave /= NO_WAVES) then
                  if (jawaveStokes > NO_STOKES_DRIFT .and. .not. flow_without_waves) then ! Ustokes correction at bed
                     adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
                  end if

                  if (jawave /= NO_WAVES .and. jawavebreakerturbulence > WAVE_BREAKER_TURB_OFF) then
                     k1 = ln(1, LL)
                     k2 = ln(2, LL)
                     ac1 = acl(LL)
                     ac2 = 1.0_dp - ac1
                     hrmsLL = min(max(ac1 * hwav(k1) + ac2 * hwav(k2), 1.0e-2_dp), gammax * hu(LL))
                     if (hrmsLL > 0.0) then
                        call wave_fillsurdis(k1, dis1)
                        call wave_fillsurdis(k2, dis2)
                        surdisLL = ac1 * dis1 + ac2 * dis2
                        if (surdisLL < 1.0e-2_dp) then
                           surdisLL = 0.0_dp
                        end if
                        rhoLL = rhomean
                        pkwmag = fbreak * 2.0_dp * surdisLL / (rhoLL * fwavpendep * hrmsLL)
                        ! tke dirichlet boundary condition at surface
                        turbulent_kinetic_energy_at_top = turbulent_kinetic_energy_at_top + (pkwmag * vonkar * fwavpendep * hrmsLL / (30.0_dp * cde))**(2.0_dp / 3.0_dp)
                     else
                        pkwmag = 0.0_dp
                     end if
                     pkwav = 0.0_dp
                     wdep = hu(LL) - fwavpendep * hrmsLL
                     whit = 0
                  end if
               end if

               do L = Lb, Lt - 1 ! Loop over layer interfaces. Doesn't work for kmx==1
                  Lu = L + 1

                  vicd = vicu
                  vicu = viskin + 0.5_dp * (vicwwu(L) + vicwwu(Lu)) * sigtkei

                  k = L - Lb + 1
                  ku = k + 1

                  dzdz1 = dzw(k) * dzu(k)
                  difd = vicd / dzdz1

                  dzdz2 = dzw(k) * dzu(ku)
                  difu = vicu / dzdz2

                  ak(k) = ak(k) - difd * THETA_KEPS_DIFFUSION
                  bk(k) = bk(k) + (difd + difu) * THETA_KEPS_DIFFUSION
                  ck(k) = ck(k) - difu * THETA_KEPS_DIFFUSION
                  if (THETA_KEPS_DIFFUSION /= 1.0_dp) then
                     dk(k) = dk(k) - difu * (turbulent_kinetic_energy_wu_old(L) - turbulent_kinetic_energy_wu_old(Lu)) * theta_complement &
                             + difd * (turbulent_kinetic_energy_wu_old(L - 1) - turbulent_kinetic_energy_wu_old(L)) * theta_complement
                  end if

                  !c Source and sink terms                                                                           k turkin
                  if (use_density()) then
                     k1 = ln(1, L)
                     k2 = ln(2, L)

                     ! Determine Brunt-Vaisala frequency at flowlinks. N.B., bruva = N**2 / sigrho.
                     if (comparereal(drhodz(k1), 0.0_dp) == 0) then
                        bruva(k) = drhodz(k2) * brunt_vaisala_coefficient
                     else if (comparereal(drhodz(k2), 0.0_dp) == 0) then
                        bruva(k) = drhodz(k1) * brunt_vaisala_coefficient
                     else ! Weighted average
                        bruva(k) = (acl(LL) * drhodz(k1) + (1.0_dp - acl(LL)) * drhodz(k2)) * brunt_vaisala_coefficient
                     end if
                     buoflu(k) = max(vicwwu(L), vicwminb) * bruva(k)

                     !c Production, dissipation, and buoyancy term in TKE equation;
                     !c dissipation and positive buoyancy are split by Newton linearization:
                     if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then
                        if (bruva(k) > 0.0_dp) then
                           dk(k) = dk(k) + buoflu(k)
                           bk(k) = bk(k) + 2.0_dp * buoflu(k) / turbulent_kinetic_energy_wu_old(L)
                           ! EdG: make buoyance term in matrix safer
                           !  bk(k) = bk(k) + 2.0_dp*buoflu(k) / max(turbulent_kinetic_energy_wu_old(L), 1d-20)
                        elseif (bruva(k) < 0.0_dp) then
                           dk(k) = dk(k) - buoflu(k)
                        end if
                     else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then
                        if (bruva(k) > 0.0_dp) then
                           bk(k) = bk(k) + buoflu(k) / turbulent_kinetic_energy_wu_old(L)
                        else if (bruva(k) < 0.0_dp) then
                           dk(k) = dk(k) - buoflu(k)
                        end if
                     end if
                  end if

                  ! Production, dissipation, and buoyancy term in TKE equation;
                  ! dissipation and positive buoyancy are split by Newton linearization;
                  ! buoyancy only for unstable stratification;
                  ! notice: application of TKE at new time level:
                  ! Addition of production and of dissipation to matrix ;
                  ! observe implicit treatment by Newton linearization.

                  if (jawave /= NO_WAVES .and. jawaveStokes >= STOKES_DRIFT_2NDORDER_VISC .and. .not. flow_without_waves) then ! vertical shear based on eulerian velocity field, see turclo,note JvK, Ardhuin 2006
                     dijdij(k) = ((u1(Lu) - ustokes(Lu) - u1(L) + ustokes(L))**2 + (v(Lu) - vstokes(Lu) - v(L) + vstokes(L))**2) / dzw(k)**2
                  else
                     dijdij(k) = ((u1(Lu) - u1(L))**2 + (v(Lu) - v(L))**2) / dzw(k)**2
                  end if

                  if (jarichardsononoutput > 0) then
                     rich(L) = sigrho * bruva(k) / max(1.0e-8_dp, dijdij(k)) ! sigrho because bruva premultiplied by 1/sigrho
                  end if

                  sourtu = max(vicwwu(L), vicwminb) * dijdij(k)

                  if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then
                     sinktu = turbulent_dissipation_wu_old(L) / turbulent_kinetic_energy_wu_old(L)
                     bk(k) = bk(k) + sinktu * 2.0_dp
                     dk(k) = dk(k) + sinktu * turbulent_kinetic_energy_wu_old(L) + sourtu ! m2/s3
                  else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then
                     sinktu = 1.0_dp / turbulent_dissipation_wu_old(L)
                     bk(k) = bk(k) + sinktu
                     dk(k) = dk(k) + sourtu
                  end if

               end do ! Lb, Lt-1

               if (jawave /= NO_WAVES .and. jawavebreakerturbulence > WAVE_BREAKER_TURB_OFF) then
                  ! check if first layer is thicker than fwavpendep*wave height
                  ! Then use JvK solution
                  if (hu(LL) - hu(Lt - 1) >= fwavpendep * hrmsLL) then
                     pkwav(kxL - 1) = 0.5_dp * pkwmag * fwavpendep * hrmsLL / (hu(LL) - hu(Lt - 1))
                     dk(kxL - 1) = dk(kxL - 1) + pkwav(kxL - 1) ! m2/s3
                  else
                     ! distribute over layers
                     do L = Lt - 1, Lb, -1
                        if (hu(L + 1) < wdep) then
                           exit
                        end if
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
               ak(kxL) = 0.0_dp
               bk(kxL) = 1.0_dp
               ck(kxL) = 0.0_dp
               dk(kxL) = turbulent_kinetic_energy_at_top
               ! TKE at the bed:
               ak(0) = 0.0_dp
               bk(0) = 1.0_dp
               ck(0) = 0.0_dp
               dk(0) = turbulent_kinetic_energy_at_bottom

               if (javau > 0 .or. turbulent_advection_scheme > 0) then
                  call calculate_womegu(womegu, LL)

                  ac1 = acL(LL)
                  ac2 = 1.0_dp - ac1
                  n1 = ln(1, LL)
                  n2 = ln(2, LL)

                  if (turbulent_advection_scheme >= 3) then ! Advection of turkin, vertical implicit, horizontal explicit

                     arLL = ac1 * a1(n1) + ac2 * a1(n2)
                     do L = Lb, Lt - 1
                        k = L - Lb + 1
                        omegu = 0.5_dp * womegu(k)
                        if (k > 1) then
                           omegu = omegu + 0.5_dp * womegu(k - 1) ! Omega at U-point in between layer interfaces
                        end if
                        if (omegu > 0.0_dp) then ! omegu(k) lies below interface(k)
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

                        if (turbulent_advection_scheme == 3) then ! turkin
                           if (q1(L) + q1(L + 1) > 0) then
                              kup = ln(1, L)
                              arLL = a1(n1)
                           else
                              kup = ln(2, L)
                              arLL = a1(n2)
                           end if
                           volki = 1.0_dp / (dzw(k) * arLL)
                           dk(k) = dk(k) + sum_q_times_turbulent_kinetic_energy_ws(kup) * volki
                           bk(k) = bk(k) + sum_inflowing_discharges_ws(kup) * volki
                        else if (turbulent_advection_scheme == 4) then ! turkin
                           k1 = ln(1, L)
                           k2 = ln(2, L)
                           volki = (ac1 * (vol1(k1) + vol1(k1 + 1)) + ac2 * (vol1(k2) + vol1(k2 + 1))) * 0.5_dp
                           volki = 1.0_dp / volki
                           dk(k) = dk(k) + (ac1 * sum_q_times_turbulent_kinetic_energy_ws(k1) + ac2 * sum_q_times_turbulent_kinetic_energy_ws(k2)) * volki
                           bk(k) = bk(k) + (ac1 * sum_inflowing_discharges_ws(k1) + ac2 * sum_inflowing_discharges_ws(k2)) * volki
                        end if
                     end do
                  end if
               end if

               if (javeg > 0) then ! in turbulence model
                  dke(1:Lt - Lb + 1) = 0.0_dp
                  k1 = ln(1, LL)
                  k2 = ln(2, LL)
                  rnv = 0.5_dp * (rnveg(ln(1, LL)) + rnveg(ln(2, LL)))
                  if (rnv > 0.0_dp) then ! if plants are here
                     do L = Lb, Lt
                        um = sqrt(u1(L) * u1(L) + v(L) * v(L)) ! umod (m2/s2)
                        if (um > 0.0_dp) then ! and if there is flow,
                           k = L - Lb + 1
                           k1 = ln(1, L)
                           k2 = ln(2, L)
                           rnv = 0.5_dp * (rnveg(k1) + rnveg(k2))
                           if (rnv > 0) then ! if in this layer
                              if (diaveg(k1) > 0 .and. diaveg(k2) > 0) then
                                 diav = 0.5_dp * (diaveg(k1) + diaveg(k2))
                              else
                                 diav = max(diaveg(k1), diaveg(k2))
                              end if
                              if (jaCdvegsp == 1) then
                                 if (Cdvegsp(k1) > 0 .and. Cdvegsp(k2) > 0) then
                                    Cdveg = 0.5_dp * (Cdvegsp(k1) + Cdvegsp(k2))
                                 else
                                    Cdveg = max(Cdvegsp(k1), Cdvegsp(k2))
                                 end if
                              end if
                              vk = 0.5_dp * Cdveg * rnv * diav * um ! (1/s)
                              advi(L) = advi(L) + vk ! add to diagonal of u1
                              wk = vk * um * um ! work done by this layer m2/s3
                              ap1 = 1.0 - diav * diav * rnv * pi * 0.25 ! Free area
                              xlveg = Clveg * sqrt(ap1 / rnv) ! typical length between plants
                              tauinv = c2e * sqrt(cmukep) * (wk / xlveg**2)**r3
                              teps = 0.5_dp * (turbulent_dissipation_wu_old(L) + turbulent_dissipation_wu_old(L))
                              tkin = 0.5_dp * (turbulent_kinetic_energy_wu_old(L) + turbulent_kinetic_energy_wu_old(L))
                              if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then
                                 tauinf = c2e * teps / tkin !
                              else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then
                                 tauinf = c2e / teps
                              end if
                              if (tauinf > tauinv) then ! turb damping not governed by plants => free flow damping only
                                 tauinv = 0.0_dp ! tauinv = max(tauinv, tauinf)
                              end if
                              if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then
                                 wke = wk * tauinv
                              else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then
                                 wke = wk * (1.0_dp - turbulent_dissipation_wu(L) * tauinv) * turbulent_dissipation_wu(L) / turbulent_kinetic_energy_wu(L)
                              end if
                              if (L < Lt) then
                                 alf = 0.5_dp * dzu(k) / dzw(k)
                                 dk(k) = dk(k) + alf * wk ! half is added to top interface to k
                                 dke(k) = dke(k) + alf * wke !                                to eps
                              end if
                              if (L > Lb) then
                                 alf = 0.5_dp * dzu(k) / dzw(k - 1)
                                 dk(k - 1) = dk(k - 1) + alf * wk ! other half added to bed interface to k
                                 dke(k - 1) = dke(k - 1) + alf * wke !                                   to eps
                              end if
                           end if
                        end if
                     end do
                  end if
               end if

               call tridag(ak, bk, ck, dk, ek, turbulent_kinetic_energy_wu(Lb0:Lt), kxL + 1) ! solve k
               turbulent_kinetic_energy_wu(Lb0:Lt) = max(tke_min, turbulent_kinetic_energy_wu(Lb0:Lt))
               do L = Lt + 1, Lb + kmxL(LL) - 1 ! copy to surface for z-layers
                  turbulent_kinetic_energy_wu(L) = turbulent_kinetic_energy_wu(Lt)
               end do

               !_____________________________________________________________________________________!

               ak(0:kxL) = 0.0_dp ! Matrix initialization eps, tau
               bk(0:kxL) = dt_inverse
               ck(0:kxL) = 0.0_dp
               dk(0:kxL) = dt_inverse * turbulent_dissipation_wu_old(Lb0:Lt)
               ! Vertical diffusion; Neumann condition on surface;
               ! Dirichlet condition on bed ; teta method:

               if (tur_time_int_factor > 0) then
                  call apply_horizontal_coupling(turbulent_dissipation_wu_old, turbulent_dissipation_ws)
               end if

               vicu = viskin + 0.5_dp * (vicwwu(Lb0) + vicwwu(Lb)) * sigepsi

               do L = Lb, Lt - 1
                  Lu = L + 1

                  vicd = vicu
                  vicu = viskin + 0.5_dp * (vicwwu(L) + vicwwu(Lu)) * sigepsi

                  k = L - Lb + 1
                  ku = k + 1

                  dzdz1 = dzw(k) * dzu(k)
                  difd = vicd / dzdz1

                  dzdz2 = dzw(k) * dzu(ku)
                  difu = vicu / dzdz2

                  ak(k) = ak(k) - difd * THETA_KEPS_DIFFUSION
                  bk(k) = bk(k) + (difd + difu) * THETA_KEPS_DIFFUSION
                  ck(k) = ck(k) - difu * THETA_KEPS_DIFFUSION
                  if (THETA_KEPS_DIFFUSION /= 1.0_dp) then
                     dk(k) = dk(k) - difu * (turbulent_dissipation_wu_old(L) - turbulent_dissipation_wu_old(Lu)) * theta_complement &
                             + difd * (turbulent_dissipation_wu_old(L - 1) - turbulent_dissipation_wu_old(L)) * theta_complement
                  end if

                  if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then

                     !c Source and sink terms                                                                epsilon
                     if (bruva(k) > 0.0_dp) then ! stable stratification
                        dk(k) = dk(k) - cmukep * c3e_stable * bruva(k) * turbulent_kinetic_energy_wu(L)
                     elseif (bruva(k) < 0.0_dp) then ! unstable stratification
                        dk(k) = dk(k) - cmukep * c3e_unstable * bruva(k) * turbulent_kinetic_energy_wu(L)
                     end if

                     sourtu = c1e * cmukep * turbulent_kinetic_energy_wu_old(L) * dijdij(k)
                     ! Add wave dissipation production term
                     if (jawave /= NO_WAVES .and. jawavebreakerturbulence > WAVE_BREAKER_TURB_OFF) then
                        sourtu = sourtu + pkwav(k) * c1e * turbulent_dissipation_wu_old(L) / max(turbulent_kinetic_energy_wu_old(L), 1.0e-7_dp)
                     end if

                     sinktu = c2e * turbulent_dissipation_wu_old(L) / turbulent_kinetic_energy_wu(L)

                     !c Addition of production and of dissipation to matrix ;                               epsilon
                     !c observe implicit treatment by Newton linearization.

                     bk(k) = bk(k) + sinktu * 2.0_dp
                     dk(k) = dk(k) + sinktu * turbulent_dissipation_wu_old(L) + sourtu

                  else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then

                     if (bruva(k) < 0.0_dp) then ! instable
                        bk(k) = bk(k) + c3t_unstable * bruva(k) * turbulent_dissipation_wu_old(L)
                     else if (bruva(k) > 0.0_dp) then ! stable
                        bk(k) = bk(k) + c3t_stable * bruva(k) * turbulent_dissipation_wu_old(L)
                     end if

                     bk(k) = bk(k) - c1t * dijdij(k) * turbulent_dissipation_wu_old(L)
                     dk(k) = dk(k) - c2t

                     gradd = 0.5_dp * (turbulent_kinetic_energy_wu_old(L - 1) + turbulent_kinetic_energy_wu_old(L)) * (turbulent_dissipation_wu_old(L) - turbulent_dissipation_wu_old(L - 1)) / dzu(k) ! The D_tt-term:
                     gradu = 0.5_dp * (turbulent_kinetic_energy_wu_old(Lu) + turbulent_kinetic_energy_wu_old(L)) * (turbulent_dissipation_wu_old(Lu) - turbulent_dissipation_wu_old(L)) / dzu(ku)
                     gradt = gradd + gradu

                     gradd = 0.5_dp * (turbulent_dissipation_wu_old(L - 1) + turbulent_dissipation_wu_old(L)) * (turbulent_kinetic_energy_wu_old(L) - turbulent_kinetic_energy_wu_old(L - 1)) / dzu(k) ! The D_kt-term:
                     gradu = 0.5_dp * (turbulent_dissipation_wu_old(Lu) + turbulent_dissipation_wu_old(L)) * (turbulent_kinetic_energy_wu_old(Lu) - turbulent_kinetic_energy_wu_old(L)) / dzu(ku)
                     gradk = gradd + gradu
                     grad = gradk - gradt ! D_kt - D_tt

                     grad = -grad * sigepsi * cmukep ! This is positive advection, dc/dt + wdc/dz
                     grad = grad / dzw(k) ! dzw is receiving volume
                     if (grad > 0.0_dp) then
                        bk(k) = bk(k) + grad
                        ak(k) = ak(k) - grad
                     else if (grad < 0.0_dp) then
                        bk(k) = bk(k) - grad
                        ck(k) = ck(k) + grad
                     end if

                  end if

               end do

               if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then ! Boundary conditions

                  ak(kxL) = -1.0_dp ! Flux at the free surface:
                  bk(kxL) = 1.0_dp
                  ck(kxL) = 0.0_dp
                  dk(kxL) = 4.0_dp * abs(ustw(LL))**3 / (vonkar * dzu(Lt - Lb + 1))
                  if (jawave /= NO_WAVES .and. jawavebreakerturbulence > WAVE_BREAKER_TURB_OFF) then ! wave dissipation at surface, neumann bc, dissipation over fwavpendep*Hrms
                     dk(kxL) = dk(kxL) + dzu(Lt - Lb + 1) * pkwmag / (fwavpendep * hrmsLL)
                  end if

                  ak(0) = 0.0_dp ! at the bed:
                  bk(0) = 1.0_dp
                  ck(0) = -1.0_dp
                  if (ustb(LL) > 0 .and. kxL > 1) then
                     dk(0) = dzu(1) * abs(ustb(LL))**3 / (vonkar * hdzb * hdzb)
                  else
                     dk(0) = 0.0_dp
                  end if

               else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then ! Boundary conditions

                  ak(kxL) = 0.0_dp ! at the free surface:
                  bk(kxL) = 1.0_dp
                  ck(kxL) = 0.0_dp
                  dk(kxL) = 0.0_dp

                  ak(0) = 0.0_dp ! at the bed:
                  bk(0) = 1.0_dp
                  ck(0) = 0.0_dp
                  if (ustb(LL) > 0) then
                     dk(0) = vonkar * c9of1 * z00 / (max(ustb(LL), EPS6) * 0.3_dp)
                  else
                     dk(0) = 0.0_dp
                  end if

               end if

               if (turbulent_advection_scheme >= 3) then ! Advection of tureps, vertical implicit, horizontal explicit
                  do L = Lb, Lt - 1
                     k = L - Lb + 1
                     omegu = 0.5_dp * womegu(k)
                     if (k > 1) then
                        omegu = omegu + 0.5_dp * womegu(k - 1) ! Omega at U-point in between layer interfaces
                     end if
                     if (omegu > 0.0_dp) then
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

                     if (turbulent_advection_scheme == 3) then ! tureps
                        if (q1(L) + q1(L + 1) > 0) then
                           kup = ln(1, L)
                           arLL = a1(n1)
                        else
                           kup = ln(2, L)
                           arLL = a1(n2)
                        end if
                        volki = 1.0_dp / (dzw(k) * arLL)
                        dk(k) = dk(k) + sum_q_times_turbulent_dissipation_ws(kup) * volki
                        bk(k) = bk(k) + sum_inflowing_discharges_ws(kup) * volki
                     else if (turbulent_advection_scheme == 4) then ! tureps
                        k1 = ln(1, L)
                        k2 = ln(2, L)
                        volki = (ac1 * (vol1(k1) + vol1(k1 + 1)) + ac2 * (vol1(k2) + vol1(k2 + 1))) * 0.5_dp
                        volki = 1.0_dp / volki
                        dk(k) = dk(k) + (ac1 * sum_q_times_turbulent_dissipation_ws(k1) + ac2 * sum_q_times_turbulent_dissipation_ws(k2)) * volki
                        bk(k) = bk(k) + (ac1 * sum_inflowing_discharges_ws(k1) + ac2 * sum_inflowing_discharges_ws(k2)) * volki
                     end if
                  end do
               end if

               if (javeg > 0) then ! in turbulence model
                  do L = Lb, Lt - 1
                     k = L - Lb + 1
                     dk(k) = dk(k) + dke(k)
                  end do
               end if

               call tridag(ak, bk, ck, dk, ek, turbulent_dissipation_wu(Lb0:Lt), kxL + 1) ! solve eps
               turbulent_dissipation_wu(Lb0:Lt) = max(eps_min, turbulent_dissipation_wu(Lb0:Lt))
               do L = Lt + 1, Lb + kmxL(LL) - 1 ! copy to surface for z-layers
                  turbulent_dissipation_wu(L) = turbulent_dissipation_wu(Lt)
               end do

               if ((t_spinup_turb_log_prof > 0.0_dp .and. Time1 < tstart_tlfsmo_user + t_spinup_turb_log_prof) .or. &
                   (jaLogprofkepsbndin == 1 .and. LL > lnxi .and. u1(LL) >= 0.0_dp) .or. &
                   (jaLogprofkepsbndin == 2 .and. LL > lnxi)) then

                  alfaT = dmiss
                  if (t_spinup_turb_log_prof > 0.0_dp) then
                     alfaT = (Time1 - tstart_tlfsmo_user) / t_spinup_turb_log_prof
                  end if
                  if (LL > lnxi) then
                     alfaT = 0.0_dp
                  end if

                  if (alfaT /= dmiss) then
                     do L = Lb, Lt - 1 ! TKE and epsilon at layer interfaces:
                        zint = hu(L) / hu(LL)
                        z1 = 1.0_dp - zint
                        k1 = ln(1, L)
                        k2 = ln(1, L)
                        tke = turbulent_kinetic_energy_at_bottom * z1 + turbulent_kinetic_energy_at_top * zint
                        zz = hu(L) * (1.0_dp - hu(L) / hu(LL)) ! parabolic visc
                        vicwww = zz * max(0.001_dp, ustb(LL)) * vonkar
                        eps = cmukep * tke * tke / vicwww
                        turbulent_kinetic_energy_wu(L) = tke * (1.0_dp - alfaT) + alfaT * turbulent_kinetic_energy_wu(L)
                        turbulent_dissipation_wu(L) = eps * (1.0_dp - alfaT) + alfaT * turbulent_dissipation_wu(L)
                     end do
                     epsbot = turbulent_dissipation_wu(Lb) + dzu(1) * abs(ustb(LL))**3 / (vonkar * hdzb * hdzb)
                     epssur = turbulent_dissipation_wu(Lt - 1) - 4.0_dp * abs(ustw(LL))**3 / (vonkar * dzu(Lt - Lb + 1))
                     if (jawave /= NO_WAVES .and. jawavebreakerturbulence > WAVE_BREAKER_TURB_OFF) then
                        epssur = epssur - dzu(Lt - Lb + 1) * pkwmag / (hrmsLL * fwavpendep)
                     end if
                     epsbot = max(epsbot, eps_min)
                     epssur = max(epssur, eps_min)
                     tke = max(tke_min, turbulent_kinetic_energy_at_top)
                     turbulent_kinetic_energy_wu(Lt) = tke * (1.0_dp - alfaT) + alfaT * turbulent_kinetic_energy_wu(Lt)
                     eps = epssur / (hu(Lt) - hu(Lt - 1))
                     turbulent_dissipation_wu(Lt) = eps * (1.0_dp - alfaT) + alfaT * turbulent_dissipation_wu(Lt)
                     tke = max(tke_min, turbulent_kinetic_energy_at_bottom)
                     turbulent_kinetic_energy_wu(Lb - 1) = tke * (1.0_dp - alfaT) + alfaT * turbulent_kinetic_energy_wu(Lb - 1)
                     eps = epsbot / (hu(Lb) - hu(Lb - 1))
                     turbulent_dissipation_wu(Lb - 1) = eps * (1.0_dp - alfaT) + alfaT * turbulent_dissipation_wu(Lb - 1)
                  end if
               end if

               if (jamodelspecific == 1) then
                  call update_turkin_modelspecific(LL) ! will update turbulent_kinetic_energy_wu and turbulent_dissipation_wu for all layers of flow link LL.
               end if

               vicwmax = 0.1_dp * hu(LL) ! 0.009UH, Elder, uavmax=
               if (iturbulencemodel == TURBULENCE_MODEL_KEPS_FLOW_LINKS) then
                  vicwwu(Lb0:Lt) = min(vicwmax, cmukep * turbulent_kinetic_energy_wu(Lb0:Lt) * turbulent_kinetic_energy_wu(Lb0:Lt) / turbulent_dissipation_wu(Lb0:Lt))
               else if (iturbulencemodel == TURBULENCE_MODEL_KTAU) then
                  vicwwu(Lb0:Lt) = min(vicwmax, cmukep * turbulent_kinetic_energy_wu(Lb0:Lt) * turbulent_dissipation_wu(Lb0:Lt))
               end if

               if (jawave == NO_WAVES .or. jawavebreakerturbulence == WAVE_BREAKER_TURB_OFF) then
                  vicwwu(Lt) = min(vicwwu(Lt), vicwwu(Lt - 1) * Eddyviscositysurfacmax)
               end if
               vicwwu(Lb0) = min(vicwwu(Lb0), vicwwu(Lb) * Eddyviscositybedfacmax)

               call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)

            else ! dry

               turbulent_dissipation_wu(Lb0:Lb + kmxL(LL) - 1) = eps_min
               turbulent_kinetic_energy_wu(Lb0:Lb + kmxL(LL) - 1) = tke_min

            end if ! if (hu(L) > 0) then
         end do ! links loop

         turbulent_kinetic_energy_wu_old = turbulent_kinetic_energy_wu
         turbulent_dissipation_wu_old = turbulent_dissipation_wu

      case (TURBULENCE_MODEL_KEPS_CELL_CENTER)

         call calculate_drhodz(zws, drhodz)

         if (turbulent_advection_scheme > 0) then ! transport switched on: prepare horizontal advection k and eps

            if (source_sinks%num_total > 0 .and. source_sinks%add_k_to_turkin) then
               call doaddksources()
            end if

            if (jampi == 1) then
               call update_ghosts(ITYPE_Sall3D, 1, Ndkx, turbulent_kinetic_energy_ws, ierror)
               call update_ghosts(ITYPE_Sall3D, 1, Ndkx, turbulent_dissipation_ws, ierror)
            end if

            sum_q_times_turbulent_kinetic_energy_ws = 0.0_dp
            sum_q_times_turbulent_dissipation_ws = 0.0_dp
            sum_inflowing_discharges_ws = 0.0_dp

            do LL = 1, lnx
               call getLbotLtop(LL, Lb, Lt)
               do L = Lb, Lt - 1
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  discharge_at_layer_interface = 0.5_dp * (q1(L) + q1(L + 1))

                  if (discharge_at_layer_interface > 0) then ! set upwind center values on links

                     sum_q_times_turbulent_kinetic_energy_ws(k2) = &
                        sum_q_times_turbulent_kinetic_energy_ws(k2) &
                        + discharge_at_layer_interface * turbulent_kinetic_energy_ws(k1)
                     
                     sum_q_times_turbulent_dissipation_ws(k2) = &
                        sum_q_times_turbulent_dissipation_ws(k2) &
                        + discharge_at_layer_interface * turbulent_dissipation_ws(k1)
                     
                     sum_inflowing_discharges_ws(k2) = sum_inflowing_discharges_ws(k2) + discharge_at_layer_interface
                  
                  else if (discharge_at_layer_interface < 0) then

                     sum_q_times_turbulent_kinetic_energy_ws(k1) = &
                        sum_q_times_turbulent_kinetic_energy_ws(k1) &
                        - discharge_at_layer_interface * turbulent_kinetic_energy_ws(k2)
                     
                     sum_q_times_turbulent_dissipation_ws(k1) = &
                        sum_q_times_turbulent_dissipation_ws(k1) &
                        - discharge_at_layer_interface * turbulent_dissipation_ws(k2)
                     
                     sum_inflowing_discharges_ws(k1) = sum_inflowing_discharges_ws(k1) - discharge_at_layer_interface
                  
                  end if

               end do
            end do
         end if

         theta_complement = 1.0_dp - THETA_KEPS_DIFFUSION
         dt_inverse = 1.0_dp / dtprev ! turbulence transport in current velocity field => do not use new timestep but previous step

         do LL = 1, lnx ! all this at velocity points
            ustb(LL) = 0.0_dp
            if (hu(LL) > 0.0_dp) then
               Lb = Lbot(LL)
               call get_ustb_cfuhi(LL, Lb, ustb(LL), cfuhi(LL), hdzb, z00, cfuhi3D) ! K-EPS, K-TAU z00 wave-enhanced roughness for jawave>0

               if (hu(LL) < trsh_u1Lb) then
                  advi(Lb:Lt) = advi(Lb:Lt) + cfuhi3D / real(Lt - Lb + 1, kind=dp)
               else
                  advi(Lb) = advi(Lb) + cfuhi3D
               end if
            end if
         end do
         call linkstocenters2Donly(ustbs, ustb)
         call linkstocenters2Donly(ustws, ustw)

         do kk = 1, ndx

            kt = ktop(kk) ! surface layer index = surface interface index
            kb = kbot(kk) ! bed layer index
            if (kt < kb) cycle
            kb0 = kb - 1 ! bed interface index

            turbulent_kinetic_energy_ws_old(kb0:kt) = turbulent_kinetic_energy_ws(kb0:kt)
            turbulent_dissipation_ws_old(kb0:kt) = turbulent_dissipation_ws(kb0:kt)

            if (hs(kk) > epshu) then
               kxL = kt - kb + 1 ! nr of layers

               do L = kb, kt ! layer thickness at layer center (org: kb + 1)
                  k = L - kb + 1
                  dzs(k) = max(eps4, zws(L) - zws(L - 1))
               end do

               do L = kb, kt - 1 ! layer thickness at layer interface
                  k = L - kb + 1
                  dzws(k) = 0.5_dp * (dzs(k) + dzs(k + 1))
               end do

               turbulent_kinetic_energy_at_bottom = ustbs(kk)**2 / sqrt(cmukep) ! this has stokes incorporated when jawave>0
               turbulent_kinetic_energy_at_top = ustws(kk)**2 / sqrt(cmukep) ! only wind+ship contribution

               ak(0:kxL) = 0.0_dp ! Matrix initialisation TKE
               bk(0:kxL) = dt_inverse
               ck(0:kxL) = 0.0_dp
               dk(0:kxL) = dt_inverse * turbulent_kinetic_energy_ws_old(kb0:kt)

               vicu = viskin + 0.5_dp * (vicwws(kb0) + vicwws(kb)) * sigtkei !

               do L = kb, kt - 1 ! Loop over layer interfaces. Doesn't work for kmx==1
                  Lu = L + 1

                  vicd = vicu
                  vicu = viskin + 0.5_dp * (vicwws(L) + vicwws(Lu)) * sigtkei

                  k = L - kb + 1
                  ku = k + 1

                  dzdz1 = dzws(k) * dzs(k)
                  difd = vicd / dzdz1

                  dzdz2 = dzws(k) * dzs(ku)
                  difu = vicu / dzdz2

                  ak(k) = ak(k) - difd * THETA_KEPS_DIFFUSION
                  bk(k) = bk(k) + (difd + difu) * THETA_KEPS_DIFFUSION
                  ck(k) = ck(k) - difu * THETA_KEPS_DIFFUSION
                  if (THETA_KEPS_DIFFUSION /= 1.0_dp) then
                     dk(k) = dk(k) - difu * (turbulent_kinetic_energy_ws_old(L) - turbulent_kinetic_energy_ws_old(Lu)) * theta_complement &
                             + difd * (turbulent_kinetic_energy_ws_old(L - 1) - turbulent_kinetic_energy_ws_old(L)) * theta_complement
                  end if

                  !c Source and sink terms                                                                           k turkin
                  if (use_density()) then

                     bruva(k) = brunt_vaisala_coefficient * drhodz(L) ! N.B., bruva = N**2 / sigrho
                     buoflu(k) = max(vicwws(L), vicwminb) * bruva(k)

                     !c Production, dissipation, and buoyancy term in TKE equation;
                     !c dissipation and positive buoyancy are split by Newton linearization:
                     if (bruva(k) > 0.0_dp) then
                        dk(k) = dk(k) + buoflu(k)
                        bk(k) = bk(k) + 2.0_dp * buoflu(k) / turbulent_kinetic_energy_ws_old(L)
                        ! EdG: make buoyance term in matrix safer
                        !  bk(k) = bk(k) + 2.0_dp*buoflu(k) / max(turbulent_kinetic_energy_ws_old(L), 1d-20)
                     elseif (bruva(k) < 0.0_dp) then
                        dk(k) = dk(k) - buoflu(k)
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

                  dijdij(k) = ((ucx(Lu) - ucx(L))**2 + (ucy(Lu) - ucy(L))**2) / dzws(k)**2

                  if (jarichardsononoutput > 0) then
                     richs(L) = sigrho * bruva(k) / max(1.0e-8_dp, dijdij(k)) ! sigrho because bruva premultiplied by 1/sigrho
                  end if

                  sourtu = max(vicwws(L), vicwminb) * dijdij(k)
                  sinktu = turbulent_dissipation_ws_old(L) / turbulent_kinetic_energy_ws_old(L)
                  bk(k) = bk(k) + sinktu * 2.0_dp
                  dk(k) = dk(k) + sinktu * turbulent_kinetic_energy_ws_old(L) + sourtu ! m2/s3

               end do ! kb, kt-1

               !
               ! Boundary conditions, dirichlet:
               ! TKE at free surface
               ak(kxL) = 0.0_dp
               bk(kxL) = 1.0_dp
               ck(kxL) = 0.0_dp
               dk(kxL) = turbulent_kinetic_energy_at_top
               ! TKE at the bed:
               ak(0) = 0.0_dp
               bk(0) = 1.0_dp
               ck(0) = 0.0_dp
               dk(0) = turbulent_kinetic_energy_at_bottom

               if (javau > 0 .or. turbulent_advection_scheme > 0) then
                  do L = kb, kt - 1 ! vertical omega velocity at layer interface u point
                     k = L - kb + 1
                     womegu(k) = qw(L) / a1(kk)
                  end do
                  womegu(kt - kb + 1) = 0.0_dp ! top layer : 0

                  if (turbulent_advection_scheme >= 3) then ! Advection of turkin, vertical implicit, horizontal explicit
                     arLL = a1(kk)
                     do L = kb, kt - 1
                        k = L - kb + 1
                        omegu = 0.5_dp * womegu(k)
                        if (k > 1) then
                           omegu = omegu + 0.5_dp * womegu(k - 1) ! Omega at U-point in between layer interfaces
                        end if
                        if (omegu > 0.0_dp) then ! omegu(k) lies below interface(k)
                           adv = omegu / dzws(k) ! omegu(k) > 0 contributes to k
                           bk(k) = bk(k) + adv
                           ak(k) = ak(k) - adv
                        else
                           if (k > 1) then
                              adv = -omegu / dzws(k - 1)
                              bk(k - 1) = bk(k - 1) + adv
                              ck(k - 1) = ck(k - 1) - adv
                           end if
                        end if

                        volki = 1.0_dp / (dzws(k) * arLL)
                        dk(k) = dk(k) + sum_q_times_turbulent_kinetic_energy_ws(k) * volki
                        bk(k) = bk(k) + sum_inflowing_discharges_ws(k) * volki
                     end do
                  end if
               end if

               call tridag(ak, bk, ck, dk, ek, turbulent_kinetic_energy_ws(kb0:kt), kxL + 1) ! solve k
               turbulent_kinetic_energy_ws(kb0:kt) = max(tke_min, turbulent_kinetic_energy_ws(kb0:kt))
               do L = kt + 1, kb + kmxn(kk) - 1 ! copy to surface for z-layers
                  turbulent_kinetic_energy_ws(L) = turbulent_kinetic_energy_ws(kt)
               end do

               !_____________________________________________________________________________________!

               ak(0:kxL) = 0.0_dp ! Matrix initialization eps, tau, Cellcenters
               bk(0:kxL) = dt_inverse
               ck(0:kxL) = 0.0_dp
               dk(0:kxL) = dt_inverse * turbulent_dissipation_ws_old(kb0:kt)
               ! Vertical diffusion; Neumann condition on surface;
               ! Dirichlet condition on bed ; teta method:

               vicu = viskin + 0.5_dp * (vicwws(kb0) + vicwws(kb)) * sigepsi

               do L = kb, kt - 1
                  Lu = L + 1

                  vicd = vicu
                  vicu = viskin + 0.5_dp * (vicwws(L) + vicwws(Lu)) * sigepsi

                  k = L - kb + 1
                  ku = k + 1

                  dzdz1 = dzws(k) * dzs(k)
                  difd = vicd / dzdz1

                  dzdz2 = dzws(k) * dzs(ku)
                  difu = vicu / dzdz2

                  ak(k) = ak(k) - difd * THETA_KEPS_DIFFUSION
                  bk(k) = bk(k) + (difd + difu) * THETA_KEPS_DIFFUSION
                  ck(k) = ck(k) - difu * THETA_KEPS_DIFFUSION
                  if (THETA_KEPS_DIFFUSION /= 1.0_dp) then
                     dk(k) = dk(k) - difu * (turbulent_dissipation_ws_old(L) - turbulent_dissipation_ws_old(Lu)) * theta_complement &
                             + difd * (turbulent_dissipation_ws_old(L - 1) - turbulent_dissipation_ws_old(L)) * theta_complement
                  end if

                  !c Source and sink terms                                                                epsilon
                  if (bruva(k) < 0.0_dp) then ! instable, increase rhs
                     dk(k) = dk(k) - cmukep * c1e * bruva(k) * turbulent_kinetic_energy_ws(L)
                  end if

                  ! Similar to the k-equation, in the eps-equation the net IWE to TKE
                  ! transfer rate (TKEPRO-TKEDIS) is added to the eps-production term, but
                  ! split for implicit treatment for avoiding negative epsilon.

                  sourtu = c1e * cmukep * turbulent_kinetic_energy_ws_old(L) * dijdij(k)

                  sinktu = c2e * turbulent_dissipation_ws_old(L) / turbulent_kinetic_energy_ws(L) ! yoeri has here : /turbulent_kinetic_energy_wu_old(L)

                  !c Addition of production and of dissipation to matrix ;                               epsilon
                  !c observe implicit treatment by Newton linearization.

                  bk(k) = bk(k) + sinktu * 2.0_dp
                  dk(k) = dk(k) + sinktu * turbulent_dissipation_ws_old(L) + sourtu

               end do

               ak(kxL) = -1.0_dp ! Flux at the free surface:
               bk(kxL) = 1.0_dp
               ck(kxL) = 0.0_dp
               dk(kxL) = 4.0_dp * abs(ustws(kk))**3 / (vonkar * dzs(kt - kb + 1))

               ak(0) = 0.0_dp ! at the bed:
               bk(0) = 1.0_dp
               ck(0) = -1.0_dp
               if (ustbs(kk) > 0 .and. kxL > 1) then
                  hdzbs = 0.5_dp * dzs(1) + c9of1 * z00 ! half bottom layer plus 9z0
                  dk(0) = dzs(1) * abs(ustbs(kk))**3 / (vonkar * hdzbs * hdzbs)
               else
                  dk(0) = 0.0_dp
               end if

               if (turbulent_advection_scheme >= 3) then ! Advection of tureps, vertical implicit, horizontal explicit
                  do L = kb, kt - 1
                     k = L - kb + 1
                     omegu = 0.5_dp * womegu(k)
                     if (k > 1) omegu = omegu + 0.5_dp * womegu(k - 1) ! Omega at U-point in between layer interfaces
                     if (omegu > 0.0_dp) then ! omegu(k) lies below interface(k)
                        adv = omegu / dzws(k) ! omegu(k) > 0 contributes to k
                        bk(k) = bk(k) + adv
                        ak(k) = ak(k) - adv
                     else
                        if (k > 1) then
                           adv = -omegu / dzws(k - 1)
                           bk(k - 1) = bk(k - 1) + adv
                           ck(k - 1) = ck(k - 1) - adv
                        end if
                     end if

                     volki = 1.0_dp / (dzws(k) * arLL)
                     dk(k) = dk(k) + sum_q_times_turbulent_dissipation_ws(k) * volki
                     bk(k) = bk(k) + sum_inflowing_discharges_ws(k) * volki
                  end do
               end if

               call tridag(ak, bk, ck, dk, ek, turbulent_dissipation_ws(kb0:kt), kxL + 1) ! solve eps
               turbulent_dissipation_ws(kb0:kt) = max(eps_min, turbulent_dissipation_ws(kb0:kt))
               do L = kt + 1, kb + kmxn(kk) - 1 ! copy to surface for z-layers
                  turbulent_dissipation_ws(L) = turbulent_dissipation_ws(kt)
               end do

               vicwmax = 0.1_dp * hs(kk) ! 0.009UH, Elder, uavmax=
               vicwws(kb0:kt) = min(vicwmax, cmukep * turbulent_kinetic_energy_ws(kb0:kt) * turbulent_kinetic_energy_ws(kb0:kt) / turbulent_dissipation_ws(kb0:kt))

               vicwws(kt) = min(vicwws(kt), vicwws(kt - 1) * Eddyviscositysurfacmax)
               vicwws(kb0) = min(vicwws(kb0), vicwws(kb) * Eddyviscositybedfacmax)

            else ! dry

               turbulent_dissipation_ws(kb0:kb + kmxn(kk) - 1) = eps_min
               turbulent_kinetic_energy_ws(kb0:kb + kmxn(kk) - 1) = tke_min

            end if ! if (hs(kk) > epshu) then
         end do ! do kk = 1, ndx

         do LL = 1, lnx
            Lt = Ltop(LL) ! surface layer index = surface interface index
            Lb = Lbot(LL) ! bed layer index
            if (Lt < Lb) cycle

            kxL = Lt - Lb + 1 ! nr of layers
            Lb0 = Lb - 1 ! bed interface index
            n1 = ln(1, LL)
            n2 = ln(2, LL)
            ac1 = acL(LL)
            ac2 = 1.0_dp - ac1

            call calculate_womegu(womegu, LL)

            do L = Lb, Lt - 1 ! vertical omega velocity at layer interface u point
               k1 = ln(1, L)
               k2 = ln(2, L)

               vicwwu(L) = ac1 * vicwws(k1) + ac2 * vicwws(k2)
               turbulent_kinetic_energy_wu(L) = ac1 * turbulent_kinetic_energy_ws(k1) + ac2 * turbulent_kinetic_energy_ws(k2) ! for now for postprocessing
               turbulent_dissipation_wu(L) = ac1 * turbulent_dissipation_ws(k1) + ac2 * turbulent_dissipation_ws(k2) ! for now for postprocessing
            end do
            k1 = ln(1, Lt)
            k2 = ln(2, Lt) ! for now for postprocessing
            turbulent_kinetic_energy_wu(Lt) = ac1 * turbulent_kinetic_energy_ws(k1) + ac2 * turbulent_kinetic_energy_ws(k2) ! for now for postprocessing
            turbulent_dissipation_wu(Lt) = ac1 * turbulent_dissipation_ws(k1) + ac2 * turbulent_dissipation_ws(k2) ! for now for postprocessing
            k1 = ln(1, Lb) - 1
            k2 = ln(2, Lb) - 1 ! for now for postprocessing
            turbulent_kinetic_energy_wu(Lb0) = ac1 * turbulent_kinetic_energy_ws(k1) + ac2 * turbulent_kinetic_energy_ws(k2) ! for now for postprocessing
            turbulent_dissipation_wu(Lb0) = ac1 * turbulent_dissipation_ws(k1) + ac2 * turbulent_dissipation_ws(k2) ! for now for postprocessing

            do L = Lb, Lt ! vertical omega velocity at layer interface u point
               dzu(L - Lb + 1) = max(eps4, hu(L) - hu(L - 1))
            end do

            call vertical_profile_u0(dzu, womegu, Lb, Lt, kxL, LL)
         end do

      end select

      if (iturbulencemodel /= TURBULENCE_MODEL_KEPS_CELL_CENTER) then
         call links_to_centers(vicwws, vicwwu)
         if (jarichardsononoutput > 0) then
            call links_to_centers(richs, rich)
         end if
      end if

   contains

      !> Lax-inspired time integration method to couple turbulence quantities horizontally
      !! By using a subroutine inside "update_verticalprofiles", all parameters defined in "update_verticalprofiles" are accessible
      subroutine apply_horizontal_coupling(tur_link, tur_node)
         real(kind=dp), dimension(:) :: tur_link, tur_node
         ! Apply horizontal coupling of turkin/tureps with care:
         ! - Do not try to couple layer k in cell k1 with a layer other than k in cell k2; that may cause creep
         do L = Lb, Lt - 1
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (tur_node(k1) > EPS20 .and. tur_node(k2) > EPS20) then
               if (tur_time_int_method == TURB_LAX_ALL .or. (zws(k1) > zws(k2 - 1) .and. zws(k1 - 1) < zws(k2))) then
                  dk(L - Lb + 1) = dt_inverse * ((1.0_dp - tur_time_int_factor) * tur_link(L) + 0.5_dp * tur_time_int_factor * (tur_node(k1) + tur_node(k2)))
               end if
            end if
         end do
      end subroutine apply_horizontal_coupling

   end subroutine update_verticalprofiles

   !> Calculates vertical density gradient for Brunt-Vaisala frequency
   subroutine calculate_drhodz(zws, drhodz)
      use m_get_kbot_ktop, only: getkbotktop
      use m_flowgeom, only: ndx
      use m_physcoef, only: ag, rhomean
      use m_density_parameters, only: apply_thermobaricity
      use m_density, only: density_at_cell_given_pressure
      use m_turbulence, only: rho
      use precision, only: dp

      real(kind=dp), dimension(:), intent(in) :: zws !< z levels  (m) of interfaces (w-points) at cell centres (s-points)
      real(kind=dp), dimension(:), intent(out) :: drhodz !< Vertical density gradient (in horizontal cell centers)

      real(kind=dp) :: dz, pressure, density_up, density_down
      integer :: k_bot, k_top, cell_index_2d, cell_index_3d

      drhodz(:) = 0.0_dp

      if (.not. apply_thermobaricity) then
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(cell_index_2d, cell_index_3d, k_bot, k_top, dz)
         do cell_index_2d = 1, ndx
            call getkbotktop(cell_index_2d, k_bot, k_top)
            do cell_index_3d = k_bot, k_top - 1
               dz = 0.5_dp * (zws(cell_index_3d + 1) - zws(cell_index_3d - 1)) ! Vertical distance between cell centers
               if (dz > 0.0_dp) then
                  drhodz(cell_index_3d) = (rho(cell_index_3d + 1) - rho(cell_index_3d)) / dz
               end if
            end do
         end do
         !$OMP END PARALLEL DO
      else
         !$OMP PARALLEL DO &
         !$OMP PRIVATE(cell_index_2d, cell_index_3d, k_bot, k_top, dz, pressure, density_up, density_down)
         do cell_index_2d = 1, ndx
            call getkbotktop(cell_index_2d, k_bot, k_top)
            do cell_index_3d = k_bot, k_top - 1
               dz = 0.5_dp * (zws(cell_index_3d + 1) - zws(cell_index_3d - 1)) ! Vertical distance between cell centers
               if (dz > 0.0_dp) then
                  pressure = ag * rhomean * (zws(k_top) - zws(cell_index_3d)) ! At vertical interface
                  density_up = density_at_cell_given_pressure(cell_index_3d + 1, pressure)
                  density_down = density_at_cell_given_pressure(cell_index_3d, pressure)
                  drhodz(cell_index_3d) = (density_up - density_down) / dz
               end if
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end subroutine calculate_drhodz

   !> Set flow node values based on flow link values (2D only)
   subroutine linkstocenters2Donly(vnod, vlin)
      use m_flow
      use m_netw
      use m_flowgeom

      ! Arguments
      real(kind=dp) :: vlin(lnx)
      real(kind=dp) :: vnod(ndx)

      ! Local variables
      integer :: L, k1, k2

      vnod = 0.0_dp

      do L = 1, lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
         vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
      end do
   end subroutine linkstocenters2Donly

   !> add sor (or sink if < 0) to diag and/or rhs to compute tur
   subroutine addsoursink(splitfac, sor, tur, b, d)
      ! Arguments
      real(kind=dp), intent(in) :: splitfac
      real(kind=dp), intent(in) :: sor
      real(kind=dp), intent(in) :: tur
      real(kind=dp), intent(inout) :: b
      real(kind=dp), intent(inout) :: d

      if (sor > 0) then
         d = d + sor
      else if (sor < 0) then
         b = b - sor * (splitfac + 1.0_dp) / tur ! splitfac : 0d0=Patankar, 1d0=Newton, 10d0=Guus
         d = d - sor * splitfac
      end if
   end subroutine addsoursink

   !> Calculate the vertical omega velocity for a given flow link
   pure subroutine calculate_womegu(womegu, flow_link)
      use m_flow, only: lbot, ltop, u1, qw, a1
      use m_flowgeom, only: acL, ln, ndxi

      ! Arguments
      real(kind=dp), dimension(:), intent(inout) :: womegu !< Vertical omega velocity array
      integer, intent(in) :: flow_link !< Flow link to compute womegu for

      ! Local variables
      integer :: l !< Loop index over vertical flow link layers
      integer :: k !< Loop index over vertical layer interfaces

      real(kind=dp) :: ac1 !< Left dx fraction for linked cell 1
      real(kind=dp) :: ac2 !< Left dx fraction for linked cell 2
      integer :: n1 !< Flow cell index for linked cell 1
      integer :: n2 !< Flow cell index for linked cell 2
      integer :: l_bottom !< Bottom flow link layer index
      integer :: l_top !< Top flow link layer index
      integer :: k1 !< Vertical layer interface index for linked cell 1
      integer :: k2 !< Vertical layer interface index for linked cell 2

      ac1 = acL(flow_link)
      ac2 = 1.0_dp - ac1
      n1 = ln(1, flow_link)
      n2 = ln(2, flow_link)
      l_bottom = lbot(flow_link)
      l_top = ltop(flow_link)

      do l = l_bottom, l_top - 1 ! vertical omega velocity at layer interface u point
         k1 = ln(1, l)
         k2 = ln(2, l)
         k = l - l_bottom + 1

         if (n1 > ndxi) then ! open boundaries
            if (u1(flow_link) < 0.0_dp) then
               womegu(k) = qw(k2) / a1(n2)
            else
               womegu(k) = 0.0_dp
            end if
         else
            womegu(k) = (ac1 * qw(k1) + ac2 * qw(k2)) / (ac1 * a1(n1) + ac2 * a1(n2))
         end if
      end do
      womegu(l_top - l_bottom + 1) = 0.0_dp ! top layer : 0

   end subroutine calculate_womegu
end module m_update_verticalprofiles
