!> Regression tests for near-bed suspended transport and morphology time steps.
module test_sed_transport
   use assertions_gtest, only: f90_expect_near, f90_expect_true, f90_expect_false
   use precision, only: dp, fp

   implicit none(type, external)
   private
   public :: test_suscor_integral, test_suscor_weights, test_suscor_reverse, test_suscor_donor
   public :: test_mor_dt_weights, test_mor_dt_reverse, test_mor_dt_zero
   public :: test_bedtr_opposing, test_bedtr_angles, test_bedtr_limits, test_bedtr_cutoffs

   real(dp), parameter :: tolerance = 1.0e-11_dp
   real(fp), parameter :: bed_d50 = 0.0002_fp
   real(fp), parameter :: bed_rhosol = 2650.0_fp, bed_rhowat = 1000.0_fp
   real(fp), parameter :: bed_ws = 0.025_fp, bed_gravity = 9.81_fp

contains

   !$gtest
   !> Integrating 5*(0.02/z)^2 from 0.02 to 0.1 m gives 0.08 kg/m^2.
   subroutine test_suscor_integral() bind(C)
      real(dp) :: correction, thinner_layer_correction
      real(dp), parameter :: upper_concentration = 5.0_dp * (0.02_dp / 0.15_dp)**2

      call evaluate_suscor(0.5_dp, [0.02_dp, 0.02_dp], [5.0_dp, 5.0_dp], &
                           [upper_concentration, upper_concentration], 0.3_dp, 0.8_dp, &
                           [1.0_dp, 1.0_dp], 0.0_dp, correction)
      call evaluate_suscor(0.5_dp, [0.02_dp, 0.02_dp], [5.0_dp, 5.0_dp], &
                           [upper_concentration, upper_concentration], 0.3_dp, 0.8_dp, &
                           [1.0_dp, 1.0_dp], 0.01_dp, thinner_layer_correction)

      call f90_expect_near(0.8_dp + 10.0_dp * correction, 0.24_dp, tolerance, &
                           "Retained flux must be velocity times concentration integral times width")
      call f90_expect_near(thinner_layer_correction, correction, tolerance, &
                           "Changing a layer bottom below the reference height must not rescale its retained flux")
   end subroutine test_suscor_integral

   !$gtest
   !> A quarter/three-quarter interpolation of concentrations 3 and 7 is 6.
   subroutine test_suscor_weights() bind(C)
      real(dp) :: correction
      real(dp), parameter :: ratio = (0.02_dp / 0.15_dp)**2

      call evaluate_suscor(0.25_dp, [0.02_dp, 0.02_dp], [3.0_dp, 7.0_dp], &
                           [3.0_dp * ratio, 7.0_dp * ratio], 0.3_dp, 0.8_dp, &
                           [1.0_dp, 1.0_dp], 0.0_dp, correction)

      call f90_expect_near(0.8_dp + 10.0_dp * correction, 0.288_dp, tolerance, &
                           "The retained flux must use real-valued interpolation weights")
   end subroutine test_suscor_weights

   !$gtest
   !> Reversing an edge and its unequal adjacent profiles must only reverse the flux sign.
   subroutine test_suscor_reverse() bind(C)
      real(dp) :: forward, reverse

      call evaluate_suscor(0.25_dp, [0.01_dp, 0.03_dp], [3.0_dp, 7.0_dp], &
                           [0.04_dp, 0.2_dp], 0.3_dp, 0.8_dp, [1.0_dp, 1.0_dp], 0.0_dp, forward)
      call evaluate_suscor(0.75_dp, [0.01_dp, 0.03_dp], [3.0_dp, 7.0_dp], &
                           [0.04_dp, 0.2_dp], -0.3_dp, -0.8_dp, [1.0_dp, 1.0_dp], 0.0_dp, reverse, .true.)

      call f90_expect_near(reverse, -forward, tolerance, &
                           "Suspended transport correction must be independent of edge orientation")
   end subroutine test_suscor_reverse

   !$gtest
   !> The correction uses its own upwind availability and vanishes without transported flux.
   subroutine test_suscor_donor() bind(C)
      real(dp) :: unlimited, limited, zero_flux
      real(dp), parameter :: upper_concentration = 5.0_dp * (0.02_dp / 0.15_dp)**2

      call evaluate_suscor(0.5_dp, [0.02_dp, 0.02_dp], [5.0_dp, 5.0_dp], &
                           [upper_concentration, upper_concentration], 0.3_dp, 0.8_dp, &
                           [1.0_dp, 1.0_dp], 0.0_dp, unlimited)
      call evaluate_suscor(0.5_dp, [0.02_dp, 0.02_dp], [5.0_dp, 5.0_dp], &
                           [upper_concentration, upper_concentration], 0.3_dp, 0.8_dp, &
                           [1.0_dp, 0.25_dp], 0.0_dp, limited)
      call evaluate_suscor(0.5_dp, [0.02_dp, 0.02_dp], [5.0_dp, 5.0_dp], &
                           [upper_concentration, upper_concentration], 0.3_dp, 0.0_dp, &
                           [1.0_dp, 1.0_dp], 0.0_dp, zero_flux)

      call f90_expect_near(limited, 0.25_dp * unlimited, tolerance, &
                           "The negative correction must use availability from its donating second cell")
      call f90_expect_near(zero_flux, 0.0_dp, tolerance, "No transported flux requires no correction")
   end subroutine test_suscor_donor

   !$gtest
   !> An outgoing volumetric transport of 5 m^3/s permits a 0.2 s bed step.
   subroutine test_mor_dt_weights() bind(C)
      real(dp) :: result
      integer :: limiting_cell

      call evaluate_mor_dt(0.5_dp, [0.0_dp, 1600.0_dp], .false., result, limiting_cell)
      call f90_expect_near(result, 0.2_dp, tolerance, &
                           "Morphology timestep must use the mean of both adjacent transport vectors")
      call f90_expect_near(real(limiting_cell, dp), 1.0_dp, 0.0_dp, &
                           "The first cell is the outgoing-transport limiting cell")

      call evaluate_mor_dt(0.25_dp, [3200.0_dp, 1600.0_dp], .false., result, limiting_cell)
      call f90_expect_near(result, 0.08_dp, tolerance, &
                           "Morphology timestep must retain nonuniform real-valued weights")
   end subroutine test_mor_dt_weights

   !$gtest
   !> Reversing the same geometric edge must not change its outgoing timestep bound.
   subroutine test_mor_dt_reverse() bind(C)
      real(dp) :: forward, reverse
      integer :: forward_cell, reverse_cell

      call evaluate_mor_dt(0.25_dp, [3200.0_dp, 1600.0_dp], .false., forward, forward_cell)
      call evaluate_mor_dt(0.25_dp, [3200.0_dp, 1600.0_dp], .true., reverse, reverse_cell)
      call f90_expect_near(reverse, forward, tolerance, &
                           "Morphology timestep must be independent of edge orientation")
      call f90_expect_near(real(reverse_cell, dp), real(forward_cell, dp), 0.0_dp, &
                           "Reversing an edge must preserve the limiting physical cell")
   end subroutine test_mor_dt_reverse

   !$gtest
   !> A zero sediment flux must leave the hydrodynamic timestep and limiting cell unchanged.
   subroutine test_mor_dt_zero() bind(C)
      real(dp) :: result
      integer :: limiting_cell

      call evaluate_mor_dt(0.5_dp, [0.0_dp, 0.0_dp], .false., result, limiting_cell)
      call f90_expect_near(result, 1.0_dp, tolerance, "No sediment transport requires no timestep reduction")
      call f90_expect_near(real(limiting_cell, dp), 7.0_dp, 0.0_dp, &
                           "No morphology restriction must preserve the prior limiting cell")
   end subroutine test_mor_dt_zero

   !$gtest
   !> Opposing wave/current transport stays bounded at and near an equal contribution ratio.
   subroutine test_bedtr_opposing() bind(C)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

      real(fp), parameter :: ratios(3) = [1.0_fp, 1.0_fp - 1.0e-6_fp, 1.0_fp + 1.0e-6_fp]
      real(fp), parameter :: angles(2) = [180.0_fp, 179.999_fp]
      real(fp) :: flux(6), uon, uoff, vcr, current, capacity, current_load, wave_load
      integer :: i, j

      call evaluate_bedtr(0.0_fp, 180.0_fp, 0.5_fp, .true., flux, uon, uoff, vcr)
      call f90_expect_true(uon > vcr, "Fixture waves must exceed the critical transport velocity")
      do i = 1, size(ratios)
         ! The wave orbital velocity is independent of the imposed current.
         if (i == 1) then
            current = uon
         else
            current = vcr + (uon - vcr) / ratios(i)**(1.0_fp / 3.0_fp)
         end if
         do j = 1, size(angles)
            call evaluate_bedtr(current, angles(j), 0.5_fp, .true., flux, uon, uoff, vcr)
            capacity = bedtr_capacity(current, uon, vcr)
            current_load = sqrt(sum(flux(1:2)**2))
            wave_load = sqrt(sum(flux(3:4)**2))

            call f90_expect_true(all(ieee_is_finite(flux)), "Opposing transport components must remain finite")
            call f90_expect_true(current_load <= capacity + tolerance, &
                                 "Current transport magnitude must not exceed total bedload capacity")
            call f90_expect_true(wave_load <= capacity + tolerance, &
                                 "Wave transport magnitude must not exceed total bedload capacity")
            call f90_expect_near(current_load**2 + wave_load**2, capacity**2, tolerance, &
                                 "The squared wave/current magnitudes must partition the squared capacity")
            call f90_expect_near(wave_load / current_load, ratios(i), tolerance, &
                                 "The wave/current magnitude ratio must match the imposed excess velocities")
            if (j == 1) then
               call f90_expect_true(abs(flux(1) + flux(3)) <= 2.0e-6_fp * capacity, &
                                    "The opposing resultant must stay continuous through an equal contribution ratio")
            end if
            if (i == 1 .and. j == 1) then
               call f90_expect_near(flux(1) + flux(3), 0.0_fp, tolerance, &
                                    "Equal opposing transport contributions must cancel without singular components")
               call f90_expect_near(flux(2) + flux(4), 0.0_fp, tolerance, &
                                    "Equal opposing transport must have zero transverse resultant")
            end if
         end do
      end do
   end subroutine test_bedtr_opposing

   !$gtest
   !> At ratio one each magnitude is qbt/sqrt(2), independently of the angle between vectors.
   subroutine test_bedtr_angles() bind(C)
      use m_mathconstants, only: degrad

      real(fp), parameter :: angles(4) = [0.0_fp, 90.0_fp, 180.0_fp, 270.0_fp]
      real(fp) :: flux(6), uon, uoff, vcr, current, component
      integer :: i

      call evaluate_bedtr(0.0_fp, 0.0_fp, 0.5_fp, .true., flux, uon, uoff, vcr)
      current = uon
      do i = 1, size(angles)
         call evaluate_bedtr(current, angles(i), 0.5_fp, .true., flux, uon, uoff, vcr)
         component = bedtr_capacity(current, uon, vcr) / sqrt(2.0_fp)
         call f90_expect_near(flux(1), component, tolerance, &
                              "Current bedload magnitude must be independent of the wave direction")
         call f90_expect_near(flux(2), 0.0_fp, tolerance, "Current transport follows the imposed current")
         call f90_expect_near(flux(3), component * cos(angles(i) * degrad), tolerance, &
                              "Wave transport x component must rotate with its direction")
         call f90_expect_near(flux(4), component * sin(angles(i) * degrad), tolerance, &
                              "Wave transport y component must rotate with its direction")
      end do
   end subroutine test_bedtr_angles

   !$gtest
   !> Preserve legacy current transport and the zero-current/zero-wave limits.
   subroutine test_bedtr_limits() bind(C)
      real(fp) :: flux(6), uon, uoff, vcr, expected

      call evaluate_bedtr(0.8_fp, 0.0_fp, 0.5_fp, .false., flux, uon, uoff, vcr)
      expected = 0.5_fp * bed_rhosol * bed_d50 * 0.1_fp * 5.0_fp**(-0.3_fp) * 2.0_fp
      call f90_expect_near(flux(1), expected, tolerance, "The original no-wave bedload formula must be preserved")
      call f90_expect_near(maxval(abs(flux(2:6))), 0.0_fp, tolerance, &
                           "The original no-wave formula produces current transport only")

      call evaluate_bedtr(0.8_fp, 0.0_fp, 0.0_fp, .true., flux, uon, uoff, vcr)
      call f90_expect_near(flux(1), bedtr_capacity(0.8_fp, uon, vcr), tolerance, &
                           "Zero wave height assigns the wave-aware capacity to current transport")
      call f90_expect_near(maxval(abs(flux(2:6))), 0.0_fp, tolerance, &
                           "Zero wave height must not generate wave or transverse transport")

      call evaluate_bedtr(0.0_fp, 0.0_fp, 0.5_fp, .true., flux, uon, uoff, vcr)
      call f90_expect_near(maxval(abs(flux(1:2))), 0.0_fp, tolerance, &
                           "Zero current must not generate current-directed bedload")
      call f90_expect_near(flux(3), bedtr_capacity(0.0_fp, uon, vcr), tolerance, &
                           "Zero current assigns the wave-aware capacity to wave transport")
      call f90_expect_near(flux(4), 0.0_fp, tolerance, "Wave-only bedload follows the wave direction")

      call evaluate_bedtr(0.0_fp, 0.0_fp, 0.0_fp, .true., flux, uon, uoff, vcr)
      call f90_expect_near(maxval(abs(flux)), 0.0_fp, tolerance, "Still water must produce no sediment transport")
   end subroutine test_bedtr_limits

   !$gtest
   !> Existing small/large wave-current ratio cutoffs retain their one-component limits.
   subroutine test_bedtr_cutoffs() bind(C)
      real(fp), parameter :: ratios(2) = [0.005_fp, 101.0_fp]
      real(fp) :: flux(6), uon, uoff, vcr, current, capacity
      integer :: i

      call evaluate_bedtr(0.0_fp, 0.0_fp, 0.5_fp, .true., flux, uon, uoff, vcr)
      do i = 1, size(ratios)
         current = vcr + (uon - vcr) / ratios(i)**(1.0_fp / 3.0_fp)
         call evaluate_bedtr(current, 0.0_fp, 0.5_fp, .true., flux, uon, uoff, vcr)
         capacity = bedtr_capacity(current, uon, vcr)
         if (i == 1) then
            call f90_expect_near(flux(1), capacity, tolerance, "A ratio below 0.01 retains current-only bedload")
            call f90_expect_near(flux(3), 0.0_fp, tolerance, "A ratio below 0.01 has zero wave bedload")
         else
            call f90_expect_near(flux(1), 0.0_fp, tolerance, "A ratio above 100 has zero current bedload")
            call f90_expect_near(flux(3), capacity, tolerance, "A ratio above 100 retains wave-only bedload")
         end if
      end do
   end subroutine test_bedtr_cutoffs

   !> Evaluate the actual transport formula using fixed sand and wave parameters.
   subroutine evaluate_bedtr(current, wave_angle, wave_height, waves, flux, uon, uoff, vcr)
      use m_bedtr1993, only: bedtr1993

      real(fp), intent(in) :: current, wave_angle, wave_height
      logical, intent(in) :: waves
      real(fp), intent(out) :: flux(6), uon, uoff, vcr
      logical :: error
      character(len=256) :: message

      call bedtr1993(uuu=current, vvv=0.0_fp, u2dh=current, d50=bed_d50, d90=0.0003_fp, &
                    h1=1.0_fp, taurat=3.0_fp, ustarc=0.1_fp, muc=1.0_fp, rhosol=bed_rhosol, &
                    dstar=5.0_fp, ws=bed_ws, hrms=wave_height, tp=6.0_fp, teta=wave_angle, rlabda=20.0_fp, &
                    umod=current, qbcu=flux(1), qbcv=flux(2), qbwu=flux(3), qbwv=flux(4), &
                    qswu=flux(5), qswv=flux(6), rhowat=bed_rhowat, ag=bed_gravity, wave=waves, eps=1.0e-10_fp, &
                    uon=uon, uoff=uoff, vcr=vcr, error=error, message=message)
      call f90_expect_false(error, "The valid sand/wave fixture must not fail: " // trim(message))
   end subroutine evaluate_bedtr

   !> Total Van Rijn capacity before partitioning into current and wave magnitudes.
   function bedtr_capacity(current, uon, vcr) result(capacity)
      real(fp), intent(in) :: current, uon, vcr
      real(fp) :: capacity, effective_velocity, mobility, excess_mobility

      effective_velocity = sqrt(current**2 + uon**2)
      mobility = effective_velocity**2 / ((bed_rhosol / bed_rhowat - 1.0_fp) * bed_gravity * bed_d50)
      excess_mobility = (effective_velocity - vcr)**2 &
                       / ((bed_rhosol / bed_rhowat - 1.0_fp) * bed_gravity * bed_d50)
      capacity = 0.006_fp * bed_rhosol * bed_ws * bed_d50 * sqrt(mobility) * excess_mobility**0.7_fp
   end function bedtr_capacity

   !> Run the actual suspended-correction routine on one internal link with two layers.
   !! All replaced module allocations, scalars and pointer associations are restored.
   subroutine evaluate_suscor(weight, reference_height, reference_concentration, upper_concentration, &
                             velocity, raw_flux, availability, lower_interface, correction, reverse_edge)
      use m_fm_bott3d, only: fm_suspended_sand_correction
      use sediment_basics_module, only: TRA_COMBINE
      use m_flow, only: u1, kmx, hu, lbot, ltop
      use m_flowgeom, only: ln, lnx, lnxi, acl, wu_mor
      use m_transport, only: fluxhortot, ised1, constituents, numconst
      use m_fm_erosed, only: aks, e_scrn, e_scrt, fixfac, kfsed, lsed, l_suscor, rca, suscorfac, sus, tratyp
      use m_partitioninfo, only: jampi

      real(dp), intent(in) :: weight, reference_height(2), reference_concentration(2), upper_concentration(2)
      real(dp), intent(in) :: velocity, raw_flux, availability(2), lower_interface
      real(dp), intent(out) :: correction
      logical, intent(in), optional :: reverse_edge
      integer :: saved_kmx, saved_lnx, saved_lnxi, saved_jampi, saved_ised1, saved_numconst
      integer, allocatable, target :: saved_ln(:, :), saved_lbot(:), saved_ltop(:)
      real(dp), allocatable, target :: saved_u1(:), saved_hu(:), saved_acl(:), saved_wu_mor(:)
      real(dp), allocatable, target :: saved_flux(:, :), saved_constituents(:, :)
      integer, pointer :: saved_lsed, saved_kfsed(:), saved_tratyp(:)
      logical, pointer :: saved_l_suscor
      real(fp), pointer :: saved_sus, saved_suscorfac, saved_aks(:, :), saved_rca(:, :)
      real(fp), pointer :: saved_fixfac(:, :), saved_scrn(:, :), saved_scrt(:, :)
      integer, target :: local_lsed, local_kfsed(2), local_tratyp(1)
      logical, target :: local_l_suscor
      real(fp), target :: local_sus, local_suscorfac, local_aks(2, 1), local_rca(2, 1)
      real(fp), target :: local_fixfac(2, 1), local_scrn(1, 1), local_scrt(1, 1)

      saved_kmx = kmx
      saved_lnx = lnx
      saved_lnxi = lnxi
      saved_jampi = jampi
      saved_ised1 = ised1
      saved_numconst = numconst
      saved_lsed => lsed
      saved_kfsed => kfsed
      saved_tratyp => tratyp
      saved_l_suscor => l_suscor
      saved_sus => sus
      saved_suscorfac => suscorfac
      saved_aks => aks
      saved_rca => rca
      saved_fixfac => fixfac
      saved_scrn => e_scrn
      saved_scrt => e_scrt
      call move_alloc(ln, saved_ln)
      call move_alloc(lbot, saved_lbot)
      call move_alloc(ltop, saved_ltop)
      call move_alloc(u1, saved_u1)
      call move_alloc(hu, saved_hu)
      call move_alloc(acl, saved_acl)
      call move_alloc(wu_mor, saved_wu_mor)
      call move_alloc(fluxhortot, saved_flux)
      call move_alloc(constituents, saved_constituents)

      kmx = 2
      lnx = 1
      lnxi = 1
      jampi = 0
      ised1 = 1
      numconst = 1
      local_lsed = 1
      local_kfsed = 1
      local_tratyp = TRA_COMBINE
      local_l_suscor = .true.
      local_sus = 1.0_fp
      local_suscorfac = 1.0_fp
      local_aks(:, 1) = reference_height
      local_rca(:, 1) = reference_concentration
      local_fixfac(:, 1) = availability
      local_scrn = 0.0_fp
      local_scrt = 0.0_fp
      lsed => local_lsed
      kfsed => local_kfsed
      tratyp => local_tratyp
      l_suscor => local_l_suscor
      sus => local_sus
      suscorfac => local_suscorfac
      aks => local_aks
      rca => local_rca
      fixfac => local_fixfac
      e_scrn => local_scrn
      e_scrt => local_scrt
      allocate (ln(2, 4), lbot(1), ltop(1), u1(4), hu(4), acl(1), wu_mor(1))
      allocate (fluxhortot(1, 4), constituents(1, 6))
      ln = 0
      ln(:, 1) = [1, 2]
      ln(:, 3) = [3, 4]
      ln(:, 4) = [5, 6]
      if (present(reverse_edge)) then
         if (reverse_edge) then
            ln(:, 1) = [2, 1]
            ln(:, 3) = [4, 3]
            ln(:, 4) = [6, 5]
         end if
      end if
      lbot = 3
      ltop = 4
      u1 = velocity
      hu = [0.2_dp, lower_interface, 0.1_dp, 0.2_dp]
      acl = weight
      wu_mor = 10.0_dp
      fluxhortot = 0.0_dp
      fluxhortot(1, 3) = raw_flux
      constituents = 0.0_dp
      constituents(1, 5:6) = upper_concentration

      call fm_suspended_sand_correction()
      correction = e_scrn(1, 1)

      call move_alloc(saved_ln, ln)
      call move_alloc(saved_lbot, lbot)
      call move_alloc(saved_ltop, ltop)
      call move_alloc(saved_u1, u1)
      call move_alloc(saved_hu, hu)
      call move_alloc(saved_acl, acl)
      call move_alloc(saved_wu_mor, wu_mor)
      call move_alloc(saved_flux, fluxhortot)
      call move_alloc(saved_constituents, constituents)
      lsed => saved_lsed
      kfsed => saved_kfsed
      tratyp => saved_tratyp
      l_suscor => saved_l_suscor
      sus => saved_sus
      suscorfac => saved_suscorfac
      aks => saved_aks
      rca => saved_rca
      fixfac => saved_fixfac
      e_scrn => saved_scrn
      e_scrt => saved_scrt
      kmx = saved_kmx
      lnx = saved_lnx
      lnxi = saved_lnxi
      jampi = saved_jampi
      ised1 = saved_ised1
      numconst = saved_numconst
   end subroutine evaluate_suscor

   !> Run the actual morphology restriction on two cells joined by one link.
   subroutine evaluate_mor_dt(weight, transport_x, reverse_edge, result, limiting_cell)
      use m_fm_mor_maxtimestep, only: fm_mor_maxtimestep
      use m_flowtimes, only: dts
      use m_flow, only: map_write_settings, kkcflmx
      use m_flowgeom, only: acl, ba, csu, snu, wu, tnode
      use m_sediment, only: dzbdtmax, kcsmor
      use m_fm_erosed, only: sxtot, sytot, cdryb, morfac, lsedtot, ndx_mor, nd_mor, ln_mor

      real(dp), intent(in) :: weight, transport_x(2)
      logical, intent(in) :: reverse_edge
      real(dp), intent(out) :: result
      integer, intent(out) :: limiting_cell
      real(dp) :: saved_dts, saved_dzbdtmax
      integer :: saved_ndx, saved_kkcflmx, saved_flow_analysis
      integer, allocatable, target :: saved_kcsmor(:), saved_ln(:, :)
      real(dp), allocatable, target :: saved_acl(:), saved_ba(:), saved_csu(:), saved_snu(:), saved_wu(:)
      type(tnode), allocatable :: saved_nd(:)
      integer, pointer :: saved_lsedtot
      real(fp), pointer :: saved_sxtot(:, :), saved_sytot(:, :), saved_cdryb(:), saved_morfac
      integer, target :: local_lsedtot
      real(fp), target :: local_sxtot(2, 1), local_sytot(2, 1), local_cdryb(1), local_morfac

      saved_dts = dts
      saved_dzbdtmax = dzbdtmax
      saved_ndx = ndx_mor
      saved_kkcflmx = kkcflmx
      saved_flow_analysis = map_write_settings%flow_analysis
      saved_lsedtot => lsedtot
      saved_sxtot => sxtot
      saved_sytot => sytot
      saved_cdryb => cdryb
      saved_morfac => morfac
      call move_alloc(kcsmor, saved_kcsmor)
      call move_alloc(ln_mor, saved_ln)
      call move_alloc(nd_mor, saved_nd)
      call move_alloc(acl, saved_acl)
      call move_alloc(ba, saved_ba)
      call move_alloc(csu, saved_csu)
      call move_alloc(snu, saved_snu)
      call move_alloc(wu, saved_wu)

      dts = 1.0_dp
      dzbdtmax = 0.1_dp
      ndx_mor = 2
      kkcflmx = 7
      map_write_settings%flow_analysis = 0
      local_lsedtot = 1
      local_sxtot(:, 1) = transport_x
      local_sytot = 0.0_fp
      local_cdryb = 1600.0_fp
      local_morfac = 1.0_fp
      lsedtot => local_lsedtot
      sxtot => local_sxtot
      sytot => local_sytot
      cdryb => local_cdryb
      morfac => local_morfac
      allocate (kcsmor(2), ln_mor(2, 1), nd_mor(2), acl(1), ba(2), csu(1), snu(1), wu(1))
      allocate (nd_mor(1)%ln(1), nd_mor(2)%ln(1))
      kcsmor = 1
      ln_mor(:, 1) = [1, 2]
      nd_mor(1)%lnx = 1
      nd_mor(2)%lnx = 1
      nd_mor(1)%ln = -1
      nd_mor(2)%ln = 1
      acl = weight
      ba = [10.0_dp, 20.0_dp]
      csu = 1.0_dp
      snu = 0.0_dp
      wu = 10.0_dp
      if (reverse_edge) then
         ln_mor(:, 1) = [2, 1]
         acl = 1.0_dp - weight
         csu = -1.0_dp
      end if

      call fm_mor_maxtimestep()
      result = dts
      limiting_cell = kkcflmx

      call move_alloc(saved_kcsmor, kcsmor)
      call move_alloc(saved_ln, ln_mor)
      call move_alloc(saved_nd, nd_mor)
      call move_alloc(saved_acl, acl)
      call move_alloc(saved_ba, ba)
      call move_alloc(saved_csu, csu)
      call move_alloc(saved_snu, snu)
      call move_alloc(saved_wu, wu)
      lsedtot => saved_lsedtot
      sxtot => saved_sxtot
      sytot => saved_sytot
      cdryb => saved_cdryb
      morfac => saved_morfac
      dts = saved_dts
      dzbdtmax = saved_dzbdtmax
      ndx_mor = saved_ndx
      kkcflmx = saved_kkcflmx
      map_write_settings%flow_analysis = saved_flow_analysis
   end subroutine evaluate_mor_dt

end module test_sed_transport
