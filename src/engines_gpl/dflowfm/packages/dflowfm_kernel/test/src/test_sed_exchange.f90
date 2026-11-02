!> Regression tests for settling reference levels and accumulated deposition.
module test_sed_exchange
   use assertions_gtest, only: f90_expect_near
   use precision, only: dp, fp

   implicit none(type, external)
   private
   public :: test_settle_cutoff, test_settle_tracer, test_settle_mass, test_settle_semi
   public :: test_settle_bedref, test_sink_substeps, test_sink_fullstep, test_sink_closure

   real(dp), parameter :: tolerance = 1.0e-11_dp

contains

   !$f90tw TESTCODE(TEST, sediment_exchange, settle_cutoff, test_settle_cutoff,
   !> Implicit settling observes the same sediment reference level as explicit fluxes.
   subroutine test_settle_cutoff() bind(C)
      real(dp) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2)

      call evaluate_settling(implicit_result, explicit_result, explicit_flux)

      call f90_expect_near(implicit_result(1, :), [7.0_dp, 2.2_dp, 2.4_dp], tolerance, &
                           "Implicit sediment settling must stop below the reference layer")
      call f90_expect_near(explicit_result(1, :), [7.0_dp, 2.5_dp, 2.25_dp], tolerance, &
                           "The explicit sediment path must use the same reference-layer cutoff")
      call f90_expect_near(explicit_flux(1, :), [0.0_dp, -1.5_dp], tolerance, &
                           "Settling is zero below the reference and remains active above it")
   end subroutine test_settle_cutoff
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, settle_tracer, test_settle_tracer,
   !> A settling tracer continues through all layers when morphology is enabled.
   subroutine test_settle_tracer() bind(C)
      real(dp) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2)

      call evaluate_settling(implicit_result, explicit_result, explicit_flux)

      call f90_expect_near(implicit_result(2, :), [127.0_dp / 15.0_dp, 22.0_dp / 15.0_dp, 2.4_dp], &
                           tolerance, "The sediment cutoff must not block implicit tracer settling")
      call f90_expect_near(explicit_result(2, :), [8.0_dp, 2.0_dp, 2.25_dp], tolerance, &
                           "The sediment cutoff must not block explicit tracer settling")
      call f90_expect_near(explicit_flux(2, :), [-0.5_dp, -1.5_dp], tolerance, &
                           "The tracer must retain its settling flux below the sediment reference layer")
   end subroutine test_settle_tracer
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, settle_mass, test_settle_mass,
   !> Both settling paths conserve each constituent across unequal layer volumes.
   subroutine test_settle_mass() bind(C)
      real(dp), parameter :: volumes(3) = [1.0_dp, 2.0_dp, 4.0_dp]
      real(dp) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2)
      integer :: j

      call evaluate_settling(implicit_result, explicit_result, explicit_flux)

      do j = 1, 2
         call f90_expect_near(dot_product(volumes, implicit_result(j, :)), 21.0_dp, tolerance, &
                              "Implicit settling must conserve mass with unequal layer volumes")
         call f90_expect_near(dot_product(volumes, explicit_result(j, :)), 21.0_dp, tolerance, &
                              "Explicit settling must conserve mass with unequal layer volumes")
      end do
   end subroutine test_settle_mass
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, settle_semi, test_settle_semi,
   !> The semi-implicit advection path observes the same sediment settling cutoff.
   subroutine test_settle_semi() bind(C)
      real(dp), parameter :: volumes(3) = [1.0_dp, 2.0_dp, 4.0_dp]
      real(dp) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2), semi_result(2, 3)
      integer :: j

      call evaluate_settling(implicit_result, explicit_result, explicit_flux, semi_result)

      call f90_expect_near(semi_result(1, :), [7.0_dp, 31.0_dp / 15.0_dp, 37.0_dp / 15.0_dp], &
                           tolerance, "Semi-implicit sediment settling must use the reference-layer cutoff")
      call f90_expect_near(semi_result(2, :), [1261.0_dp / 107.0_dp, -69.0_dp / 107.0_dp, 281.0_dp / 107.0_dp], &
                           tolerance, "Semi-implicit tracer settling must remain active below the sediment reference")
      do j = 1, 2
         call f90_expect_near(dot_product(volumes, semi_result(j, :)), 21.0_dp, tolerance, &
                              "Semi-implicit settling must conserve each constituent with unequal layer volumes")
      end do
   end subroutine test_settle_semi
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, settle_bedref, test_settle_bedref,
   !> A cohesive or bottom-layer reference must retain sediment settling on every internal face.
   subroutine test_settle_bedref() bind(C)
      real(dp), parameter :: volumes(3) = [1.0_dp, 2.0_dp, 4.0_dp]
      real(dp) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2)

      call evaluate_settling(implicit_result, explicit_result, explicit_flux, reference_layer=2)

      call f90_expect_near(implicit_result(1, :), [127.0_dp / 15.0_dp, 22.0_dp / 15.0_dp, 2.4_dp], &
                           tolerance, "A bed-layer reference must preserve implicit sediment settling on all faces")
      call f90_expect_near(explicit_result(1, :), [8.0_dp, 2.0_dp, 2.25_dp], tolerance, &
                           "A bed-layer reference must preserve explicit sediment settling on all faces")
      call f90_expect_near(explicit_flux(1, :), [-0.5_dp, -1.5_dp], tolerance, &
                           "A reference at the bed must not block an internal settling flux")
      call f90_expect_near(dot_product(volumes, implicit_result(1, :)), 21.0_dp, tolerance, &
                           "Full-depth implicit sediment settling must conserve mass")
      call f90_expect_near(dot_product(volumes, explicit_result(1, :)), 21.0_dp, tolerance, &
                           "Full-depth explicit sediment settling must conserve mass")
   end subroutine test_settle_bedref
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, sink_substeps, test_sink_substeps,
   !> Accumulate updated internal cells using their full local timestep and each solver's dry-cell rule.
   subroutine test_sink_substeps() bind(C)
      real(dp) :: deposited(2, 4), fluff(2, 4), expected_deposition(4), expected_fluff(4)
      integer :: layers

      do layers = 0, 2, 2
         call evaluate_deposition(layers, 4, deposited, fluff)
         expected_deposition = [10.0_dp, 16.0_dp, 12.4_dp, 10.0_dp]
         expected_fluff = [20.0_dp, 21.5_dp, 20.6_dp, 20.0_dp]
         if (layers > 0) then
            expected_deposition(3) = 10.0_dp
            expected_fluff(3) = 20.0_dp
         end if
         call f90_expect_near(deposited(2, :), expected_deposition, tolerance, &
                              "Deposition must use dt*deltasteps and the same active cells as its transport solver")
         call f90_expect_near(fluff(2, :), expected_fluff, tolerance, &
                              "Fluff deposition must use the same local timestep and active cells as bed deposition")
         call f90_expect_near(deposited(1, :), [10.0_dp, 10.0_dp, 10.0_dp, 10.0_dp], tolerance, &
                              "Non-sediment constituents must not accumulate sediment deposition")
         call f90_expect_near(fluff(1, :), [20.0_dp, 20.0_dp, 20.0_dp, 20.0_dp], tolerance, &
                              "Non-sediment constituents must not accumulate fluff deposition")
      end do
   end subroutine test_sink_substeps
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, sink_fullstep, test_sink_fullstep,
   !> Without subcycling the ordinary timestep applies independently of work-array values.
   subroutine test_sink_fullstep() bind(C)
      real(dp) :: deposited(2, 4), fluff(2, 4), expected_deposition(4), expected_fluff(4)
      integer :: layers

      do layers = 0, 2, 2
         call evaluate_deposition(layers, 1, deposited, fluff)
         expected_deposition = [10.6_dp, 12.0_dp, 10.6_dp, 10.0_dp]
         expected_fluff = [20.15_dp, 20.5_dp, 20.15_dp, 20.0_dp]
         if (layers > 0) then
            expected_deposition(3) = 10.0_dp
            expected_fluff(3) = 20.0_dp
         end if
         call f90_expect_near(deposited(2, :), expected_deposition, tolerance, &
                              "A single step must use dt and its solver's internal-cell set without substep masks")
         call f90_expect_near(fluff(2, :), expected_fluff, tolerance, &
                              "Single-step fluff deposition must use dt without substep multipliers")
      end do
   end subroutine test_sink_fullstep
   !$f90tw)

   !$f90tw TESTCODE(TEST, sediment_exchange, sink_closure, test_sink_closure,
   !> A skipped substep followed by a two-step implicit sink update closes the actual sediment budget.
   subroutine test_sink_closure() bind(C)
      use m_solve_2d, only: solve_2d
      use m_comp_sinktot, only: comp_sinktot
      use m_transport, only: ised1, isedn, sinksetot, constituents, sinkftot
      use m_sediment, only: stm_included, mxgr, sedtra, stmpar
      use m_flow, only: vol1, kmx
      use m_flowgeom, only: ndx, ndxi
      use m_flowtimes, only: dts
      use m_flowparameters, only: testdryflood
      use morphology_data_module, only: morpar_type, fluffy_type
      use timers, only: timon

      integer :: kbot(1), ktop(1), jaupdate(1), ndeltasteps(1)
      real(dp) :: sumhorflux(1, 1), fluxver(1, 1), source(1, 1), sink(1, 1), rhs(1, 1)
      real(dp) :: skipped(2), updated(2)
      integer :: saved_ised1, saved_isedn, saved_mxgr, saved_kmx, saved_ndx, saved_ndxi, saved_testdryflood
      real(dp) :: saved_dts
      logical :: saved_stm_included, saved_timon
      real(dp), allocatable, target :: saved_vol1(:), saved_constituents(:, :)
      real(dp), allocatable :: saved_sinksetot(:, :), saved_sinkftot(:, :)
      real(fp), pointer :: saved_sinkse(:, :)
      type(morpar_type), pointer :: saved_morpar
      real(fp), target :: local_sinkse(1, 1)
      type(morpar_type), target :: local_morpar
      type(fluffy_type), target :: local_fluff

      saved_ised1 = ised1
      saved_isedn = isedn
      saved_mxgr = mxgr
      saved_kmx = kmx
      saved_ndx = ndx
      saved_ndxi = ndxi
      saved_testdryflood = testdryflood
      saved_dts = dts
      saved_stm_included = stm_included
      saved_timon = timon
      saved_sinkse => sedtra%sinkse
      saved_morpar => stmpar%morpar
      call move_alloc(vol1, saved_vol1)
      call move_alloc(constituents, saved_constituents)
      call move_alloc(sinksetot, saved_sinksetot)
      call move_alloc(sinkftot, saved_sinkftot)

      ised1 = 1
      isedn = 1
      mxgr = 1
      kmx = 0
      ndx = 1
      ndxi = 1
      testdryflood = 0
      dts = 1.0_dp
      stm_included = .true.
      timon = .false.
      local_sinkse = 1.0_fp
      local_fluff%iflufflyr = 0
      local_morpar%flufflyr => local_fluff
      stmpar%morpar => local_morpar
      sedtra%sinkse => local_sinkse
      allocate (vol1(1), constituents(1, 1), sinksetot(1, 1), sinkftot(1, 1))
      vol1 = 1.0_dp
      constituents = 1.0_dp
      sinksetot = 0.0_dp
      sinkftot = 0.0_dp
      kbot = 1
      ktop = 1
      ndeltasteps = 2
      sumhorflux = 0.0_dp
      fluxver = 0.0_dp
      source = 0.0_dp
      sink = 1.0_dp

      jaupdate = 0
      call solve_2d(1, 1, vol1, kbot, ktop, sumhorflux, fluxver, source, sink, &
                    2, jaupdate, ndeltasteps, constituents, rhs)
      call comp_sinktot(2, jaupdate, ndeltasteps)
      skipped = [constituents(1, 1), sinksetot(1, 1)]

      jaupdate = 1
      call solve_2d(1, 1, vol1, kbot, ktop, sumhorflux, fluxver, source, sink, &
                    2, jaupdate, ndeltasteps, constituents, rhs)
      call comp_sinktot(2, jaupdate, ndeltasteps)
      updated = [constituents(1, 1), sinksetot(1, 1)]

      call move_alloc(saved_vol1, vol1)
      call move_alloc(saved_constituents, constituents)
      call move_alloc(saved_sinksetot, sinksetot)
      call move_alloc(saved_sinkftot, sinkftot)
      sedtra%sinkse => saved_sinkse
      stmpar%morpar => saved_morpar
      ised1 = saved_ised1
      isedn = saved_isedn
      mxgr = saved_mxgr
      kmx = saved_kmx
      ndx = saved_ndx
      ndxi = saved_ndxi
      testdryflood = saved_testdryflood
      dts = saved_dts
      stm_included = saved_stm_included
      timon = saved_timon

      call f90_expect_near(skipped, [1.0_dp, 0.0_dp], tolerance, &
                           "A skipped local step must change neither suspended mass nor accumulated deposition")
      call f90_expect_near(updated, [1.0_dp / 3.0_dp, 2.0_dp / 3.0_dp], tolerance, &
                           "The two-step implicit sink loss must equal accumulated deposition")
      call f90_expect_near(sum(updated), 1.0_dp, tolerance, &
                           "Remaining suspended mass plus deposited mass must close the coupled sediment budget")
   end subroutine test_sink_closure
   !$f90tw)

   !> Compare actual implicit and explicit settling paths on one three-layer column.
   !! All replaced allocations, scalars and pointer associations are restored.
   subroutine evaluate_settling(implicit_result, explicit_result, explicit_flux, semi_result, reference_layer)
      use m_solve_vertical, only: solve_vertical
      use m_comp_fluxver, only: comp_fluxver
      use m_flowgeom, only: ndx, ndxi, kfs, ba
      use m_flowtimes, only: dts
      use m_flow, only: kmxn, xlozmidov, a1, wsf, jaimplicitfallvelocity, s1, epshsdif, cffacver
      use m_turbulence, only: difwws, difwws_total
      use m_flowparameters, only: testdryflood
      use m_sediment, only: mtd, jased, sedtra, stm_included
      use m_fm_erosed, only: tpsnumber
      use m_transport, only: isalt, ised1, isedn, molecular_diffusion_coeff
      use m_physcoef, only: dicoww
      use timers, only: timon

      real(dp), intent(out) :: implicit_result(2, 3), explicit_result(2, 3), explicit_flux(2, 2)
      real(dp), intent(out), optional :: semi_result(2, 3)
      integer, intent(in), optional :: reference_layer
      real(dp), parameter :: zws(4) = [0.0_dp, 1.0_dp, 3.0_dp, 7.0_dp]
      real(dp), parameter :: volumes(4) = [1.0_dp, 1.0_dp, 2.0_dp, 4.0_dp]
      real(dp) :: qw(4), concentrations(2, 4), initial(2, 4), fluxver(2, 4), sumhorflux(2, 4)
      real(dp) :: source(2, 4), sink(2, 4), theta(2), sigdifi(2), vicwws(4)
      real(dp) :: lower(3, 2), diagonal(3, 2), upper(3, 2), column_rhs(3, 2)
      real(dp) :: normalized_upper(3), sol(3), rhs(2, 4)
      integer :: kbot(1), ktop(1), jaupdate(1), ndeltasteps(1)
      integer :: saved_ndx, saved_ndxi, saved_jased, saved_isalt, saved_ised1, saved_isedn
      integer :: saved_implicit, saved_testdryflood
      real(dp) :: saved_dts, saved_xlozmidov, saved_epshsdif, saved_cffacver, saved_dicoww
      logical :: saved_stm_included, saved_timon
      integer, allocatable, target :: saved_kfs(:), saved_kmxn(:)
      real(dp), allocatable, target :: saved_ba(:), saved_a1(:), saved_wsf(:), saved_s1(:)
      real(dp), allocatable :: saved_difwws(:), saved_difwws_total(:), saved_molecular(:), saved_dicoww_values(:)
      integer, pointer :: saved_kmxsed(:, :)
      real(fp), pointer :: saved_ws(:, :), saved_seddif(:, :), saved_tpsnumber(:)
      integer, target :: local_kmxsed(1, 1)
      real(fp), target :: local_ws(4, 1), local_seddif(1, 4), local_tpsnumber(1)

      saved_ndx = ndx
      saved_ndxi = ndxi
      saved_jased = jased
      saved_isalt = isalt
      saved_ised1 = ised1
      saved_isedn = isedn
      saved_implicit = jaimplicitfallvelocity
      saved_testdryflood = testdryflood
      saved_dts = dts
      saved_xlozmidov = xlozmidov
      saved_epshsdif = epshsdif
      saved_cffacver = cffacver
      saved_dicoww = dicoww%scalar
      saved_stm_included = stm_included
      saved_timon = timon
      saved_kmxsed => sedtra%kmxsed
      saved_ws => mtd%ws
      saved_seddif => mtd%seddif
      saved_tpsnumber => tpsnumber
      call move_alloc(kfs, saved_kfs)
      call move_alloc(kmxn, saved_kmxn)
      call move_alloc(ba, saved_ba)
      call move_alloc(a1, saved_a1)
      call move_alloc(wsf, saved_wsf)
      call move_alloc(s1, saved_s1)
      call move_alloc(difwws, saved_difwws)
      call move_alloc(difwws_total, saved_difwws_total)
      call move_alloc(molecular_diffusion_coeff, saved_molecular)
      call move_alloc(dicoww%values, saved_dicoww_values)

      ndx = 1
      ndxi = 1
      jased = 4
      stm_included = .true.
      isalt = 0
      ised1 = 1
      isedn = 1
      testdryflood = 0
      dts = 2.0_dp
      xlozmidov = 0.0_dp
      epshsdif = 1.0e-6_dp
      cffacver = 0.0_dp
      dicoww%scalar = 0.0_dp
      timon = .false.
      local_kmxsed = 3
      if (present(reference_layer)) then
         local_kmxsed = reference_layer
      end if
      local_ws = 0.5_fp
      local_seddif = 0.0_fp
      local_tpsnumber = 1.0_fp
      sedtra%kmxsed => local_kmxsed
      mtd%ws => local_ws
      mtd%seddif => local_seddif
      tpsnumber => local_tpsnumber
      allocate (kfs(1), kmxn(1), ba(1), a1(1), wsf(2), s1(1))
      allocate (difwws(4), difwws_total(4), molecular_diffusion_coeff(2))
      kfs = 1
      kmxn = 3
      ba = 1.0_dp
      a1 = 1.0_dp
      wsf = 0.5_dp
      s1 = 7.0_dp
      difwws = 0.0_dp
      difwws_total = 0.0_dp
      molecular_diffusion_coeff = 0.0_dp
      kbot = 2
      ktop = 4
      jaupdate = 1
      ndeltasteps = 1
      qw = 0.0_dp
      theta = 0.0_dp
      sigdifi = 0.0_dp
      vicwws = 0.0_dp
      source = 0.0_dp
      sink = 0.0_dp
      initial(:, 1) = 0.0_dp
      initial(:, 2) = 7.0_dp
      initial(:, 3) = 1.0_dp
      initial(:, 4) = 3.0_dp

      jaimplicitfallvelocity = 1
      concentrations = initial
      sumhorflux = 0.0_dp
      fluxver = 0.0_dp
      call solve_vertical(2, 1, 1, theta, 4, 3, zws, qw, volumes, kbot, ktop, &
                          sumhorflux, fluxver, source, sink, sigdifi, vicwws, &
                          1, jaupdate, ndeltasteps, concentrations, &
                          lower, diagonal, upper, column_rhs, normalized_upper, sol, rhs)
      implicit_result = concentrations(:, 2:4)

      jaimplicitfallvelocity = 0
      concentrations = initial
      sumhorflux = 0.0_dp
      fluxver = 0.0_dp
      call comp_fluxver(2, 0, theta, 4, zws, qw, kbot, ktop, concentrations, &
                        1, jaupdate, ndeltasteps, fluxver, wsf)
      explicit_flux = fluxver(:, 2:3)
      call solve_vertical(2, 1, 1, theta, 4, 3, zws, qw, volumes, kbot, ktop, &
                          sumhorflux, fluxver, source, sink, sigdifi, vicwws, &
                          1, jaupdate, ndeltasteps, concentrations, &
                          lower, diagonal, upper, column_rhs, normalized_upper, sol, rhs)
      explicit_result = concentrations(:, 2:4)

      if (present(semi_result)) then
         theta = 0.5_dp
         concentrations = initial
         sumhorflux = 0.0_dp
         fluxver = 0.0_dp
         call comp_fluxver(2, 0, theta, 4, zws, qw, kbot, ktop, concentrations, &
                           1, jaupdate, ndeltasteps, fluxver, wsf)
         call solve_vertical(2, 1, 1, theta, 4, 3, zws, qw, volumes, kbot, ktop, &
                             sumhorflux, fluxver, source, sink, sigdifi, vicwws, &
                             1, jaupdate, ndeltasteps, concentrations, &
                             lower, diagonal, upper, column_rhs, normalized_upper, sol, rhs)
         semi_result = concentrations(:, 2:4)
      end if

      call move_alloc(saved_kfs, kfs)
      call move_alloc(saved_kmxn, kmxn)
      call move_alloc(saved_ba, ba)
      call move_alloc(saved_a1, a1)
      call move_alloc(saved_wsf, wsf)
      call move_alloc(saved_s1, s1)
      call move_alloc(saved_difwws, difwws)
      call move_alloc(saved_difwws_total, difwws_total)
      call move_alloc(saved_molecular, molecular_diffusion_coeff)
      call move_alloc(saved_dicoww_values, dicoww%values)
      sedtra%kmxsed => saved_kmxsed
      mtd%ws => saved_ws
      mtd%seddif => saved_seddif
      tpsnumber => saved_tpsnumber
      ndx = saved_ndx
      ndxi = saved_ndxi
      jased = saved_jased
      isalt = saved_isalt
      ised1 = saved_ised1
      isedn = saved_isedn
      jaimplicitfallvelocity = saved_implicit
      testdryflood = saved_testdryflood
      dts = saved_dts
      xlozmidov = saved_xlozmidov
      epshsdif = saved_epshsdif
      cffacver = saved_cffacver
      dicoww%scalar = saved_dicoww
      stm_included = saved_stm_included
      timon = saved_timon
   end subroutine evaluate_settling

   !> Accumulate bed/fluff sinks on wet, skipped, dry and ghost cells in 2D or 3D.
   !! All replaced allocations, scalars and pointer associations are restored.
   subroutine evaluate_deposition(layers, nsubsteps, deposited, fluff)
      use m_comp_sinktot, only: comp_sinktot
      use m_transport, only: ised1, isedn, sinksetot, constituents, sinkftot
      use m_sediment, only: stm_included, mxgr, sedtra, stmpar
      use m_flow, only: vol1, kmx
      use m_flowgeom, only: ndx, ndxi, kfs
      use m_flowtimes, only: dts
      use morphology_data_module, only: morpar_type, fluffy_type
      use timers, only: timon

      integer, intent(in) :: layers, nsubsteps
      real(dp), intent(out) :: deposited(2, 4), fluff(2, 4)
      integer :: jaupdate(4), ndeltasteps(4), reference(4)
      integer :: saved_ised1, saved_isedn, saved_mxgr, saved_kmx, saved_ndx, saved_ndxi
      real(dp) :: saved_dts
      logical :: saved_stm_included, saved_timon
      integer, allocatable, target :: saved_kfs(:)
      real(dp), allocatable, target :: saved_vol1(:), saved_constituents(:, :)
      real(dp), allocatable :: saved_sinksetot(:, :), saved_sinkftot(:, :)
      integer, pointer :: saved_kmxsed(:, :)
      real(fp), pointer :: saved_sinkse(:, :)
      type(morpar_type), pointer :: saved_morpar
      integer, target :: local_kmxsed(4, 1)
      real(fp), target :: local_sinkse(4, 1), local_sinkf(1, 4)
      type(morpar_type), target :: local_morpar
      type(fluffy_type), target :: local_fluff

      saved_ised1 = ised1
      saved_isedn = isedn
      saved_mxgr = mxgr
      saved_kmx = kmx
      saved_ndx = ndx
      saved_ndxi = ndxi
      saved_dts = dts
      saved_stm_included = stm_included
      saved_timon = timon
      saved_kmxsed => sedtra%kmxsed
      saved_sinkse => sedtra%sinkse
      saved_morpar => stmpar%morpar
      call move_alloc(kfs, saved_kfs)
      call move_alloc(vol1, saved_vol1)
      call move_alloc(constituents, saved_constituents)
      call move_alloc(sinksetot, saved_sinksetot)
      call move_alloc(sinkftot, saved_sinkftot)

      ised1 = 2
      isedn = 2
      mxgr = 1
      kmx = layers
      ndx = 4
      ndxi = 3
      dts = 0.5_dp
      stm_included = .true.
      timon = .false.
      local_sinkse = 0.2_fp
      local_sinkf = 0.05_fp
      local_fluff%iflufflyr = 1
      local_fluff%sinkf => local_sinkf
      local_morpar%flufflyr => local_fluff
      stmpar%morpar => local_morpar
      sedtra%sinkse => local_sinkse
      sedtra%kmxsed => local_kmxsed
      if (layers == 0) then
         reference = [1, 2, 3, 4]
         allocate (vol1(4), constituents(2, 4))
      else
         reference = [6, 8, 10, 12]
         allocate (vol1(12), constituents(2, 12))
      end if
      local_kmxsed(:, 1) = reference
      allocate (kfs(4), sinksetot(2, 4), sinkftot(2, 4))
      kfs = [1, 1, 0, 1]
      vol1 = 2.0_dp
      constituents = 3.0_dp
      vol1(reference(2)) = 4.0_dp
      constituents(2, reference(2)) = 5.0_dp
      sinksetot = 10.0_dp
      sinkftot = 20.0_dp
      if (nsubsteps > 1) then
         jaupdate = [0, 1, 1, 1]
         ndeltasteps = [2, 3, 4, 5]
      else
         ! These values are intentionally unused by the ordinary single-step path.
         jaupdate = 0
         ndeltasteps = 0
      end if

      call comp_sinktot(nsubsteps, jaupdate, ndeltasteps)
      deposited = sinksetot
      fluff = sinkftot

      call move_alloc(saved_kfs, kfs)
      call move_alloc(saved_vol1, vol1)
      call move_alloc(saved_constituents, constituents)
      call move_alloc(saved_sinksetot, sinksetot)
      call move_alloc(saved_sinkftot, sinkftot)
      sedtra%kmxsed => saved_kmxsed
      sedtra%sinkse => saved_sinkse
      stmpar%morpar => saved_morpar
      ised1 = saved_ised1
      isedn = saved_isedn
      mxgr = saved_mxgr
      kmx = saved_kmx
      ndx = saved_ndx
      ndxi = saved_ndxi
      dts = saved_dts
      stm_included = saved_stm_included
      timon = saved_timon
   end subroutine evaluate_deposition

end module test_sed_exchange
