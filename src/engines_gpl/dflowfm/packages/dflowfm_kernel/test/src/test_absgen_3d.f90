!> Verify ABSGEN boundary closure, relaxation, and layer transport.
module test_absgen_3d
   use assertions_gtest
   use m_absgen_3d, only: accumulate_absgen_boundary_mean, apply_absgen_barotropic_increment, &
                         compute_absgen_depth_averaged_velocity, compute_absgen_friction_acceleration, &
                         compute_absgen_incident_velocity, compute_absgen_reflected_velocity, &
                         relax_absgen_boundary_means
   use precision, only: dp

   implicit none(type, external)

contains

   !$gtest
   !> Exercise an ABSGEN-only section through the flow boundary update.
   !! Check the prescribed velocity, relaxation across consecutive updates,
   !! and the explicit reset of the running means.
   subroutine test_absgen_section() bind(C)
      use fm_external_forcings_data, only: kbndu, nbndu, zbndu
      use m_boundary_condition_type, only: BOUNDARY_ABSORBING_GENERATING
      use m_flow, only: hs, kbot, kmx, ktop, s0, s1, u1, ucx, ucy, v, zws
      use m_flowgeom, only: bl, csu, nd, ndx, snu, wu
      use m_flowparameters, only: epshu
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi
      use m_physcoef, only: ag
      use m_xbeach_data, only: absgentype, cats, cgwav, freewave, kbndu2kbndw, Trep, u1rm, uin, umeanrm, vin, vmeanrm
      use m_xbeachwaves, only: xbeach_flow_bc, xbeach_reset

      integer, parameter :: INTERIOR_CELL = 1, BOUNDARY_CELL = 2, BOUNDARY_LINK = 1, BOUNDARY_SECTION = 7
      real(kind=dp), parameter :: TOLERANCE = 1.0e-12_dp

      jampi = 0
      kmx = 2
      ndx = 2
      if (allocated(nd)) then
         deallocate (nd)
      end if
      allocate (nd(ndx))
      nd(INTERIOR_CELL)%lnx = 0
      kbot = [3, 6]
      ktop = [4, 7]
      zws = [0.0_dp, -2.0_dp, -1.0_dp, 0.0_dp, -2.0_dp, -1.0_dp, 0.0_dp]
      ucx = [0.0_dp, 0.0_dp, 0.2_dp, 0.4_dp, 0.0_dp, 0.2_dp, 0.4_dp]
      ucy = 0.0_dp * ucx
      bl = [-2.0_dp, -2.0_dp]
      hs = [2.0_dp, 2.0_dp]
      s1 = [0.0_dp, 0.0_dp]
      s0 = s1

      nbndu = 1
      kbndu = reshape([BOUNDARY_CELL, INTERIOR_CELL, BOUNDARY_LINK, BOUNDARY_ABSORBING_GENERATING, &
                      BOUNDARY_SECTION], [5, nbndu])
      zbndu = [-0.75_dp]
      csu = [1.0_dp]
      snu = [0.0_dp]
      wu = [1.0_dp]
      u1 = [0.0_dp]
      v = [0.4_dp]
      u1rm = [0.0_dp]
      kbndu2kbndw = [1]
      uin = [0.5_dp]
      vin = [0.0_dp]

      absgentype = 'abs_1d'
      freewave = 0
      ag = 2.0_dp
      epshu = 1.0e-4_dp
      cgwav = [2.0_dp, 2.0_dp]
      cats = 4.0_dp
      Trep = 1.0_dp
      dts = 1.0_dp

      call xbeach_reset()
      call xbeach_flow_bc()

      call f90_expect_near(u1(BOUNDARY_LINK), 0.25_dp, TOLERANCE, &
                           'ABSGEN section must apply the characteristic velocity')

      call xbeach_flow_bc()
      call f90_expect_near(u1(BOUNDARY_LINK), 0.3125_dp, TOLERANCE, &
                           'The next boundary update must include the relaxed normal mean')
      call f90_expect_near(vmeanrm(BOUNDARY_SECTION), 0.175_dp, TOLERANCE, &
                           'The tangential mean must retain history across updates')

      call xbeach_reset()
      call f90_expect_near(umeanrm(BOUNDARY_SECTION), 0.0_dp, TOLERANCE, 'Reset clears the normal mean')
      call f90_expect_near(vmeanrm(BOUNDARY_SECTION), 0.0_dp, TOLERANCE, 'Reset clears the tangential mean')

      freewave = 1
      cgwav = 1.0_dp
      call xbeach_flow_bc()
      call f90_expect_near(u1(BOUNDARY_LINK), 0.25_dp, TOLERANCE, 'A free wave in 3D uses the shallow-water speed')

      call xbeach_reset()
      kmx = 0
      call xbeach_flow_bc()
      call f90_expect_near(u1(BOUNDARY_LINK), 0.75_dp, TOLERANCE, 'Legacy 2D ABSGEN retains the group-speed gain')
   end subroutine test_absgen_section

   !$gtest
   !> Apply active absorption at grazing incidence and across a dry/rewet transition.
   subroutine test_absgen_arc_faces() bind(C)
      use fm_external_forcings_data, only: kbndu, nbndu, zbndu
      use m_boundary_condition_type, only: BOUNDARY_ABSORBING_GENERATING
      use m_flow, only: cfuhi, hu, kmx, s1, u1, ucx, ucy, v
      use m_flowgeom, only: acl, bl, csu, dxi, nd, ndx, snu, wu
      use m_flowparameters, only: epshu
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi
      use m_physcoef, only: ag, rhomean
      use m_waves, only: ustokes, vstokes
      use m_wind, only: jawind
      use m_xbeach_data, only: ARC, absgentype, cats, dlengthrm, freewave, Fx_cc, Fy_cc, kbndu2kbndw, &
                               order, Trep, u1rm, uin, umeanrm, vin, vmeanrm
      use m_xbeachwaves, only: xbeach_flow_bc, xbeach_reset

      integer, parameter :: BOUNDARY_SECTION = 1
      real(kind=dp), parameter :: TOLERANCE = 1.0e-12_dp

      jampi = 0
      kmx = 0
      ndx = 4
      if (allocated(nd)) then
         deallocate (nd)
      end if
      allocate (nd(ndx))
      nd%lnx = 0
      bl = [-1.0_dp, -1.0_dp, -1.0_dp, -1.0_dp]
      s1 = 0.0_dp * bl
      ucx = 0.0_dp * bl
      ucy = 0.0_dp * bl

      nbndu = 2
      kbndu = reshape([3, 1, 1, BOUNDARY_ABSORBING_GENERATING, BOUNDARY_SECTION, &
                       4, 2, 2, BOUNDARY_ABSORBING_GENERATING, BOUNDARY_SECTION], [5, nbndu])
      zbndu = [0.0_dp, 0.0_dp]
      hu = [1.0_dp, 1.0_dp]
      csu = [1.0_dp, 1.0_dp]
      snu = [0.0_dp, 0.0_dp]
      wu = [1.0_dp, 3.0_dp]
      dxi = [1.0_dp, 1.0_dp]
      acl = [0.5_dp, 0.5_dp]
      u1 = [2.0_dp, 1.0_dp]
      v = [0.0_dp, 0.0_dp]
      cfuhi = [0.0_dp, 0.0_dp]
      ustokes = [0.0_dp, 0.0_dp]
      vstokes = [0.0_dp, 0.0_dp]
      Fx_cc = 0.0_dp * bl
      Fy_cc = 0.0_dp * bl
      u1rm = [0.0_dp, 0.0_dp]
      kbndu2kbndw = [1, 2]
      uin = [0.0_dp, 0.0_dp]
      vin = [1.0_dp, 0.0_dp]

      absgentype = 'abs_2d'
      ARC = 1
      freewave = 1
      order = 2
      jawind = 0
      ag = 1.0_dp
      rhomean = 1000.0_dp
      epshu = 1.0e-4_dp
      cats = 4.0_dp
      Trep = 1.0_dp
      dts = 1.0_dp

      call xbeach_reset()
      call xbeach_flow_bc()
      call f90_expect_near(u1, [0.75_dp, 0.5_dp], TOLERANCE, 'Grazing forcing must use the finite reflected branch')

      hu(1) = 0.0_dp
      v(1) = 10.0_dp
      call xbeach_flow_bc()
      call f90_expect_near(dlengthrm(BOUNDARY_SECTION), 3.0_dp, TOLERANCE, 'Only wet face widths enter the section mean')
      call f90_expect_near(umeanrm(BOUNDARY_SECTION), 0.125_dp, TOLERANCE, 'The dry face must not bias the normal mean')
      call f90_expect_near(vmeanrm(BOUNDARY_SECTION), 0.0_dp, TOLERANCE, 'The dry face must not bias the tangential mean')
      call f90_expect_near(u1rm(1), 0.0_dp, TOLERANCE, 'Drying clears the last wet normal velocity')

      hu(2) = 0.0_dp
      call xbeach_flow_bc()
      call f90_expect_near(umeanrm(BOUNDARY_SECTION), 0.125_dp, TOLERANCE, 'A fully dry section retains its relaxed mean')

      hu(1) = 1.0_dp
      v(1) = 0.0_dp
      u1(1) = 2.0_dp
      call xbeach_flow_bc()
      call f90_expect_near(umeanrm(BOUNDARY_SECTION), 0.09375_dp, TOLERANCE, 'Rewetting must not reuse the old wet sample')
   end subroutine test_absgen_arc_faces

   !$gtest
   !> Skip the cell-velocity snapshot when the only flow boundary is another type.
   subroutine test_absgen_no_faces() bind(C)
      use fm_external_forcings_data, only: kbndu, nbndu
      use m_flow, only: kmx, u1
      use m_flowgeom, only: ndx
      use m_partitioninfo, only: jampi
      use m_xbeachwaves, only: xbeach_flow_bc

      jampi = 0
      kmx = 2
      ndx = 1
      nbndu = 1
      kbndu = reshape([1, 1, 1, 0, 1], [5, nbndu])
      u1 = [2.0_dp]

      call xbeach_flow_bc()

      call f90_expect_near(u1(1), 2.0_dp, 1.0e-12_dp, 'ABSGEN must leave other boundary types alone')
   end subroutine test_absgen_no_faces

   !$gtest
   !> Verify that the corrected layer velocities carry the prescribed transport.
   subroutine test_absgen_transport() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp), parameter :: target_mean = 1.25_dp
      real(kind=dp) :: area(3)
      real(kind=dp) :: current_velocity(3)
      real(kind=dp) :: target_velocity(3)
      real(kind=dp) :: resulting_mean

      area = [1.0_dp, 2.0_dp, 3.0_dp]
      current_velocity = [-0.4_dp, 0.2_dp, 0.8_dp]

      call apply_absgen_barotropic_increment(area, current_velocity, target_mean, target_velocity)

      resulting_mean = dot_product(area, target_velocity) / sum(area)
      call f90_expect_near(resulting_mean, target_mean, tolerance, &
                           "Absgen layer profile does not conserve the target transport")
   end subroutine test_absgen_transport

   !$gtest
   !> Verify that imposing the ABSGEN mean retains the predictor's shear.
   subroutine test_absgen_pred_force() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp), parameter :: target_mean = 1.25_dp, time_step = 0.25_dp
      real(kind=dp) :: area(3), previous_velocity(3), acceleration(3), predicted_velocity(3), target_velocity(3)

      area = [1.0_dp, 2.0_dp, 1.0_dp]
      previous_velocity = 0.2_dp
      acceleration = [-0.3_dp, 0.1_dp, 0.1_dp]
      predicted_velocity = previous_velocity + time_step * acceleration

      call apply_absgen_barotropic_increment(area, predicted_velocity, target_mean, target_velocity)

      call f90_expect_near(target_velocity, [1.175_dp, 1.275_dp, 1.275_dp], tolerance, &
                           "Absgen discarded zero-mean momentum forcing in the predictor")
      call f90_expect_near(dot_product(area, target_velocity) / sum(area), target_mean, tolerance, &
                           "Absgen predictor projection changed the prescribed transport")
      call f90_expect_near(predicted_velocity, [0.125_dp, 0.225_dp, 0.225_dp], tolerance, &
                           "Absgen projection modified its momentum predictor input")
   end subroutine test_absgen_pred_force

   !$gtest
   !> Verify that a single wet layer takes the prescribed mean velocity.
   subroutine test_absgen_one_layer() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: area(1)
      real(kind=dp) :: current_velocity(1)
      real(kind=dp) :: target_velocity(1)

      area = [2.0_dp]
      current_velocity = [-0.4_dp]

      call apply_absgen_barotropic_increment(area, current_velocity, 1.2_dp, target_velocity)

      call f90_expect_near(target_velocity, [1.2_dp], tolerance, &
                           "Single-layer absgen projection did not set the target velocity")
   end subroutine test_absgen_one_layer

   !$gtest
   !> Verify thickness-weighted averaging without modifying input velocities.
   subroutine test_absgen_depth_average() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: interface_elevation(3)
      real(kind=dp) :: layer_velocity_x(2)
      real(kind=dp) :: layer_velocity_y(2)
      real(kind=dp) :: depth_averaged_velocity_x
      real(kind=dp) :: depth_averaged_velocity_y

      interface_elevation = [0.0_dp, 1.0_dp, 3.0_dp]
      layer_velocity_x = [1.0_dp, 3.0_dp]
      layer_velocity_y = [-2.0_dp, 2.0_dp]

      call compute_absgen_depth_averaged_velocity(interface_elevation, layer_velocity_x, layer_velocity_y, &
                                                  depth_averaged_velocity_x, depth_averaged_velocity_y)

      call f90_expect_near(depth_averaged_velocity_x, 7.0_dp / 3.0_dp, tolerance, &
                           "Absgen x velocity snapshot has an incorrect depth average")
      call f90_expect_near(depth_averaged_velocity_y, 2.0_dp / 3.0_dp, tolerance, &
                           "Absgen y velocity snapshot has an incorrect depth average")
      call f90_expect_near(interface_elevation, [0.0_dp, 1.0_dp, 3.0_dp], tolerance, &
                           "Absgen depth averaging modified interface elevations")
      call f90_expect_near(layer_velocity_x, [1.0_dp, 3.0_dp], tolerance, &
                           "Absgen depth averaging modified x velocity state")
      call f90_expect_near(layer_velocity_y, [-2.0_dp, 2.0_dp], tolerance, &
                           "Absgen depth averaging modified y velocity state")
   end subroutine test_absgen_depth_average

   !$gtest
   !> Verify that the transport correction preserves layer velocity differences.
   subroutine test_absgen_shear() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: area(3)
      real(kind=dp) :: current_velocity(3)
      real(kind=dp) :: target_velocity(3)

      area = [0.5_dp, 1.5_dp, 2.0_dp]
      current_velocity = [-0.5_dp, 0.25_dp, 1.0_dp]

      call apply_absgen_barotropic_increment(area, current_velocity, -0.3_dp, target_velocity)

      call f90_expect_near(target_velocity(2) - target_velocity(1), &
                           current_velocity(2) - current_velocity(1), tolerance, &
                           "Absgen layer projection changed the lower-layer shear")
      call f90_expect_near(target_velocity(3) - target_velocity(2), &
                           current_velocity(3) - current_velocity(2), tolerance, &
                           "Absgen layer projection changed the upper-layer shear")
   end subroutine test_absgen_shear

   !$gtest
   !> Verify the uniform velocity fallback when the layer areas are zero.
   subroutine test_absgen_dry() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp), parameter :: target_mean = 0.7_dp
      real(kind=dp) :: area(2)
      real(kind=dp) :: current_velocity(2)
      real(kind=dp) :: target_velocity(2)

      area = 0.0_dp
      current_velocity = [-2.0_dp, 3.0_dp]

      call apply_absgen_barotropic_increment(area, current_velocity, target_mean, target_velocity)

      call f90_expect_near(target_velocity, [target_mean, target_mean], tolerance, &
                           "Dry absgen layers did not receive a finite fallback target")
   end subroutine test_absgen_dry

   !$gtest
   !> Verify relaxation of section means and retention across empty sections.
   subroutine test_absgen_mean_relax() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: weighted_u(3)
      real(kind=dp) :: weighted_v(3)
      real(kind=dp) :: boundary_length(3)
      real(kind=dp) :: mean_u(3)
      real(kind=dp) :: mean_v(3)

      ! These totals represent the result after summing contributions from
      ! multiple MPI ranks. Section 2 is absent and must retain its history.
      weighted_u = [2.0_dp, 0.0_dp, 12.0_dp]
      weighted_v = [-4.0_dp, 0.0_dp, 4.0_dp]
      boundary_length = [2.0_dp, 0.0_dp, 4.0_dp]
      mean_u = [5.0_dp, -3.0_dp, 1.0_dp]
      mean_v = [2.0_dp, 7.0_dp, -1.0_dp]

      call relax_absgen_boundary_means(weighted_u, weighted_v, boundary_length, &
                                        0.25_dp, mean_u, mean_v)

      call f90_expect_near(mean_u, [4.0_dp, -3.0_dp, 1.5_dp], tolerance, &
                           "Absgen normal boundary means were relaxed incorrectly")
      call f90_expect_near(mean_v, [1.0_dp, 7.0_dp, -0.5_dp], tolerance, &
                           "Absgen tangential boundary means were relaxed incorrectly")
   end subroutine test_absgen_mean_relax

   !$gtest
   !> Verify that only owned faces contribute to means across MPI partitions.
   subroutine test_absgen_owned_mean() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: rank_one_u(4)
      real(kind=dp) :: rank_one_v(4)
      real(kind=dp) :: rank_one_length(4)
      real(kind=dp) :: rank_two_u(4)
      real(kind=dp) :: rank_two_v(4)
      real(kind=dp) :: rank_two_length(4)
      real(kind=dp) :: mean_u(4)
      real(kind=dp) :: mean_v(4)

      rank_one_u = 0.0_dp
      rank_one_v = 0.0_dp
      rank_one_length = 0.0_dp
      rank_two_u = 0.0_dp
      rank_two_v = 0.0_dp
      rank_two_length = 0.0_dp

      call accumulate_absgen_boundary_mean(.true., 1, 2.0_dp, 1.0_dp, -2.0_dp, &
                                           rank_one_u, rank_one_v, rank_one_length)
      call accumulate_absgen_boundary_mean(.false., 1, 2.0_dp, 99.0_dp, 99.0_dp, &
                                           rank_two_u, rank_two_v, rank_two_length)
      call accumulate_absgen_boundary_mean(.true., 4, 1.0_dp, 3.0_dp, 4.0_dp, &
                                           rank_one_u, rank_one_v, rank_one_length)
      call accumulate_absgen_boundary_mean(.true., 4, 3.0_dp, 1.0_dp, 0.0_dp, &
                                           rank_two_u, rank_two_v, rank_two_length)

      rank_one_u = rank_one_u + rank_two_u
      rank_one_v = rank_one_v + rank_two_v
      rank_one_length = rank_one_length + rank_two_length
      mean_u = [8.0_dp, -3.0_dp, 6.0_dp, 9.0_dp]
      mean_v = [7.0_dp, 5.0_dp, -4.0_dp, 8.0_dp]

      call relax_absgen_boundary_means(rank_one_u, rank_one_v, rank_one_length, 1.0_dp, mean_u, mean_v)

      call f90_expect_near(mean_u, [1.0_dp, -3.0_dp, 6.0_dp, 1.5_dp], tolerance, &
                           "Owned absgen normal means or sparse sections were incorrect")
      call f90_expect_near(mean_v, [-2.0_dp, 5.0_dp, -4.0_dp, 1.0_dp], tolerance, &
                           "Owned absgen tangential means or sparse sections were incorrect")
   end subroutine test_absgen_owned_mean

   !$gtest
   !> Verify the sign of the normal friction term relative to Stokes drift.
   subroutine test_absgen_drag() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp) :: acceleration

      acceleration = compute_absgen_friction_acceleration(0.04_dp, 2.0_dp, 1.0_dp, 0.5_dp, -1.0_dp)
      call f90_expect_near(acceleration, 0.15_dp, tolerance, &
                           "Absgen depth-averaged friction acceleration was incorrect")

      acceleration = compute_absgen_friction_acceleration(0.04_dp, -1.0_dp, 1.0_dp, 0.5_dp, -1.0_dp)
      call f90_expect_near(acceleration, -0.15_dp, tolerance, &
                           "Absgen friction acceleration did not preserve the normal sign")
   end subroutine test_absgen_drag

   !$gtest
   !> Verify the signed incident velocity for opposing and grazing phases.
   subroutine test_absgen_incident() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp
      real(kind=dp), parameter :: cosine_floor = 1.0e-6_dp

      call f90_expect_near(compute_absgen_incident_velocity(3.0_dp, 4.0_dp, cosine_floor), 5.0_dp, tolerance, &
                           "Forward incident-wave velocity was incorrect")

      call f90_expect_near(compute_absgen_incident_velocity(-3.0_dp, -4.0_dp, cosine_floor), -5.0_dp, tolerance, &
                           "Reversed incident-wave velocity lost its sign")

      call f90_expect_near(compute_absgen_incident_velocity(0.0_dp, 0.0_dp, cosine_floor), 0.0_dp, tolerance, &
                           "Zero incident velocity produced a nonzero magnitude")

      call f90_expect_near(compute_absgen_incident_velocity(1.0e-12_dp, 1.0_dp, cosine_floor), 1.0e-6_dp, tolerance, &
                           "Grazing incident velocity was not bounded")
   end subroutine test_absgen_incident

   !$gtest
   !> Verify reflected velocities on both sides of the tangential threshold.
   subroutine test_absgen_reflection() bind(C)
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp

      call f90_expect_near(compute_absgen_reflected_velocity(-2.0_dp, 1.0_dp), -0.75_dp, tolerance, &
                           'The reflected normal velocity follows the sign of the characteristic forcing')
      call f90_expect_near(compute_absgen_reflected_velocity(1.01_dp, -1.0_dp), 0.00995049504950495_dp, tolerance, &
                           'The reflected normal velocity tapers continuously toward zero')
      call f90_expect_near(compute_absgen_reflected_velocity(1.0_dp, -1.0_dp), 0.0_dp, tolerance, &
                           'The reflected normal velocity vanishes at the threshold')
      call f90_expect_near(compute_absgen_reflected_velocity(0.5_dp, -1.0_dp), 0.0_dp, tolerance, &
                           'The reflected normal velocity vanishes below the threshold')
   end subroutine test_absgen_reflection

end module test_absgen_3d
