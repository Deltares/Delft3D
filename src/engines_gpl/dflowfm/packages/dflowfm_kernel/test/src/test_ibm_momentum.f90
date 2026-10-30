module test_ibm_momentum
   use assertions_gtest
   use precision, only: dp
   use m_immersed_boundary, only: ibm_build_geometry, ibm_measure_wet_polygon, ibm_set_perot_weights, &
                                 ibm_set_wet_link_metrics, t_ibm_cut_cell, t_ibm_state
   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_perot_triangle_tangent, test_perot_triangle_tangent,
   subroutine test_perot_triangle_tangent() bind(C)
      real(kind=dp), dimension(3) :: polygon_x, polygon_y
      real(kind=dp), dimension(4) :: wet_width
      real(kind=dp) :: ucx, ucy
      integer :: nseg
      logical :: valid
      character(len=256) :: reason

      polygon_x = [0.0_dp, 10.0_dp, 10.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_width = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call reconstruct_perot(polygon_x, polygon_y, wet_width, sqrt(0.5_dp), sqrt(0.5_dp), ucx, ucy, valid, nseg, reason)

      call F90_ASSERT_TRUE(valid, 'Triangle Perot geometry was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_EQ(nseg, 1, 'Expected one immersed segment in the triangle Perot test.')
         call F90_ASSERT_NEAR(ucx, sqrt(0.5_dp), 1.0e-13_dp, 'Triangle Perot reconstruction changed tangent x velocity.')
         call F90_ASSERT_NEAR(ucy, sqrt(0.5_dp), 1.0e-13_dp, 'Triangle Perot reconstruction changed tangent y velocity.')
      end if
   end subroutine test_perot_triangle_tangent
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_perot_trapezoid_tangent, test_perot_trapezoid_tangent,
   subroutine test_perot_trapezoid_tangent() bind(C)
      real(kind=dp), dimension(4) :: polygon_x, polygon_y
      real(kind=dp), dimension(4) :: wet_width
      real(kind=dp) :: ux, uy, ucx, ucy
      integer :: nseg
      logical :: valid
      character(len=256) :: reason

      polygon_x = [0.0_dp, 10.0_dp, 10.0_dp, 2.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_width = [0.0_dp, 10.0_dp, 10.0_dp, 8.0_dp]
      ux = 2.0_dp / sqrt(104.0_dp)
      uy = 10.0_dp / sqrt(104.0_dp)
      call reconstruct_perot(polygon_x, polygon_y, wet_width, ux, uy, ucx, ucy, valid, nseg, reason)

      call F90_ASSERT_TRUE(valid, 'Trapezoid Perot geometry was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_EQ(nseg, 1, 'Expected one immersed segment in the trapezoid Perot test.')
         call F90_ASSERT_NEAR(ucx, ux, 1.0e-13_dp, 'Trapezoid Perot reconstruction changed tangent x velocity.')
         call F90_ASSERT_NEAR(ucy, uy, 1.0e-13_dp, 'Trapezoid Perot reconstruction changed tangent y velocity.')
      end if
   end subroutine test_perot_trapezoid_tangent
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_perot_pentagon_tangent, test_perot_pentagon_tangent,
   subroutine test_perot_pentagon_tangent() bind(C)
      real(kind=dp), dimension(5) :: polygon_x, polygon_y
      real(kind=dp), dimension(4) :: wet_width
      real(kind=dp) :: tangent_velocity, ucx, ucy
      integer :: nseg
      logical :: valid
      character(len=256) :: reason

      polygon_x = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp]
      wet_width = [5.0_dp, 10.0_dp, 10.0_dp, 5.0_dp]
      tangent_velocity = sqrt(0.5_dp)
      call reconstruct_perot(polygon_x, polygon_y, wet_width, tangent_velocity, tangent_velocity, ucx, ucy, valid, nseg, reason)

      call F90_ASSERT_TRUE(valid, 'Pentagon Perot geometry was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_EQ(nseg, 1, 'Expected one immersed segment in the pentagon Perot test.')
         call F90_ASSERT_NEAR(ucx, tangent_velocity, 1.0e-13_dp, 'Pentagon Perot reconstruction changed tangent x velocity.')
         call F90_ASSERT_NEAR(ucy, tangent_velocity, 1.0e-13_dp, 'Pentagon Perot reconstruction changed tangent y velocity.')
      end if
   end subroutine test_perot_pentagon_tangent
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_perot_normal_component, test_perot_normal_component,
   subroutine test_perot_normal_component() bind(C)
      real(kind=dp), dimension(3) :: polygon_x, polygon_y
      real(kind=dp), dimension(4) :: wet_width
      real(kind=dp) :: ucx, ucy
      integer :: nseg
      logical :: valid
      character(len=256) :: reason

      polygon_x = [0.0_dp, 10.0_dp, 10.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_width = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call reconstruct_perot(polygon_x, polygon_y, wet_width, 1.0_dp, 0.0_dp, ucx, ucy, valid, nseg, reason)

      call F90_ASSERT_TRUE(valid, 'Normal-component Perot geometry was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_EQ(nseg, 1, 'Expected one immersed segment in the normal-component Perot test.')
         call F90_ASSERT_NEAR(ucx, 2.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected analytic Perot x velocity with wall-normal flow.')
         call F90_ASSERT_NEAR(ucy, 1.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected analytic Perot y velocity with wall-normal flow.')
      end if
   end subroutine test_perot_normal_component
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_perot_two_segment_corner, test_perot_two_segment_corner,
   subroutine test_perot_two_segment_corner() bind(C)
      real(kind=dp), dimension(5) :: polygon_x, polygon_y
      real(kind=dp), dimension(4) :: wet_width
      real(kind=dp) :: ucx, ucy
      integer :: nseg
      logical :: valid
      character(len=256) :: reason

      polygon_x = [0.0_dp, 10.0_dp, 10.0_dp, 4.0_dp, 0.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 4.0_dp, 8.0_dp]
      wet_width = [8.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call reconstruct_perot(polygon_x, polygon_y, wet_width, 1.0_dp, 0.5_dp, ucx, ucy, valid, nseg, reason)

      call F90_ASSERT_TRUE(valid, 'Two-segment corner Perot geometry was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_EQ(nseg, 2, 'Expected two perpendicular immersed segments in the corner Perot test.')
         call F90_ASSERT_NEAR(ucx, 3007.0_dp / 2178.0_dp, 1.0e-13_dp, &
                              'Unexpected analytic Perot x velocity for the two-segment corner.')
         call F90_ASSERT_NEAR(ucy, 470.0_dp / 1089.0_dp, 1.0e-13_dp, &
                              'Unexpected analytic Perot y velocity for the two-segment corner.')
      end if
   end subroutine test_perot_two_segment_corner
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_momentum, test_wet_centroid_gradient_decomposition, test_wet_centroid_gradient_decomposition,
   subroutine test_wet_centroid_gradient_decomposition() bind(C)
      real(kind=dp), dimension(5) :: polygon_x, polygon_y
      real(kind=dp) :: normal_gradient, decomposition_error, uncut_error
      integer :: nvertices, cut_type
      logical :: valid
      character(len=256) :: reason

      do cut_type = 1, 4
         select case (cut_type)
         case (1)
            polygon_x(1:3) = [0.0_dp, 10.0_dp, 10.0_dp]
            polygon_y(1:3) = [0.0_dp, 0.0_dp, 10.0_dp]
            nvertices = 3
         case (2)
            polygon_x(1:4) = [0.0_dp, 10.0_dp, 10.0_dp, 2.0_dp]
            polygon_y(1:4) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
            nvertices = 4
         case (3)
            polygon_x = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
            polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp]
            nvertices = 5
         case (4)
            polygon_x = [0.0_dp, 10.0_dp, 10.0_dp, 4.0_dp, 0.0_dp]
            polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 4.0_dp, 8.0_dp]
            nvertices = 5
         end select

         call wet_centroid_gradient_errors(polygon_x(1:nvertices), polygon_y(1:nvertices), normal_gradient, &
                                           decomposition_error, uncut_error, valid, reason)
         call F90_ASSERT_TRUE(valid, 'Wet-centroid gradient geometry was rejected: '//trim(reason))
         if (valid) then
            call F90_ASSERT_NEAR(normal_gradient, 2.0_dp, 1.0e-12_dp, &
                                 'Wet-centroid pressure gradient was not exact for a normal linear field.')
            call F90_ASSERT_NEAR(decomposition_error, 0.0_dp, 1.0e-12_dp, &
                                 'Wet-centroid pressure-gradient error did not match the tangential-offset term.')
            if (cut_type == 1) then
               call F90_ASSERT_TRUE(uncut_error > 0.5_dp, &
                                    'The uncut center distance did not expose the expected oblique-gradient error.')
            end if
         end if
      end do
   end subroutine test_wet_centroid_gradient_decomposition
   !$f90tw)

   subroutine reconstruct_perot(polygon_x, polygon_y, wet_width, ux, uy, ucx, ucy, valid, nseg, reason)
      use fm_external_forcings_data, only: nbndn, nbndt, nbndu, nbnduxy_external => nbnduxy, nbndz, zbnduxyval
      use m_flow, only: hs_global => hs, hu_global => hu, kmx_global => kmx, nbnduxy_flow => nbnduxy, &
                        qa_global => qa, ucx_global => ucx, ucxq_global => ucxq, ucxu_global => ucxu, &
                        ucy_global => ucy, ucyq_global => ucyq, ucyu_global => ucyu, u1_global => u1
      use m_flowgeom, only: iadv_global => iadv, kcu_global => kcu, ln_global => ln, lnx_global => lnx, &
                            lnx1d_global => lnx1d, ndxi_global => ndxi, wcx1_global => wcx1, &
                            wcx2_global => wcx2, wcy1_global => wcy1, wcy2_global => wcy2
      use m_flowparameters, only: ChangeVelocityAtStructures, jaZerozbndinflowadvection, jabarrieradvection, &
                                  jasedtrails, jarhoxu, limtypmom, map_write_settings, Perot_type, PEROT_VOLUME_BASED
      use m_missing, only: dmiss
      use m_setucxucyucxuucyu, only: setucxucyucxuucyu
      use m_sferic, only: jasfer3D
      use m_sobekdfm, only: nbnd1d2d
      real(kind=dp), dimension(:), intent(in) :: polygon_x, polygon_y !< Wet-polygon vertices in counter-clockwise order.
      real(kind=dp), dimension(4), intent(in) :: wet_width !< Wet widths for west, east, south and north faces.
      real(kind=dp), intent(in) :: ux, uy !< Constant Cartesian velocity sampled at the four faces.
      real(kind=dp), intent(out) :: ucx, ucy !< Reconstructed cell-centre velocity.
      logical, intent(out) :: valid !< Whether the geometry and Perot weights were accepted.
      integer, intent(out) :: nseg !< Number of immersed segments in the cut cell.
      character(len=*), intent(out) :: reason !< Failure reason when geometry or weights are rejected.

      integer, parameter :: ncell = 5, nlink = 4
      real(kind=dp), dimension(4, ncell) :: cell_corner_x, cell_corner_y
      integer, dimension(2, nlink) :: link_cells
      real(kind=dp), dimension(2, nlink) :: face_x, face_y
      real(kind=dp), dimension(ncell) :: area_full, area_used
      real(kind=dp), dimension(nlink) :: width_full, width_used
      real(kind=dp), dimension(nlink) :: wcx1, wcy1, wcx2, wcy2, normal_velocity
      integer, dimension(ncell) :: wet_count
      real(kind=dp), dimension(5, ncell) :: wet_x, wet_y
      type(t_ibm_state) :: state
      integer :: error_index, L

      ucx = 0.0_dp
      ucy = 0.0_dp
      valid = .false.
      nseg = 0
      reason = ''
      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [-10.0_dp, 0.0_dp, 0.0_dp, -10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 3) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 3) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 4) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 4) = [-10.0_dp, -10.0_dp, 0.0_dp, 0.0_dp]
      cell_corner_x(:, 5) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 5) = [10.0_dp, 10.0_dp, 20.0_dp, 20.0_dp]
      wet_x = 0.0_dp
      wet_y = 0.0_dp
      wet_x(1:size(polygon_x), 1) = polygon_x
      wet_y(1:size(polygon_y), 1) = polygon_y
      wet_x(1:4, 2:ncell) = cell_corner_x(:, 2:ncell)
      wet_y(1:4, 2:ncell) = cell_corner_y(:, 2:ncell)
      wet_count = [size(polygon_x), 4, 4, 4, 4]
      area_full = 100.0_dp
      area_used = area_full
      area_used(1) = polygon_area(polygon_x, polygon_y)

      link_cells = 0
      link_cells(1, :) = 1
      link_cells(2, :) = [2, 3, 4, 5]
      face_x(:, 1) = [0.0_dp, 0.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 10.0_dp]
      face_y(:, 2) = [0.0_dp, 10.0_dp]
      face_x(:, 3) = [0.0_dp, 10.0_dp]
      face_y(:, 3) = [0.0_dp, 0.0_dp]
      face_x(:, 4) = [10.0_dp, 0.0_dp]
      face_y(:, 4) = [10.0_dp, 10.0_dp]
      width_full = 10.0_dp
      width_used = wet_width

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      if (.not. valid) then
         return
      end if
      nseg = state%cut(1)%nseg
      state%ba_full = area_full

      wcx1 = 0.0_dp
      wcy1 = 0.0_dp
      wcx2 = 0.0_dp
      wcy2 = 0.0_dp
      call ibm_set_perot_weights(link_cells, width_used, wcx1, wcy1, wcx2, wcy2, state, valid, error_index, reason)
      if (.not. valid) then
         return
      end if

      normal_velocity = [-ux, ux, -uy, uy]
      if (allocated(ln_global)) then
         deallocate (ln_global)
      end if
      if (allocated(wcx1_global)) then
         deallocate (wcx1_global)
      end if
      if (allocated(wcy1_global)) then
         deallocate (wcy1_global)
      end if
      if (allocated(wcx2_global)) then
         deallocate (wcx2_global)
      end if
      if (allocated(wcy2_global)) then
         deallocate (wcy2_global)
      end if
      if (allocated(kcu_global)) then
         deallocate (kcu_global)
      end if
      if (allocated(iadv_global)) then
         deallocate (iadv_global)
      end if
      if (allocated(ucx_global)) then
         deallocate (ucx_global)
      end if
      if (allocated(ucy_global)) then
         deallocate (ucy_global)
      end if
      if (allocated(ucxq_global)) then
         deallocate (ucxq_global)
      end if
      if (allocated(ucyq_global)) then
         deallocate (ucyq_global)
      end if
      if (allocated(ucxu_global)) then
         deallocate (ucxu_global)
      end if
      if (allocated(ucyu_global)) then
         deallocate (ucyu_global)
      end if
      if (allocated(u1_global)) then
         deallocate (u1_global)
      end if
      if (allocated(hu_global)) then
         deallocate (hu_global)
      end if
      if (allocated(hs_global)) then
         deallocate (hs_global)
      end if
      if (allocated(qa_global)) then
         deallocate (qa_global)
      end if

      allocate (ln_global(2, nlink), wcx1_global(nlink), wcy1_global(nlink), wcx2_global(nlink), wcy2_global(nlink))
      allocate (kcu_global(nlink), iadv_global(nlink))
      allocate (ucx_global(ncell), ucy_global(ncell), ucxq_global(ncell), ucyq_global(ncell), hs_global(ncell))
      allocate (ucxu_global(nlink), ucyu_global(nlink), u1_global(nlink), hu_global(nlink), qa_global(nlink))
      ln_global = link_cells
      wcx1_global = wcx1
      wcy1_global = wcy1
      wcx2_global = wcx2
      wcy2_global = wcy2
      kcu_global = 0
      iadv_global = 0
      ucx_global = 0.0_dp
      ucy_global = 0.0_dp
      ucxq_global = 0.0_dp
      ucyq_global = 0.0_dp
      ucxu_global = 0.0_dp
      ucyu_global = 0.0_dp
      u1_global = 0.0_dp
      do L = 1, nlink
         if (width_used(L) > tiny(1.0_dp)) then
            u1_global(L) = normal_velocity(L)
         end if
      end do
      hu_global = 1.0_dp
      hs_global = 1.0_dp
      qa_global = 0.0_dp

      lnx_global = nlink
      lnx1d_global = 0
      ndxi_global = ncell
      kmx_global = 0
      Perot_type = PEROT_VOLUME_BASED
      ChangeVelocityAtStructures = .false.
      jabarrieradvection = 0
      jasedtrails = 0
      map_write_settings%ucmag = 0
      map_write_settings%ucvec = 0
      jaZerozbndinflowadvection = 0
      limtypmom = 0
      jarhoxu = 0
      jasfer3D = 0
      nbndz = 0
      nbndu = 0
      nbndt = 0
      nbndn = 0
      nbnduxy_flow = 0
      nbnduxy_external = 0
      nbnd1d2d = 0
      zbnduxyval = dmiss

      call setucxucyucxuucyu()
      ucx = ucx_global(1)
      ucy = ucy_global(1)
   end subroutine reconstruct_perot

   subroutine wet_centroid_gradient_errors(polygon_x, polygon_y, normal_gradient, decomposition_error, uncut_error, &
                                           valid, reason)
      real(kind=dp), dimension(:), intent(in) :: polygon_x, polygon_y !< Wet-polygon vertices in counter-clockwise order.
      real(kind=dp), intent(out) :: normal_gradient !< Recovered gradient when the surface gradient is normal to the link.
      real(kind=dp), intent(out) :: decomposition_error !< Difference from the predicted tangential-offset contribution.
      real(kind=dp), intent(out) :: uncut_error !< Error from using the uncut cell-centre distance.
      logical, intent(out) :: valid !< Whether geometry and wet-link metrics were accepted.
      character(len=*), intent(out) :: reason !< Failure reason when geometry or metrics are rejected.

      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(4) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(2) :: cell_x, cell_y
      real(kind=dp), dimension(1) :: normal_x, normal_y, width_used, acl_full, dxi_full
      real(kind=dp) :: normal_zeta1, normal_zeta2, oblique_zeta1, oblique_zeta2
      real(kind=dp) :: dn, oblique_gradient, expected_gradient, uncut_gradient
      type(t_ibm_cut_cell) :: cut_cell
      type(t_ibm_state) :: state
      integer :: error_index

      normal_gradient = 0.0_dp
      decomposition_error = 0.0_dp
      uncut_error = 0.0_dp
      valid = .false.
      reason = ''
      cell_corner_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_corner_x, cell_corner_y, polygon_x, polygon_y, 100.0_dp, &
                                   polygon_area(polygon_x, polygon_y), 0.0_dp, 0.0_dp, 1.0e-7_dp, cut_cell)
      if (.not. cut_cell%valid) then
         reason = cut_cell%reason
         return
      end if
      cut_cell%k = 1

      state%grid%dx = 10.0_dp
      state%grid%dy = 10.0_dp
      allocate (state%cell_class(2), state%cut(1), state%clink(1))
      state%cell_class = [1, 0]
      state%ncut = 1
      state%cut(1) = cut_cell
      state%clink(1)%L = 1
      state%clink(1)%xm = 10.0_dp
      state%clink(1)%ym = 5.0_dp

      link_cells(:, 1) = [1, 2]
      cell_x = [5.0_dp, 15.0_dp]
      cell_y = [5.0_dp, 5.0_dp]
      normal_x = 1.0_dp
      normal_y = 0.0_dp
      width_used = 10.0_dp
      acl_full = 0.5_dp
      dxi_full = 0.1_dp
      call ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                    state, valid, error_index, reason)
      if (.not. valid) then
         return
      end if

      dn = 1.0_dp / state%clink(1)%dxi_wet
      if (dn <= tiny(1.0_dp)) then
         valid = .false.
         reason = 'wet-centroid link distance is not positive'
         return
      end if
      normal_zeta1 = 2.0_dp * cut_cell%xc
      normal_zeta2 = 2.0_dp * cell_x(2)
      normal_gradient = (normal_zeta2 - normal_zeta1) / dn
      oblique_zeta1 = 2.0_dp * cut_cell%xc + 4.0_dp * cut_cell%yc
      oblique_zeta2 = 2.0_dp * cell_x(2) + 4.0_dp * cell_y(2)
      oblique_gradient = (oblique_zeta2 - oblique_zeta1) / dn
      expected_gradient = 2.0_dp + 4.0_dp * state%clink(1)%dt12 / dn
      decomposition_error = abs(oblique_gradient - expected_gradient)
      uncut_gradient = (oblique_zeta2 - (2.0_dp * cell_x(1) + 4.0_dp * cell_y(1))) * dxi_full(1)
      uncut_error = abs(uncut_gradient - expected_gradient)
   end subroutine wet_centroid_gradient_errors

   pure real(kind=dp) function polygon_area(polygon_x, polygon_y) result(area)
      real(kind=dp), dimension(:), intent(in) :: polygon_x, polygon_y

      integer :: i, next_i

      area = 0.0_dp
      do i = 1, size(polygon_x)
         next_i = mod(i, size(polygon_x)) + 1
         area = area + polygon_x(i) * polygon_y(next_i) - polygon_x(next_i) * polygon_y(i)
      end do
      area = 0.5_dp * area
   end function polygon_area

end module test_ibm_momentum
