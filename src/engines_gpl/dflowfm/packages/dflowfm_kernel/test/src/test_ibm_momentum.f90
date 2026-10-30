module test_ibm_momentum
   use assertions_gtest
   use precision, only: dp
   use m_immersed_boundary, only: ibm_build_geometry, ibm_set_perot_weights, t_ibm_state
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

   subroutine reconstruct_perot(polygon_x, polygon_y, wet_width, ux, uy, ucx, ucy, valid, nseg, reason)
      real(kind=dp), dimension(:), intent(in) :: polygon_x, polygon_y !< Wet-polygon vertices in counter-clockwise order.
      real(kind=dp), dimension(4), intent(in) :: wet_width !< Wet widths for west, east, south and north faces.
      real(kind=dp), intent(in) :: ux, uy !< Constant Cartesian velocity sampled at the four faces.
      real(kind=dp), intent(out) :: ucx, ucy !< Reconstructed cell-centre velocity.
      logical, intent(out) :: valid !< Whether the geometry and Perot weights were accepted.
      integer, intent(out) :: nseg !< Number of immersed segments in the cut cell.
      character(len=*), intent(out) :: reason !< Failure reason when geometry or weights are rejected.

      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(1) :: area_full, area_used
      real(kind=dp), dimension(4) :: width_full, width_used
      real(kind=dp), dimension(4) :: wcx1, wcy1, wcx2, wcy2, normal_velocity
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(:, :), allocatable :: wet_x, wet_y
      type(t_ibm_state) :: state
      integer :: error_index

      ucx = 0.0_dp
      ucy = 0.0_dp
      valid = .false.
      nseg = 0
      reason = ''
      allocate (wet_x(size(polygon_x), 1), wet_y(size(polygon_y), 1))
      wet_x(:, 1) = polygon_x
      wet_y(:, 1) = polygon_y
      wet_count = [size(polygon_x)]

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      area_full = 100.0_dp
      area_used = polygon_area(polygon_x, polygon_y)
      link_cells = 0
      link_cells(1, :) = 1
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
      ucx = sum(wcx1 * normal_velocity)
      ucy = sum(wcy1 * normal_velocity)
   end subroutine reconstruct_perot

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
