module test_ibm_cartesian
   use assertions_gtest
   use precision, only: dp
   use m_immersed_boundary, only: ibm_check_cartesian, t_ibm_cartesian
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_uniform_square_lattice, test_uniform_square_lattice,
   subroutine test_uniform_square_lattice() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 0, 0.0_dp, 0.0_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(result%is_cartesian, 'Uniform square lattice was rejected.')
      call F90_ASSERT_NEAR(result%dx, 10.0_dp, 1.0e-12_dp, 'Unexpected x spacing.')
      call F90_ASSERT_NEAR(result%dy, 10.0_dp, 1.0e-12_dp, 'Unexpected y spacing.')
      call F90_ASSERT_NEAR(result%x0, 0.0_dp, 1.0e-12_dp, 'Unexpected lattice x origin.')
      call F90_ASSERT_NEAR(result%y0, 0.0_dp, 1.0e-12_dp, 'Unexpected lattice y origin.')
   end subroutine test_uniform_square_lattice
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_uniform_rectilinear_lattice, test_uniform_rectilinear_lattice,
   subroutine test_uniform_rectilinear_lattice() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 20.0_dp, 0.0_dp, 0, 0.0_dp, 0.0_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(result%is_cartesian, 'Uniform rectangular lattice was rejected.')
      call F90_ASSERT_NEAR(result%dx, 10.0_dp, 1.0e-12_dp, 'Unexpected x spacing.')
      call F90_ASSERT_NEAR(result%dy, 20.0_dp, 1.0e-12_dp, 'Unexpected y spacing.')
   end subroutine test_uniform_rectilinear_lattice
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_rotated_lattice_is_rejected, test_rotated_lattice_is_rejected,
   subroutine test_rotated_lattice_is_rejected() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result
      real(kind=dp) :: angle

      angle = 10.0_dp * acos(-1.0_dp) / 180.0_dp
      call make_lattice(6, 6, 10.0_dp, 10.0_dp, angle, 0, 0.0_dp, 0.0_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(.not. result%is_cartesian, 'Rotated lattice was accepted.')
      call F90_ASSERT_TRUE(index(result%reason, 'axis alignment') > 0, 'Rotation failure did not identify axis alignment.')
   end subroutine test_rotated_lattice_is_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_perturbed_node_is_rejected, test_perturbed_node_is_rejected,
   subroutine test_perturbed_node_is_rejected() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 14, 0.0_dp, 1.0e-2_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(.not. result%is_cartesian, 'Perturbed lattice was accepted.')
      call F90_ASSERT_TRUE(result%offending_node > 0, 'Perturbed-node failure did not report a node.')
   end subroutine test_perturbed_node_is_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_near_cartesian_node_is_accepted, test_near_cartesian_node_is_accepted,
   subroutine test_near_cartesian_node_is_accepted() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 14, 0.0_dp, 1.0e-8_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(result%is_cartesian, 'Near-Cartesian lattice was rejected.')
   end subroutine test_near_cartesian_node_is_accepted
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_triangular_cell_is_rejected, test_triangular_cell_is_rejected,
   subroutine test_triangular_cell_is_rejected() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 0, 0.0_dp, 0.0_dp, 0, 1, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(.not. result%is_cartesian, 'Triangular cell was accepted.')
      call F90_ASSERT_TRUE(index(result%reason, '4 nodes') > 0, 'Triangle failure did not identify the four-node criterion.')
   end subroutine test_triangular_cell_is_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_nonuniform_lattice_is_rejected, test_nonuniform_lattice_is_rejected,
   subroutine test_nonuniform_lattice_is_rejected() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result
      integer :: j, node

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 0, 0.0_dp, 0.0_dp, 0, 0, x, y, links, cell_offsets, cell_nodes)
      do j = 1, 6
         node = 3 + (j - 1) * 6
         x(node) = x(node) + 1.0_dp
      end do
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(.not. result%is_cartesian, 'Nonuniform lattice was accepted.')
      call F90_ASSERT_TRUE(index(result%reason, 'lattice') > 0, 'Nonuniform failure did not identify the lattice criterion.')
   end subroutine test_nonuniform_lattice_is_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_cartesian, test_lattice_with_removed_cell_is_accepted, test_lattice_with_removed_cell_is_accepted,
   subroutine test_lattice_with_removed_cell_is_accepted() bind(C)
      real(kind=dp), dimension(:), allocatable :: x, y
      integer, dimension(:, :), allocatable :: links
      integer, dimension(:), allocatable :: cell_offsets, cell_nodes
      type(t_ibm_cartesian) :: result

      call make_lattice(6, 6, 10.0_dp, 10.0_dp, 0.0_dp, 0, 0.0_dp, 0.0_dp, 7, 0, x, y, links, cell_offsets, cell_nodes)
      call ibm_check_cartesian(x, y, links, cell_offsets, cell_nodes, 1.0e-6_dp, result)

      call F90_ASSERT_TRUE(result%is_cartesian, 'Cartesian lattice with a removed cell was rejected.')
   end subroutine test_lattice_with_removed_cell_is_accepted
   !$f90tw)

   subroutine make_lattice(nx, ny, dx, dy, rotation, perturb_node, perturb_x, perturb_y, skipped_cell, triangle_cell, &
                           x, y, link_nodes, cell_offsets, cell_nodes)
      integer, intent(in) :: nx !< Number of nodes in the x direction.
      integer, intent(in) :: ny !< Number of nodes in the y direction.
      real(kind=dp), intent(in) :: dx !< Lattice spacing in x.
      real(kind=dp), intent(in) :: dy !< Lattice spacing in y.
      real(kind=dp), intent(in) :: rotation !< Rotation angle in radians.
      integer, intent(in) :: perturb_node !< Node to perturb, or zero.
      real(kind=dp), intent(in) :: perturb_x !< x displacement of the perturbed node.
      real(kind=dp), intent(in) :: perturb_y !< y displacement of the perturbed node.
      integer, intent(in) :: skipped_cell !< Cell to omit, or zero.
      integer, intent(in) :: triangle_cell !< Cell to reduce to three nodes, or zero.
      real(kind=dp), dimension(:), allocatable, intent(out) :: x !< Generated x coordinates.
      real(kind=dp), dimension(:), allocatable, intent(out) :: y !< Generated y coordinates.
      integer, dimension(:, :), allocatable, intent(out) :: link_nodes !< Generated link endpoints.
      integer, dimension(:), allocatable, intent(out) :: cell_offsets !< Generated CSR offsets.
      integer, dimension(:), allocatable, intent(out) :: cell_nodes !< Generated cell connectivity.

      integer :: i, j, node, link, cell, active_cells, cell_node_count, offset
      real(kind=dp) :: xx, yy, cosine, sine

      allocate (x(nx * ny), y(nx * ny))
      cosine = cos(rotation)
      sine = sin(rotation)
      do j = 1, ny
         do i = 1, nx
            node = i + (j - 1) * nx
            xx = real(i - 1, kind=dp) * dx
            yy = real(j - 1, kind=dp) * dy
            x(node) = xx * cosine - yy * sine
            y(node) = xx * sine + yy * cosine
            if (node == perturb_node) then
               x(node) = x(node) + perturb_x
               y(node) = y(node) + perturb_y
            end if
         end do
      end do

      allocate (link_nodes(2, (nx - 1) * ny + nx * (ny - 1)))
      link = 0
      do j = 1, ny
         do i = 1, nx - 1
            link = link + 1
            link_nodes(:, link) = [i + (j - 1) * nx, i + 1 + (j - 1) * nx]
         end do
      end do
      do j = 1, ny - 1
         do i = 1, nx
            link = link + 1
            link_nodes(:, link) = [i + (j - 1) * nx, i + j * nx]
         end do
      end do

      active_cells = (nx - 1) * (ny - 1)
      if (skipped_cell > 0) then
         active_cells = active_cells - 1
      end if
      allocate (cell_offsets(active_cells + 1))
      cell_offsets(1) = 1
      cell = 0
      do j = 1, ny - 1
         do i = 1, nx - 1
            node = i + (j - 1) * (nx - 1)
            if (node == skipped_cell) then
               cycle
            end if
            cell = cell + 1
            cell_node_count = 4
            if (node == triangle_cell) then
               cell_node_count = 3
            end if
            cell_offsets(cell + 1) = cell_offsets(cell) + cell_node_count
         end do
      end do
      allocate (cell_nodes(cell_offsets(active_cells + 1) - 1))
      cell = 0
      offset = 1
      do j = 1, ny - 1
         do i = 1, nx - 1
            node = i + (j - 1) * (nx - 1)
            if (node == skipped_cell) then
               cycle
            end if
            cell = cell + 1
            if (node == triangle_cell) then
               cell_nodes(offset:offset + 2) = [i + (j - 1) * nx, i + 1 + (j - 1) * nx, i + 1 + j * nx]
               offset = offset + 3
            else
               cell_nodes(offset:offset + 3) = [i + (j - 1) * nx, i + 1 + (j - 1) * nx, i + 1 + j * nx, i + j * nx]
               offset = offset + 4
            end if
         end do
      end do
   end subroutine make_lattice

end module test_ibm_cartesian
