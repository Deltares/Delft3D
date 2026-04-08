module test_dflowfm_dambreak
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_skip_1d_flowlinks, test_skip_1d_flowlinks, test_skip_1d_flowlinks,
   !> Test whether remove_1d_links_from_dambreak_polygon_list removes 1D flow links from the list of polygon vertices in a dambreak test case. This is necessary to prevent the 1D flow links
   subroutine test_skip_1d_flowlinks() bind(C)
      use precision, only: dp
      use m_flowgeom, only: kcu
      use m_dambreak_breach, only: remove_1d_links_from_dambreak_polygon_list
      integer, parameter :: PATH_LENGTH = 2
      integer :: num_cells
      integer, dimension(:), allocatable :: cell_numbers
      real(kind=dp), parameter :: tolerance = 1e-8_dp
      real(kind=dp), dimension(PATH_LENGTH) :: x, y, running_distance, expected_running_distance

      cell_numbers = [1, 2, 3, 4, 5, 6]
      kcu = [2, 4, 2, 3, 1, 2]
      num_cells = size(cell_numbers)
      call remove_1d_links_from_dambreak_polygon_list(num_cells, cell_numbers)
      call f90_expect_eq(num_cells, 4, "number of 2d and 1d2d cells must be equal to 4")
      call f90_expect_eq(cell_numbers(1), 1, "cell number(1) must be 1")
      call f90_expect_eq(cell_numbers(2), 3, "cell number(2) must be 3")
      call f90_expect_eq(cell_numbers(3), 4, "cell number(3) must be 4")
      call f90_expect_eq(cell_numbers(4), 6, "cell number(4) must be 6")
   end subroutine test_skip_1d_flowlinks
   !$f90tw)
end module test_dflowfm_dambreak
