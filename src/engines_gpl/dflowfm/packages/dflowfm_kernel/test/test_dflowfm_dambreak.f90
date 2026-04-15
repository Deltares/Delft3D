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

      cell_numbers = [1, 2, 3, 4, 5, 6, 7, 8]
      kcu = [2, 4, 2, 3, 1, 2, -1, -2 ]
      num_cells = size(cell_numbers)
      call remove_1d_links_from_dambreak_polygon_list(num_cells, cell_numbers)
      call f90_expect_eq(num_cells, 5, "number of cells must be equal to 5")
      call f90_expect_true(all(cell_numbers(1:num_cells) == [1, 3, 4, 6, 8]), "cells numbers must be equal to [1, 3, 4, 6, 8]")
   end subroutine test_skip_1d_flowlinks
   !$f90tw)
end module test_dflowfm_dambreak
