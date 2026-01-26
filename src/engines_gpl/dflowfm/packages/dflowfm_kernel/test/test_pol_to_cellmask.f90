module test_pol_to_cellmask
   use assertions_gtest
   use precision, only: dp
   use m_missing, only: dmiss
   use network_data, only: cellmask, npl, nump, xzw, yzw, xpl, ypl, zpl
   use m_cellmask_from_polygon_set, only: cellmask_from_polygon_set_init, cellmask_from_polygon_set, cellmask_from_polygon_set_cleanup
   use geometry_module, only: pinpok_legacy, pinpok_raycast

   implicit none(external)

contains

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_mixed_polygon, test_mixed_polygon,
   subroutine test_mixed_polygon() bind(C)
      ! Test with both enclosure (-1) and dry point (1) polygons
      integer :: i, k

      ! Setup test grid: 5x5 grid of cells
      nump = 25
      npl = 15 ! 5 points for enclosure + separator + 5 points for dry point + separators

      call realloc_polyline_arrays(nump, npl)

      ! Create 5x5 grid of cell centers
      do i = 1, 25
         xzw(i) = mod(i - 1, 5) * 10.0_dp + 5.0_dp ! x: 5, 15, 25, 35, 45
         yzw(i) = ((i - 1) / 5) * 10.0_dp + 5.0_dp ! y: 5, 15, 25, 35, 45
      end do

      ! Setup polygons:
      ! 1. Enclosure polygon (zpl=-1): covers cells at x=[0,30], y=[0,30]
      ! 2. Dry point polygon (zpl=1): inside enclosure at x=[10,20], y=[10,20]

      ! Enclosure polygon (rectangle from 0,0 to 30,30) with zpl=-1
      xpl(1) = 0.0_dp
      ypl(1) = 0.0_dp
      zpl(1) = -1.0_dp
      xpl(2) = 30.0_dp
      ypl(2) = 0.0_dp
      zpl(2) = -1.0_dp
      xpl(3) = 30.0_dp
      ypl(3) = 30.0_dp
      zpl(3) = -1.0_dp
      xpl(4) = 0.0_dp
      ypl(4) = 30.0_dp
      zpl(4) = -1.0_dp
      xpl(5) = 0.0_dp
      ypl(5) = 0.0_dp
      zpl(5) = -1.0_dp

      ! Separator
      xpl(6) = dmiss
      ypl(6) = dmiss
      zpl(6) = dmiss

      ! Dry point polygon (rectangle from 10,10 to 20,20) with zpl=1
      xpl(7) = 10.0_dp
      ypl(7) = 10.0_dp
      zpl(7) = 1.0_dp
      xpl(8) = 20.0_dp
      ypl(8) = 10.0_dp
      zpl(8) = 1.0_dp
      xpl(9) = 20.0_dp
      ypl(9) = 20.0_dp
      zpl(9) = 1.0_dp
      xpl(10) = 10.0_dp
      ypl(10) = 20.0_dp
      zpl(10) = 1.0_dp
      xpl(11) = 10.0_dp
      ypl(11) = 10.0_dp
      zpl(11) = 1.0_dp

      ! Separators
      xpl(12) = dmiss
      ypl(12) = dmiss
      zpl(12) = dmiss
      xpl(13) = dmiss
      ypl(13) = dmiss
      zpl(13) = dmiss
      xpl(14) = dmiss
      ypl(14) = dmiss
      zpl(14) = dmiss
      xpl(15) = dmiss
      ypl(15) = dmiss
      zpl(15) = dmiss

      ! Initialize polygon data structures
      call cellmask_from_polygon_set_init(NPL, xpl, ypl, zpl)

      ! Process all cells
      cellmask = 0
      cellmask = cellmask_from_polygon_set(xzw, yzw)

      ! Cleanup
      call cellmask_from_polygon_set_cleanup()

      ! Check results:
      ! Cell at (5,5) - inside enclosure, outside dry point -> mask=0
      call f90_expect_eq(cellmask(1), 0, "Cell (5,5) should not be masked")

      ! Cell at (15,15) - inside enclosure AND inside dry point -> mask=1
      call f90_expect_eq(cellmask(7), 1, "Cell (15,15) should be masked (dry point)")

      ! Cell at (35,5) - outside enclosure -> mask=1
      call f90_expect_eq(cellmask(4), 1, "Cell (35,5) should be masked (outside enclosure)")

      ! Cell at (25,25) - inside enclosure, outside dry point -> mask=0
      call f90_expect_eq(cellmask(13), 0, "Cell (25,25) should not be masked")

      ! Cleanup
      deallocate (xzw, yzw, xpl, ypl, zpl, cellmask)

   end subroutine test_mixed_polygon
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_nested_drypoint_polygons, test_nested_drypoint_polygons,
   subroutine test_nested_drypoint_polygons() bind(C)
      ! Test nested dry point polygons (odd-even rule)
      integer :: i, k

      ! Setup test grid: 5x5 grid of cells
      nump = 25

      ! define polygon length for allocation
      npl = 13

      call realloc_polyline_arrays(nump, npl)

      ! Create 5x5 grid of cell centers
      do i = 1, 25
         xzw(i) = mod(i - 1, 5) * 10.0_dp + 5.0_dp
         yzw(i) = ((i - 1) / 5) * 10.0_dp + 5.0_dp
      end do

      ! Setup nested dry point polygons:
      ! 1. Outer dry point polygon: x=[0,40], y=[0,40] with zpl=1
      ! 2. Inner dry point polygon: x=[10,30], y=[10,30] with zpl=1
      ! Cells inside odd number of polygons are masked

      ! Outer dry point polygon (rectangle from 0,0 to 40,40)
      xpl(1) = 0.0_dp
      ypl(1) = 0.0_dp
      zpl(1) = 1.0_dp
      xpl(2) = 40.0_dp
      ypl(2) = 0.0_dp
      zpl(2) = 1.0_dp
      xpl(3) = 40.0_dp
      ypl(3) = 40.0_dp
      zpl(3) = 1.0_dp
      xpl(4) = 0.0_dp
      ypl(4) = 40.0_dp
      zpl(4) = 1.0_dp
      xpl(5) = 0.0_dp
      ypl(5) = 0.0_dp
      zpl(5) = 1.0_dp

      ! Separator
      xpl(6) = dmiss
      ypl(6) = dmiss
      zpl(6) = dmiss

      ! Inner dry point polygon (rectangle from 10,10 to 30,30)
      xpl(7) = 10.0_dp
      ypl(7) = 10.0_dp
      zpl(7) = 1.0_dp
      xpl(8) = 30.0_dp
      ypl(8) = 10.0_dp
      zpl(8) = 1.0_dp
      xpl(9) = 30.0_dp
      ypl(9) = 30.0_dp
      zpl(9) = 1.0_dp
      xpl(10) = 10.0_dp
      ypl(10) = 30.0_dp
      zpl(10) = 1.0_dp
      xpl(11) = 10.0_dp
      ypl(11) = 10.0_dp
      zpl(11) = 1.0_dp

      ! Separators
      xpl(12) = dmiss
      ypl(12) = dmiss
      zpl(12) = dmiss
      xpl(13) = dmiss
      ypl(13) = dmiss
      zpl(13) = dmiss

      ! Initialize polygon data structures
      call cellmask_from_polygon_set_init(NPL, xpl, ypl, zpl)

      ! Process all cells
      cellmask = 0
      cellmask = cellmask_from_polygon_set(xzw, yzw)

      ! Cleanup
      call cellmask_from_polygon_set_cleanup()

      ! Check results (odd-even rule):
      ! Cell at (5,5) - inside 1 polygon (outer) -> mask=1
      call f90_expect_eq(cellmask(1), 1, "Cell (5,5) should be masked (inside 1 polygon)")

      ! Cell at (15,15) - inside 2 polygons (outer+inner) -> mask=0
      call f90_expect_eq(cellmask(7), 0, "Cell (15,15) should not be masked (inside 2 polygons)")

      ! Cell at (25,25) - inside 2 polygons (outer+inner) -> mask=0
      call f90_expect_eq(cellmask(13), 0, "Cell (25,25) should not be masked (inside 2 polygons)")

      ! Cell at (35,35) - inside 1 polygon (outer) -> mask=1
      call f90_expect_eq(cellmask(19), 1, "Cell (35,35) should be masked (inside 1 polygon)")

      ! Cell at (45,45) - outside all polygons -> mask=0
      call f90_expect_eq(cellmask(25), 0, "Cell (45,45) should not be masked (outside all)")

      ! Cleanup
      deallocate (xzw, yzw, xpl, ypl, zpl, cellmask)

   end subroutine test_nested_drypoint_polygons
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_pinpok_raycast_basic, test_pinpok_raycast_basic,
   subroutine test_pinpok_raycast_basic() bind(C)
      ! Test basic ray-casting algorithm directly
      real(kind=dp), dimension(5) :: x_poly, y_poly
      real(kind=dp) :: x_test, y_test
      integer :: inside_pinpok
      logical :: inside_raycast
      integer, parameter :: jins_val = 1

      ! Setup simple square polygon: (0,0) -> (10,0) -> (10,10) -> (0,10) -> (0,0)
      x_poly = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp]
      y_poly = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]

      ! Test 1: Point clearly inside
      x_test = 5.0_dp
      y_test = 5.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "Inside point: pinpok_legacy vs raycast")

      ! Test 2: Point clearly outside
      x_test = 15.0_dp
      y_test = 15.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "Outside point: pinpok_legacy vs raycast")

      ! Test 3: Point on vertex
      x_test = 0.0_dp
      y_test = 0.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "On vertex: pinpok_legacy vs raycast")

      ! Test 4: Point on edge (horizontal)
      x_test = 5.0_dp
      y_test = 0.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "On horizontal edge: pinpok_legacy vs raycast")

      ! Test 5: Point on edge (vertical)
      x_test = 0.0_dp
      y_test = 5.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "On vertical edge: pinpok_legacy vs raycast")

   end subroutine test_pinpok_raycast_basic
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_pinpok_raycast_edge_cases, test_pinpok_raycast_edge_cases,
   subroutine test_pinpok_raycast_edge_cases() bind(C)
      ! Test edge cases and special geometries
      real(kind=dp), dimension(5) :: x_poly, y_poly
      real(kind=dp) :: x_test, y_test
      integer :: inside_pinpok
      logical :: inside_raycast
      integer, parameter :: jins_val = 1

      ! Setup triangle polygon for edge intersection tests
      x_poly = [0.0_dp, 10.0_dp, 5.0_dp, 0.0_dp, 0.0_dp]
      y_poly = [0.0_dp, 0.0_dp, 10.0_dp, 0.0_dp, 0.0_dp]

      ! Test 1: Point on diagonal edge
      x_test = 5.0_dp
      y_test = 5.0_dp
      call pinpok_legacy(x_test, y_test, 4, x_poly(1:4), y_poly(1:4), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:4), y_poly(1:4), 4)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "On diagonal edge: pinpok_legacy vs raycast")

      ! Test 2: Ray passing through vertex (classic edge case)
      x_test = 2.0_dp
      y_test = 0.0_dp
      call pinpok_legacy(x_test, y_test, 4, x_poly(1:4), y_poly(1:4), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:4), y_poly(1:4), 4)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "Ray through vertex: pinpok_legacy vs raycast")

      ! Test 3: Point just inside
      x_test = 5.0_dp
      y_test = 2.0_dp
      call pinpok_legacy(x_test, y_test, 4, x_poly(1:4), y_poly(1:4), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:4), y_poly(1:4), 4)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "Just inside: pinpok_legacy vs raycast")

      ! Test 4: Point just outside
      x_test = 5.0_dp
      y_test = 7.0_dp
      call pinpok_legacy(x_test, y_test, 4, x_poly(1:4), y_poly(1:4), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:4), y_poly(1:4), 4)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "Just outside: pinpok_legacy vs raycast")

   end subroutine test_pinpok_raycast_edge_cases
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_pinpok_raycast_jins_modes, test_pinpok_raycast_jins_modes,
   subroutine test_pinpok_raycast_jins_modes() bind(C)
      ! Test both jins=1 (inside) and jins=0 (outside) modes
      use m_missing, only: jins

      real(kind=dp), dimension(5) :: x_poly, y_poly
      real(kind=dp) :: x_test, y_test
      integer :: inside_pinpok
      logical :: inside_raycast
      integer :: original_jins

      ! Save original jins value
      original_jins = jins

      ! Setup square polygon
      x_poly = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp]
      y_poly = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]

      ! Test with jins = 1 (inside mode)
      jins = 1

      x_test = 5.0_dp
      y_test = 5.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "jins=1, inside: pinpok_legacy vs raycast")

      x_test = 15.0_dp
      y_test = 15.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "jins=1, outside: pinpok_legacy vs raycast")

      ! Test with jins = 0 (outside mode)
      jins = 0

      x_test = 5.0_dp
      y_test = 5.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "jins=0, inside: pinpok_legacy vs raycast")

      x_test = 15.0_dp
      y_test = 15.0_dp
      call pinpok_legacy(x_test, y_test, 5, x_poly, y_poly, inside_pinpok, jins, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly, y_poly, 5)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "jins=0, outside: pinpok_legacy vs raycast")

      ! Restore original jins
      jins = original_jins

   end subroutine test_pinpok_raycast_jins_modes
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_pinpok_raycast_complex, test_pinpok_raycast_complex,
   subroutine test_pinpok_raycast_complex() bind(C)
      ! Test complex polygon shapes (concave, with multiple crossings)
      real(kind=dp), dimension(9) :: x_poly, y_poly
      real(kind=dp) :: x_test, y_test
      integer :: inside_pinpok
      logical :: inside_raycast
      integer, parameter :: jins_val = 1

      ! Setup L-shaped polygon (concave)
      x_poly = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 5.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]
      y_poly = [0.0_dp, 0.0_dp, 5.0_dp, 5.0_dp, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 0.0_dp]

      ! Test 1: Inside the L-shape (bottom part)
      x_test = 2.0_dp
      y_test = 2.0_dp
      call pinpok_legacy(x_test, y_test, 7, x_poly(1:7), y_poly(1:7), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:7), y_poly(1:7), 7)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "L-shape bottom: pinpok_legacy vs raycast")

      ! Test 2: Inside the L-shape (vertical part)
      x_test = 2.0_dp
      y_test = 7.0_dp
      call pinpok_legacy(x_test, y_test, 7, x_poly(1:7), y_poly(1:7), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:7), y_poly(1:7), 7)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "L-shape vertical: pinpok_legacy vs raycast")

      ! Test 3: Inside the concave notch (should be outside)
      x_test = 7.0_dp
      y_test = 7.0_dp
      call pinpok_legacy(x_test, y_test, 7, x_poly(1:7), y_poly(1:7), inside_pinpok, jins_val, dmiss)
      inside_raycast = pinpok_raycast(x_test, y_test, x_poly(1:7), y_poly(1:7), 7)
      call f90_expect_eq(inside_pinpok, merge(1, 0, inside_raycast), "L-shape notch: pinpok_legacy vs raycast")

   end subroutine test_pinpok_raycast_complex
   !$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_incells_basic_functionality, test_incells_basic_functionality,
   subroutine test_incells_basic_functionality() bind(C)
      ! Test basic incells functionality: point inside/outside netcells
      use gridoperations, only: incells
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use network_data, only: netcell, nump, xk, yk
      use m_alloc, only: realloc

      integer :: kin_old, kin_new
      real(kind=dp) :: xa, ya

      npl = 0 !> in case previous tests set npl

      ! Setup simple test network: 2 square netcells
      nump = 2
      call setup_simple_netcells()

      ! Initialize cache for new implementation
      call init_cell_geom_as_polylines()

      ! Test 1: Point clearly inside first cell (0,0 to 10,10)
      xa = 5.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point inside first cell")
      call f90_expect_eq(kin_new, 1, "Should be in cell 1")

      ! Test 2: Point clearly inside second cell (10,0 to 20,10)
      xa = 15.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point inside second cell")
      call f90_expect_eq(kin_new, 2, "Should be in cell 2")

      ! Test 3: Point outside all cells
      xa = 25.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point outside all cells")
      call f90_expect_eq(kin_new, 0, "Should be in no cell")

      ! Test 4: Point on boundary between cells
      xa = 10.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point on cell boundary")

      ! Test 5: Point on cell corner
      xa = 0.0_dp
      ya = 0.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point on cell corner")

      ! Cleanup
      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

   end subroutine test_incells_basic_functionality
!$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_incells_complex_geometry, test_incells_complex_geometry,
   subroutine test_incells_complex_geometry() bind(C)
      ! Test incells with more complex netcell geometries (triangles, pentagons, hexagons)
      use gridoperations, only: incells
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use network_data, only: netcell, nump, xk, yk

      integer :: kin_old, kin_new
      real(kind=dp) :: xa, ya

      npl = 0 !> in case previous tests set npl

      ! Setup network with different polygon types
      nump = 3
      call setup_complex_netcells()

      call init_cell_geom_as_polylines()

      ! Test 1: Inside triangle (cell 1)
      xa = 5.0_dp
      ya = 3.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point inside triangle")

      ! Test 2: Inside pentagon (cell 2)
      xa = 15.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point inside pentagon")

      ! Test 3: Inside hexagon (cell 3)
      xa = 25.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point inside hexagon")

      ! Test 4: On edge of complex polygon
      xa = 10.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Point on complex polygon edge")

      ! Cleanup
      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

   end subroutine test_incells_complex_geometry
!$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_incells_large_grid, test_incells_large_grid,
   subroutine test_incells_large_grid() bind(C)
      ! Test incells with a larger grid (performance sanity check)
      use gridoperations, only: incells
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use network_data, only: netcell, nump

      integer :: kin_old, kin_new, i, mismatches
      real(kind=dp) :: xa, ya

      npl = 0 !> in case previous tests set npl

      ! Setup 10x10 grid of square cells
      nump = 100
      call setup_grid_netcells(10, 10, 10.0_dp)

      call init_cell_geom_as_polylines()

      mismatches = 0

      ! Test points on a grid
      do i = 1, 20
         xa = (i - 1) * 5.0_dp + 2.5_dp ! Offset to avoid boundaries
         ya = 25.0_dp

         call incells(xa, ya, kin_old)
         kin_new = point_find_netcell(xa, ya)

         if (kin_old /= kin_new) then
            mismatches = mismatches + 1
         end if
      end do

      call f90_expect_eq(mismatches, 0, "No mismatches in large grid test")

      ! Cleanup
      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

   end subroutine test_incells_large_grid
!$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_incells_cache_consistency, test_incells_cache_consistency,
   subroutine test_incells_cache_consistency() bind(C)
      ! Test that cache initialization produces consistent results
      use gridoperations, only: incells
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use network_data, only: nump

      integer :: kin1, kin2, kin_old
      real(kind=dp) :: xa, ya

      npl = 0 !> in case previous tests set npl

      ! Setup simple network
      nump = 2
      call setup_simple_netcells()

      ! Test point
      xa = 5.0_dp
      ya = 5.0_dp

      ! Initialize cache and query
      call init_cell_geom_as_polylines()
      kin1 = point_find_netcell(xa, ya)

      ! Query again (should use cached data)
      kin2 = point_find_netcell(xa, ya)

      call f90_expect_eq(kin1, kin2, "Cached queries should match")

      ! Compare with old implementation
      call incells(xa, ya, kin_old)
      call f90_expect_eq(kin1, kin_old, "Cached result should match old implementation")

      ! Cleanup and re-initialize
      call cleanup_cell_geom_polylines()
      call init_cell_geom_as_polylines()

      ! Query after re-initialization
      kin2 = point_find_netcell(xa, ya)
      call f90_expect_eq(kin1, kin2, "Re-initialized cache should give same result")

      ! Cleanup
      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

   end subroutine test_incells_cache_consistency
!$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_incells_edge_cases, test_incells_edge_cases,
   subroutine test_incells_edge_cases() bind(C)
      ! Test edge cases: empty grid, single cell, point at infinity
      use gridoperations, only: incells
      use m_cellmask_from_polygon_set, only: init_cell_geom_as_polylines, point_find_netcell, cleanup_cell_geom_polylines
      use network_data, only: netcell, nump

      integer :: kin_old, kin_new
      real(kind=dp) :: xa, ya

      npl = 0 !> in case previous tests set npl

      ! Test 1: Empty grid (nump = 0)
      nump = 0
      call setup_empty_netcells()
      call init_cell_geom_as_polylines()

      xa = 5.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Empty grid")
      call f90_expect_eq(kin_new, 0, "Should return 0 for empty grid")

      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

      ! Test 2: Single cell
      nump = 1
      call setup_single_netcell()
      call init_cell_geom_as_polylines()

      xa = 5.0_dp
      ya = 5.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Single cell - inside")

      xa = 15.0_dp
      ya = 15.0_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Single cell - outside")

      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

      ! Test 3: Very large coordinates
      nump = 1
      call setup_single_netcell()
      call init_cell_geom_as_polylines()

      xa = 1.0e6_dp
      ya = 1.0e6_dp
      call incells(xa, ya, kin_old)
      kin_new = point_find_netcell(xa, ya)
      call f90_expect_eq(kin_old, kin_new, "Very large coordinates")
      call f90_expect_eq(kin_new, 0, "Should be outside")

      call cleanup_cell_geom_polylines()
      call cleanup_netcells()

   end subroutine test_incells_edge_cases
!$f90tw)

!$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_samples_to_cellmask_basic, test_samples_to_cellmask_basic,
   subroutine test_samples_to_cellmask_basic() bind(C)
      ! Test samples_to_cellmask: mark cells that contain at least one sample
      use m_samples_to_cellmask, only: samples_to_cellmask
      use m_samples, only: ns, xs, ys
      use network_data, only: netcell, nump, cellmask
      use m_alloc, only: realloc

      integer :: i

      ! Setup simple test network: 5 square cells in a row
      nump = 5
      call setup_row_netcells()

      ! Setup samples:
      ! - Cell 1 (0-10): No samples
      ! - Cell 2 (10-20): One sample at (15, 5)
      ! - Cell 3 (20-30): Multiple samples at (22, 5), (25, 5), (28, 5)
      ! - Cell 4 (30-40): No samples
      ! - Cell 5 (40-50): One sample at (45, 5)

      ns = 5
      call realloc(xs, ns, keepexisting=.false.)
      call realloc(ys, ns, keepexisting=.false.)

      ! Cell 2: one sample
      xs(1) = 15.0_dp
      ys(1) = 5.0_dp

      ! Cell 3: three samples
      xs(2) = 22.0_dp
      ys(2) = 5.0_dp

      xs(3) = 25.0_dp
      ys(3) = 5.0_dp

      xs(4) = 28.0_dp
      ys(4) = 5.0_dp

      ! Cell 5: one sample
      xs(5) = 45.0_dp
      ys(5) = 5.0_dp

      ! Call the optimized function
      call samples_to_cellmask()

      ! Verify results:
      ! Cell 1: No samples -> mask=0
      call f90_expect_eq(cellmask(1), 0, "Cell 1 should not be masked (no samples)")

      ! Cell 2: One sample -> mask=1
      call f90_expect_eq(cellmask(2), 1, "Cell 2 should be masked (one sample)")

      ! Cell 3: Multiple samples -> mask=1
      call f90_expect_eq(cellmask(3), 1, "Cell 3 should be masked (multiple samples)")

      ! Cell 4: No samples -> mask=0
      call f90_expect_eq(cellmask(4), 0, "Cell 4 should not be masked (no samples)")

      ! Cell 5: One sample -> mask=1
      call f90_expect_eq(cellmask(5), 1, "Cell 5 should be masked (one sample)")

      ! Cleanup
      call cleanup_netcells()
      deallocate (xs, ys, cellmask)

   end subroutine test_samples_to_cellmask_basic
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_samples_to_cellmask_edge_cases, test_samples_to_cellmask_edge_cases,
   subroutine test_samples_to_cellmask_edge_cases() bind(C)
      ! Test edge cases: no samples, samples outside all cells, samples on boundaries
      use m_samples_to_cellmask, only: samples_to_cellmask
      use m_samples, only: ns, xs, ys
      use network_data, only: nump, cellmask
      use m_alloc, only: realloc

      ! Test 1: No samples at all
      nump = 3
      call setup_row_netcells_small()

      ns = 0
      call realloc(xs, ns, keepexisting=.false.)
      call realloc(ys, ns, keepexisting=.false.)

      call samples_to_cellmask()

      ! All cells should be unmarked
      call f90_expect_eq(cellmask(1), 0, "No samples: Cell 1 should not be masked")
      call f90_expect_eq(cellmask(2), 0, "No samples: Cell 2 should not be masked")
      call f90_expect_eq(cellmask(3), 0, "No samples: Cell 3 should not be masked")

      call cleanup_netcells()
      deallocate (xs, ys, cellmask)

      ! Test 2: Samples outside all cells
      nump = 3
      call setup_row_netcells_small()

      ns = 2
      call realloc(xs, ns, keepexisting=.false.)
      call realloc(ys, ns, keepexisting=.false.)

      xs(1) = -5.0_dp ! Outside to the left
      ys(1) = 5.0_dp

      xs(2) = 35.0_dp ! Outside to the right
      ys(2) = 5.0_dp

      call samples_to_cellmask()

      ! All cells should be unmarked
      call f90_expect_eq(cellmask(1), 0, "Outside samples: Cell 1 should not be masked")
      call f90_expect_eq(cellmask(2), 0, "Outside samples: Cell 2 should not be masked")
      call f90_expect_eq(cellmask(3), 0, "Outside samples: Cell 3 should not be masked")

      call cleanup_netcells()
      deallocate (xs, ys, cellmask)

      ! Test 3: Sample on cell boundary (should be counted as inside)
      nump = 3
      call setup_row_netcells_small()

      ns = 1
      call realloc(xs, ns, keepexisting=.false.)
      call realloc(ys, ns, keepexisting=.false.)

      xs(1) = 10.0_dp ! Exactly on boundary between cell 1 and 2
      ys(1) = 5.0_dp

      call samples_to_cellmask()

      ! At least one cell should be marked (boundary behavior)
      call f90_expect_true(cellmask(1) == 1 .or. cellmask(2) == 1, "Boundary sample: at least one cell should be masked")

      call cleanup_netcells()
      deallocate (xs, ys, cellmask)

   end subroutine test_samples_to_cellmask_edge_cases
   !$f90tw)

   ! Helper subroutine: setup 5 cells in a row (0-10, 10-20, 20-30, 30-40, 40-50)
   subroutine setup_row_netcells()
      use network_data, only: netcell, nump, xk, yk, numk, nump1d2d
      use m_alloc, only: realloc
      integer :: i, ierr

      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      numk = 24 ! 5 cells * 4 corners + 4 shared corners
      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)
      nump1d2d = 5

      ! Create 5 square cells in a row
      do i = 1, nump1d2d
         ! Cell i: square from (i-1)*10 to i*10, y from 0 to 10
         xk((i - 1) * 4 + 1:(i - 1) * 4 + 4) = [(i - 1) * 10.0_dp, i * 10.0_dp, i * 10.0_dp, (i - 1) * 10.0_dp]
         yk((i - 1) * 4 + 1:(i - 1) * 4 + 4) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]

         netcell(i)%N = 4
         allocate (netcell(i)%nod(4))
         netcell(i)%nod = [(i - 1) * 4 + 1, (i - 1) * 4 + 2, (i - 1) * 4 + 3, (i - 1) * 4 + 4]
      end do

   end subroutine setup_row_netcells

   ! Helper subroutine: setup 3 cells in a row (0-10, 10-20, 20-30)
   subroutine setup_row_netcells_small()
      use network_data, only: netcell, nump, xk, yk, numk, nump1d2d
      use m_alloc, only: realloc
      integer :: i, ierr

      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      numk = 12 ! 3 cells * 4 corners
      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)
      nump1d2d = 3
      do i = 1, nump1d2d
         xk((i - 1) * 4 + 1:(i - 1) * 4 + 4) = [(i - 1) * 10.0_dp, i * 10.0_dp, i * 10.0_dp, (i - 1) * 10.0_dp]
         yk((i - 1) * 4 + 1:(i - 1) * 4 + 4) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]

         netcell(i)%N = 4
         allocate (netcell(i)%nod(4))
         netcell(i)%nod = [(i - 1) * 4 + 1, (i - 1) * 4 + 2, (i - 1) * 4 + 3, (i - 1) * 4 + 4]
      end do

   end subroutine setup_row_netcells_small

! ============================================================================
! Helper subroutines for setting up test geometries
! ============================================================================

   subroutine setup_simple_netcells()
      use network_data, only: netcell, nump, xk, yk, numk
      use m_alloc, only: realloc
      integer :: ierr

      ! Allocate netcell array
      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      ! Allocate node arrays
      numk = 8
      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)

      ! Cell 1: square (0,0) to (10,10)
      xk(1:4) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      yk(1:4) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]

      netcell(1)%N = 4
      allocate (netcell(1)%nod(4))
      netcell(1)%nod = [1, 2, 3, 4]

      ! Cell 2: square (10,0) to (20,10)
      xk(5:8) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      yk(5:8) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]

      netcell(2)%N = 4
      allocate (netcell(2)%nod(4))
      netcell(2)%nod = [5, 6, 7, 8]

   end subroutine setup_simple_netcells

   subroutine setup_complex_netcells()
      use network_data, only: netcell, nump, xk, yk, numk
      use m_alloc, only: realloc
      integer :: ierr

      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      numk = 14 ! 3 + 5 + 6 nodes
      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)

      ! Cell 1: Triangle
      xk(1:3) = [0.0_dp, 10.0_dp, 5.0_dp]
      yk(1:3) = [0.0_dp, 0.0_dp, 10.0_dp]
      netcell(1)%N = 3
      allocate (netcell(1)%nod(3))
      netcell(1)%nod = [1, 2, 3]

      ! Cell 2: Pentagon
      xk(4:8) = [10.0_dp, 15.0_dp, 17.0_dp, 15.0_dp, 10.0_dp]
      yk(4:8) = [0.0_dp, 0.0_dp, 5.0_dp, 10.0_dp, 10.0_dp]
      netcell(2)%N = 5
      allocate (netcell(2)%nod(5))
      netcell(2)%nod = [4, 5, 6, 7, 8]

      ! Cell 3: Hexagon
      xk(9:14) = [20.0_dp, 25.0_dp, 30.0_dp, 30.0_dp, 25.0_dp, 20.0_dp]
      yk(9:14) = [5.0_dp, 0.0_dp, 5.0_dp, 10.0_dp, 15.0_dp, 10.0_dp]
      netcell(3)%N = 6
      allocate (netcell(3)%nod(6))
      netcell(3)%nod = [9, 10, 11, 12, 13, 14]

   end subroutine setup_complex_netcells

   subroutine setup_grid_netcells(nx, ny, cellsize)
      use network_data, only: netcell, nump, xk, yk, numk
      use m_alloc, only: realloc
      integer, intent(in) :: nx, ny
      real(kind=dp), intent(in) :: cellsize
      integer :: i, j, icell, inode, ierr

      nump = nx * ny
      numk = (nx + 1) * (ny + 1)

      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)

      ! Create grid of nodes
      inode = 0
      do j = 0, ny
         do i = 0, nx
            inode = inode + 1
            xk(inode) = i * cellsize
            yk(inode) = j * cellsize
         end do
      end do

      ! Create cells
      icell = 0
      do j = 0, ny - 1
         do i = 0, nx - 1
            icell = icell + 1
            netcell(icell)%N = 4
            allocate (netcell(icell)%nod(4))
            ! Bottom-left, bottom-right, top-right, top-left
            netcell(icell)%nod(1) = j * (nx + 1) + i + 1
            netcell(icell)%nod(2) = j * (nx + 1) + i + 2
            netcell(icell)%nod(3) = (j + 1) * (nx + 1) + i + 2
            netcell(icell)%nod(4) = (j + 1) * (nx + 1) + i + 1
         end do
      end do

   end subroutine setup_grid_netcells

   subroutine cleanup_netcells()
      use network_data, only: netcell, xk, yk
      integer :: i
      if (allocated(netcell)) then
         do i = 1, size(netcell)
            if (allocated(netcell(i)%nod)) then
               deallocate (netcell(i)%nod)
            end if
         end do
         deallocate (netcell)
      end if
      if (allocated(xk)) then
         deallocate (xk)
      end if
      if (allocated(yk)) then
         deallocate (yk)
      end if

   end subroutine cleanup_netcells

   subroutine setup_empty_netcells()
      use network_data, only: netcell, nump

      nump = 0
      if (allocated(netcell)) then
         deallocate (netcell)
      end if

   end subroutine setup_empty_netcells

   subroutine setup_single_netcell()
      use network_data, only: netcell, nump, xk, yk, numk
      use m_alloc, only: realloc
      integer :: ierr

      nump = 1
      numk = 4

      if (allocated(netcell)) then
         deallocate (netcell)
      end if
      allocate (netcell(nump), stat=ierr)

      call realloc(xk, numk, keepexisting=.false.)
      call realloc(yk, numk, keepexisting=.false.)

      ! Single square cell (0,0) to (10,10)
      xk = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      yk = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]

      netcell(1)%N = 4
      allocate (netcell(1)%nod(4))
      netcell(1)%nod = [1, 2, 3, 4]

   end subroutine setup_single_netcell

   subroutine realloc_polyline_arrays(nump, npl)
      use m_alloc, only: realloc
      integer, intent(in) :: nump, npl

      call realloc(cellmask, nump, keepexisting=.false.)
      call realloc(xzw, nump, keepexisting=.false.)
      call realloc(yzw, nump, keepexisting=.false.)

      call realloc(xpl, npl, keepexisting=.false.)
      call realloc(ypl, npl, keepexisting=.false.)
      call realloc(zpl, npl, keepexisting=.false.)

   end subroutine realloc_polyline_arrays

end module test_pol_to_cellmask
