module test_pol_to_cellmask
   use assertions_gtest
   use precision, only: dp
   use m_missing, only: dmiss
   use network_data, only: cellmask, npl, nump, xzw, yzw, xpl, ypl, zpl
   use m_cellmask_from_polygon_set, only: cellmask_from_polygon_set_init, cellmask_from_polygon_set, cellmask_from_polygon_set_cleanup
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_mixed_polygon, test_mixed_polygon,
   subroutine test_mixed_polygon() bind(C)
      ! Test with both enclosure (-1) and dry point (1) polygons
      integer :: i, k
      
      ! Setup test grid: 5x5 grid of cells
      nump = 25
      npl = 15  ! 5 points for enclosure + separator + 5 points for dry point + separators

      call realloc_polyline_arrays(nump,npl)
      
      ! Create 5x5 grid of cell centers
      do i = 1, 25
         xzw(i) = mod(i-1, 5) * 10.0_dp + 5.0_dp  ! x: 5, 15, 25, 35, 45
         yzw(i) = ((i-1) / 5) * 10.0_dp + 5.0_dp  ! y: 5, 15, 25, 35, 45
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
      deallocate(xzw, yzw, xpl, ypl, zpl, cellmask)
      
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
      
      call realloc_polyline_arrays(nump,npl)

      ! Create 5x5 grid of cell centers
      do i = 1, 25
         xzw(i) = mod(i-1, 5) * 10.0_dp + 5.0_dp
         yzw(i) = ((i-1) / 5) * 10.0_dp + 5.0_dp
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
      deallocate(xzw, yzw, xpl, ypl, zpl, cellmask)
      
   end subroutine test_nested_drypoint_polygons
   !$f90tw)

   subroutine realloc_polyline_arrays(nump,npl)
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