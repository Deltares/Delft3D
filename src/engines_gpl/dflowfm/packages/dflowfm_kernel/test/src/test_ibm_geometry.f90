module test_ibm_geometry
   use assertions_gtest
   use precision, only: dp
   use m_immersed_boundary, only: ibm_build_geometry, ibm_measure_wet_polygon, ibm_validate_cutcelllist, &
                                  ibm_set_perot_weights, ibm_set_wet_link_metrics, ibm_set_wet_bed, &
                                  ibm_wet_area_from_polygon, t_ibm_cut_cell, t_ibm_state
   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_half_plane_cut_metrics, test_half_plane_cut_metrics,
   subroutine test_half_plane_cut_metrics() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(3) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 50.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'Half-plane cut geometry was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.5_dp, 1.0e-14_dp, 'Unexpected half-plane area fraction.')
      call F90_ASSERT_NEAR(cell%xc, 10.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected wet centroid x coordinate.')
      call F90_ASSERT_NEAR(cell%yc, 10.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected wet centroid y coordinate.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Expected one immersed segment.')
      call F90_ASSERT_NEAR(cell%seg_len, sqrt(200.0_dp), 1.0e-13_dp, 'Unexpected immersed segment length.')
      call F90_ASSERT_NEAR(cell%nx, -sqrt(0.5_dp), 1.0e-14_dp, 'Unexpected inward normal x component.')
      call F90_ASSERT_NEAR(cell%ny, -sqrt(0.5_dp), 1.0e-14_dp, 'Unexpected inward normal y component.')
   end subroutine test_half_plane_cut_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_oblique_bank_through_net_node, test_oblique_bank_through_net_node,
   subroutine test_oblique_bank_through_net_node() bind(C)
      use kdtree2Factory, only: build_kdtree, delete_kdtree2, treeglob
      use m_cutcelwu, only: cutcelwu, cutcelwu_ibm_union
      use m_flow, only: baorgfracmin
      use m_flow_geominit, only: flow_geominit
      use m_flowgeom, only: acl, ba, csu, dxi, kfs, lne2ln, ln, lncn, lnx, nd, snu, wu, xz, yz
      use m_flowparameters, only: immersed_boundary
      use m_immersed_boundary, only: ibm_build_geometry, ibm_reset_state, ibm_set_wet_link_metrics, ibm_state
      use m_missing, only: dmiss
      use m_network_helpers, only: t_grid_helper
      use m_partitioninfo, only: jampi
      use m_polygon, only: npl
      use m_sferic, only: jsferic
      use m_tpoly, only: tpoly, tpoly_to_pol
      use network_data, only: kn, netcell, numk, nump, numl, xk, yk
      use Timers, only: timini
      use unstruc_model, only: md_cutcelllist

      type(t_grid_helper) :: grid_helper
      type(tpoly), dimension(:), allocatable :: polygons
      integer :: ierror, cell, ll, link, node1, node2, flow_link, internal_flow_faces, geometry_link_count
      integer :: sw_cell, se_cell, nw_cell, ne_cell, max_wet_vertices, geometry_error, boundary_cut_link
      integer, allocatable :: wet_count(:), link_cells(:, :)
      real(kind=dp) :: slope, center_x, center_y, phi1, phi2, wet_fraction, expected_width, full_width
      real(kind=dp), allocatable :: cell_corner_x(:, :), cell_corner_y(:, :), wet_x(:, :), wet_y(:, :)
      real(kind=dp), allocatable :: face_x(:, :), face_y(:, :)
      real(kind=dp), allocatable :: width_full_test(:), width_used_test(:), acl_full_test(:), dxi_full_test(:)
      real(kind=dp), allocatable :: normal_x_test(:), normal_y_test(:)
      logical :: geometry_valid
      character(len=256) :: geometry_reason
      character(len=256) :: link_message

      call ibm_reset_state()
      jampi = 0
      immersed_boundary = 0
      md_cutcelllist = ''
      baorgfracmin = 0.1_dp
      call timini()
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(-10.0_dp, -10.0_dp, 10.0_dp, 2, 2)
      call flow_geominit(0)

      slope = 43.0_dp / 80.0_dp
      allocate (polygons(1))
      allocate (polygons(1)%x(4), polygons(1)%y(4), polygons(1)%z(4))
      polygons(1)%len = 4
      polygons(1)%x = [-20.0_dp, 20.0_dp, 20.0_dp, -20.0_dp]
      polygons(1)%y = [-20.0_dp, -20.0_dp, 20.0_dp * slope, -20.0_dp * slope]
      polygons(1)%z = 0.0_dp
      polygons(1)%xmin = minval(polygons(1)%x)
      polygons(1)%xmax = maxval(polygons(1)%x)
      polygons(1)%ymin = minval(polygons(1)%y)
      polygons(1)%ymax = maxval(polygons(1)%y)
      call tpoly_to_pol(polygons, iselect=1)

      ibm_state%active = .true.
      ibm_state%grid%dx = 10.0_dp
      ibm_state%grid%dy = 10.0_dp
      ibm_state%ba_full = ba(1:nump)
      ibm_state%wu_full = wu(1:lnx)
      ibm_state%acl_full = acl(1:lnx)
      ibm_state%dxi_full = dxi(1:lnx)
      immersed_boundary = 1

      call build_kdtree(treeglob, numk, xk, yk, ierror, jsferic, dmiss)
      call F90_ASSERT_EQ(ierror, 0, 'Could not build the net-node tree for the IBM crossing test.')
      if (ierror /= 0) then
         call ibm_reset_state()
         immersed_boundary = 0
         return
      end if
      call cutcelwu(4, 0, 0)
      call cutcelwu_ibm_union(polygons)

      sw_cell = 0
      se_cell = 0
      nw_cell = 0
      ne_cell = 0
      do cell = 1, nump
         center_x = 0.0_dp
         center_y = 0.0_dp
         do ll = 1, netcell(cell)%n
            center_x = center_x + xk(netcell(cell)%nod(ll))
            center_y = center_y + yk(netcell(cell)%nod(ll))
         end do
         center_x = center_x / real(netcell(cell)%n, kind=dp)
         center_y = center_y / real(netcell(cell)%n, kind=dp)
         if (abs(center_x + 5.0_dp) < 1.0e-12_dp .and. abs(center_y + 5.0_dp) < 1.0e-12_dp) then
            sw_cell = cell
         else if (abs(center_x - 5.0_dp) < 1.0e-12_dp .and. abs(center_y + 5.0_dp) < 1.0e-12_dp) then
            se_cell = cell
         else if (abs(center_x + 5.0_dp) < 1.0e-12_dp .and. abs(center_y - 5.0_dp) < 1.0e-12_dp) then
            nw_cell = cell
         else if (abs(center_x - 5.0_dp) < 1.0e-12_dp .and. abs(center_y - 5.0_dp) < 1.0e-12_dp) then
            ne_cell = cell
         end if
      end do

      call F90_ASSERT_TRUE(sw_cell > 0 .and. se_cell > 0 .and. nw_cell > 0 .and. ne_cell > 0, &
                           'Could not locate all cells around the oblique-bank node.')
      if (sw_cell > 0 .and. se_cell > 0 .and. nw_cell > 0 .and. ne_cell > 0) then
         call F90_ASSERT_NEAR(ba(sw_cell), 26.875_dp, 1.0e-10_dp, 'Wrong wet area southwest of the net-node crossing.')
         call F90_ASSERT_NEAR(ba(se_cell), 0.0_dp, 1.0e-12_dp, 'Solid cell southeast of the crossing stayed active.')
         call F90_ASSERT_NEAR(ba(nw_cell), 100.0_dp, 1.0e-10_dp, 'Node-touch-only cell was incorrectly cut.')
         call F90_ASSERT_NEAR(ba(ne_cell), 73.125_dp, 1.0e-10_dp, 'Wrong wet area northeast of the net-node crossing.')
         if (allocated(kfs)) then
            call F90_ASSERT_EQ(kfs(sw_cell), 1, 'Southwest cut cell was not flagged.')
            call F90_ASSERT_EQ(kfs(se_cell), 0, 'Southeast solid cell was flagged as cut.')
            call F90_ASSERT_EQ(kfs(nw_cell), 0, 'Node-touch-only cell was flagged as cut.')
            call F90_ASSERT_EQ(kfs(ne_cell), 1, 'Northeast cut cell was not flagged.')
         else
            call F90_ASSERT_TRUE(.false., 'Cut-cell flags were not allocated.')
         end if
      end if

      internal_flow_faces = 0
      do link = 1, numl
         flow_link = lne2ln(link)
         if (flow_link <= 0) then
            cycle
         end if
         internal_flow_faces = internal_flow_faces + 1
         call F90_ASSERT_TRUE(flow_link <= lnx, 'A net face maps beyond the flow-link array.')
         if (flow_link > lnx) then
            cycle
         end if
         node1 = kn(1, link)
         node2 = kn(2, link)
         phi1 = yk(node1) - slope * xk(node1)
         phi2 = yk(node2) - slope * xk(node2)
         if (phi1 >= 0.0_dp .and. phi2 >= 0.0_dp) then
            wet_fraction = 1.0_dp
         else if (phi1 <= 0.0_dp .and. phi2 <= 0.0_dp) then
            wet_fraction = 0.0_dp
         else
            wet_fraction = max(phi1, phi2) / (abs(phi1) + abs(phi2))
         end if
         expected_width = sqrt((xk(node2) - xk(node1))**2 + (yk(node2) - yk(node1))**2) * wet_fraction
         full_width = ibm_state%wu_full(flow_link)
         write (link_message, '(a,i0,a,i0,a,4f8.3,a,f8.3,a,f8.3)') 'Internal net link ', link, ', flow link ', flow_link, &
            ', endpoints ', xk(node1), yk(node1), xk(node2), yk(node2), ', actual ', wu(flow_link), ', expected ', expected_width
         call F90_ASSERT_NEAR(wu(flow_link), expected_width, 1.0e-8_dp, trim(link_message))
         call F90_ASSERT_TRUE(wu(flow_link) >= -1.0e-12_dp .and. wu(flow_link) <= full_width + 1.0e-12_dp, &
                              'An oblique-bank face aperture is outside [0, 1].')
      end do
      call F90_ASSERT_EQ(internal_flow_faces, 4, 'Unexpected number of internal faces in the node-crossing fixture.')

      max_wet_vertices = 4
      do cell = 1, nump
         if (allocated(nd(cell)%x)) then
            max_wet_vertices = max(max_wet_vertices, size(nd(cell)%x))
         end if
      end do
      allocate (cell_corner_x(4, nump), cell_corner_y(4, nump))
      allocate (wet_x(max_wet_vertices, nump), wet_y(max_wet_vertices, nump), wet_count(nump))
      geometry_link_count = lnx + 1
      allocate (link_cells(2, geometry_link_count), face_x(2, geometry_link_count), face_y(2, geometry_link_count))
      allocate (width_full_test(geometry_link_count), width_used_test(geometry_link_count))
      allocate (acl_full_test(geometry_link_count), dxi_full_test(geometry_link_count))
      allocate (normal_x_test(geometry_link_count), normal_y_test(geometry_link_count))
      wet_x = 0.0_dp
      wet_y = 0.0_dp
      do cell = 1, nump
         wet_count(cell) = 0
         if (allocated(nd(cell)%x) .and. allocated(nd(cell)%y)) then
            wet_count(cell) = size(nd(cell)%x)
            if (wet_count(cell) > 0) then
               wet_x(1:wet_count(cell), cell) = nd(cell)%x
               wet_y(1:wet_count(cell), cell) = nd(cell)%y
            end if
         end if
         do ll = 1, 4
            node1 = netcell(cell)%nod(ll)
            cell_corner_x(ll, cell) = xk(node1)
            cell_corner_y(ll, cell) = yk(node1)
         end do
      end do
      do link = 1, lnx
         link_cells(:, link) = ln(:, link)
         face_x(:, link) = xk(lncn(:, link))
         face_y(:, link) = yk(lncn(:, link))
      end do
      width_full_test(1:lnx) = ibm_state%wu_full
      width_used_test(1:lnx) = wu
      acl_full_test(1:lnx) = ibm_state%acl_full
      dxi_full_test(1:lnx) = ibm_state%dxi_full
      normal_x_test(1:lnx) = csu
      normal_y_test(1:lnx) = snu

      ! The square-grid fixture has no virtual boundary flow links, so add one
      ! on the cut cell's left face to exercise preservation of its legacy metrics.
      link_cells(:, geometry_link_count) = [nump + 1, sw_cell]
      face_x(:, geometry_link_count) = [-10.0_dp, -10.0_dp]
      face_y(:, geometry_link_count) = [-10.0_dp, 0.0_dp]
      width_full_test(geometry_link_count) = 10.0_dp
      width_used_test(geometry_link_count) = 10.0_dp * slope
      acl_full_test(geometry_link_count) = 0.375_dp
      dxi_full_test(geometry_link_count) = 0.125_dp
      normal_x_test(geometry_link_count) = 1.0_dp
      normal_y_test(geometry_link_count) = 0.0_dp
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, ibm_state%ba_full, ba, &
                              link_cells, face_x, face_y, width_full_test, width_used_test, ibm_state%grid%dx, &
                              ibm_state%grid%dy, baorgfracmin, 0.0_dp, 1.0e-6_dp, 0.5_dp, ibm_state, &
                              geometry_valid, geometry_error, geometry_reason)
      call F90_ASSERT_TRUE(geometry_valid, 'IBM geometry build rejected the boundary-crossing bank: '//trim(geometry_reason))
      if (geometry_valid) then
         call ibm_set_wet_link_metrics(link_cells, xz, yz, normal_x_test, normal_y_test, width_used_test, acl_full_test, &
                                       dxi_full_test, ibm_state, geometry_valid, geometry_error, geometry_reason)
         call F90_ASSERT_TRUE(geometry_valid, 'IBM wet-link metrics rejected the boundary-crossing bank: '//trim(geometry_reason))
      end if

      boundary_cut_link = 0
      if (geometry_valid) then
         do ll = 1, size(ibm_state%clink)
            flow_link = ibm_state%clink(ll)%L
            node1 = link_cells(1, flow_link)
            node2 = link_cells(2, flow_link)
            if (ibm_state%clink(ll)%edge_frac <= 1.0e-12_dp .or. ibm_state%clink(ll)%edge_frac >= 1.0_dp - 1.0e-12_dp) then
               cycle
            end if
            if (node1 > ibm_state%geometry_cell_count .and. node2 >= 1 .and. &
                node2 <= ibm_state%geometry_cell_count) then
               if (ibm_state%cell_class(node2) == 1) then
                  boundary_cut_link = ll
                  exit
               end if
            else if (node2 > ibm_state%geometry_cell_count .and. node1 >= 1 .and. &
                     node1 <= ibm_state%geometry_cell_count) then
               if (ibm_state%cell_class(node1) == 1) then
                  boundary_cut_link = ll
                  exit
               end if
            end if
         end do
         call F90_ASSERT_TRUE(boundary_cut_link > 0, 'No partially wet cut link to a virtual boundary cell was found.')
         if (boundary_cut_link > 0) then
            flow_link = ibm_state%clink(boundary_cut_link)%L
            call F90_ASSERT_EQ(flow_link, geometry_link_count, 'The virtual-boundary cut link was not the synthetic test link.')
            call F90_ASSERT_NEAR(ibm_state%clink(boundary_cut_link)%edge_frac, slope, 1.0e-12_dp, &
                                 'The virtual-boundary link has the wrong wet-face fraction.')
            call F90_ASSERT_NEAR(ibm_state%clink(boundary_cut_link)%acl_wet, acl_full_test(flow_link), &
                                 1.0e-14_dp, 'Boundary-link ACL did not retain its legacy value.')
            call F90_ASSERT_NEAR(ibm_state%clink(boundary_cut_link)%dxi_wet, dxi_full_test(flow_link), &
                                 1.0e-14_dp, 'Boundary-link DXI did not retain its legacy value.')
         end if
      end if

      call delete_kdtree2(treeglob)
      call ibm_reset_state()
      immersed_boundary = 0
      npl = 0
   end subroutine test_oblique_bank_through_net_node
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_union_clip_multiple_polygons, test_union_clip_multiple_polygons,
   subroutine test_union_clip_multiple_polygons() bind(C)
      use m_cutcelwu, only: cutcelwu_ibm_union
      use m_flow, only: baorgfracmin
      use m_flow_geominit, only: flow_geominit
      use m_flowgeom, only: ba, kfs, lne2ln, lnx, wu
      use m_flowparameters, only: immersed_boundary
      use m_immersed_boundary, only: ibm_reset_state, ibm_state
      use m_network_helpers, only: t_grid_helper
      use m_partitioninfo, only: jampi
      use m_tpoly, only: tpoly
      use network_data, only: netcell, nump
      use Timers, only: timini

      type(t_grid_helper) :: grid_helper
      type(tpoly), dimension(:), allocatable :: polygons
      integer :: net_link, flow_link

      call ibm_reset_state()
      jampi = 0
      immersed_boundary = 0
      baorgfracmin = 0.1_dp
      call timini()
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(0.0_dp, 0.0_dp, 10.0_dp, 1, 2)
      call flow_geominit(0)

      allocate (polygons(2))
      allocate (polygons(1)%x(4), polygons(1)%y(4), polygons(1)%z(4))
      polygons(1)%len = 4
      polygons(1)%x = [-1.0_dp, 4.0_dp, 4.0_dp, -1.0_dp]
      polygons(1)%y = [-1.0_dp, -1.0_dp, 11.0_dp, 11.0_dp]
      polygons(1)%z = 0.0_dp
      polygons(1)%xmin = minval(polygons(1)%x)
      polygons(1)%xmax = maxval(polygons(1)%x)
      polygons(1)%ymin = minval(polygons(1)%y)
      polygons(1)%ymax = maxval(polygons(1)%y)
      allocate (polygons(2)%x(4), polygons(2)%y(4), polygons(2)%z(4))
      polygons(2)%len = 4
      polygons(2)%x = [4.0_dp, 10.0_dp, 10.0_dp, 4.0_dp]
      polygons(2)%y = [-1.0_dp, -1.0_dp, 4.0_dp, 4.0_dp]
      polygons(2)%z = 0.0_dp
      polygons(2)%xmin = minval(polygons(2)%x)
      polygons(2)%xmax = maxval(polygons(2)%x)
      polygons(2)%ymin = minval(polygons(2)%y)
      polygons(2)%ymax = maxval(polygons(2)%y)

      ibm_state%grid%dx = 10.0_dp
      ibm_state%grid%dy = 10.0_dp
      call F90_ASSERT_TRUE(allocated(wu), 'Flow-link widths were not initialized.')
      if (.not. allocated(wu) .or. lnx < 1 .or. nump < 2) then
         call ibm_reset_state()
         return
      end if
      ibm_state%ba_full = ba(1:nump)
      ibm_state%wu_full = wu(1:lnx)
      call cutcelwu_ibm_union(polygons)

      call F90_ASSERT_NEAR(ba(1), 36.0_dp, 1.0e-12_dp, 'Union clipping returned the wrong wet area.')
      call F90_ASSERT_NEAR(ba(2), 100.0_dp, 1.0e-12_dp, 'Polygon subtraction changed the adjacent fluid cell.')
      call F90_ASSERT_EQ(kfs(1), 1, 'Union-clipped cell was not marked as cut.')
      call F90_ASSERT_EQ(kfs(2), 0, 'Adjacent fluid cell was incorrectly marked as cut.')
      net_link = abs(netcell(1)%lin(2))
      flow_link = lne2ln(net_link)
      call F90_ASSERT_TRUE(flow_link > 0, 'Shared cell face does not map to a flow link.')
      if (flow_link > 0 .and. flow_link <= size(wu)) then
         call F90_ASSERT_NEAR(wu(flow_link), 6.0_dp, 1.0e-12_dp, 'Union clipping returned the wrong shared-face aperture.')
      end if

      call ibm_reset_state()
   end subroutine test_union_clip_multiple_polygons
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_floor_crosscheck, test_area_floor_crosscheck,
   subroutine test_area_floor_crosscheck() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(4) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [9.5_dp, 9.5_dp, 10.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 10.0_dp, 0.1_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'Correctly floored cut-cell area was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.05_dp, 1.0e-14_dp, 'Unexpected unfloored area fraction.')
      call F90_ASSERT_NEAR(cell%ba_used, 10.0_dp, 1.0e-14_dp, 'Unexpected floored area.')

      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 11.0_dp, 0.1_dp, 0.0_dp, 1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(.not. cell%valid, 'Mismatched cutcelwu area was accepted.')
      call F90_ASSERT_TRUE(index(cell%reason, 'cutcelwu') > 0, 'Area cross-check failure did not identify cutcelwu.')
   end subroutine test_area_floor_crosscheck
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_independent_polygon_cell_area, test_independent_polygon_cell_area,
   subroutine test_independent_polygon_cell_area() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y, polygon_x, polygon_y
      real(kind=dp) :: area
      logical :: valid

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      polygon_x = [0.0_dp, 5.0_dp, 5.0_dp, 0.0_dp]
      polygon_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call ibm_wet_area_from_polygon(cell_x, cell_y, polygon_x, polygon_y, 1.0e-8_dp, area, valid)

      call F90_ASSERT_TRUE(valid, 'Independent polygon clipping rejected a valid rectangle.')
      call F90_ASSERT_NEAR(area, 50.0_dp, 1.0e-13_dp, 'Independent polygon clipping returned the wrong area.')
   end subroutine test_independent_polygon_cell_area
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_classification_uses_geometric_tolerance, test_area_classification_uses_geometric_tolerance,
   subroutine test_area_classification_uses_geometric_tolerance() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 2) :: link_cells
      real(kind=dp), dimension(2, 2) :: face_x, face_y
      real(kind=dp), dimension(2) :: width_full, width_used, wcx1, wcy1, wcx2, wcy2
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 0.005_dp, 0.005_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 9.995_dp, 9.995_dp]
      wet_count = [4, 4]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [0.05_dp, 99.95_dp]
      link_cells(:, 1) = [1, 3]
      link_cells(:, 2) = [2, 3]
      face_x(:, 1) = [0.0_dp, 10.0_dp]
      face_x(:, 2) = [10.0_dp, 20.0_dp]
      face_y = 0.0_dp
      width_full = 10.0_dp
      width_used = 10.0_dp
      state%ba_full = area_full

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-3_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Sliver and near-full wet cells were rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 2, 'Area classification used the Cartesian tolerance.')
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Sliver wet cell was classified as solid.')
      call F90_ASSERT_EQ(state%cell_class(2), 1, 'Near-full wet cell was classified as uncut.')
      if (valid) then
         wcx1 = -999.0_dp
         wcy1 = -999.0_dp
         wcx2 = -999.0_dp
         wcy2 = -999.0_dp
         call ibm_set_perot_weights(link_cells, width_used, wcx1, wcy1, wcx2, wcy2, state, valid, error_index, reason)
         call F90_ASSERT_TRUE(valid, 'Perot weights rejected sliver or near-full cut cells: '//trim(reason))
         if (valid) then
            call F90_ASSERT_NEAR(wcx1(1), 0.0_dp, 1.0e-12_dp, 'Sliver-cell Perot x weight was not applied.')
            call F90_ASSERT_NEAR(wcy1(1), -0.5_dp, 1.0e-12_dp, 'Sliver-cell Perot y weight was not applied.')
            call F90_ASSERT_NEAR(wcx1(2), 0.0_dp, 1.0e-12_dp, 'Near-full-cell Perot x weight was not applied.')
            call F90_ASSERT_NEAR(wcy1(2), -0.5_dp, 1.0e-12_dp, 'Near-full-cell Perot y weight was not applied.')
         end if
      end if
   end subroutine test_area_classification_uses_geometric_tolerance
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_dmiss_separated_polygons_are_cut, test_dmiss_separated_polygons_are_cut,
   subroutine test_dmiss_separated_polygons_are_cut() bind(C)
      use kdtree2Factory, only: build_kdtree, delete_kdtree2, treeglob
      use m_cutcelwu, only: cutcelwu
      use m_flow, only: baorgfracmin
      use m_flow_geominit, only: flow_geominit
      use m_flowgeom, only: kfs
      use m_flowparameters, only: immersed_boundary
      use m_immersed_boundary, only: ibm_reset_state
      use m_missing, only: dmiss
      use m_network_helpers, only: t_grid_helper
      use m_partitioninfo, only: jampi
      use m_polygon, only: npl, xpl
      use m_reapol, only: reapol
      use m_sferic, only: jsferic
      use network_data, only: numk, xk, yk
      use precision_basics, only: comparereal
      use unstruc_model, only: md_cutcelllist
      use Timers, only: timini

      character(len=*), parameter :: polygon_file = 'ibm_geometry_two_polygons.pol'
      type(t_grid_helper) :: grid_helper
      integer :: ierror, unit

      call ibm_reset_state()
      jampi = 0
      immersed_boundary = 0
      md_cutcelllist = ''
      baorgfracmin = 0.1_dp
      call timini()
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(0.0_dp, 0.0_dp, 10.0_dp, 2, 1)
      call flow_geominit(0)

      open (newunit=unit, file=polygon_file, status='replace', action='write')
      write (unit, '(a)') 'left solid polygon'
      write (unit, '(a)') '4 2'
      write (unit, *) -1.0_dp, -1.0_dp
      write (unit, *) 4.0_dp, -1.0_dp
      write (unit, *) 4.0_dp, 11.0_dp
      write (unit, *) -1.0_dp, 11.0_dp
      write (unit, '(a)') 'right solid polygon'
      write (unit, '(a)') '4 2'
      write (unit, *) 16.0_dp, -1.0_dp
      write (unit, *) 21.0_dp, -1.0_dp
      write (unit, *) 21.0_dp, 11.0_dp
      write (unit, *) 16.0_dp, 11.0_dp
      close (unit)

      open (newunit=unit, file=polygon_file, status='old', action='read')
      call reapol(unit, 0)
      close (unit)
      call F90_ASSERT_EQ(npl, 9, 'The .pol reader did not separate the two polygon blocks.')
      if (npl >= 5) then
         call F90_ASSERT_EQ(comparereal(xpl(5), dmiss), 0, 'The .pol reader did not insert a dmiss separator.')
      end if

      immersed_boundary = 1
      call build_kdtree(treeglob, numk, xk, yk, ierror, jsferic, dmiss)
      call F90_ASSERT_EQ(ierror, 0, 'Could not build the net-node tree for the multipart polygon test.')
      if (ierror == 0) then
         call cutcelwu(4, 0, 0)
         call F90_ASSERT_EQ(kfs(1), 1, 'The first dmiss-separated polygon did not flag its cut cell.')
         call F90_ASSERT_EQ(kfs(2), 1, 'The second dmiss-separated polygon did not flag its cut cell.')
         call delete_kdtree2(treeglob)
      end if

      open (newunit=unit, file=polygon_file, status='old')
      close (unit, status='delete')
      call ibm_reset_state()
      immersed_boundary = 0
      md_cutcelllist = ''
      npl = 0
   end subroutine test_dmiss_separated_polygons_are_cut
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_geometry_state_and_wet_edge, test_geometry_state_and_wet_edge,
   subroutine test_geometry_state_and_wet_edge() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(5, 2) :: wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 5.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_count = [5, 4]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [87.5_dp, 100.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [5.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Consistent cut-cell geometry was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 1, 'Unexpected number of cut cells.')
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Cut cell was not classified as cut.')
      call F90_ASSERT_EQ(state%cell_class(2), 0, 'Uncut cell was not classified as fluid.')
      call F90_ASSERT_EQ(size(state%clink), 1, 'Unexpected number of cut links.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.5_dp, 1.0e-14_dp, 'Unexpected wet-edge fraction.')
      call F90_ASSERT_NEAR(state%clink(1)%xm, 10.0_dp, 1.0e-14_dp, 'Unexpected wet-edge midpoint x coordinate.')
      call F90_ASSERT_NEAR(state%clink(1)%ym, 2.5_dp, 1.0e-14_dp, 'Unexpected wet-edge midpoint y coordinate.')
   end subroutine test_geometry_state_and_wet_edge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_closed_link_next_to_solid_cell, test_closed_link_next_to_solid_cell,
   subroutine test_closed_link_next_to_solid_cell() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(2) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_corner_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [5.0_dp, 10.0_dp, 10.0_dp, 5.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 2) = 0.0_dp
      wet_y(:, 2) = 0.0_dp
      wet_count = [4, 0]
      area_full = [100.0_dp, 100.0_dp]
      area_used = [50.0_dp, 0.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [0.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Closed link next to a solid cell was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%cell_class(1), 1, 'Expected one cut cell.')
      call F90_ASSERT_EQ(state%cell_class(2), 2, 'Expected one solid cell.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.0_dp, 1.0e-14_dp, 'Solid-adjacent link should be closed.')
   end subroutine test_closed_link_next_to_solid_cell
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_open_boundary_link_keeps_legacy_metrics, test_open_boundary_link_keeps_legacy_metrics,
   subroutine test_open_boundary_link_keeps_legacy_metrics() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(2) :: area_full, area_used
      integer, dimension(2, 1) :: link_cells
      real(kind=dp), dimension(2, 1) :: face_x, face_y
      real(kind=dp), dimension(1) :: width_full, width_used, cell_x, cell_y, normal_x, normal_y, acl_full, dxi_full
      real(kind=dp), dimension(2) :: bed_level
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 5.0_dp, 5.0_dp]
      wet_count = [4]
      area_full = [100.0_dp, 0.0_dp]
      area_used = [50.0_dp, 0.0_dp]
      link_cells(:, 1) = [1, 2]
      face_x(:, 1) = [10.0_dp, 10.0_dp]
      face_y(:, 1) = [0.0_dp, 10.0_dp]
      width_full = [10.0_dp]
      width_used = [5.0_dp]

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Open-boundary cut geometry was rejected: '//trim(reason))

      state%grid%dx = 10.0_dp
      state%grid%dy = 10.0_dp
      cell_x = [5.0_dp]
      cell_y = [5.0_dp]
      normal_x = [1.0_dp]
      normal_y = [0.0_dp]
      acl_full = [0.5_dp]
      dxi_full = [0.1_dp]
      call ibm_set_wet_link_metrics(link_cells, [5.0_dp, 15.0_dp], [5.0_dp, 5.0_dp], normal_x, normal_y, &
                                    width_used, acl_full, dxi_full, state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Open-boundary IBM metrics were rejected: '//trim(reason))
      call F90_ASSERT_NEAR(state%clink(1)%acl_wet, acl_full(1), 1.0e-14_dp, 'Open-boundary ACL was overridden.')
      call F90_ASSERT_NEAR(state%clink(1)%dxi_wet, dxi_full(1), 1.0e-14_dp, 'Open-boundary DXI was overridden.')

      bed_level = [7.0_dp, -999.0_dp]
      call ibm_set_wet_bed(link_cells, width_used, [5.0_dp, 15.0_dp], [5.0_dp, 5.0_dp], -999.0_dp, bed_level, &
                           state, valid, error_index, reason, owned_cell=[.false.])
      call F90_ASSERT_TRUE(valid, 'Unowned IBM ghost cut cell was fitted: '//trim(reason))
      call F90_ASSERT_NEAR(bed_level(1), 7.0_dp, 1.0e-14_dp, 'Unowned ghost-cell bed level was modified.')
   end subroutine test_open_boundary_link_keeps_legacy_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_cutcelllist_validation, test_cutcelllist_validation,
   subroutine test_cutcelllist_validation() bind(C)
      character(len=*), parameter :: list_file = 'ibm_geometry_check.lst'
      character(len=*), parameter :: empty_list_file = 'ibm_geometry_empty.lst'
      character(len=*), parameter :: polygon_file = 'ibm_geometry_check.pol'
      character(len=256) :: reason
      logical :: valid
      integer :: unit

      open (newunit=unit, file=empty_list_file, status='replace', action='write')
      close (unit)
      call ibm_validate_cutcelllist(empty_list_file, valid, reason)
      call F90_ASSERT_TRUE(.not. valid, 'Empty Cutcelllist was accepted.')
      call F90_ASSERT_TRUE(index(reason, 'at least one polygon') > 0, 'Empty-list failure did not identify the missing polygon.')
      open (newunit=unit, file=empty_list_file, status='old')
      close (unit, status='delete')

      open (newunit=unit, file=polygon_file, status='replace', action='write')
      write (unit, '(a)') 'polygon test data'
      close (unit)
      open (newunit=unit, file=list_file, status='replace', action='write')
      write (unit, '(a)') polygon_file
      close (unit)
      call ibm_validate_cutcelllist(list_file, valid, reason)
      call F90_ASSERT_TRUE(valid, 'Valid Cutcelllist was rejected: '//trim(reason))
      open (newunit=unit, file=list_file, status='old')
      close (unit, status='delete')
      open (newunit=unit, file=polygon_file, status='old')
      close (unit, status='delete')
   end subroutine test_cutcelllist_validation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_angle_43_80_geometry, test_angle_43_80_geometry,
   subroutine test_angle_43_80_geometry() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(3) :: wet_x, wet_y
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x_matrix, wet_y_matrix
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_cut_cell) :: cell
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: normal_length

      cell_x = [0.0_dp, 80.0_dp, 80.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 43.0_dp, 43.0_dp]
      wet_x = [0.0_dp, 80.0_dp, 80.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 43.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 3440.0_dp, 1720.0_dp, 0.0_dp, 0.0_dp, &
                                   1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(cell%valid, 'The 43/80 triangle cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.5_dp, 1.0e-14_dp, 'Unexpected 43/80 triangle area fraction.')
      normal_length = sqrt(43.0_dp**2 + 80.0_dp**2)
      call F90_ASSERT_NEAR(cell%nx, 43.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 triangle normal x.')
      call F90_ASSERT_NEAR(cell%ny, -80.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 triangle normal y.')

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_corner_x(:, 1) = cell_x
      cell_corner_y(:, 1) = cell_y
      wet_x_matrix(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y_matrix(:, 1) = [0.0_dp, 0.0_dp, 7.375_dp, 2.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x_matrix(:, 1), wet_y_matrix(:, 1), 100.0_dp, 46.875_dp, &
                                   0.0_dp, 0.0_dp, 1.0e-7_dp, cell)
      call F90_ASSERT_TRUE(cell%valid, 'The 43/80 trapezoid cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.46875_dp, 1.0e-14_dp, 'Unexpected 43/80 trapezoid area fraction.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Expected one immersed trapezoid segment.')
      normal_length = sqrt(8249.0_dp)
      call F90_ASSERT_NEAR(cell%nx, 43.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 trapezoid normal x.')
      call F90_ASSERT_NEAR(cell%ny, -80.0_dp / normal_length, 1.0e-14_dp, 'Unexpected 43/80 trapezoid normal y.')

      wet_count = 4
      area_full = 100.0_dp
      area_used = 46.875_dp
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
      width_used = [2.0_dp, 7.375_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x_matrix, wet_y_matrix, wet_count, area_full, area_used, &
                              link_cells, face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, &
                              1.0e-6_dp, 0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Consistent 43/80 wet-face geometry was rejected: '//trim(reason))
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.2_dp, 1.0e-14_dp, 'Unexpected left-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(2), 0.7375_dp, 1.0e-14_dp, 'Unexpected right-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(3), 1.0_dp, 1.0e-14_dp, 'Unexpected bottom-face fraction.')
      call F90_ASSERT_NEAR(state%edge_frac(4), 0.0_dp, 1.0e-14_dp, 'Unexpected top-face fraction.')
   end subroutine test_angle_43_80_geometry
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_cut_through_net_node, test_cut_through_net_node,
   subroutine test_cut_through_net_node() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(3, 1) :: wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_count = 3
      area_full = 100.0_dp
      area_used = 50.0_dp
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
      width_used = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'A cut through the net node was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 1, 'Expected one cut cell at the net node.')
      call F90_ASSERT_EQ(state%cut(1)%nseg, 1, 'Expected one nonzero immersed segment.')
      call F90_ASSERT_NEAR(state%cut(1)%seg_len, sqrt(200.0_dp), 1.0e-14_dp, 'Unexpected net-node segment length.')
      call F90_ASSERT_NEAR(state%edge_frac(1), 0.0_dp, 1.0e-14_dp, 'Expected a dry left face.')
      call F90_ASSERT_NEAR(state%edge_frac(2), 1.0_dp, 1.0e-14_dp, 'Expected a fully wet right face.')
      call F90_ASSERT_NEAR(state%edge_frac(3), 1.0_dp, 1.0e-14_dp, 'Expected a fully wet bottom face.')
      call F90_ASSERT_NEAR(state%edge_frac(4), 0.0_dp, 1.0e-14_dp, 'Expected a dry top face.')
   end subroutine test_cut_through_net_node
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_face_aligned_fluid_cell, test_face_aligned_fluid_cell,
   subroutine test_face_aligned_fluid_cell() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = cell_corner_x(:, 1)
      wet_y(:, 1) = cell_corner_y(:, 1)
      wet_count = 4
      area_full = 100.0_dp
      area_used = 100.0_dp
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
      width_used = 10.0_dp
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'A face-aligned full cell was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%ncut, 0, 'A face-aligned boundary created a cut cell.')
      call F90_ASSERT_EQ(state%cell_class(1), 0, 'The face-aligned full cell was not classified as fluid.')
      call F90_ASSERT_EQ(size(state%clink), 0, 'A full cell created an immersed cut link.')
   end subroutine test_face_aligned_fluid_cell
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_two_segment_corner, test_two_segment_corner,
   subroutine test_two_segment_corner() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(5) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 10.0_dp, 10.0_dp, 5.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 10.0_dp, 5.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 75.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'A right-angle corner cut was rejected.')
      call F90_ASSERT_NEAR(cell%area_frac, 0.75_dp, 1.0e-14_dp, 'Unexpected right-angle corner area.')
      call F90_ASSERT_EQ(cell%nseg, 2, 'Expected two immersed segments at the corner.')
      call F90_ASSERT_NEAR(cell%nx, 0.0_dp, 1.0e-14_dp, 'Unexpected corner-cut normal x component.')
      call F90_ASSERT_NEAR(cell%ny, -1.0_dp, 1.0e-14_dp, 'Unexpected corner-cut normal y component.')
   end subroutine test_two_segment_corner
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_smooth_multivertex_segment, test_smooth_multivertex_segment,
   subroutine test_smooth_multivertex_segment() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(8) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 2.0_dp, 2.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 2.0_dp, 2.0_dp]
      wet_x = [0.0_dp, 2.0_dp, 2.0_dp, 0.0_dp, 0.1_dp, 1.0_dp, 1.5_dp, 1.7_dp]
      wet_y = [0.0_dp, 0.0_dp, 2.0_dp, 2.0_dp, 1.9_dp, 0.9_dp, 0.3_dp, 0.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 4.0_dp, 2.215_dp, 0.1_dp, 1.0e-6_dp, &
                                   1.0e-8_dp, cell)

      call F90_ASSERT_TRUE(cell%valid, 'A polygonized smooth wall was rejected.')
      call F90_ASSERT_EQ(cell%nseg, 1, 'Nearly collinear wall edges were not coalesced.')
   end subroutine test_smooth_multivertex_segment
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_split_cell_rejected, test_split_cell_rejected,
   subroutine test_split_cell_rejected() bind(C)
      real(kind=dp), dimension(4) :: cell_x, cell_y
      real(kind=dp), dimension(8) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell

      cell_x = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x = [0.0_dp, 4.0_dp, 4.0_dp, 6.0_dp, 6.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y = [0.0_dp, 0.0_dp, 3.0_dp, 3.0_dp, 0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call ibm_measure_wet_polygon(cell_x, cell_y, wet_x, wet_y, 100.0_dp, 94.0_dp, 0.0_dp, 0.0_dp, 1.0e-7_dp, cell)

      call F90_ASSERT_TRUE(.not. cell%valid, 'A cell with a three-segment solid spit was accepted.')
      call F90_ASSERT_TRUE(index(cell%reason, 'more than two') > 0, 'Split-cell failure did not identify excess segments.')
   end subroutine test_split_cell_rejected
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_conservation, test_area_conservation,
   subroutine test_area_conservation() bind(C)
      real(kind=dp), dimension(4, 2) :: cell_x, cell_y
      real(kind=dp), dimension(3, 2) :: wet_x, wet_y
      type(t_ibm_cut_cell) :: cell
      real(kind=dp) :: total_area
      integer :: k

      cell_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      cell_x(:, 2) = [10.0_dp, 20.0_dp, 20.0_dp, 10.0_dp]
      cell_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_x(:, 2) = [10.0_dp, 20.0_dp, 10.0_dp]
      wet_y(:, 2) = [0.0_dp, 0.0_dp, 10.0_dp]

      total_area = 0.0_dp
      do k = 1, 2
         call ibm_measure_wet_polygon(cell_x(:, k), cell_y(:, k), wet_x(:, k), wet_y(:, k), 100.0_dp, 50.0_dp, &
                                      0.0_dp, 0.0_dp, 1.0e-7_dp, cell)
         call F90_ASSERT_TRUE(cell%valid, 'A triangle in the closed two-cell polygon was rejected.')
         total_area = total_area + cell%area_frac * 100.0_dp
      end do
      call F90_ASSERT_NEAR(total_area, 100.0_dp, 1.0e-12_dp, 'Wet areas do not conserve the closed triangular polygon area.')
   end subroutine test_area_conservation
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_area_floor_accounting, test_area_floor_accounting,
   subroutine test_area_floor_accounting() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y, wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 0) :: link_cells
      real(kind=dp), dimension(2, 0) :: face_x, face_y
      real(kind=dp), dimension(0) :: width_full, width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      wet_y(:, 1) = [9.5_dp, 9.5_dp, 10.0_dp, 10.0_dp]
      wet_count = 4
      area_full = 100.0_dp
      area_used = 10.0_dp

      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.1_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Area-floor geometry was rejected: '//trim(reason))
      call F90_ASSERT_EQ(state%nfloored, 1, 'Expected one floored cell.')
      call F90_ASSERT_NEAR(state%area_deficit, 5.0_dp, 1.0e-14_dp, 'Unexpected area-floor deficit.')
   end subroutine test_area_floor_accounting
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_perot_tangential_constant, test_perot_tangential_constant,
   subroutine test_perot_tangential_constant() bind(C)
      real(kind=dp), dimension(4, 1) :: cell_corner_x, cell_corner_y
      real(kind=dp), dimension(3, 1) :: wet_x, wet_y
      integer, dimension(1) :: wet_count
      real(kind=dp), dimension(1) :: area_full, area_used
      integer, dimension(2, 4) :: link_cells
      real(kind=dp), dimension(2, 4) :: face_x, face_y
      real(kind=dp), dimension(4) :: width_full, width_used
      real(kind=dp), dimension(4) :: wcx1, wcy1, wcx2, wcy2, normal_velocity
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: ucx, ucy

      cell_corner_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      cell_corner_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      wet_x(:, 1) = [0.0_dp, 10.0_dp, 10.0_dp]
      wet_y(:, 1) = [0.0_dp, 0.0_dp, 10.0_dp]
      wet_count = 3
      area_full = 100.0_dp
      area_used = 50.0_dp
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
      width_used = [0.0_dp, 10.0_dp, 10.0_dp, 0.0_dp]
      call ibm_build_geometry(cell_corner_x, cell_corner_y, wet_x, wet_y, wet_count, area_full, area_used, link_cells, &
                              face_x, face_y, width_full, width_used, 10.0_dp, 10.0_dp, 0.0_dp, 0.0_dp, 1.0e-6_dp, &
                              0.5_dp, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Perot test geometry was rejected: '//trim(reason))
      state%ba_full = area_full

      wcx1 = 0.0_dp
      wcy1 = 0.0_dp
      wcx2 = 0.0_dp
      wcy2 = 0.0_dp
      call ibm_set_perot_weights(link_cells, width_used, wcx1, wcy1, wcx2, wcy2, state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Wet-polygon Perot weights were rejected: '//trim(reason))

      normal_velocity = [-1.0_dp, 1.0_dp, -1.0_dp, 1.0_dp]
      ucx = sum(wcx1 * normal_velocity)
      ucy = sum(wcy1 * normal_velocity)
      call F90_ASSERT_NEAR(ucx, 1.0_dp, 1.0e-13_dp, 'Tangential Perot reconstruction did not preserve constant x velocity.')
      call F90_ASSERT_NEAR(ucy, 1.0_dp, 1.0e-13_dp, 'Tangential Perot reconstruction did not preserve constant y velocity.')

      normal_velocity = [-1.0_dp, 1.0_dp, 0.0_dp, 0.0_dp]
      ucx = sum(wcx1 * normal_velocity)
      ucy = sum(wcy1 * normal_velocity)
      call F90_ASSERT_NEAR(ucx, 2.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected normal-flow Perot x component.')
      call F90_ASSERT_NEAR(ucy, 1.0_dp / 3.0_dp, 1.0e-13_dp, 'Unexpected normal-flow Perot y component.')
   end subroutine test_perot_tangential_constant
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_link_metrics, test_wet_link_metrics,
   subroutine test_wet_link_metrics() bind(C)
      integer, dimension(2, 2) :: link_cells
      real(kind=dp), dimension(3) :: cell_x, cell_y
      real(kind=dp), dimension(2) :: normal_x, normal_y, width_used, acl_full, dxi_full
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason
      real(kind=dp) :: zeta1, zeta2, dn

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(3), state%cut(1), state%clink(1))
      state%cell_class = [1, 0, 0]
      state%ncut = 1
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.5_dp
      state%cut(1)%yc = 0.5_dp
      state%clink(1)%L = 1
      state%clink(1)%xm = 1.0_dp
      state%clink(1)%ym = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [2, 3]
      cell_x = [0.5_dp, 1.5_dp, 2.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp]
      normal_x = 1.0_dp
      normal_y = 0.0_dp
      width_used = 1.0_dp
      acl_full = 0.5_dp
      dxi_full = 1.0_dp
      call ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                    state, valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-link metrics were rejected: '//trim(reason))
      if (valid) then
         dn = 1.0_dp / state%clink(1)%dxi_wet
         zeta1 = 2.0_dp * state%cut(1)%xc + 4.0_dp * state%cut(1)%yc
         zeta2 = 2.0_dp * cell_x(2) + 4.0_dp * cell_y(2)
         call F90_ASSERT_NEAR((zeta2 - zeta1) / dn, 2.0_dp, 1.0e-14_dp, &
                              'Wet-centroid pressure gradient was not exact for a normal linear field.')
      end if

      state%cut(1)%xc = 0.95_dp
      cell_x(2) = 1.05_dp
      cell_y(2) = 0.7_dp
      call ibm_set_wet_link_metrics(link_cells, cell_x, cell_y, normal_x, normal_y, width_used, acl_full, dxi_full, &
                                    state, valid, error_index, reason)
      call F90_ASSERT_TRUE(valid, 'Sliver wet-link metrics were rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(state%clink(1)%dxi_wet, 4.0_dp, 1.0e-14_dp, 'Unexpected clipped wet-centroid inverse distance.')
         call F90_ASSERT_NEAR(state%clink(1)%acl_wet, 0.5_dp, 1.0e-14_dp, 'Unexpected wet-face interpolation weight.')
         call F90_ASSERT_NEAR(state%clink(1)%dt12, 0.2_dp, 1.0e-14_dp, 'Unexpected tangential centroid offset.')
         call F90_ASSERT_EQ(state%nclipped_dn, 1, 'Expected one clipped wet-centroid distance.')
         call F90_ASSERT_EQ(state%cut_link_index(1), 1, 'Cut-link lookup was not populated.')
         call F90_ASSERT_EQ(state%cut_link_index(2), 0, 'Non-cut link was added to the cut-link lookup.')
      end if
   end subroutine test_wet_link_metrics
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_bed_planar_fit, test_wet_bed_planar_fit,
   subroutine test_wet_bed_planar_fit() bind(C)
      integer, dimension(2, 3) :: link_cells
      real(kind=dp), dimension(4) :: cell_x, cell_y, bed_level
      real(kind=dp), dimension(3) :: width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(4), state%cut(1))
      state%cell_class = [1, 0, 0, 0]
      state%ncut = 1
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.25_dp
      state%cut(1)%yc = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [1, 3]
      link_cells(:, 3) = [1, 4]
      cell_x = [0.5_dp, 1.5_dp, -0.5_dp, 0.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp, 1.5_dp]
      width_used = 1.0_dp
      bed_level = [10.0_dp, 4.5_dp, -1.5_dp, -2.5_dp]
      call ibm_set_wet_bed(link_cells, width_used, cell_x, cell_y, -9999.0_dp, bed_level, state, &
                           valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-bed planar fit was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(bed_level(1), 0.75_dp, 1.0e-13_dp, 'Wet-bed fit did not recover the planar value at the centroid.')
      end if
   end subroutine test_wet_bed_planar_fit
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ibm_geometry, test_wet_bed_boundary_fallback, test_wet_bed_boundary_fallback,
   subroutine test_wet_bed_boundary_fallback() bind(C)
      integer, dimension(2, 5) :: link_cells
      real(kind=dp), dimension(6) :: cell_x, cell_y, bed_level
      real(kind=dp), dimension(5) :: width_used
      type(t_ibm_state) :: state
      logical :: valid
      integer :: error_index
      character(len=256) :: reason

      state%grid%dx = 1.0_dp
      state%grid%dy = 1.0_dp
      allocate (state%cell_class(6), state%cut(3))
      state%cell_class = [1, 1, 1, 0, 0, 0]
      state%ncut = 3
      state%cut(1)%k = 1
      state%cut(1)%xc = 0.25_dp
      state%cut(1)%yc = 0.5_dp
      state%cut(2)%k = 2
      state%cut(2)%xc = 1.5_dp
      state%cut(2)%yc = 0.5_dp
      state%cut(3)%k = 3
      state%cut(3)%xc = 2.5_dp
      state%cut(3)%yc = 0.5_dp

      link_cells(:, 1) = [1, 2]
      link_cells(:, 2) = [2, 3]
      link_cells(:, 3) = [3, 4]
      link_cells(:, 4) = [3, 5]
      link_cells(:, 5) = [3, 6]
      width_used = 1.0_dp
      cell_x = [0.5_dp, 1.5_dp, 2.5_dp, 3.5_dp, 2.5_dp, 3.5_dp]
      cell_y = [0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 1.5_dp, 1.5_dp]
      bed_level = [100.0_dp, 101.0_dp, 102.0_dp, 10.5_dp, 3.5_dp, 6.5_dp]
      call ibm_set_wet_bed(link_cells, width_used, cell_x, cell_y, -9999.0_dp, bed_level, state, &
                           valid, error_index, reason)

      call F90_ASSERT_TRUE(valid, 'Wet-bed boundary fallback was rejected: '//trim(reason))
      if (valid) then
         call F90_ASSERT_NEAR(bed_level(1), 0.75_dp, 1.0e-13_dp, 'Fallback fit did not recover the planar value.')
      end if
   end subroutine test_wet_bed_boundary_fallback
   !$f90tw)

end module test_ibm_geometry
