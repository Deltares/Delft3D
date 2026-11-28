module test_longculverts
   use m_message_helpers, only: t_abort_threshold_guard
   use assertions_gtest
   use m_longculverts, only: convert1D2DLongCulverts, default_longculverts
   use m_network_helpers, only: t_grid_helper
   use iso_c_utils, only: cstr
   use m_file_helpers, only: create_file
   use m_netcdf_helpers, only: create_netcdf_from_cdl
   use precision, only: dp

   implicit none(type, external)

   real(kind=dp), dimension(2, 2), parameter :: LC2_X = reshape([50.0_dp, 350.0_dp, 50.0_dp, 350.0_dp], [2, 2])
   real(kind=dp), dimension(2, 2), parameter :: LC2_Y = reshape([50.0_dp, 50.0_dp, 150.0_dp, 150.0_dp], [2, 2])
   real(kind=dp), dimension(2, 2), parameter :: LC2_Z = -5.0_dp
   real(kind=dp), dimension(3, 2), parameter :: LC3_X = reshape([50.0_dp, 200.0_dp, 350.0_dp, 50.0_dp, 200.0_dp, 350.0_dp], [3, 2])
   real(kind=dp), dimension(3, 2), parameter :: LC3_Y = reshape([50.0_dp, 50.0_dp, 50.0_dp, 150.0_dp, 150.0_dp, 150.0_dp], [3, 2])
   real(kind=dp), dimension(3, 2), parameter :: LC3_Z = -5.0_dp
   real(kind=dp), dimension(4, 2), parameter :: LC4_X = reshape([50.0_dp, 150.0_dp, 250.0_dp, 350.0_dp, 50.0_dp, 150.0_dp, 250.0_dp, 350.0_dp], [4, 2])
   real(kind=dp), dimension(4, 2), parameter :: LC4_Y = reshape([50.0_dp, 50.0_dp, 50.0_dp, 50.0_dp, 150.0_dp, 150.0_dp, 150.0_dp, 150.0_dp], [4, 2])
   real(kind=dp), dimension(4, 2), parameter :: LC4_Z = -5.0_dp

contains
   !> Create a structures.ini file from long-culvert coordinates and properties.
   subroutine create_longculvert_structure_file(filename, x_coordinates, y_coordinates, z_coordinates, ids, &
                                                allowed_flowdirs, widths, heights, friction_types, friction_values, &
                                                valve_openings)
      use precision, only: dp

      character(len=*), intent(in) :: filename
      real(kind=dp), dimension(:, :), intent(in) :: x_coordinates, y_coordinates, z_coordinates
      character(len=*), dimension(:), optional, intent(in) :: ids, allowed_flowdirs, friction_types
      real(kind=dp), dimension(:), optional, intent(in) :: widths, heights, friction_values, valve_openings

      character(len=256), allocatable :: lines(:)
      character(len=64) :: id, allowed_flowdir, friction_type
      integer :: i, num_culverts, num_coordinates, structure_start
      real(kind=dp) :: width, height, friction_value, valve_opening

      num_coordinates = size(x_coordinates, 1)
      num_culverts = size(x_coordinates, 2)
      if (any(shape(y_coordinates) /= shape(x_coordinates)) .or. any(shape(z_coordinates) /= shape(x_coordinates))) then
         error stop "Long-culvert coordinate arrays must have equal shapes"
      end if

      allocate (lines(3 + 14 * num_culverts))
      lines(1:4) = [character(len=256) :: "[General]", "    fileVersion     = 3.00", "    fileType        = structures", ""]

      do i = 1, num_culverts
         write (id, '("lc",i2.2)') i
         allowed_flowdir = "both"
         width = 2.0_dp
         height = 2.0_dp
         friction_type = "Manning"
         friction_value = 0.02_dp
         valve_opening = 1.0_dp

         if (present(ids)) then
            id = ids(i)
         end if
         if (present(allowed_flowdirs)) then
            allowed_flowdir = allowed_flowdirs(i)
         end if
         if (present(widths)) then
            width = widths(i)
         end if
         if (present(heights)) then
            height = heights(i)
         end if
         if (present(friction_types)) then
            friction_type = friction_types(i)
         end if
         if (present(friction_values)) then
            friction_value = friction_values(i)
         end if
         if (present(valve_openings)) then
            valve_opening = valve_openings(i)
         end if

         structure_start = 4 + (i - 1) * 14
         if (i > 1) then
            lines(structure_start) = ""
         end if
         lines(structure_start + 1) = "[Structure]"
         lines(structure_start + 2) = "    id              = "//trim(id)
         lines(structure_start + 3) = "    type            = longCulvert"
         write (lines(structure_start + 4), '(a,i0)') "    numCoordinates  = ", num_coordinates
         write (lines(structure_start + 5), '(a,*(g0,1x))') "    xCoordinates    = ", x_coordinates(:, i)
         write (lines(structure_start + 6), '(a,*(g0,1x))') "    yCoordinates    = ", y_coordinates(:, i)
         write (lines(structure_start + 7), '(a,*(g0,1x))') "    zCoordinates    = ", z_coordinates(:, i)
         lines(structure_start + 8) = "    allowedFlowDir  = "//trim(allowed_flowdir)
         write (lines(structure_start + 9), '(a,g0)') "    width           = ", width
         write (lines(structure_start + 10), '(a,g0)') "    height          = ", height
         lines(structure_start + 11) = "    frictionType    = "//trim(friction_type)
         write (lines(structure_start + 12), '(a,g0)') "    frictionValue   = ", friction_value
         write (lines(structure_start + 13), '(a,g0)') "    valveRelativeOpening = ", valve_opening
      end do

      call create_file(filename, lines)
   end subroutine create_longculvert_structure_file

   !$gtest
   subroutine test_convert1d2dlongculverts__single_four_point() bind(C)
      use precision, only: dp
      use network_data, only: numk, numl, kn
      use m_missing, only: dmiss
      use m_polygon, only: xpl, ypl, zpl, npl
      use m_longculverts, only: convert1D2DLongCulverts
      use m_longculverts_data, only: longculverts

      integer, parameter :: COORD_COUNT = 4
      type(t_grid_helper) :: grid_helper
      real(kind=dp) :: x_coords(COORD_COUNT)
      real(kind=dp) :: y_coords(COORD_COUNT)
      real(kind=dp) :: z_coords(COORD_COUNT)
      integer :: i

      ! Arrange
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, rows=1, columns=2, side_length=10.0_dp, array_size_margin=16)

      x_coords = [5._dp, 9._dp, 11._dp, 15._dp]
      y_coords = [6._dp, 6._dp, 4._dp, 4._dp]
      z_coords = -1.0_dp

      ! Subroutine `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
      xpl = x_coords
      ypl = y_coords
      zpl = z_coords
      npl = COORD_COUNT

      allocate (longculverts(1))
      allocate (longculverts(1)%netlinks(3))
      ! Act
      call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

      ! Assert
      call f90_expect_double_eq(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
      call f90_expect_double_eq(y_coords(1), 5._dp)
      call f90_expect_double_eq(x_coords(COORD_COUNT), 15._dp)
      call f90_expect_double_eq(y_coords(COORD_COUNT), 5._dp)

      call f90_expect_eq(numk, 10) ! 6 Netnodes for the grid, 4 For the long culvert.
      if (.not. f90_assert_eq(numl, 10)) then ! 7 Netlinks for the grid, 3 For the long culvert.
         call default_longculverts()
         return
      end if

      call f90_expect_eq(kn(3, longculverts(1)%netlinks(1)), 5, cstr("Expected first new link to be a 1D2D link."))
      call f90_expect_eq(kn(3, longculverts(1)%netlinks(2)), 1, cstr("Expected middle link to be a 1D link."))
      call f90_expect_eq(kn(3, longculverts(1)%netlinks(3)), 5, cstr("Expected last new link to be a 1D2D link."))
   end subroutine test_convert1d2dlongculverts__single_four_point

   !$gtest
   subroutine test_convert1d2dlongculverts__single_two_point() bind(C)
      use precision, only: dp
      use network_data, only: numk, numl, kn
      use m_missing, only: dmiss
      use m_polygon, only: xpl, ypl, zpl, npl
      use m_longculverts, only: convert1D2DLongCulverts
      use m_longculverts_data, only: longculverts

      implicit none

      integer, parameter :: COORD_COUNT = 2
      type(t_grid_helper) :: grid_helper
      real(kind=dp) :: x_coords(COORD_COUNT)
      real(kind=dp) :: y_coords(COORD_COUNT)
      real(kind=dp) :: z_coords(COORD_COUNT)
      integer :: i

      npl = 0
      ! Arrange
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, rows=1, columns=2, side_length=10.0_dp, array_size_margin=16)

      x_coords = [5._dp, 15._dp]
      y_coords = [6._dp, 4._dp]
      z_coords = -1.0_dp

      ! `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
      xpl = x_coords
      ypl = y_coords
      zpl = z_coords
      npl = COORD_COUNT
      if (allocated(longculverts)) then
         deallocate (longculverts)
      end if
      allocate (longculverts(1))
      allocate (longculverts(1)%netlinks(1))
      ! Act
      call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

      ! Assert
      call f90_expect_double_eq(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
      call f90_expect_double_eq(y_coords(1), 5._dp)
      call f90_expect_double_eq(x_coords(COORD_COUNT), 15._dp)
      call f90_expect_double_eq(y_coords(COORD_COUNT), 5._dp)

      call f90_expect_eq(numk, 8) ! 6 Netnodes for the grid, 2 For the long culvert.
      if (.not. f90_assert_eq(numl, 8)) then ! 7 Netlinks for the grid, 1 For the long culvert.
         call default_longculverts()
         return
      end if

      call f90_expect_eq(kn(3, longculverts(1)%netlinks(1)), 5, cstr("Expected first new link to be a 1D2D link."))
   end subroutine test_convert1d2dlongculverts__single_two_point

   !$gtest
   subroutine test_convert1d2dlongculverts__multiple_culverts() bind(C)
      use precision, only: dp
      use network_data, only: numk, numl, kn
      use m_missing, only: dmiss
      use m_polygon, only: xpl, ypl, zpl, npl
      use m_longculverts, only: convert1D2DLongCulverts
      use m_longculverts_data, only: longculverts
      use m_save_ugrid_state, only: meshgeom1d

      implicit none

      integer, parameter :: COORD_COUNT_LC1 = 4
      integer, parameter :: COORD_COUNT_LC2 = 2
      integer, parameter :: ARRAY_SIZE = COORD_COUNT_LC1 + COORD_COUNT_LC2 + 1
      type(t_grid_helper) :: grid_helper
      real(kind=dp) :: x_coords(ARRAY_SIZE)
      real(kind=dp) :: y_coords(ARRAY_SIZE)
      real(kind=dp) :: z_coords(ARRAY_SIZE)
      integer :: i

      npl = 0
      ! Arrange
      ! 2 x 2 grid.
      grid_helper = t_grid_helper()
      call grid_helper%make_square_grid(bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, rows=2, columns=2, side_length=10.0_dp, array_size_margin=16)

      x_coords = [5._dp, 9._dp, 11._dp, 15._dp, dmiss, 5._dp, 15._dp]
      y_coords = [6._dp, 6._dp, 4._dp, 4._dp, dmiss, 16._dp, 14._dp]
      z_coords = -1.0_dp
      z_coords(5) = dmiss

      ! Subroutine `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
      xpl = x_coords
      ypl = y_coords
      zpl = z_coords
      npl = ARRAY_SIZE

      !> ensure meshgeom1d state is disregarded
      meshgeom1d%numnode = -1
      meshgeom1d%nnodes = -1

      if (allocated(longculverts)) then
         deallocate (longculverts)
      end if
      allocate (longculverts(2))
      allocate (longculverts(1)%netlinks(3))
      allocate (longculverts(2)%netlinks(1))

      ! Act
      call convert1D2DLongCulverts(x_coords, y_coords, z_coords, ARRAY_SIZE)

      ! Assert
      call f90_expect_double_eq(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
      call f90_expect_double_eq(y_coords(1), 5._dp)
      call f90_expect_double_eq(x_coords(COORD_COUNT_LC1), 15._dp)
      call f90_expect_double_eq(y_coords(COORD_COUNT_LC1), 5._dp)
      call f90_expect_double_eq(x_coords(COORD_COUNT_LC1 + 2), 5._dp) ! First and last point snapped to cell centers.
      call f90_expect_double_eq(y_coords(COORD_COUNT_LC1 + 2), 15._dp)
      call f90_expect_double_eq(x_coords(ARRAY_SIZE), 15._dp)
      call f90_expect_double_eq(y_coords(ARRAY_SIZE), 15._dp)

      call f90_expect_eq(numk, 9 + 4 + 2) ! 9 Netnodes for the grid, 4 for LC1, 2 for LC2.
      call f90_expect_eq(numl, 12 + 3 + 1) ! 12 Netlinks for the grid, 3 for LC1, 1 for LC2.
   end subroutine test_convert1d2dlongculverts__multiple_culverts

   !> Create a minimal UGRID 2D net file: a simple channel of 4 quads in a row.
   !! Nodes form a 5x2 grid (10 nodes), edges connect them into 4 rectangular cells.
   subroutine create_minimal_netfile(filename, success)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: success

      ! Node layout (y=0 bottom row, y=100 top row):
      !   6---7---8---9---10      (y=100)
      !   |   |   |   |   |
      !   1---2---3---4---5       (y=0)
      ! x=  0  100 200 300 400
      call create_netcdf_from_cdl( &
         filename, &
         [character(len=80) :: &
            'netcdf minimal_net {', &
            'dimensions:', &
            '    mesh2d_nNodes = 10 ;', &
            '    mesh2d_nEdges = 13 ;', &
            '    mesh2d_nFaces = 4 ;', &
            '    mesh2d_nMax_face_nodes = 4 ;', &
            '    Two = 2 ;', &
            'variables:', &
            '    int mesh2d ;', &
            '        mesh2d:cf_role = "mesh_topology" ;', &
            '        mesh2d:topology_dimension = 2 ;', &
            '        mesh2d:node_coordinates = "mesh2d_node_x mesh2d_node_y" ;', &
            '        mesh2d:edge_node_connectivity = "mesh2d_edge_nodes" ;', &
            '        mesh2d:face_node_connectivity = "mesh2d_face_nodes" ;', &
            '    double mesh2d_node_x(mesh2d_nNodes) ;', &
            '        mesh2d_node_x:standard_name = "projection_x_coordinate" ;', &
            '        mesh2d_node_x:units = "m" ;', &
            '    double mesh2d_node_y(mesh2d_nNodes) ;', &
            '        mesh2d_node_y:standard_name = "projection_y_coordinate" ;', &
            '        mesh2d_node_y:units = "m" ;', &
            '    int mesh2d_edge_nodes(mesh2d_nEdges, Two) ;', &
            '        mesh2d_edge_nodes:cf_role = "edge_node_connectivity" ;', &
            '        mesh2d_edge_nodes:start_index = 1 ;', &
            '    int mesh2d_face_nodes(mesh2d_nFaces, mesh2d_nMax_face_nodes) ;', &
            '        mesh2d_face_nodes:cf_role = "face_node_connectivity" ;', &
            '        mesh2d_face_nodes:start_index = 1 ;', &
            '', &
            '// global attributes:', &
            '        :Conventions = "CF-1.8 UGRID-1.0" ;', &
            'data:', &
            '', &
            ' mesh2d_node_x = 0, 100, 200, 300, 400, 0, 100, 200, 300, 400 ;', &
            ' mesh2d_node_y = 0, 0, 0, 0, 0, 100, 100, 100, 100, 100 ;', &
            '', &
            ' // One edge per row: 4 bottom horizontal, 4 top horizontal, then 5 vertical edges.', &
            ' mesh2d_edge_nodes =', &
            '    1, 2,   2, 3,   3, 4,   4, 5,', &
            '    6, 7,   7, 8,   8, 9,   9, 10,', &
            '    1, 6,   2, 7,   3, 8,   4, 9,   5, 10 ;', &
            '', &
            ' // One face per row (counter-clockwise).', &
            ' mesh2d_face_nodes =', &
            '    1, 2, 7, 6,   2, 3, 8, 7,   3, 4, 9, 8,   4, 5, 10, 9 ;', &
            '}' &
         ], success)
   end subroutine create_minimal_netfile

   !> Create a minimal MDU file that references the net file and structure file.
   subroutine create_mdu_file(mdu_file, net_file, str_file)
      character(len=*), intent(in) :: mdu_file, net_file, str_file

      call create_file(mdu_file, [character(len=512) :: &
                                  "[General]", &
                                  "    fileVersion           = 1.09", &
                                  "    fileType              = modelDef", &
                                  "    program               = D-Flow FM", &
                                  "    ConvertLongCulverts   = 1", &
                                  "", &
                                  "[geometry]", &
                                  "    netFile               = "//trim(net_file), &
                                  "    StructureFile         = "//trim(str_file), &
                                  "    UseCaching             = 0", &
                                  "", &
                                  "[time]", &
                                  "    refDate               = 20000101", &
                                  "    tUnit                 = S", &
                                  "    tStart                = 0.0", &
                                  "    tStop                 = 100.0", &
                                  "    dtMax                 = 10.0", &
                                  "    dtUser                = 10.0", &
                                  "    dtInit                = 1.0"])
   end subroutine create_mdu_file

   !$gtest
   !> Verifies that flow_modelinit succeeds for a minimal 2D model containing
   !! a long culvert, and that flow is driven through it by a water level gradient.
   !! The two middle cells have a raised bed level (barrier), forcing all flow
   !! through the culvert. The left cell starts at a higher water level than the
   !! right cell, so we expect a positive discharge through the culvert link.
   subroutine test_flow_modelinit_with_longculvert() bind(C)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use m_flowgeom, only: ndx, lnx, xz, yz, bl
      use m_flow, only: s1, q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use messagehandling, only: LEVEL_FATAL
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use MessageHandling, only: SetMessageHandling
      use m_resetfullflowmodel, only: resetfullflowmodel
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      type(t_abort_threshold_guard) :: abort_threshold
      integer :: iresult, i
      logical :: success

      character(len=*), parameter :: NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"

      character(len=256) :: mdu_local
      integer :: lc_link

      ! ARRANGE: Create all input files
      call create_minimal_netfile(NET_FILE, success)
      if (.not. f90_assert_true(success, cstr("NetCDF net file creation should succeed"))) then
         call default_longculverts()
         return
      end if

      call create_longculvert_structure_file(TEST_STR_FILE, LC2_X(:, :1), LC2_Y(:, :1), LC2_Z(:, :1))
      call create_mdu_file(TEST_MDU_FILE, NET_FILE, TEST_STR_FILE)
      md_ident = TEST_MDU_FILE
      call abort_threshold%set(LEVEL_FATAL)
      call resetFullFlowModel()
      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      mdu_local = TEST_MDU_FILE
      call loadModel(mdu_local)
      iresult = flow_modelinit()

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("flow_modelinit should return DFM_NOERR for a valid model with a long culvert"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 1, cstr("one long culvert should be registered"))) then
         call default_longculverts()
         return
      end if

      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp ! barrier in cells 2 and 3
         end if
      end do
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp ! left cell: high water level
         else
            s1(i) = 0.0_dp ! remaining cells: low water level
         end if
      end do

      call flow_spatietimestep()

      ! ASSERT: Flow should pass through the culvert from left to right.
      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid (> 0)"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(q1(lc_link) > 0.0_dp, cstr("discharge through culvert should be positive (left to right)"))

      call default_longculverts()

   end subroutine test_flow_modelinit_with_longculvert

   !> Shared helper: initializes the model from the test MDU file.
   !! Returns flow_modelinit result in iresult.
   subroutine setup_longculvert_model(iresult)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
      use messagehandling, only: LEVEL_FATAL
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use MessageHandling, only: SetMessageHandling
      use m_resetfullflowmodel, only: resetfullflowmodel
      type(t_abort_threshold_guard) :: abort_threshold
      integer, intent(out) :: iresult

      character(len=*), parameter :: NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"
      character(len=256) :: mdu_local
      logical :: success

      call create_minimal_netfile(NET_FILE, success)
      if (.not. success) then
         iresult = DFM_GENERICERROR
         return
      end if
      call create_longculvert_structure_file(TEST_STR_FILE, LC2_X(:, :1), LC2_Y(:, :1), LC2_Z(:, :1))
      call create_mdu_file(TEST_MDU_FILE, NET_FILE, TEST_STR_FILE)
      md_ident = TEST_MDU_FILE
      call abort_threshold%set(LEVEL_FATAL)
      call resetFullFlowModel()
      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      mdu_local = TEST_MDU_FILE
      call loadModel(mdu_local)
      iresult = flow_modelinit()
   end subroutine setup_longculvert_model

   !$gtest
   !> Verifies that flow_modelinit succeeds for a minimal 2D model with a long culvert.
   subroutine test_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_longculvert_model(iresult)

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("flow_modelinit should return DFM_NOERR"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(ndx > 0, cstr("ndx should be > 0"))
      call f90_expect_true(lnx > 0, cstr("lnx should be > 0"))
      if (.not. f90_assert_eq(nlongculverts, 1, cstr("one long culvert should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(longculverts(1)%flowlinks(1) > 0, cstr("culvert should have a valid flow link"))

      call default_longculverts()
   end subroutine test_modelinit_succeeds

   !$gtest
   !> With a bed level barrier in the middle cells and a water level gradient,
   !! flow should pass through the culvert from the high-head side to the low-head side.
   subroutine test_flow_head_difference_drives_discharge() bind(C)
      use m_flow_modelinit, only: flow_modelinit
      use m_flowgeom, only: ndx, lnx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_longculvert_model(iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      ! Raise bed level on middle cells to block overland flow.
      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp
         end if
      end do
      ! Apply water level gradient: left cell high, rest low.
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp
         else
            s1(i) = 0.0_dp
         end if
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(q1(lc_link) > 0.0_dp, cstr("discharge should be positive (left to right)"))

      call default_longculverts()
   end subroutine test_flow_head_difference_drives_discharge

   !$gtest
   !> With a uniform water level across all cells there should be no discharge
   !! through the culvert.
   subroutine test_flow_no_head_difference_no_discharge() bind(C)
      use m_flowgeom, only: ndx, lnx, bl
      use m_flow, only: s1, q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_longculvert_model(iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      ! Uniform water level everywhere no driving force.
      do i = 1, ndx
         s1(i) = 1.0_dp
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, cstr("discharge should be ~zero with no head difference"))

      call default_longculverts()
   end subroutine test_flow_no_head_difference_no_discharge

   !$gtest
   !> With the valve fully closed (valve_relative_opening = 0), no flow should
   !! pass through the culvert even with a head difference.
   subroutine test_valve_closed_blocks_flow() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_longculvert_model(iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      ! Close the valve completely.
      longculverts(1)%valve_relative_opening = 0.0_dp

      ! Raise barrier and apply head difference as before.
      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp
         end if
      end do
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp
         else
            s1(i) = 0.0_dp
         end if
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, cstr("discharge should be ~zero when valve is closed"))

      call default_longculverts()
   end subroutine test_valve_closed_blocks_flow

   !$gtest
   !> With the head gradient reversed (right higher than left), the discharge
   !! through the culvert should be negative.
   subroutine test_flow_reverse_head_gives_negative_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_longculvert_model(iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      ! Raise barrier in middle cells.
      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp
         end if
      end do
      ! Reversed gradient: right cell high, left cell low.
      do i = 1, ndx
         if (xz(i) > 300.0_dp) then
            s1(i) = 2.0_dp
         else
            s1(i) = 0.0_dp
         end if
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(q1(lc_link) < 0.0_dp, cstr("discharge should be negative (right to left)"))

      call default_longculverts()
   end subroutine test_flow_reverse_head_gives_negative_discharge

   !$gtest
   !> Verifies that direction reconstruction and allowedFlowDir follow the
   !! input-coordinate order when the local flow-link orientation changes.
   subroutine test_allowed_flowdir_follows_coordinate_order() bind(C)
      use m_1d_structures, only: FLOWDIR_POSITIVE
      use m_flow, only: au, u1
      use m_flowgeom, only: ln
      use m_longculverts, only: default_longculverts, find1d2dculvertlinks, longculvertsToProfs, reduceFlowAreaAtLongculverts
      use m_longculverts_data, only: longculverts
      use network_data, only: xzw, yzw
      use unstruc_channel_flow, only: network
      use dfm_error, only: DFM_NOERR
      use precision, only: dp

      integer :: iresult, lc_link, node, flow_dir_before
      character(len=*), parameter :: NET_FILE = "test_lc_allowed_flowdir_net.nc"
      character(len=*), parameter :: STR_FILE = "test_lc_allowed_flowdir_str.ini"
      character(len=256) :: mdu_file

      mdu_file = "test_lc_allowed_flowdir.mdu"
      call create_longculvert_structure_file(STR_FILE, LC2_X(:, :1), LC2_Y(:, :1), LC2_Z(:, :1))
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("converted model init must succeed"))) then
         call default_longculverts()
         return
      end if

      call find1d2dculvertlinks(network, longculverts(1), size(longculverts(1)%xcoords))
      lc_link = longculverts(1)%flowlinks(1)
      flow_dir_before = longculverts(1)%orientation
      node = ln(1, lc_link)
      ln(1, lc_link) = ln(2, lc_link)
      ln(2, lc_link) = node
      call find1d2dculvertlinks(network, longculverts(1), size(longculverts(1)%xcoords))

      call f90_expect_true(longculverts(1)%orientation == -flow_dir_before, cstr("reversing the flow link must reverse its mapping to input-coordinate direction"))

      longculverts(1)%allowed_flowdir = FLOWDIR_POSITIVE
      longculverts(1)%valve_relative_opening = 1.0_dp
      au(lc_link) = 1.0_dp
      u1(lc_link) = real(longculverts(1)%orientation, dp)
      call reduceFlowAreaAtLongculverts()

      call f90_expect_true(au(lc_link) > 0.0_dp, cstr("positive flow in input-coordinate direction must remain open"))

      ! Simulate a partition where the first global segment is absent and the
      ! representative local flowlinks(1) corresponds to coordinate pair 2->3.
      ! Include the rounding error introduced by writing converted coordinates to an INI file.
      longculverts(1)%xcoords = [-huge(1.0_dp), &
                                 xzw(ln(1, lc_link)) + 3.0_dp * epsilon(1.0_dp) * max(abs(xzw(ln(1, lc_link))), 1.0_dp), &
                                 xzw(ln(2, lc_link)) + 3.0_dp * epsilon(1.0_dp) * max(abs(xzw(ln(2, lc_link))), 1.0_dp)]
      longculverts(1)%ycoords = [-huge(1.0_dp), yzw(ln(1, lc_link)), yzw(ln(2, lc_link))]
      longculverts(1)%orientation = -1
      call longculvertsToProfs(.true.)
      call f90_expect_eq(longculverts(1)%orientation, 1, cstr("orientation reconstruction must find the second segment as the first local flow link"))

      call default_longculverts()
   end subroutine test_allowed_flowdir_follows_coordinate_order

   !$gtest
   !> Verifies converted-culvert orientation using actual discharge, then checks
   !! the complete allowedFlowDir truth table on the initialized flow links.
   subroutine test_converted_allowed_flowdir_controls_discharge() bind(C)
      use m_1d_structures, only: FLOWDIR_POSITIVE, FLOWDIR_NEGATIVE
      use m_flow, only: au, hu, q1, u1
      use m_longculverts, only: reduceFlowAreaAtLongculverts
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, flow_link, duplicate_flow_link
      integer :: flow_links(2)
      character(len=*), parameter :: STR_FILE = "test_lc_flowdir_str.ini"
      character(len=*), parameter :: NET_FILE = "test_lc_flowdir_net.nc"
      character(len=256) :: mdu_file

      mdu_file = "test_lc_flowdir.mdu"
      call create_longculvert_structure_file(STR_FILE, &
                                             reshape([50.0_dp, 350.0_dp, 350.0_dp, 50.0_dp, &
                                                      50.0_dp, 350.0_dp], [2, 3]), &
                                             reshape([50.0_dp, 50.0_dp, 150.0_dp, 150.0_dp, &
                                                      50.0_dp, 50.0_dp], [2, 3]), &
                                             reshape([-5.0_dp, -5.0_dp, -5.0_dp, -5.0_dp, &
                                                      -5.0_dp, -5.0_dp], [2, 3]), &
                                             ids=[character(len=32) :: "lc_left_to_right", "lc_right_to_left", &
                                                  "lc_left_to_right_duplicate"], &
                                             allowed_flowdirs=[character(len=8) :: "positive", "positive", "positive"])
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("converted positive-flow model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 3, cstr("three converted positive-flow culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_eq(longculverts(1)%allowed_flowdir, FLOWDIR_POSITIVE, cstr("left-to-right culvert should retain allowedFlowDir=positive"))
      call f90_expect_eq(longculverts(2)%allowed_flowdir, FLOWDIR_POSITIVE, cstr("right-to-left culvert should retain allowedFlowDir=positive"))
      call f90_expect_eq(longculverts(3)%allowed_flowdir, FLOWDIR_POSITIVE, cstr("duplicate culvert should retain allowedFlowDir=positive"))

      call flow_spatietimestep()
      flow_links = [abs(longculverts(1)%flowlinks(1)), abs(longculverts(2)%flowlinks(1))]
      duplicate_flow_link = abs(longculverts(3)%flowlinks(1))
      if (.not. f90_assert_true(all(flow_links > 0) .and. all(flow_links <= size(q1)), &
                                "culvert flow links must index the discharge array")) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_true(duplicate_flow_link > 0 .and. duplicate_flow_link <= size(q1), &
                                "duplicate culvert flow link must index the discharge array")) then
         call default_longculverts()
         return
      end if
      call f90_expect_ne(duplicate_flow_link, flow_links(1), &
                         cstr("identical snapped culverts must retain distinct valid flow links"))
      call f90_expect_eq(longculverts(3)%orientation, longculverts(1)%orientation, cstr("identical snapped culverts must have identical orientation"))
      call f90_expect_true(abs(q1(flow_links(1))) > 0.0_dp, cstr("positive must permit left-to-right flow for left-to-right coordinates"))
      call f90_expect_near(q1(duplicate_flow_link), q1(flow_links(1)), 1.0e-10_dp, cstr("identical snapped culverts must have identical discharge"))
      call f90_expect_near(q1(flow_links(2)), 0.0_dp, 1.0e-10_dp, cstr("positive must block left-to-right flow for right-to-left coordinates"))

      do i = 1, 2
         flow_link = flow_links(i)
         longculverts(i)%allowed_flowdir = FLOWDIR_POSITIVE
         au(flow_link) = 1.0_dp
         hu(flow_link) = 1.0_dp
         u1(flow_link) = real(longculverts(i)%orientation, dp)
      end do
      call reduceFlowAreaAtLongculverts()
      call f90_expect_true(au(flow_links(1)) > 0.0_dp .and. au(flow_links(2)) > 0.0_dp, &
                           cstr("positive must permit flow in coordinate order for both link orientations"))

      do i = 1, 2
         flow_link = flow_links(i)
         au(flow_link) = 1.0_dp
         hu(flow_link) = 1.0_dp
         u1(flow_link) = -real(longculverts(i)%orientation, dp)
      end do
      call reduceFlowAreaAtLongculverts()
      call f90_expect_near(au(flow_links(1)), 0.0_dp, 1.0e-10_dp, cstr("positive must block reverse flow for the left-to-right culvert"))
      call f90_expect_near(au(flow_links(2)), 0.0_dp, 1.0e-10_dp, cstr("positive must block reverse flow for the right-to-left culvert"))

      do i = 1, 2
         flow_link = flow_links(i)
         longculverts(i)%allowed_flowdir = FLOWDIR_NEGATIVE
         au(flow_link) = 1.0_dp
         hu(flow_link) = 1.0_dp
         u1(flow_link) = real(longculverts(i)%orientation, dp)
      end do
      call reduceFlowAreaAtLongculverts()
      call f90_expect_near(au(flow_links(1)), 0.0_dp, 1.0e-10_dp, cstr("negative must block forward flow for the left-to-right culvert"))
      call f90_expect_near(au(flow_links(2)), 0.0_dp, 1.0e-10_dp, cstr("negative must block forward flow for the right-to-left culvert"))

      do i = 1, 2
         flow_link = flow_links(i)
         au(flow_link) = 1.0_dp
         hu(flow_link) = 1.0_dp
         u1(flow_link) = -real(longculverts(i)%orientation, dp)
      end do
      call reduceFlowAreaAtLongculverts()
      call f90_expect_true(au(flow_links(1)) > 0.0_dp .and. au(flow_links(2)) > 0.0_dp, cstr("negative must permit reverse flow for both link orientations"))

      call default_longculverts()
   end subroutine test_converted_allowed_flowdir_controls_discharge

   !> Create a 5x3-node UGRID net (4 cells wide, 2 rows) from a CDL description.
   subroutine create_two_row_netfile(filename)
      character(len=*), intent(in) :: filename

      logical :: success

      call create_netcdf_from_cdl( &
         filename, &
         [character(len=80) :: &
            'netcdf two_row_net {', &
            'dimensions:', &
            '    mesh2d_nNodes = 15 ;', &
            '    mesh2d_nEdges = 22 ;', &
            '    mesh2d_nFaces = 8 ;', &
            '    mesh2d_nMax_face_nodes = 4 ;', &
            '    Two = 2 ;', &
            'variables:', &
            '    int mesh2d ;', &
            '        mesh2d:cf_role = "mesh_topology" ;', &
            '        mesh2d:topology_dimension = 2 ;', &
            '        mesh2d:node_coordinates = "mesh2d_node_x mesh2d_node_y" ;', &
            '        mesh2d:edge_node_connectivity = "mesh2d_edge_nodes" ;', &
            '        mesh2d:face_node_connectivity = "mesh2d_face_nodes" ;', &
            '    double mesh2d_node_x(mesh2d_nNodes) ;', &
            '        mesh2d_node_x:standard_name = "projection_x_coordinate" ;', &
            '        mesh2d_node_x:units = "m" ;', &
            '    double mesh2d_node_y(mesh2d_nNodes) ;', &
            '        mesh2d_node_y:standard_name = "projection_y_coordinate" ;', &
            '        mesh2d_node_y:units = "m" ;', &
            '    int mesh2d_edge_nodes(mesh2d_nEdges, Two) ;', &
            '        mesh2d_edge_nodes:cf_role = "edge_node_connectivity" ;', &
            '        mesh2d_edge_nodes:start_index = 1 ;', &
            '    int mesh2d_face_nodes(mesh2d_nFaces, mesh2d_nMax_face_nodes) ;', &
            '        mesh2d_face_nodes:cf_role = "face_node_connectivity" ;', &
            '        mesh2d_face_nodes:start_index = 1 ;', &
            '', &
            '// global attributes:', &
            '        :Conventions = "CF-1.8 UGRID-1.0" ;', &
            'data:', &
            '', &
            ' // 5 columns x 3 rows of nodes', &
            ' mesh2d_node_x = 0, 100, 200, 300, 400,', &
            '                0, 100, 200, 300, 400,', &
            '                0, 100, 200, 300, 400 ;', &
            ' mesh2d_node_y = 0, 0, 0, 0, 0,', &
            '                100, 100, 100, 100, 100,', &
            '                200, 200, 200, 200, 200 ;', &
            '', &
            ' // One edge per row: bottom, middle and top horizontal edges,', &
            ' // then the vertical edges of the bottom tier and of the top tier.', &
            ' mesh2d_edge_nodes =', &
            '    1, 2,   2, 3,   3, 4,   4, 5,', &
            '    6, 7,   7, 8,   8, 9,   9, 10,', &
            '    11, 12, 12, 13, 13, 14, 14, 15,', &
            '    1, 6,   2, 7,   3, 8,   4, 9,   5, 10,', &
            '    6, 11,  7, 12,  8, 13,  9, 14,  10, 15 ;', &
            '', &
            ' // One face per row (counter-clockwise): bottom row, then top row.', &
            ' mesh2d_face_nodes =', &
            '    1, 2, 7, 6,   2, 3, 8, 7,   3, 4, 9, 8,   4, 5, 10, 9,', &
            '    6, 7, 12, 11, 7, 8, 13, 12, 8, 9, 14, 13, 9, 10, 15, 14 ;', &
            '}' &
         ], success)
      call f90_expect_true(success, "Could not create the two-row net file "//filename)
   end subroutine create_two_row_netfile

   !> Shared helper for two-culvert integration tests.
   !! Creates the two-row net, initialises the model from the given structure and
   !! MDU files, raises a bed level barrier on the middle cells, and applies a
   !! left-to-right water level gradient. The caller only needs to write the
   !! structure file before calling this, then call flow_spatietimestep and assert.
   subroutine init_two_culvert_scenario(mdu_file, iresult)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use m_flowgeom, only: ndx, ndx2d, bl
      use m_flow, only: s1
      use m_cell_geometry, only: xz
      use dfm_error, only: DFM_NOERR
      use messagehandling, only: LEVEL_FATAL, SetMessageHandling
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use m_resetfullflowmodel, only: resetfullflowmodel
      use precision, only: dp

      type(t_abort_threshold_guard) :: abort_threshold
      character(len=*), intent(in) :: mdu_file
      integer, intent(out) :: iresult

      character(len=256) :: mdu_local
      integer :: ierr, i

      md_ident = mdu_file
      call abort_threshold%set(LEVEL_FATAL)
      call resetFullFlowModel()
      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      mdu_local = mdu_file
      call loadModel(mdu_local)
      iresult = flow_modelinit()

      if (iresult /= DFM_NOERR) then
         return
      end if

      do i = 1, ndx2D
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp
         end if
      end do
      do i = 1, ndx
         s1(i) = merge(2.0_dp, 0.0_dp, xz(i) < 100.0_dp)
      end do
   end subroutine init_two_culvert_scenario

   !$gtest
   subroutine test_valve_half_open_reduces_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link_full, lc_link_half
      real(kind=dp) :: q_full, q_half
      character(len=*), parameter :: STR_FILE = "test_lc_valve_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc_valve.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC2_X, LC2_Y, LC2_Z, valve_openings=[1.0_dp, 0.5_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if

      call flow_spatietimestep()

      q_full = q1(longculverts(1)%flowlinks(1))
      q_half = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_full > 0.0_dp, cstr("full-open discharge should be positive"))
      call f90_expect_true(q_half > 0.0_dp, cstr("half-open discharge should be positive"))
      call f90_expect_true(q_half < q_full, cstr("half-open discharge should be less than fully-open"))
      call default_longculverts()
   end subroutine test_valve_half_open_reduces_discharge

   !$gtest
   subroutine test_friction_higher_value_reduces_discharge() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc_friction_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc_friction.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC2_X, LC2_Y, LC2_Z, friction_values=[0.01_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4 !> multiple timesteps, from rest friction has no effect
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1)) ! lc01: Manning n=0.01
      q_high_friction = q1(longculverts(2)%flowlinks(1)) ! lc02: Manning n=0.05
      call f90_expect_true(q_low_friction > 0.0_dp, cstr("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, cstr("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, cstr("higher Manning friction should produce less discharge"))
      call default_longculverts()
   end subroutine test_friction_higher_value_reduces_discharge

   !$gtest
   subroutine test_2pt_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc2pt_fric_str.ini"
      character(len=256) :: MDU_FILE
      character(len=*), parameter :: NET_FILE = "test_lc_convert_2pt_net.nc"

      MDU_FILE = "test_lc2pt_fric_converted.mdu"
      call create_longculvert_structure_file(STR_FILE, LC2_X, LC2_Y, LC2_Z, friction_values=[0.01_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(longculverts(1)%numlinks, 1, cstr("2-point culvert should have 1 link"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, cstr("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, cstr("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, cstr("higher Manning friction should produce less discharge"))
      call default_longculverts()
   end subroutine test_2pt_friction_converted

   !$gtest
   subroutine test_2pt_default_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc2pt_default_friction_str.ini"
      character(len=256) :: MDU_FILE
      character(len=*), parameter :: NET_FILE = "test_lc_convert_2pt_net.nc"

      MDU_FILE = "test_lc2pt_default_friction_converted.mdu"
      call create_file(STR_FILE, [character(len=64) :: &
                                  "[General]", &
                                  "    fileVersion     = 3.00", &
                                  "    fileType        = structures", &
                                  "", &
                                  "[Structure]", &
                                  "    id              = lc01", &
                                  "    type            = longCulvert", &
                                  "    numCoordinates  = 2", &
                                  "    xCoordinates    = 50.0 350.0", &
                                  "    yCoordinates    = 50.0 50.0", &
                                  "    zCoordinates    = -5.0 -5.0", &
                                  "    allowedFlowDir  = both", &
                                  "    width           = 2.0", &
                                  "    height          = 2.0", &
                                  "    valveRelativeOpening = 1.0", &
                                  "", &
                                  "[Structure]", &
                                  "    id              = lc02", &
                                  "    type            = longCulvert", &
                                  "    numCoordinates  = 2", &
                                  "    xCoordinates    = 50.0 350.0", &
                                  "    yCoordinates    = 150.0 150.0", &
                                  "    zCoordinates    = -5.0 -5.0", &
                                  "    allowedFlowDir  = both", &
                                  "    width           = 2.0", &
                                  "    height          = 2.0", &
                                  "    frictionType    = Manning", &
                                  "    frictionValue   = 0.023", &
                                  "    valveRelativeOpening = 1.0"])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(longculverts(1)%numlinks, 1, cstr("2-point culvert should have 1 link"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, cstr("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, cstr("high-friction discharge should be positive"))
      call f90_expect_near(q_high_friction, q_low_friction, 1.0e-10_dp, cstr("discharge should be equal due to equal roughness"))
      call default_longculverts()
   end subroutine test_2pt_default_friction_converted

   !$gtest
   subroutine test_friction_type_affects_discharge() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_manning, q_colebrook
      character(len=*), parameter :: STR_FILE = "test_lc_frtype_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc_frtype.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC2_X, LC2_Y, LC2_Z, &
                                             friction_types=[character(len=16) :: "Manning", "WhiteColebrook"], &
                                             friction_values=[0.05_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4 !> multiple timesteps, from rest friction has no effect
         call flow_spatietimestep()
      end do

      q_manning = q1(longculverts(1)%flowlinks(1))
      q_colebrook = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_manning > 0.0_dp, cstr("Manning culvert discharge should be positive"))
      call f90_expect_true(q_colebrook > 0.0_dp, cstr("WhiteColebrook culvert discharge should be positive"))
      call f90_expect_true(abs(q_manning - q_colebrook) > 1.0e-6_dp, cstr("different friction types with same coefficient should give different discharge"))
      call default_longculverts()
   end subroutine test_friction_type_affects_discharge

   !$gtest
   subroutine test_larger_cross_section_increases_discharge() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult
      real(kind=dp) :: q_small, q_large
      character(len=*), parameter :: STR_FILE = "test_lc_cross_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc_cross.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC2_X, LC2_Y, LC2_Z, widths=[1.0_dp, 4.0_dp], heights=[1.0_dp, 4.0_dp])
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, "two long culverts should be registered")) then
         call default_longculverts()
         return
      end if

      call flow_spatietimestep()

      q_small = q1(longculverts(1)%flowlinks(1)) ! lc01: 1x1 m
      q_large = q1(longculverts(2)%flowlinks(1)) ! lc02: 4x4 m
      call f90_expect_true(q_small > 0.0_dp, "small culvert discharge should be positive")
      call f90_expect_true(q_large > 0.0_dp, "large culvert discharge should be positive")
      call f90_expect_true(q_large > q_small, "larger cross-section should give more discharge")
      call default_longculverts()
   end subroutine test_larger_cross_section_increases_discharge

   ! ============================================================================
   ! 3-POINT LONG CULVERT TESTS
   ! ============================================================================

   !> Create and initialize a two-row model containing the specified long culverts.
   subroutine setup_two_row_longculvert_model(file_prefix, x_coordinates, y_coordinates, z_coordinates, iresult)
      character(len=*), intent(in) :: file_prefix
      real(kind=dp), dimension(:, :), intent(in) :: x_coordinates, y_coordinates, z_coordinates
      integer, intent(out) :: iresult

      character(len=256) :: mdu_file, net_file, str_file

      mdu_file = trim(file_prefix)//".mdu"
      net_file = trim(file_prefix)//"_net.nc"
      str_file = trim(file_prefix)//"_structures.ini"
      call create_longculvert_structure_file(STR_FILE, x_coordinates, y_coordinates, z_coordinates)
      call create_two_row_netfile(net_file)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(mdu_file, iresult)

   end subroutine setup_two_row_longculvert_model

   !$gtest
   subroutine test_3pt_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_two_row_longculvert_model("test_lc3pt", LC3_X, LC3_Y, LC3_Z, iresult)

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("flow_modelinit should succeed for 3-point culvert"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_eq(longculverts(1)%numlinks, 2, cstr("3-point culvert should have 2 links"))

      call default_longculverts()
   end subroutine test_3pt_modelinit_succeeds

   !$gtest
   subroutine test_3pt_head_difference_drives_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_two_row_longculvert_model("test_lc3pt", LC3_X, LC3_Y, LC3_Z, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(q1(lc_link) > 0.0_dp, cstr("discharge should be positive (left to right)"))

      call default_longculverts()
   end subroutine test_3pt_head_difference_drives_discharge

   !$gtest
   subroutine test_3pt_valve_closed_blocks_flow() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_two_row_longculvert_model("test_lc3pt", LC3_X, LC3_Y, LC3_Z, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      longculverts(1)%valve_relative_opening = 0.0_dp

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, cstr("discharge should be ~zero when valve is closed"))

      call default_longculverts()
   end subroutine test_3pt_valve_closed_blocks_flow

   !$gtest
   subroutine test_3pt_friction_higher_value_reduces_discharge() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc3pt_fric_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc3pt_fric.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC3_X, LC3_Y, LC3_Z, friction_values=[0.01_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, cstr("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, cstr("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, cstr("higher Manning friction should produce less discharge"))
      call default_longculverts()
   end subroutine test_3pt_friction_higher_value_reduces_discharge

   !$gtest
   subroutine test_3pt_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc3pt_fric_str.ini"
      character(len=256) :: MDU_FILE
      character(len=*), parameter :: NET_FILE = "test_lc_convert_3pt_net.nc"

      MDU_FILE = "test_lc3pt_fric.mdu"
      call create_longculvert_structure_file(STR_FILE, LC3_X, LC3_Y, LC3_Z, friction_values=[0.01_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_eq(longculverts(1)%numlinks, 2, cstr("3-point culvert should have 0 links"))
      !> the test stops here, as a 3 PT culvert cannot be written to the netfile in a ugrid compliant way

   end subroutine test_3pt_friction_converted

   ! ============================================================================
   ! 4-POINT LONG CULVERT TESTS
   ! ============================================================================

   !$gtest
   subroutine test_4pt_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_two_row_longculvert_model("test_lc4pt", LC4_X, LC4_Y, LC4_Z, iresult)

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("flow_modelinit should succeed for 4-point culvert"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_eq(longculverts(1)%numlinks, 3, cstr("4-point culvert should have 3 links"))

      call default_longculverts()
   end subroutine test_4pt_modelinit_succeeds

   !$gtest
   subroutine test_4pt_head_difference_drives_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_two_row_longculvert_model("test_lc4pt", LC4_X, LC4_Y, LC4_Z, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(q1(lc_link) > 0.0_dp, cstr("discharge should be positive (left to right)"))

      call default_longculverts()
   end subroutine test_4pt_head_difference_drives_discharge

   !$gtest
   subroutine test_4pt_valve_closed_blocks_flow() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_two_row_longculvert_model("test_lc4pt", LC4_X, LC4_Y, LC4_Z, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if

      longculverts(1)%valve_relative_opening = 0.0_dp

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      if (.not. f90_assert_true(lc_link > 0 .and. lc_link <= size(q1), cstr("culvert flow link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, cstr("discharge should be ~zero when valve is closed"))

      call default_longculverts()
   end subroutine test_4pt_valve_closed_blocks_flow

   !$gtest
   subroutine test_4pt_friction_higher_value_reduces_discharge() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc4pt_fric_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc4pt_fric.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc2_net.nc"

      call create_longculvert_structure_file(STR_FILE, LC4_X, LC4_Y, LC4_Z, friction_values=[0.01_dp, 0.05_dp])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, cstr("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, cstr("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, cstr("higher Manning friction should produce less discharge"))
      call default_longculverts()

   end subroutine test_4pt_friction_higher_value_reduces_discharge

   !$gtest
   !> For a 4-point culvert, verify that the discharge is consistent across all 3 links
   !! (mass conservation within the culvert pipe).
   subroutine test_4pt_flow_continuity_across_links() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, L1, L2, L3

      call setup_two_row_longculvert_model("test_lc4pt", LC4_X, LC4_Y, LC4_Z, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(longculverts(1)%numlinks, 3, cstr("4-point culvert should have 3 links"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 15
         call flow_spatietimestep()
      end do

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      call f90_expect_true(q1(L1) > 0.0_dp, cstr("discharge at link 1 should be positive"))
      ! In steady state, Q should be equal across all links (continuity).
      ! After a few timesteps it wont be perfectly steady, but should be close enough (15%)
      call f90_expect_near(q1(L1), q1(L2), 0.15_dp * abs(q1(L1)), cstr("discharge at links 1 and 2 should be similar (continuity)"))
      call f90_expect_near(q1(L2), q1(L3), 0.15_dp * abs(q1(L2)), cstr("discharge at links 2 and 3 should be similar (continuity)"))

      call default_longculverts()
   end subroutine test_4pt_flow_continuity_across_links

   !$gtest
   !> For a 4-point culvert, verify that the discharge is consistent across all 3 links
   !! (mass conservation within the culvert pipe).
   subroutine test_4pt_flow_continuity_converted() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, L1, L2, L3
      character(len=*), parameter :: NET_FILE = "test_lc_convert_4pt_net.nc"
      character(len=*), parameter :: STR_FILE = "test_lc_convert_4pt_str.ini"
      character(len=256) :: mdu_file

      mdu_file = "test_lc_convert_4pt.mdu"
      call create_longculvert_structure_file(STR_FILE, LC4_X, LC4_Y, LC4_Z)
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("model init must succeed"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(longculverts(1)%numlinks, 3, cstr("4-point culvert should have 3 links"))) then
         call default_longculverts()
         return
      end if

      do i = 1, 15
         call flow_spatietimestep()
      end do

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      call f90_expect_true(q1(L1) < 0.0_dp, cstr("discharge at link 1 should be negative"))
      ! In steady state, Q should be equal across all links (continuity).
      ! After a few timesteps it wont be perfectly steady, but should be close enough (15%)
      call f90_expect_near(-q1(L1), q1(L2), 0.15_dp * abs(q1(L1)), cstr("discharge at links 1 and 2 should be similar (continuity)"))
      call f90_expect_near(q1(L2), q1(L3), 0.15_dp * abs(q1(L2)), cstr("discharge at links 2 and 3 should be similar (continuity)"))

      call default_longculverts()
   end subroutine test_4pt_flow_continuity_converted

   !$gtest
   !> Verify that a 4-point long culvert works correctly when there is already
   !! an existing 1D network (branch + cross sections) in the model.
   !! This tests that meshgeom1d arrays are properly extended and that
   !! admin_network handles both regular and culvert branches.
   subroutine test_4pt_with_existing_1d_network() bind(C)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use m_longculverts_data, only: nlongculverts, longculverts
      use m_flow, only: q1, s1, au
      use m_flowgeom, only: ndx, ndx2d, bl
      use m_cell_geometry, only: xz
      use dfm_error, only: DFM_NOERR
      use messagehandling, only: LEVEL_FATAL, SetMessageHandling
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use m_resetfullflowmodel, only: resetfullflowmodel
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, L1, L2, L3
      character(len=*), parameter :: NET_FILE = "test_lc_existing1d_net.nc"
      character(len=*), parameter :: STR_FILE = "test_lc_existing1d_str.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc_existing1d.mdu"
      character(len=256) :: mdu_local
      logical :: success

      ! Create a net file that includes a 1D network (branch) alongside the 2D grid.
      call create_net_with_1d_branch(NET_FILE, success)
      if (.not. f90_assert_true(success, cstr("Net file with 1D branch creation should succeed"))) then
         call default_longculverts()
         return
      end if

      ! Create structure file with a 4-point long culvert
      call create_longculvert_structure_file(STR_FILE, LC4_X, LC4_Y, LC4_Z)

      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call init_two_culvert_scenario(MDU_FILE, iresult)

      if (.not. f90_assert_eq(iresult, DFM_NOERR, cstr("flow_modelinit should succeed with existing 1D network + long culvert"))) then
         call default_longculverts()
         return
      end if
      if (.not. f90_assert_eq(nlongculverts, 2, cstr("two long culverts should be registered"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_eq(longculverts(1)%numlinks, 3, cstr("4-point culvert should have 3 links"))

      call flow_spatietimestep()

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      if (.not. f90_assert_true(L1 > 0 .and. L1 <= size(q1), cstr("entry link should be valid"))) then
         call default_longculverts()
         return
      end if
      call f90_expect_true(au(L2) > 0.0_dp, cstr("au on interior link should be > 0 with existing 1D network"))
      call f90_expect_true(q1(L1) > 0.0_dp, cstr("discharge at entry should be positive with existing 1D network"))

      call default_longculverts()
   end subroutine test_4pt_with_existing_1d_network

   !> Create a UGRID net file that has both a 2D grid (5x3 nodes, 8 cells)
   !! AND a pre-existing, network-backed 1D mesh: a single network1d branch
   !! along y=50 from x=-200 to x=-100 (outside the 2D grid, so it does not
   !! interfere with the culvert placement), discretised into 3 mesh1d nodes.
   !! The 1D part is written as a full network1d + network-coupled mesh1d
   !! (branch index + offset) so the reader populates meshgeom1d (networkIndex > 0).
   subroutine create_net_with_1d_branch(filename, success)
      character(len=*), intent(in) :: filename
      logical, intent(out) :: success

      ! The 2D grid is the same as in create_two_row_netfile. The 1D branch is a single branch from (-200, 50)
      ! to (-100, 50) with length 100, with 3 mesh1d nodes at offsets 0, 50 and 100 on branch 1 (0-based index).
      ! The string variables have fixed lengths: 40 characters for ids and 80 characters for long names.
      call create_netcdf_from_cdl( &
         filename, &
         [character(len=110) :: &
            'netcdf net_with_1d_branch {', &
            'dimensions:', &
            '    mesh2d_nNodes = 15 ;', &
            '    mesh2d_nEdges = 22 ;', &
            '    mesh2d_nFaces = 8 ;', &
            '    mesh2d_nMax_face_nodes = 4 ;', &
            '    Two = 2 ;', &
            '    strLengthIds = 40 ;', &
            '    strLengthLongNames = 80 ;', &
            '    network1d_nNodes = 2 ;', &
            '    network1d_nEdges = 1 ;', &
            '    network1d_nGeometryNodes = 2 ;', &
            '    mesh1d_nNodes = 3 ;', &
            '    mesh1d_nEdges = 2 ;', &
            'variables:', &
            '    int mesh2d ;', &
            '        mesh2d:cf_role = "mesh_topology" ;', &
            '        mesh2d:topology_dimension = 2 ;', &
            '        mesh2d:node_coordinates = "mesh2d_node_x mesh2d_node_y" ;', &
            '        mesh2d:edge_node_connectivity = "mesh2d_edge_nodes" ;', &
            '        mesh2d:face_node_connectivity = "mesh2d_face_nodes" ;', &
            '    double mesh2d_node_x(mesh2d_nNodes) ;', &
            '        mesh2d_node_x:standard_name = "projection_x_coordinate" ;', &
            '        mesh2d_node_x:units = "m" ;', &
            '    double mesh2d_node_y(mesh2d_nNodes) ;', &
            '        mesh2d_node_y:standard_name = "projection_y_coordinate" ;', &
            '        mesh2d_node_y:units = "m" ;', &
            '    int mesh2d_edge_nodes(mesh2d_nEdges, Two) ;', &
            '        mesh2d_edge_nodes:cf_role = "edge_node_connectivity" ;', &
            '        mesh2d_edge_nodes:start_index = 1 ;', &
            '    int mesh2d_face_nodes(mesh2d_nFaces, mesh2d_nMax_face_nodes) ;', &
            '        mesh2d_face_nodes:cf_role = "face_node_connectivity" ;', &
            '        mesh2d_face_nodes:start_index = 1 ;', &
            '    int network1d ;', &
            '        network1d:cf_role = "mesh_topology" ;', &
            '        network1d:long_name = "Topology data of 1D network" ;', &
            '        network1d:topology_dimension = 1 ;', &
            '        network1d:node_dimension = "network1d_nNodes" ;', &
            '        network1d:edge_dimension = "network1d_nEdges" ;', &
            '        network1d:node_coordinates = "network1d_node_x network1d_node_y" ;', &
            '        network1d:node_id = "network1d_node_id" ;', &
            '        network1d:node_long_name = "network1d_node_long_name" ;', &
            '        network1d:edge_node_connectivity = "network1d_edge_nodes" ;', &
            '        network1d:edge_length = "network1d_edge_length" ;', &
            '        network1d:edge_geometry = "network1d_geometry" ;', &
            '        network1d:branch_id = "network1d_branch_id" ;', &
            '        network1d:branch_long_name = "network1d_branch_long_name" ;', &
            '    double network1d_node_x(network1d_nNodes) ;', &
            '        network1d_node_x:standard_name = "projection_x_coordinate" ;', &
            '        network1d_node_x:units = "m" ;', &
            '    double network1d_node_y(network1d_nNodes) ;', &
            '        network1d_node_y:standard_name = "projection_y_coordinate" ;', &
            '        network1d_node_y:units = "m" ;', &
            '    char network1d_node_id(network1d_nNodes, strLengthIds) ;', &
            '    char network1d_node_long_name(network1d_nNodes, strLengthLongNames) ;', &
            '    int network1d_edge_nodes(network1d_nEdges, Two) ;', &
            '        network1d_edge_nodes:cf_role = "edge_node_connectivity" ;', &
            '        network1d_edge_nodes:start_index = 1 ;', &
            '    double network1d_edge_length(network1d_nEdges) ;', &
            '        network1d_edge_length:units = "m" ;', &
            '    char network1d_branch_id(network1d_nEdges, strLengthIds) ;', &
            '    char network1d_branch_long_name(network1d_nEdges, strLengthLongNames) ;', &
            '    int network1d_geometry ;', &
            '        network1d_geometry:geometry_type = "line" ;', &
            '        network1d_geometry:node_count = "network1d_geom_node_count" ;', &
            '        network1d_geometry:node_coordinates = "network1d_geom_x network1d_geom_y" ;', &
            '    int network1d_geom_node_count(network1d_nEdges) ;', &
            '    double network1d_geom_x(network1d_nGeometryNodes) ;', &
            '        network1d_geom_x:standard_name = "projection_x_coordinate" ;', &
            '        network1d_geom_x:units = "m" ;', &
            '    double network1d_geom_y(network1d_nGeometryNodes) ;', &
            '        network1d_geom_y:standard_name = "projection_y_coordinate" ;', &
            '        network1d_geom_y:units = "m" ;', &
            '    int network1d_branch_order(network1d_nEdges) ;', &
            '        network1d_branch_order:long_name = "Order of branches for interpolation" ;', &
            '        network1d_branch_order:mesh = "network1d" ;', &
            '        network1d_branch_order:location = "edge" ;', &
            '    int network1d_branch_type(network1d_nEdges) ;', &
            '        network1d_branch_type:long_name = "Type of branches" ;', &
            '        network1d_branch_type:mesh = "network1d" ;', &
            '        network1d_branch_type:location = "edge" ;', &
            '    int mesh1d ;', &
            '        mesh1d:cf_role = "mesh_topology" ;', &
            '        mesh1d:long_name = "Topology data of 1D mesh" ;', &
            '        mesh1d:topology_dimension = 1 ;', &
            '        mesh1d:coordinate_space = "network1d" ;', &
            '        mesh1d:node_dimension = "mesh1d_nNodes" ;', &
            '        mesh1d:edge_dimension = "mesh1d_nEdges" ;', &
            '        mesh1d:edge_node_connectivity = "mesh1d_edge_nodes" ;', &
            '        mesh1d:node_coordinates = "mesh1d_node_branch mesh1d_node_offset mesh1d_node_x mesh1d_node_y" ;', &
            '        mesh1d:edge_coordinates = "mesh1d_edge_branch mesh1d_edge_offset" ;', &
            '        mesh1d:node_id = "mesh1d_node_id" ;', &
            '        mesh1d:node_long_name = "mesh1d_node_long_name" ;', &
            '    int mesh1d_node_branch(mesh1d_nNodes) ;', &
            '        mesh1d_node_branch:long_name = "Index of branch on which mesh nodes are located" ;', &
            '        mesh1d_node_branch:start_index = 0 ;', &
            '    double mesh1d_node_offset(mesh1d_nNodes) ;', &
            '        mesh1d_node_offset:long_name = "Offset along branch of mesh nodes" ;', &
            '        mesh1d_node_offset:units = "m" ;', &
            '    double mesh1d_node_x(mesh1d_nNodes) ;', &
            '        mesh1d_node_x:standard_name = "projection_x_coordinate" ;', &
            '        mesh1d_node_x:units = "m" ;', &
            '    double mesh1d_node_y(mesh1d_nNodes) ;', &
            '        mesh1d_node_y:standard_name = "projection_y_coordinate" ;', &
            '        mesh1d_node_y:units = "m" ;', &
            '    int mesh1d_edge_nodes(mesh1d_nEdges, Two) ;', &
            '        mesh1d_edge_nodes:cf_role = "edge_node_connectivity" ;', &
            '        mesh1d_edge_nodes:start_index = 1 ;', &
            '    int mesh1d_edge_branch(mesh1d_nEdges) ;', &
            '        mesh1d_edge_branch:start_index = 0 ;', &
            '    double mesh1d_edge_offset(mesh1d_nEdges) ;', &
            '        mesh1d_edge_offset:units = "m" ;', &
            '    char mesh1d_node_id(mesh1d_nNodes, strLengthIds) ;', &
            '        mesh1d_node_id:long_name = "ID of mesh nodes" ;', &
            '        mesh1d_node_id:cf_role = "timeseries_id" ;', &
            '    char mesh1d_node_long_name(mesh1d_nNodes, strLengthLongNames) ;', &
            '        mesh1d_node_long_name:long_name = "Long name of mesh nodes" ;', &
            '', &
            '// global attributes:', &
            '        :Conventions = "CF-1.8 UGRID-1.0 Deltares-0.10" ;', &
            'data:', &
            '', &
            ' // 2D grid: 5 columns x 3 rows of nodes', &
            ' mesh2d_node_x = 0, 100, 200, 300, 400,', &
            '                0, 100, 200, 300, 400,', &
            '                0, 100, 200, 300, 400 ;', &
            ' mesh2d_node_y = 0, 0, 0, 0, 0,', &
            '                100, 100, 100, 100, 100,', &
            '                200, 200, 200, 200, 200 ;', &
            ' mesh2d_edge_nodes =', &
            '    1, 2,   2, 3,   3, 4,   4, 5,', &
            '    6, 7,   7, 8,   8, 9,   9, 10,', &
            '    11, 12, 12, 13, 13, 14, 14, 15,', &
            '    1, 6,   2, 7,   3, 8,   4, 9,   5, 10,', &
            '    6, 11,  7, 12,  8, 13,  9, 14,  10, 15 ;', &
            ' mesh2d_face_nodes =', &
            '    1, 2, 7, 6,   2, 3, 8, 7,   3, 4, 9, 8,   4, 5, 10, 9,', &
            '    6, 7, 12, 11, 7, 8, 13, 12, 8, 9, 14, 13, 9, 10, 15, 14 ;', &
            '', &
            ' // network1d: a single branch', &
            ' network1d_node_x = -200, -100 ;', &
            ' network1d_node_y = 50, 50 ;', &
            ' network1d_node_id =', &
            '  "nNode1",', &
            '  "nNode2" ;', &
            ' network1d_node_long_name =', &
            '  "nNode1",', &
            '  "nNode2" ;', &
            ' network1d_edge_nodes = 1, 2 ;', &
            ' network1d_edge_length = 100 ;', &
            ' network1d_branch_id =', &
            '  "branch1" ;', &
            ' network1d_branch_long_name =', &
            '  "branch1" ;', &
            ' network1d_geom_node_count = 2 ;', &
            ' network1d_geom_x = -200, -100 ;', &
            ' network1d_geom_y = 50, 50 ;', &
            ' network1d_branch_order = -1 ;', &
            ' network1d_branch_type = 0 ;', &
            '', &
            ' // mesh1d: 3 nodes on branch 1 (0-based branch index) at offsets 0, 50 and 100', &
            ' mesh1d_node_branch = 0, 0, 0 ;', &
            ' mesh1d_node_offset = 0, 50, 100 ;', &
            ' mesh1d_node_x = -200, -150, -100 ;', &
            ' mesh1d_node_y = 50, 50, 50 ;', &
            ' mesh1d_edge_nodes =', &
            '    1, 2,', &
            '    2, 3 ;', &
            ' mesh1d_edge_branch = 0, 0 ;', &
            ' mesh1d_edge_offset = 25, 75 ;', &
            ' mesh1d_node_id =', &
            '  "mesh1d_node0001",', &
            '  "mesh1d_node0002",', &
            '  "mesh1d_node0003" ;', &
            ' mesh1d_node_long_name =', &
            '  "mesh1d_node0001",', &
            '  "mesh1d_node0002",', &
            '  "mesh1d_node0003" ;', &
            '}' &
         ], success)
   end subroutine create_net_with_1d_branch

   subroutine convertlongculverts(mdufile, structurefile, netfile)
      use m_resetfullflowmodel, only: resetfullflowmodel
      use m_inidat, only: inidat, jaSkipCmdLineArgs
      use gridoperations, only: findcells
      use m_find1dcells, only: find1dcells
      use m_longculverts, only: makelongculverts_commandline
      use m_globalparameters, only: t_filenames
      use unstruc_model, only: writeMDUfile, md_netfile, md_japartition, md_structurefile, md_crsfile, md_ident, md_convertlongculverts
      use m_partitioninfo, only: jampi
      use Timers, only: timini, timon
      use messagehandling, only: SetMessageHandling
      use m_commandline_option, only: inputfiles, numfiles
      use unstruc_netcdf, only: unc_write_net

      character(len=*), intent(inout) :: mdufile
      character(len=*), intent(in) :: structurefile
      character(len=*), intent(in) :: netfile
      character(len=256) :: mdufile_local, netfile_local
      type(t_filenames) :: filenames
      integer :: ierr, iunit
      logical :: file_exists, converted_file_open

      ! Delete leftover converted_crsdef.ini from previous test runs
      inquire (file='converted_crsdef.ini', exist=file_exists, number=iunit)
      if (file_exists) then
         if (iunit /= 0) then
            close (iunit, iostat=ierr)
         end if
         open (newunit=iunit, file='converted_crsdef.ini', status='old', iostat=ierr)
         if (ierr == 0) then
            close (iunit, status='delete', iostat=ierr)
         end if
      end if

      call resetFullFlowModel()
      call default_longculverts()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      inputfiles(1) = mdufile
      numfiles = 1
      md_japartition = 1 !> to make long culvert initialisation write the converted files
      jaSkipCmdLineArgs = 0
      call INIDAT()
      inquire (file="converted_"//trim(structurefile), opened=converted_file_open)
      call f90_expect_true(.not. converted_file_open, "Converted structures file should be closed")
      ! Write the updated net file (now includes culvert netlinks)
      netfile_local = "converted_"//trim(netfile)
      call findcells(0)
      call find1dcells()
      call unc_write_net(netfile_local, janetcell=1, janetbnd=1, jaidomain=0, jaiglobal_s=0, md_ident=md_ident) ! Save net bnds to prevent unnecessary open bnds
      md_netfile = netfile_local
      mdufile_local = "converted_"//mdufile
      md_structurefile = "converted_"//structurefile
      md_crsfile = "converted_crsdef.ini"
      md_convertlongculverts = 0
      call writeMDUfile(mdufile_local, ierr)
      mdufile = mdufile_local
      call default_longculverts() !> not automatically reset
      numfiles = 0 !> not automatically reset
      md_japartition = 0 !> not automatically reset

   end subroutine convertlongculverts

end module test_longculverts
