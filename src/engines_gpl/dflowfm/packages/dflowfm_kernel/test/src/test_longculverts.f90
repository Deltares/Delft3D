module test_longculverts
    use assertions_gtest
    use m_longculverts, only: convert1D2DLongCulverts, default_longculverts
    use m_network_helpers, only: t_grid_helper
    implicit none(type, external)

contains
    function to_c_string(string) result(res)
        use iso_c_binding, only: c_null_char
        implicit none
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: res
        res = trim(string) // c_null_char
    end function to_c_string
    

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts_single_four_point, test_convert1d2dlongculverts_single_four_point,
    subroutine test_convert1d2dlongculverts_single_four_point() bind(C)
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
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=1, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

        x_coords = [5._dp, 9._dp, 11._dp, 15._dp]
        y_coords = [6._dp, 6._dp, 4._dp, 4._dp]
        z_coords = -1.0_dp

        ! Subroutine `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = COORD_COUNT

        allocate(longculverts(1))
        allocate(longculverts(1)%netlinks(3))
        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT), 5._dp)
        
        call F90_ASSERT_EQ(numk, 10) ! 6 Netnodes for the grid, 4 For the long culvert.
        call F90_ASSERT_EQ(numl, 10) ! 7 Netlinks for the grid, 3 For the long culvert.

        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(1)), 5, to_c_string("Expected first new link to be a 1D2D link."))
        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(2)), 1, to_c_string("Expected middle link to be a 1D link."))
        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(3)), 5, to_c_string("Expected last new link to be a 1D2D link."))
    end subroutine test_convert1d2dlongculverts_single_four_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts_single_two_point, test_convert1d2dlongculverts_single_two_point,
    subroutine test_convert1d2dlongculverts_single_two_point() bind(C)
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
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=1, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

        x_coords = [5._dp, 15._dp]
        y_coords = [6._dp, 4._dp]
        z_coords = -1.0_dp

        ! `longculvert_create_endpiont` requires these arrays in `m_polygon` to be allocated.
        xpl = x_coords
        ypl = y_coords
        zpl = z_coords
        npl = COORD_COUNT
        if (allocated(longculverts)) then
            deallocate(longculverts)
        end if
        allocate(longculverts(1))
        allocate(longculverts(1)%netlinks(1))
        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, COORD_COUNT)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT), 5._dp)
        
        call F90_ASSERT_EQ(numk, 8) ! 6 Netnodes for the grid, 2 For the long culvert.
        call F90_ASSERT_EQ(numl, 8) ! 7 Netlinks for the grid, 1 For the long culvert.

        call F90_ASSERT_EQ(kn(3, longculverts(1)%netlinks(1)), 5, to_c_string("Expected first new link to be a 1D2D link."))
    end subroutine test_convert1d2dlongculverts_single_two_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts_multiple_culverts, test_convert1d2dlongculverts_multiple_culverts,
    subroutine test_convert1d2dlongculverts_multiple_culverts() bind(C)
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
        call grid_helper%make_square_grid( &
            bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
            rows=2, columns=2, side_length=10.0_dp, array_size_margin=16 &
        )

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
            deallocate(longculverts)
        end if
        allocate(longculverts(2))
        allocate(longculverts(1)%netlinks(3))
        allocate(longculverts(2)%netlinks(1))

        ! Act
        call convert1D2DLongCulverts(x_coords, y_coords, z_coords, ARRAY_SIZE)

        ! Assert
        call F90_ASSERT_DOUBLE_EQ(x_coords(1), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT_LC1), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT_LC1), 5._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(COORD_COUNT_LC1 + 2), 5._dp) ! First and last point snapped to cell centers.
        call F90_ASSERT_DOUBLE_EQ(y_coords(COORD_COUNT_LC1 + 2), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(x_coords(ARRAY_SIZE), 15._dp)
        call F90_ASSERT_DOUBLE_EQ(y_coords(ARRAY_SIZE), 15._dp)
        
        call F90_ASSERT_EQ(numk, 9 + 4 + 2) ! 9 Netnodes for the grid, 4 for LC1, 2 for LC2.
        call F90_ASSERT_EQ(numl, 12 + 3 + 1) ! 12 Netlinks for the grid, 3 for LC1, 1 for LC2.
    end subroutine test_convert1d2dlongculverts_multiple_culverts
    !$f90tw )

   !> Create a minimal UGRID 2D net file: a simple channel of 4 quads in a row.
   !! Nodes form a 5x2 grid (10 nodes), edges connect them into 4 rectangular cells.
   subroutine create_minimal_netfile(filename, ierr)
      use precision, only: dp
      use netcdf
      character(len=*), intent(in) :: filename
      integer, intent(out) :: ierr

      integer :: ncid, dimid_node, dimid_edge, dimid_face, dimid_maxnodes, dimid_two
      integer :: varid_mesh, varid_xn, varid_yn, varid_en, varid_fn
      integer :: nNodes, nEdges, nFaces
      real(kind=dp) :: xnodes(10), ynodes(10)
      integer :: edge_nodes(2, 13), face_nodes(4, 4)
      integer :: i, j, k

      ! 5 columns x 2 rows of nodes => 10 nodes
      ! Node layout (y=0 bottom row, y=100 top row):
      !   6---7---8---9---10      (y=100)
      !   |   |   |   |   |
      !   1---2---3---4---5       (y=0)
      ! x=  0  100 200 300 400
      nNodes = 10
      nEdges = 13  ! 4 horizontal bottom + 4 horizontal top + 5 vertical
      nFaces = 4

      k = 0
      do j = 1, 2
         do i = 1, 5
            k = k + 1
            xnodes(k) = real((i - 1) * 100, dp)
            ynodes(k) = real((j - 1) * 100, dp)
         end do
      end do

      ! Edge connectivity (1-based)
      ! Bottom horizontal edges: 1-2, 2-3, 3-4, 4-5
      edge_nodes(:, 1) = [1, 2]
      edge_nodes(:, 2) = [2, 3]
      edge_nodes(:, 3) = [3, 4]
      edge_nodes(:, 4) = [4, 5]
      ! Top horizontal edges: 6-7, 7-8, 8-9, 9-10
      edge_nodes(:, 5) = [6, 7]
      edge_nodes(:, 6) = [7, 8]
      edge_nodes(:, 7) = [8, 9]
      edge_nodes(:, 8) = [9, 10]
      ! Vertical edges: 1-6, 2-7, 3-8, 4-9, 5-10
      edge_nodes(:, 9)  = [1, 6]
      edge_nodes(:, 10) = [2, 7]
      edge_nodes(:, 11) = [3, 8]
      edge_nodes(:, 12) = [4, 9]
      edge_nodes(:, 13) = [5, 10]

      ! Face-node connectivity (CCW): 4 quads
      face_nodes(:, 1) = [1, 2, 7, 6]
      face_nodes(:, 2) = [2, 3, 8, 7]
      face_nodes(:, 3) = [3, 4, 9, 8]
      face_nodes(:, 4) = [4, 5, 10, 9]

      ! Create NetCDF file
      ierr = nf90_create(filename, NF90_CLOBBER, ncid)
      if (ierr /= nf90_noerr) return

      ! Global attributes
      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.8 UGRID-1.0')

      ! Dimensions
      ierr = nf90_def_dim(ncid, 'mesh2d_nNodes', nNodes, dimid_node)
      ierr = nf90_def_dim(ncid, 'mesh2d_nEdges', nEdges, dimid_edge)
      ierr = nf90_def_dim(ncid, 'mesh2d_nFaces', nFaces, dimid_face)
      ierr = nf90_def_dim(ncid, 'mesh2d_nMax_face_nodes', 4, dimid_maxnodes)
      ierr = nf90_def_dim(ncid, 'Two', 2, dimid_two)

      ! Mesh topology variable
      ierr = nf90_def_var(ncid, 'mesh2d', NF90_INT, varid_mesh)
      ierr = nf90_put_att(ncid, varid_mesh, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(ncid, varid_mesh, 'topology_dimension', 2)
      ierr = nf90_put_att(ncid, varid_mesh, 'node_coordinates', 'mesh2d_node_x mesh2d_node_y')
      ierr = nf90_put_att(ncid, varid_mesh, 'edge_node_connectivity', 'mesh2d_edge_nodes')
      ierr = nf90_put_att(ncid, varid_mesh, 'face_node_connectivity', 'mesh2d_face_nodes')

      ! Node coordinates
      ierr = nf90_def_var(ncid, 'mesh2d_node_x', NF90_DOUBLE, [dimid_node], varid_xn)
      ierr = nf90_put_att(ncid, varid_xn, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_xn, 'units', 'm')

      ierr = nf90_def_var(ncid, 'mesh2d_node_y', NF90_DOUBLE, [dimid_node], varid_yn)
      ierr = nf90_put_att(ncid, varid_yn, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_yn, 'units', 'm')

      ! Edge-node connectivity
      ierr = nf90_def_var(ncid, 'mesh2d_edge_nodes', NF90_INT, [dimid_two, dimid_edge], varid_en)
      ierr = nf90_put_att(ncid, varid_en, 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, varid_en, 'start_index', 1)

      ! Face-node connectivity
      ierr = nf90_def_var(ncid, 'mesh2d_face_nodes', NF90_INT, [dimid_maxnodes, dimid_face], varid_fn)
      ierr = nf90_put_att(ncid, varid_fn, 'cf_role', 'face_node_connectivity')
      ierr = nf90_put_att(ncid, varid_fn, 'start_index', 1)

      ierr = nf90_enddef(ncid)
      if (ierr /= nf90_noerr) then
         ierr = nf90_close(ncid)
         return
      end if

      ! Write data
      ierr = nf90_put_var(ncid, varid_xn, xnodes)
      ierr = nf90_put_var(ncid, varid_yn, ynodes)
      ierr = nf90_put_var(ncid, varid_en, edge_nodes)
      ierr = nf90_put_var(ncid, varid_fn, face_nodes)

      ierr = nf90_close(ncid)
   end subroutine create_minimal_netfile

   !> Create a structures.ini file containing a single long culvert
   !! that runs through the middle of the mesh (y=50) from x=50 to x=350.
   subroutine create_structure_file(filename)
      use m_file_helpers, only: create_file
      character(len=*), intent(in) :: filename

      call create_file(filename, [ &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0       ", &
                       "    yCoordinates    = 50.0 50.0         ", &
                       "    zCoordinates    = -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                "])
   end subroutine create_structure_file

   !> Create a minimal MDU file that references the net file and structure file.
   subroutine create_mdu_file(mdu_file, net_file, str_file)
      character(len=*), intent(in) :: mdu_file, net_file, str_file
      integer :: mout, ierr

      open(newunit=mout, file=mdu_file, status='replace', action='write', iostat=ierr)

      write(mout, '(a)') '[General]'
      write(mout, '(a)') '    fileVersion           = 1.09'
      write(mout, '(a)') '    fileType              = modelDef'
      write(mout, '(a)') '    program               = D-Flow FM'
      write(mout, '(a)') '    ConvertLongCulverts   = 1'
      write(mout, '(a)') ''
      write(mout, '(a)') '[geometry]'
      write(mout, '(2a)') '    netFile               = ', trim(net_file)
      write(mout, '(2a)') '    StructureFile         = ', trim(str_file)
      write(mout, '(a)') ''
      write(mout, '(a)') '[time]'
      write(mout, '(a)') '    refDate               = 20000101'
      write(mout, '(a)') '    tUnit                 = S'
      write(mout, '(a)') '    tStart                = 0.0'
      write(mout, '(a)') '    tStop                 = 100.0'
      write(mout, '(a)') '    dtMax                 = 10.0'
      write(mout, '(a)') '    dtUser                = 10.0'
      write(mout, '(a)') '    dtInit                = 1.0'
      close(mout)
   end subroutine create_mdu_file

   !$f90tw TESTCODE(TEST, test_longculvert, test_flow_modelinit_with_longculvert, test_flow_modelinit_with_longculvert,
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
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use MessageHandling, only: SetMessageHandling
      use m_resetfullflowmodel, only: resetfullflowmodel
      use netcdf, only: nf90_noerr
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: ierr, iresult, i

      character(len=*), parameter :: TEST_NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"

      character(len=256) :: mdu_local
      integer :: lc_link

      ! ARRANGE: Create all input files
      call create_minimal_netfile(TEST_NET_FILE, ierr)
      call f90_assert_eq(ierr, nf90_noerr, "NetCDF net file creation should succeed")

      call create_structure_file(TEST_STR_FILE)
      call create_mdu_file(TEST_MDU_FILE, TEST_NET_FILE, TEST_STR_FILE)
      md_ident = TEST_MDU_FILE
      threshold_abort = LEVEL_FATAL

      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      call resetFullFlowModel()
      mdu_local = TEST_MDU_FILE
      call loadModel(mdu_local)
      iresult = flow_modelinit()

      call f90_expect_eq(iresult, DFM_NOERR, &
                         "flow_modelinit should return DFM_NOERR for a valid model with a long culvert")
      call f90_expect_eq(nlongculverts, 1, "one long culvert should be registered")

      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp ! barrier in cells 2 and 3
         end if
      end do
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp  ! left cell: high water level
         else
            s1(i) = 0.0_dp  ! remaining cells: low water level
         end if
      end do

      call flow_spatietimestep()

      ! ASSERT: Flow should pass through the culvert from left to right.
      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, "culvert flow link should be valid (> 0)")
      call f90_expect_true(q1(lc_link) > 0.0_dp, &
                           "discharge through culvert should be positive (left to right)")

      call default_longculverts

   end subroutine test_flow_modelinit_with_longculvert
   !$f90tw)

   !> Shared helper: initializes the model from the test MDU file.
   !! Returns flow_modelinit result in iresult.
   subroutine setup_longculvert_model(iresult)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use dfm_error, only: DFM_NOERR
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use MessageHandling, only: SetMessageHandling
      use m_resetfullflowmodel, only: resetfullflowmodel
      use netcdf, only: nf90_noerr
      integer, intent(out) :: iresult

      character(len=*), parameter :: TEST_NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"
      character(len=256) :: mdu_local
      integer :: ierr

      call create_minimal_netfile(TEST_NET_FILE, ierr)
      call create_structure_file(TEST_STR_FILE)
      call create_mdu_file(TEST_MDU_FILE, TEST_NET_FILE, TEST_STR_FILE)
      md_ident = TEST_MDU_FILE
      threshold_abort = LEVEL_FATAL

      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      call resetFullFlowModel()
      mdu_local = TEST_MDU_FILE
      call loadModel(mdu_local)
      iresult = flow_modelinit()
   end subroutine setup_longculvert_model

   !$f90tw TESTCODE(TEST, test_longculvert, test_modelinit_succeeds, test_modelinit_succeeds,
   !> Verifies that flow_modelinit succeeds for a minimal 2D model with a long culvert.
   subroutine test_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_longculvert_model(iresult)

      call f90_expect_eq(iresult, DFM_NOERR, &
                         "flow_modelinit should return DFM_NOERR")
      call f90_expect_true(ndx > 0, "ndx should be > 0")
      call f90_expect_true(lnx > 0, "lnx should be > 0")
      call f90_expect_eq(nlongculverts, 1, "one long culvert should be registered")
      call f90_expect_true(longculverts(1)%flowlinks(1) > 0, &
                           "culvert should have a valid flow link")

      call default_longculverts
   end subroutine test_modelinit_succeeds
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_flow_head_difference_drives_discharge, test_flow_head_difference_drives_discharge,
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
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")

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
      call f90_expect_true(lc_link > 0, "culvert flow link should be valid")
      call f90_expect_true(q1(lc_link) > 0.0_dp, &
                           "discharge should be positive (left to right)")

      call default_longculverts
   end subroutine test_flow_head_difference_drives_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_flow_no_head_difference_no_discharge, test_flow_no_head_difference_no_discharge,
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
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")

      ! Uniform water level everywhere — no driving force.
      do i = 1, ndx
         s1(i) = 1.0_dp
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, "culvert flow link should be valid")
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, &
                           "discharge should be ~zero with no head difference")

      call default_longculverts
   end subroutine test_flow_no_head_difference_no_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_valve_closed_blocks_flow, test_valve_closed_blocks_flow,
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
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")

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
      call f90_expect_true(lc_link > 0, "culvert flow link should be valid")
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, &
                           "discharge should be ~zero when valve is closed")

      call default_longculverts
   end subroutine test_valve_closed_blocks_flow
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_flow_reverse_head_gives_negative_discharge, test_flow_reverse_head_gives_negative_discharge,
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
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")

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
      call f90_expect_true(lc_link > 0, "culvert flow link should be valid")
      call f90_expect_true(q1(lc_link) < 0.0_dp, &
                           "discharge should be negative (right to left)")

      call default_longculverts
   end subroutine test_flow_reverse_head_gives_negative_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_valve_half_open_reduces_discharge, test_valve_half_open_reduces_discharge,
   !> A half-open valve should produce less discharge than a fully-open valve
   !! under the same head difference.
   subroutine test_valve_half_open_reduces_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link
      real(kind=dp) :: q_full, q_half

      ! --- Run 1: fully open valve ---
      call setup_longculvert_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")

      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) bl(i) = 10.0_dp
      end do
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp
         else
            s1(i) = 0.0_dp
         end if
      end do

      longculverts(1)%valve_relative_opening = 1.0_dp
      call flow_spatietimestep()
      lc_link = longculverts(1)%flowlinks(1)
      q_full = q1(lc_link)
      call default_longculverts

      ! --- Run 2: half-open valve ---
      call setup_longculvert_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed (run 2)")

      do i = 1, ndx
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) bl(i) = 10.0_dp
      end do
      do i = 1, ndx
         if (xz(i) < 100.0_dp) then
            s1(i) = 2.0_dp
         else
            s1(i) = 0.0_dp
         end if
      end do

      longculverts(1)%valve_relative_opening = 0.5_dp
      call flow_spatietimestep()
      lc_link = longculverts(1)%flowlinks(1)
      q_half = q1(lc_link)
      call default_longculverts

      ! Assert
      call f90_expect_true(q_full > 0.0_dp, "full-open discharge should be positive")
      call f90_expect_true(q_half > 0.0_dp, "half-open discharge should be positive")
      call f90_expect_true(q_half < q_full, &
                           "half-open discharge should be less than fully-open discharge")
   end subroutine test_valve_half_open_reduces_discharge
   !$f90tw)

end module test_longculverts


