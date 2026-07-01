module test_longculverts
    use assertions_gtest
   use m_longculverts, only: convert1D2DLongCulverts, default_longculverts
    use m_network_helpers, only: t_grid_helper
   use m_file_helpers, only: create_file

     implicit none(type, external)
   
contains
    function to_c_string(string) result(res)
        use iso_c_binding, only: c_null_char
        implicit none
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: res
        res = trim(string) // c_null_char
    end function to_c_string
    

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__single_four_point, test_convert1d2dlongculverts__single_four_point,
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
    end subroutine test_convert1d2dlongculverts__single_four_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__single_two_point, test_convert1d2dlongculverts__single_two_point,
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
    end subroutine test_convert1d2dlongculverts__single_two_point
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_longculvert, test_convert1d2dlongculverts__multiple_culverts, test_convert1d2dlongculverts__multiple_culverts,
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
    end subroutine test_convert1d2dlongculverts__multiple_culverts
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
      nEdges = 13 ! 4 horizontal bottom + 4 horizontal top + 5 vertical
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
      edge_nodes(:, 9) = [1, 6]
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
      character(len=*), intent(in) :: filename

      call create_file(filename, [character(len=512) :: &
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

      open (newunit=mout, file=mdu_file, status='replace', action='write', iostat=ierr)

      write (mout, '(a)') '[General]'
      write (mout, '(a)') '    fileVersion           = 1.09'
      write (mout, '(a)') '    fileType              = modelDef'
      write (mout, '(a)') '    program               = D-Flow FM'
      write (mout, '(a)') '    ConvertLongCulverts   = 1'
      write (mout, '(a)') ''
      write (mout, '(a)') '[geometry]'
      write (mout, '(2a)') '    netFile               = ', trim(net_file)
      write (mout, '(2a)') '    StructureFile         = ', trim(str_file)
      write (mout, '(a)') '    UseCaching             = 0'
      write (mout, '(a)') ''
      write (mout, '(a)') '[time]'
      write (mout, '(a)') '    refDate               = 20000101'
      write (mout, '(a)') '    tUnit                 = S'
      write (mout, '(a)') '    tStart                = 0.0'
      write (mout, '(a)') '    tStop                 = 100.0'
      write (mout, '(a)') '    dtMax                 = 10.0'
      write (mout, '(a)') '    dtUser                = 10.0'
      write (mout, '(a)') '    dtInit                = 1.0'

      close (mout)
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

      character(len=*), parameter :: NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"

      character(len=256) :: mdu_local
      integer :: lc_link

      ! ARRANGE: Create all input files
      call create_minimal_netfile(NET_FILE, ierr)
      call f90_assert_eq(ierr, nf90_noerr, to_c_string("NetCDF net file creation should succeed"))

      call create_structure_file(TEST_STR_FILE)
      call create_mdu_file(TEST_MDU_FILE, NET_FILE, TEST_STR_FILE)
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

      call f90_expect_eq(iresult, DFM_NOERR, to_c_string("flow_modelinit should return DFM_NOERR for a valid model with a long culvert"))
      call f90_expect_eq(nlongculverts, 1, to_c_string("one long culvert should be registered"))

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
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid (> 0)"))
      call f90_expect_true(q1(lc_link) > 0.0_dp, to_c_string("discharge through culvert should be positive (left to right)"))

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

      character(len=*), parameter :: NET_FILE = "test_lc_net.nc"
      character(len=*), parameter :: TEST_STR_FILE = "test_lc_structures.ini"
      character(len=*), parameter :: TEST_MDU_FILE = "test_lc.mdu"
      character(len=256) :: mdu_local
      integer :: ierr

      call create_minimal_netfile(NET_FILE, ierr)
      call create_structure_file(TEST_STR_FILE)
      call create_mdu_file(TEST_MDU_FILE, NET_FILE, TEST_STR_FILE)
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

      call f90_expect_eq(iresult, DFM_NOERR, to_c_string("flow_modelinit should return DFM_NOERR"))
      call f90_expect_true(ndx > 0, to_c_string("ndx should be > 0"))
      call f90_expect_true(lnx > 0, to_c_string("lnx should be > 0"))
      call f90_expect_eq(nlongculverts, 1, to_c_string("one long culvert should be registered"))
      call f90_expect_true(longculverts(1)%flowlinks(1) > 0, to_c_string("culvert should have a valid flow link"))

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
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

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
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_true(q1(lc_link) > 0.0_dp, to_c_string("discharge should be positive (left to right)"))

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
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

      ! Uniform water level everywhere no driving force.
      do i = 1, ndx
         s1(i) = 1.0_dp
      end do

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, to_c_string("discharge should be ~zero with no head difference"))

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
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

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
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, to_c_string("discharge should be ~zero when valve is closed"))

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
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

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
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_true(q1(lc_link) < 0.0_dp, to_c_string("discharge should be negative (right to left)"))

      call default_longculverts
   end subroutine test_flow_reverse_head_gives_negative_discharge
   !$f90tw)

   !> Create a 5x3-node UGRID net (4 cells wide, 2 rows).
   subroutine create_two_row_netfile(filename)
      use precision, only: dp
      use netcdf
      character(len=*), intent(in) :: filename

      integer :: ncid, dimid_node, dimid_edge, dimid_face, dimid_maxnodes, dimid_two
      integer :: varid_mesh, varid_xn, varid_yn, varid_en, varid_fn
      ! 5 cols x 3 rows = 15 nodes, 8 cells in 2 rows of 4
      integer, parameter :: NNODES = 15, NEDGES = 22, NFACES = 8
      real(kind=dp) :: xnodes(NNODES), ynodes(NNODES)
      integer :: edge_nodes(2, NEDGES), face_nodes(4, NFACES)
      integer :: i, j, k, ierr

      k = 0
      do j = 1, 3
         do i = 1, 5
            k = k + 1
            xnodes(k) = real((i - 1) * 100, dp)
            ynodes(k) = real((j - 1) * 100, dp)
         end do
      end do

      ! Bottom horizontal edges (row 0-1): 1-2, 2-3, 3-4, 4-5
      edge_nodes(:, 1) = [1, 2]
      edge_nodes(:, 2) = [2, 3]
      edge_nodes(:, 3) = [3, 4]
      edge_nodes(:, 4) = [4, 5]
      ! Middle horizontal edges (row 1-2): 6-7, 7-8, 8-9, 9-10
      edge_nodes(:, 5) = [6, 7]
      edge_nodes(:, 6) = [7, 8]
      edge_nodes(:, 7) = [8, 9]
      edge_nodes(:, 8) = [9, 10]
      ! Top horizontal edges (row 2-3): 11-12, 12-13, 13-14, 14-15
      edge_nodes(:, 9) = [11, 12]
      edge_nodes(:, 10) = [12, 13]
      edge_nodes(:, 11) = [13, 14]
      edge_nodes(:, 12) = [14, 15]
      ! Vertical edges bottom tier: 1-6, 2-7, 3-8, 4-9, 5-10
      edge_nodes(:, 13) = [1, 6]
      edge_nodes(:, 14) = [2, 7]
      edge_nodes(:, 15) = [3, 8]
      edge_nodes(:, 16) = [4, 9]
      edge_nodes(:, 17) = [5, 10]
      ! Vertical edges top tier: 6-11, 7-12, 8-13, 9-14, 10-15
      edge_nodes(:, 18) = [6, 11]
      edge_nodes(:, 19) = [7, 12]
      edge_nodes(:, 20) = [8, 13]
      edge_nodes(:, 21) = [9, 14]
      edge_nodes(:, 22) = [10, 15]

      ! Bottom row of faces (CCW)
      face_nodes(:, 1) = [1, 2, 7, 6]
      face_nodes(:, 2) = [2, 3, 8, 7]
      face_nodes(:, 3) = [3, 4, 9, 8]
      face_nodes(:, 4) = [4, 5, 10, 9]
      ! Top row of faces (CCW)
      face_nodes(:, 5) = [6, 7, 12, 11]
      face_nodes(:, 6) = [7, 8, 13, 12]
      face_nodes(:, 7) = [8, 9, 14, 13]
      face_nodes(:, 8) = [9, 10, 15, 14]

      ierr = nf90_create(filename, NF90_CLOBBER, ncid)
      if (ierr /= nf90_noerr) return

      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.8 UGRID-1.0')
      ierr = nf90_def_dim(ncid, 'mesh2d_nNodes', NNODES, dimid_node)
      ierr = nf90_def_dim(ncid, 'mesh2d_nEdges', NEDGES, dimid_edge)
      ierr = nf90_def_dim(ncid, 'mesh2d_nFaces', NFACES, dimid_face)
      ierr = nf90_def_dim(ncid, 'mesh2d_nMax_face_nodes', 4, dimid_maxnodes)
      ierr = nf90_def_dim(ncid, 'Two', 2, dimid_two)

      ierr = nf90_def_var(ncid, 'mesh2d', NF90_INT, varid_mesh)
      ierr = nf90_put_att(ncid, varid_mesh, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(ncid, varid_mesh, 'topology_dimension', 2)
      ierr = nf90_put_att(ncid, varid_mesh, 'node_coordinates', 'mesh2d_node_x mesh2d_node_y')
      ierr = nf90_put_att(ncid, varid_mesh, 'edge_node_connectivity', 'mesh2d_edge_nodes')
      ierr = nf90_put_att(ncid, varid_mesh, 'face_node_connectivity', 'mesh2d_face_nodes')

      ierr = nf90_def_var(ncid, 'mesh2d_node_x', NF90_DOUBLE, [dimid_node], varid_xn)
      ierr = nf90_put_att(ncid, varid_xn, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_xn, 'units', 'm')
      ierr = nf90_def_var(ncid, 'mesh2d_node_y', NF90_DOUBLE, [dimid_node], varid_yn)
      ierr = nf90_put_att(ncid, varid_yn, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_yn, 'units', 'm')

      ierr = nf90_def_var(ncid, 'mesh2d_edge_nodes', NF90_INT, [dimid_two, dimid_edge], varid_en)
      ierr = nf90_put_att(ncid, varid_en, 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, varid_en, 'start_index', 1)
      ierr = nf90_def_var(ncid, 'mesh2d_face_nodes', NF90_INT, [dimid_maxnodes, dimid_face], varid_fn)
      ierr = nf90_put_att(ncid, varid_fn, 'cf_role', 'face_node_connectivity')
      ierr = nf90_put_att(ncid, varid_fn, 'start_index', 1)

      ierr = nf90_enddef(ncid)
      if (ierr /= nf90_noerr) then
         ierr = nf90_close(ncid)
         return
      end if

      ierr = nf90_put_var(ncid, varid_xn, xnodes)
      ierr = nf90_put_var(ncid, varid_yn, ynodes)
      ierr = nf90_put_var(ncid, varid_en, edge_nodes)
      ierr = nf90_put_var(ncid, varid_fn, face_nodes)
      ierr = nf90_close(ncid)
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
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL, SetMessageHandling
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use m_resetfullflowmodel, only: resetfullflowmodel
      use precision, only: dp

      character(len=*), intent(in) :: mdu_file
      integer, intent(out) :: iresult

      character(len=256) :: mdu_local
      integer :: ierr, i

      md_ident = mdu_file
      threshold_abort = LEVEL_FATAL
      call inidat()
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      call resetFullFlowModel()
      mdu_local = mdu_file
      call loadModel(mdu_local)
      iresult = flow_modelinit()

      do i = 1, ndx2D
         if (xz(i) > 75.0_dp .and. xz(i) < 325.0_dp) then
            bl(i) = 10.0_dp
         end if
      end do
      do i = 1, ndx
         s1(i) = merge(2.0_dp, 0.0_dp, xz(i) < 100.0_dp)
      end do
   end subroutine init_two_culvert_scenario

   !$f90tw TESTCODE(TEST, test_longculvert, test_valve_half_open_reduces_discharge, test_valve_half_open_reduces_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 0.5                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      call flow_spatietimestep()

      q_full = q1(longculverts(1)%flowlinks(1))
      q_half = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_full > 0.0_dp, to_c_string("full-open discharge should be positive"))
      call f90_expect_true(q_half > 0.0_dp, to_c_string("half-open discharge should be positive"))
      call f90_expect_true(q_half < q_full, to_c_string("half-open discharge should be less than fully-open"))
      call default_longculverts
   end subroutine test_valve_half_open_reduces_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_friction_higher_value_reduces_discharge, test_friction_higher_value_reduces_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.01                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4 !> multiple timesteps, from rest friction has no effect
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1)) ! lc01: Manning n=0.01
      q_high_friction = q1(longculverts(2)%flowlinks(1)) ! lc02: Manning n=0.05
      call f90_expect_true(q_low_friction > 0.0_dp, to_c_string("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, to_c_string("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, to_c_string("higher Manning friction should produce less discharge"))
      call default_longculverts
   end subroutine test_friction_higher_value_reduces_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_2pt_friction_converted, test_2pt_friction_converted,
   subroutine test_2pt_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc2pt_fric_str.ini"
      character(len=256) :: MDU_FILE = "test_lc2pt_fric_converted.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc_convert_2pt_net.nc"

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.01                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(longculverts(1)%numlinks, 1, to_c_string("2-point culvert should have 1 link"))
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, to_c_string("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, to_c_string("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, to_c_string("higher Manning friction should produce less discharge"))
      call default_longculverts
   end subroutine test_2pt_friction_converted
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_2pt_default_friction_converted, test_2pt_default_friction_converted,
   subroutine test_2pt_default_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc2pt_default_friction_str.ini"
      character(len=256) :: MDU_FILE = "test_lc2pt_default_friction_converted.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc_convert_2pt_net.nc"

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.023                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(longculverts(1)%numlinks, 1, to_c_string("2-point culvert should have 1 link"))
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, to_c_string("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, to_c_string("high-friction discharge should be positive"))
      call f90_expect_near(q_high_friction, q_low_friction, 1.0e-10_dp, to_c_string("discharge should be equal due to equal roughness"))
      call default_longculverts
   end subroutine test_2pt_default_friction_converted
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_friction_type_affects_discharge, test_friction_type_affects_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = WhiteColebrook          ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4 !> multiple timesteps, from rest friction has no effect
         call flow_spatietimestep()
      end do

      q_manning = q1(longculverts(1)%flowlinks(1))
      q_colebrook = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_manning > 0.0_dp, to_c_string("Manning culvert discharge should be positive"))
      call f90_expect_true(q_colebrook > 0.0_dp, to_c_string("WhiteColebrook culvert discharge should be positive"))
      call f90_expect_true(abs(q_manning - q_colebrook) > 1.0e-6_dp, &
                           to_c_string("different friction types with same coefficient should give different discharge"))
      call default_longculverts
   end subroutine test_friction_type_affects_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_larger_cross_section_increases_discharge, test_larger_cross_section_increases_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 50.0 50.0               ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 1.0                     ", &
                       "    height          = 1.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 2                       ", &
                       "    xCoordinates    = 50.0 350.0              ", &
                       "    yCoordinates    = 150.0 150.0             ", &
                       "    zCoordinates    = -5.0 -5.0               ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 4.0                     ", &
                       "    height          = 4.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                "])
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, "model init must succeed")
      call f90_assert_eq(nlongculverts, 2, "two long culverts should be registered")

      call flow_spatietimestep()

      q_small = q1(longculverts(1)%flowlinks(1)) ! lc01: 1x1 m
      q_large = q1(longculverts(2)%flowlinks(1)) ! lc02: 4x4 m
      call f90_expect_true(q_small > 0.0_dp, "small culvert discharge should be positive")
      call f90_expect_true(q_large > 0.0_dp, "large culvert discharge should be positive")
      call f90_expect_true(q_large > q_small, &
                           "larger cross-section should give more discharge")
      call default_longculverts
   end subroutine test_larger_cross_section_increases_discharge
   !$f90tw)

   ! ============================================================================
   ! 3-POINT LONG CULVERT TESTS
   ! ============================================================================

   !> Create a structure file with a single 3-point long culvert (2 links: kcu=5, kcu=5).
   subroutine create_structure_file_3pt(filename)
      character(len=*), intent(in) :: filename

      call create_file(filename, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 50.0 50.0 50.0         ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 150.0 150.0 150.0         ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                "])
   end subroutine create_structure_file_3pt

   !> Shared helper for 3-point single-culvert tests.
   subroutine setup_3pt_model(iresult)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use dfm_error, only: DFM_NOERR
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL, SetMessageHandling
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use m_resetfullflowmodel, only: resetfullflowmodel
      use netcdf, only: nf90_noerr
      integer, intent(out) :: iresult

      character(len=*), parameter :: NET_FILE = "test_lc3pt_net.nc"
      character(len=*), parameter :: STR_FILE = "test_lc3pt_structures.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc3pt.mdu"
      character(len=256) :: mdu_local
      integer :: ierr

      call create_structure_file_3pt(STR_FILE)
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)

   end subroutine setup_3pt_model

   !$f90tw TESTCODE(TEST, test_longculvert, test_3pt_modelinit_succeeds, test_3pt_modelinit_succeeds,
   subroutine test_3pt_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_3pt_model(iresult)

      call f90_expect_eq(iresult, DFM_NOERR, to_c_string("flow_modelinit should succeed for 3-point culvert"))
      call f90_expect_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))
      call f90_expect_eq(longculverts(1)%numlinks, 2, to_c_string("3-point culvert should have 2 links"))

      call default_longculverts
   end subroutine test_3pt_modelinit_succeeds
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_3pt_head_difference_drives_discharge, test_3pt_head_difference_drives_discharge,
   subroutine test_3pt_head_difference_drives_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_3pt_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_true(q1(lc_link) > 0.0_dp, to_c_string("discharge should be positive (left to right)"))

      call default_longculverts
   end subroutine test_3pt_head_difference_drives_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_3pt_valve_closed_blocks_flow, test_3pt_valve_closed_blocks_flow,
   subroutine test_3pt_valve_closed_blocks_flow() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_3pt_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

      longculverts(1)%valve_relative_opening = 0.0_dp

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, to_c_string("discharge should be ~zero when valve is closed"))

      call default_longculverts
   end subroutine test_3pt_valve_closed_blocks_flow
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_3pt_friction_higher_value_reduces_discharge, test_3pt_friction_higher_value_reduces_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 50.0 50.0 50.0         ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.01                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 150.0 150.0 150.0      ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, to_c_string("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, to_c_string("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, &
                           to_c_string("higher Manning friction should produce less discharge"))
      call default_longculverts
   end subroutine test_3pt_friction_higher_value_reduces_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_3pt_friction_converted, test_3pt_friction_converted,
   subroutine test_3pt_friction_converted() bind(C)
      use m_flow, only: q1
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i
      real(kind=dp) :: q_low_friction, q_high_friction
      character(len=*), parameter :: STR_FILE = "test_lc3pt_fric_str.ini"
      character(len=256) :: MDU_FILE = "test_lc3pt_fric.mdu"
      character(len=*), parameter :: NET_FILE = "test_lc_convert_3pt_net.nc"

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 50.0 50.0 50.0         ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.01                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 3                       ", &
                       "    xCoordinates    = 50.0 200.0 350.0       ", &
                       "    yCoordinates    = 150.0 150.0 150.0      ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0         ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(longculverts(1)%numlinks, 2, to_c_string("3-point culvert should have 0 links"))
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))
      !> the test stops here, as a 3 PT culvert cannot be written to the netfile in a ugrid compliant way

   end subroutine test_3pt_friction_converted
   !$f90tw)

   ! ============================================================================
   ! 4-POINT LONG CULVERT TESTS
   ! ============================================================================

   !> Create a structure file with a single 4-point long culvert (3 links: kcu=5, kcu=1, kcu=5).
   subroutine create_structure_file_4pt(filename)
      character(len=*), intent(in) :: filename

      call create_file(filename, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 4                       ", &
                       "    xCoordinates    = 50.0 150.0 250.0 350.0 ", &
                       "    yCoordinates    = 50.0 50.0 50.0 50.0    ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0 -5.0   ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 4                       ", &
                       "    xCoordinates    = 50.0 150.0 250.0 350.0 ", &
                       "    yCoordinates    = 150.0 150.0 150.0 150.0", &
                       "    zCoordinates    = -5.0 -5.0 -5.0 -5.0   ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.02                    ", &
                       "    valveRelativeOpening = 1.0                "])

   end subroutine create_structure_file_4pt

   !> Shared helper for 4-point single-culvert tests.
   subroutine setup_4pt_model(iresult)
      use m_flow_modelinit, only: flow_modelinit
      use unstruc_model, only: loadModel, md_ident
      use dfm_error, only: DFM_NOERR
      use unstruc_messages, only: threshold_abort
      use messagehandling, only: LEVEL_FATAL, SetMessageHandling
      use m_inidat, only: inidat
      use Timers, only: timini, timon
      use m_partitioninfo, only: jampi
      use m_resetfullflowmodel, only: resetfullflowmodel
      use netcdf, only: nf90_noerr
      integer, intent(out) :: iresult

      character(len=*), parameter :: NET_FILE = "test_lc4pt_net.nc"
      character(len=*), parameter :: STR_FILE = "test_lc4pt_structures.ini"
      character(len=*), parameter :: MDU_FILE = "test_lc4pt.mdu"

      call create_structure_file_4pt(STR_FILE)
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)

   end subroutine setup_4pt_model

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_modelinit_succeeds, test_4pt_modelinit_succeeds,
   subroutine test_4pt_modelinit_succeeds() bind(C)
      use m_flowgeom, only: ndx, lnx
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR

      integer :: iresult

      call setup_4pt_model(iresult)

      call f90_expect_eq(iresult, DFM_NOERR, to_c_string("flow_modelinit should succeed for 4-point culvert"))
      call f90_expect_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))
      call f90_expect_eq(longculverts(1)%numlinks, 3, to_c_string("4-point culvert should have 3 links"))

      call default_longculverts
   end subroutine test_4pt_modelinit_succeeds
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_head_difference_drives_discharge, test_4pt_head_difference_drives_discharge,
   subroutine test_4pt_head_difference_drives_discharge() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_4pt_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_true(q1(lc_link) > 0.0_dp, to_c_string("discharge should be positive (left to right)"))

      call default_longculverts
   end subroutine test_4pt_head_difference_drives_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_valve_closed_blocks_flow, test_4pt_valve_closed_blocks_flow,
   subroutine test_4pt_valve_closed_blocks_flow() bind(C)
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1, q1
      use m_cell_geometry, only: xz
      use m_longculverts_data, only: nlongculverts, longculverts
      use dfm_error, only: DFM_NOERR
      use m_flow_spatietimestep, only: flow_spatietimestep
      use precision, only: dp

      integer :: iresult, i, lc_link

      call setup_4pt_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))

      longculverts(1)%valve_relative_opening = 0.0_dp

      call flow_spatietimestep()

      lc_link = longculverts(1)%flowlinks(1)
      call f90_expect_true(lc_link > 0, to_c_string("culvert flow link should be valid"))
      call f90_expect_near(q1(lc_link), 0.0_dp, 1.0e-10_dp, to_c_string("discharge should be ~zero when valve is closed"))

      call default_longculverts
   end subroutine test_4pt_valve_closed_blocks_flow
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_friction_higher_value_reduces_discharge, test_4pt_friction_higher_value_reduces_discharge,
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

      call create_file(STR_FILE, [character(len=512) :: &
                       "[General]                                     ", &
                       "    fileVersion     = 3.00                    ", &
                       "    fileType        = structures              ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc01                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 4                       ", &
                       "    xCoordinates    = 50.0 150.0 250.0 350.0 ", &
                       "    yCoordinates    = 50.0 50.0 50.0 50.0    ", &
                       "    zCoordinates    = -5.0 -5.0 -5.0 -5.0   ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.01                    ", &
                       "    valveRelativeOpening = 1.0                ", &
                       "                                              ", &
                       "[Structure]                                   ", &
                       "    id              = lc02                    ", &
                       "    type            = longCulvert             ", &
                       "    numCoordinates  = 4                       ", &
                       "    xCoordinates    = 50.0 150.0 250.0 350.0 ", &
                       "    yCoordinates    = 150.0 150.0 150.0 150.0", &
                       "    zCoordinates    = -5.0 -5.0 -5.0 -5.0   ", &
                       "    allowedFlowDir  = both                    ", &
                       "    width           = 2.0                     ", &
                       "    height          = 2.0                     ", &
                       "    frictionType    = Manning                 ", &
                       "    frictionValue   = 0.05                    ", &
                       "    valveRelativeOpening = 1.0                "])

      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, str_file)
      call init_two_culvert_scenario(MDU_FILE, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))

      do i = 1, 4
         call flow_spatietimestep()
      end do

      q_low_friction = q1(longculverts(1)%flowlinks(1))
      q_high_friction = q1(longculverts(2)%flowlinks(1))
      call f90_expect_true(q_low_friction > 0.0_dp, to_c_string("low-friction discharge should be positive"))
      call f90_expect_true(q_high_friction > 0.0_dp, to_c_string("high-friction discharge should be positive"))
      call f90_expect_true(q_high_friction < q_low_friction, &
                           to_c_string("higher Manning friction should produce less discharge"))
      call default_longculverts

   end subroutine test_4pt_friction_higher_value_reduces_discharge
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_flow_continuity_across_links, test_4pt_flow_continuity_across_links,
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

      call setup_4pt_model(iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(longculverts(1)%numlinks, 3, to_c_string("4-point culvert should have 3 links"))

      do i = 1, 15
         call flow_spatietimestep()
      end do

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      call f90_expect_true(q1(L1) > 0.0_dp, to_c_string("discharge at link 1 should be positive"))
      ! In steady state, Q should be equal across all links (continuity).
      ! After a few timesteps it wont be perfectly steady, but should be close enough (15%)
      call f90_expect_near(q1(L1), q1(L2), 0.15_dp * abs(q1(L1)), &
                           to_c_string("discharge at links 1 and 2 should be similar (continuity)"))
      call f90_expect_near(q1(L2), q1(L3), 0.15_dp * abs(q1(L2)), &
                           to_c_string("discharge at links 2 and 3 should be similar (continuity)"))

      call default_longculverts
   end subroutine test_4pt_flow_continuity_across_links
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_flow_continuity_converted, test_4pt_flow_continuity_converted,
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
      character(len=256) :: mdu_file = "test_lc_convert_4pt.mdu"

      call create_structure_file_4pt(STR_FILE)
      call create_two_row_netfile(NET_FILE)
      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call convertlongculverts(mdu_file, STR_FILE, NET_FILE)
      call init_two_culvert_scenario(mdu_file, iresult)
      call f90_assert_eq(iresult, DFM_NOERR, to_c_string("model init must succeed"))
      call f90_assert_eq(longculverts(1)%numlinks, 3, to_c_string("4-point culvert should have 3 links"))

      do i = 1, 15
         call flow_spatietimestep()
      end do

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      call f90_expect_true(q1(L1) < 0.0_dp, to_c_string("discharge at link 1 should be negative"))
      ! In steady state, Q should be equal across all links (continuity).
      ! After a few timesteps it wont be perfectly steady, but should be close enough (15%)
      call f90_expect_near(-q1(L1), q1(L2), 0.15_dp * abs(q1(L1)), &
                           to_c_string("discharge at links 1 and 2 should be similar (continuity)"))
      call f90_expect_near(q1(L2), q1(L3), 0.15_dp * abs(q1(L2)), &
                           to_c_string("discharge at links 2 and 3 should be similar (continuity)"))

      call default_longculverts
   end subroutine test_4pt_flow_continuity_converted
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_longculvert, test_4pt_with_existing_1d_network, test_4pt_with_existing_1d_network,
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
      use unstruc_messages, only: threshold_abort
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
      integer :: ierr

      ! Create a net file that includes a 1D network (branch) alongside the 2D grid.
      call create_net_with_1d_branch(NET_FILE, ierr)
      call f90_assert_eq(ierr, 0, to_c_string("Net file with 1D branch creation should succeed"))

      ! Create structure file with a 4-point long culvert
      call create_structure_file_4pt(STR_FILE)

      call create_mdu_file(mdu_file, NET_FILE, STR_FILE)
      call init_two_culvert_scenario(MDU_FILE, iresult)

      call f90_expect_eq(iresult, DFM_NOERR, to_c_string("flow_modelinit should succeed with existing 1D network + long culvert"))
      call f90_expect_eq(nlongculverts, 2, to_c_string("two long culverts should be registered"))
      call f90_expect_eq(longculverts(1)%numlinks, 3, to_c_string("4-point culvert should have 3 links"))

      call flow_spatietimestep()

      L1 = abs(longculverts(1)%flowlinks(1))
      L2 = abs(longculverts(1)%flowlinks(2))
      L3 = abs(longculverts(1)%flowlinks(3))

      call f90_expect_true(L1 > 0, to_c_string("entry link should be valid"))
      call f90_expect_true(au(L2) > 0.0_dp, to_c_string("au on interior link should be > 0 with existing 1D network"))
      call f90_expect_true(q1(L1) > 0.0_dp, to_c_string("discharge at entry should be positive with existing 1D network"))

      call default_longculverts
   end subroutine test_4pt_with_existing_1d_network
   !$f90tw)

   !> Create a UGRID net file that has both a 2D grid (5x3 nodes, 8 cells)
   !! AND a pre-existing, network-backed 1D mesh: a single network1d branch
   !! along y=50 from x=-200 to x=-100 (outside the 2D grid, so it does not
   !! interfere with the culvert placement), discretised into 3 mesh1d nodes.
   !! The 1D part is written as a full network1d + network-coupled mesh1d
   !! (branch index + offset) so the reader populates meshgeom1d (networkIndex > 0).
   subroutine create_net_with_1d_branch(filename, ierr)
      use precision, only: dp
      use netcdf
      character(len=*), intent(in) :: filename
      integer, intent(out) :: ierr

      ! 2D
      integer :: ncid, dimid_node, dimid_edge, dimid_face, dimid_maxnodes, dimid_two
      integer :: varid_mesh, varid_xn, varid_yn, varid_en, varid_fn
      integer, parameter :: NNODES = 15, NEDGES = 22, NFACES = 8
      real(kind=dp) :: xnodes(NNODES), ynodes(NNODES)
      integer :: edge_nodes(2, NEDGES), face_nodes(4, NFACES)

      ! network1d
      integer, parameter :: NET1D_NNODES = 2, NET1D_NEDGES = 1, NET1D_NGEOM = 2
      integer, parameter :: IDLEN = 40, LNLEN = 80
      integer :: dimid_idlen, dimid_lnlen
      integer :: dimid_1dnnodes, dimid_1dnedges, dimid_1dgeom
      integer :: varid_net1d, varid_net_en, varid_net_brid, varid_net_brln
      integer :: varid_net_elen, varid_net_nid, varid_net_nln
      integer :: varid_net_nx, varid_net_ny
      integer :: varid_net_geom, varid_net_gcount, varid_net_gx, varid_net_gy
      integer :: varid_net_border, varid_net_btype
      integer :: net_edge_nodes(2, NET1D_NEDGES)
      real(kind=dp) :: net_node_x(NET1D_NNODES), net_node_y(NET1D_NNODES)
      real(kind=dp) :: net_edge_length(NET1D_NEDGES)
      integer :: net_geom_count(NET1D_NEDGES)
      real(kind=dp) :: net_geom_x(NET1D_NGEOM), net_geom_y(NET1D_NGEOM)
      integer :: net_branch_order(NET1D_NEDGES), net_branch_type(NET1D_NEDGES)
      character(len=IDLEN) :: net_branch_id(NET1D_NEDGES), net_node_id(NET1D_NNODES)
      character(len=LNLEN) :: net_branch_ln(NET1D_NEDGES), net_node_ln(NET1D_NNODES)

      ! mesh1d (3 mesh nodes on the single branch)
      integer, parameter :: M1D_NNODES = 3, M1D_NEDGES = 2
      integer :: dimid_m1dnodes, dimid_m1dedges
      integer :: varid_m1d, varid_m1d_nbr, varid_m1d_noff, varid_m1d_nx, varid_m1d_ny
      integer :: varid_m1d_en, varid_m1d_ebr, varid_m1d_eoff
      integer :: varid_m1d_nid, varid_m1d_nln
      integer :: m1d_node_branch(M1D_NNODES), m1d_edge_branch(M1D_NEDGES)
      real(kind=dp) :: m1d_node_offset(M1D_NNODES), m1d_edge_offset(M1D_NEDGES)
      real(kind=dp) :: m1d_node_x(M1D_NNODES), m1d_node_y(M1D_NNODES)
      integer :: m1d_edge_nodes(2, M1D_NEDGES)
      character(len=IDLEN) :: m1d_node_id(M1D_NNODES)
      character(len=LNLEN) :: m1d_node_ln(M1D_NNODES)

      integer :: i, j, k

      ! 2D grid (same as create_two_row_netfile)
      k = 0
      do j = 1, 3
         do i = 1, 5
            k = k + 1
            xnodes(k) = real((i - 1) * 100, dp)
            ynodes(k) = real((j - 1) * 100, dp)
         end do
      end do

      edge_nodes(:, 1) = [1, 2]
      edge_nodes(:, 2) = [2, 3]
      edge_nodes(:, 3) = [3, 4]
      edge_nodes(:, 4) = [4, 5]
      edge_nodes(:, 5) = [6, 7]
      edge_nodes(:, 6) = [7, 8]
      edge_nodes(:, 7) = [8, 9]
      edge_nodes(:, 8) = [9, 10]
      edge_nodes(:, 9) = [11, 12]
      edge_nodes(:, 10) = [12, 13]
      edge_nodes(:, 11) = [13, 14]
      edge_nodes(:, 12) = [14, 15]
      edge_nodes(:, 13) = [1, 6]
      edge_nodes(:, 14) = [2, 7]
      edge_nodes(:, 15) = [3, 8]
      edge_nodes(:, 16) = [4, 9]
      edge_nodes(:, 17) = [5, 10]
      edge_nodes(:, 18) = [6, 11]
      edge_nodes(:, 19) = [7, 12]
      edge_nodes(:, 20) = [8, 13]
      edge_nodes(:, 21) = [9, 14]
      edge_nodes(:, 22) = [10, 15]

      face_nodes(:, 1) = [1, 2, 7, 6]
      face_nodes(:, 2) = [2, 3, 8, 7]
      face_nodes(:, 3) = [3, 4, 9, 8]
      face_nodes(:, 4) = [4, 5, 10, 9]
      face_nodes(:, 5) = [6, 7, 12, 11]
      face_nodes(:, 6) = [7, 8, 13, 12]
      face_nodes(:, 7) = [8, 9, 14, 13]
      face_nodes(:, 8) = [9, 10, 15, 14]

      ! network1d: single branch from (-200,50) to (-100,50), length 100
      net_node_x = [-200.0_dp, -100.0_dp]
      net_node_y = [50.0_dp, 50.0_dp]
      net_edge_nodes(:, 1) = [1, 2]
      net_edge_length(1) = 100.0_dp
      net_geom_count(1) = NET1D_NGEOM
      net_geom_x = [-200.0_dp, -100.0_dp]
      net_geom_y = [50.0_dp, 50.0_dp]
      net_branch_order(1) = -1
      net_branch_type(1) = 0
      net_branch_id(1) = 'branch1'
      net_branch_ln(1) = 'branch1'
      net_node_id(1) = 'nNode1'
      net_node_id(2) = 'nNode2'
      net_node_ln(1) = 'nNode1'
      net_node_ln(2) = 'nNode2'

      ! mesh1d: 3 nodes on branch 1 (0-based branch index) at offsets 0/50/100
      m1d_node_branch = [0, 0, 0]
      m1d_node_offset = [0.0_dp, 50.0_dp, 100.0_dp]
      m1d_node_x = [-200.0_dp, -150.0_dp, -100.0_dp]
      m1d_node_y = [50.0_dp, 50.0_dp, 50.0_dp]
      m1d_edge_nodes(:, 1) = [1, 2]
      m1d_edge_nodes(:, 2) = [2, 3]
      m1d_edge_branch = [0, 0]
      m1d_edge_offset = [25.0_dp, 75.0_dp]
      m1d_node_id(1) = 'mesh1d_node0001'
      m1d_node_id(2) = 'mesh1d_node0002'
      m1d_node_id(3) = 'mesh1d_node0003'
      m1d_node_ln(1) = 'mesh1d_node0001'
      m1d_node_ln(2) = 'mesh1d_node0002'
      m1d_node_ln(3) = 'mesh1d_node0003'

      !----------------------------------------------------------------!
      ! Write NetCDF
      !----------------------------------------------------------------!
      ierr = nf90_create(filename, NF90_CLOBBER, ncid)
      if (ierr /= nf90_noerr) return

      ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.8 UGRID-1.0 Deltares-0.10')

      ! ---- 2D dimensions and variables ----
      ierr = nf90_def_dim(ncid, 'mesh2d_nNodes', NNODES, dimid_node)
      ierr = nf90_def_dim(ncid, 'mesh2d_nEdges', NEDGES, dimid_edge)
      ierr = nf90_def_dim(ncid, 'mesh2d_nFaces', NFACES, dimid_face)
      ierr = nf90_def_dim(ncid, 'mesh2d_nMax_face_nodes', 4, dimid_maxnodes)
      ierr = nf90_def_dim(ncid, 'Two', 2, dimid_two)

      ierr = nf90_def_var(ncid, 'mesh2d', NF90_INT, varid_mesh)
      ierr = nf90_put_att(ncid, varid_mesh, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(ncid, varid_mesh, 'topology_dimension', 2)
      ierr = nf90_put_att(ncid, varid_mesh, 'node_coordinates', 'mesh2d_node_x mesh2d_node_y')
      ierr = nf90_put_att(ncid, varid_mesh, 'edge_node_connectivity', 'mesh2d_edge_nodes')
      ierr = nf90_put_att(ncid, varid_mesh, 'face_node_connectivity', 'mesh2d_face_nodes')

      ierr = nf90_def_var(ncid, 'mesh2d_node_x', NF90_DOUBLE, [dimid_node], varid_xn)
      ierr = nf90_put_att(ncid, varid_xn, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_xn, 'units', 'm')
      ierr = nf90_def_var(ncid, 'mesh2d_node_y', NF90_DOUBLE, [dimid_node], varid_yn)
      ierr = nf90_put_att(ncid, varid_yn, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_yn, 'units', 'm')

      ierr = nf90_def_var(ncid, 'mesh2d_edge_nodes', NF90_INT, [dimid_two, dimid_edge], varid_en)
      ierr = nf90_put_att(ncid, varid_en, 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, varid_en, 'start_index', 1)
      ierr = nf90_def_var(ncid, 'mesh2d_face_nodes', NF90_INT, [dimid_maxnodes, dimid_face], varid_fn)
      ierr = nf90_put_att(ncid, varid_fn, 'cf_role', 'face_node_connectivity')
      ierr = nf90_put_att(ncid, varid_fn, 'start_index', 1)

      ! ---- string-length dimensions ----
      ierr = nf90_def_dim(ncid, 'strLengthIds', IDLEN, dimid_idlen)
      ierr = nf90_def_dim(ncid, 'strLengthLongNames', LNLEN, dimid_lnlen)

      ! ---- network1d ----
      ierr = nf90_def_dim(ncid, 'network1d_nNodes', NET1D_NNODES, dimid_1dnnodes)
      ierr = nf90_def_dim(ncid, 'network1d_nEdges', NET1D_NEDGES, dimid_1dnedges)
      ierr = nf90_def_dim(ncid, 'network1d_nGeometryNodes', NET1D_NGEOM, dimid_1dgeom)

      ierr = nf90_def_var(ncid, 'network1d', NF90_INT, varid_net1d)
      ierr = nf90_put_att(ncid, varid_net1d, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(ncid, varid_net1d, 'long_name', 'Topology data of 1D network')
      ierr = nf90_put_att(ncid, varid_net1d, 'topology_dimension', 1)
      ierr = nf90_put_att(ncid, varid_net1d, 'node_dimension', 'network1d_nNodes')
      ierr = nf90_put_att(ncid, varid_net1d, 'edge_dimension', 'network1d_nEdges')
      ierr = nf90_put_att(ncid, varid_net1d, 'node_coordinates', 'network1d_node_x network1d_node_y')
      ierr = nf90_put_att(ncid, varid_net1d, 'node_id', 'network1d_node_id')
      ierr = nf90_put_att(ncid, varid_net1d, 'node_long_name', 'network1d_node_long_name')
      ierr = nf90_put_att(ncid, varid_net1d, 'edge_node_connectivity', 'network1d_edge_nodes')
      ierr = nf90_put_att(ncid, varid_net1d, 'edge_length', 'network1d_edge_length')
      ierr = nf90_put_att(ncid, varid_net1d, 'edge_geometry', 'network1d_geometry')
      ierr = nf90_put_att(ncid, varid_net1d, 'branch_id', 'network1d_branch_id')
      ierr = nf90_put_att(ncid, varid_net1d, 'branch_long_name', 'network1d_branch_long_name')

      ierr = nf90_def_var(ncid, 'network1d_node_x', NF90_DOUBLE, [dimid_1dnnodes], varid_net_nx)
      ierr = nf90_put_att(ncid, varid_net_nx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_net_nx, 'units', 'm')
      ierr = nf90_def_var(ncid, 'network1d_node_y', NF90_DOUBLE, [dimid_1dnnodes], varid_net_ny)
      ierr = nf90_put_att(ncid, varid_net_ny, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_net_ny, 'units', 'm')

      ierr = nf90_def_var(ncid, 'network1d_node_id', NF90_CHAR, [dimid_idlen, dimid_1dnnodes], varid_net_nid)
      ierr = nf90_def_var(ncid, 'network1d_node_long_name', NF90_CHAR, [dimid_lnlen, dimid_1dnnodes], varid_net_nln)

      ierr = nf90_def_var(ncid, 'network1d_edge_nodes', NF90_INT, [dimid_two, dimid_1dnedges], varid_net_en)
      ierr = nf90_put_att(ncid, varid_net_en, 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, varid_net_en, 'start_index', 1)
      ierr = nf90_def_var(ncid, 'network1d_edge_length', NF90_DOUBLE, [dimid_1dnedges], varid_net_elen)
      ierr = nf90_put_att(ncid, varid_net_elen, 'units', 'm')
      ierr = nf90_def_var(ncid, 'network1d_branch_id', NF90_CHAR, [dimid_idlen, dimid_1dnedges], varid_net_brid)
      ierr = nf90_def_var(ncid, 'network1d_branch_long_name', NF90_CHAR, [dimid_lnlen, dimid_1dnedges], varid_net_brln)

      ierr = nf90_def_var(ncid, 'network1d_geometry', NF90_INT, varid_net_geom)
      ierr = nf90_put_att(ncid, varid_net_geom, 'geometry_type', 'line')
      ierr = nf90_put_att(ncid, varid_net_geom, 'node_count', 'network1d_geom_node_count')
      ierr = nf90_put_att(ncid, varid_net_geom, 'node_coordinates', 'network1d_geom_x network1d_geom_y')
      ierr = nf90_def_var(ncid, 'network1d_geom_node_count', NF90_INT, [dimid_1dnedges], varid_net_gcount)
      ierr = nf90_def_var(ncid, 'network1d_geom_x', NF90_DOUBLE, [dimid_1dgeom], varid_net_gx)
      ierr = nf90_put_att(ncid, varid_net_gx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_net_gx, 'units', 'm')
      ierr = nf90_def_var(ncid, 'network1d_geom_y', NF90_DOUBLE, [dimid_1dgeom], varid_net_gy)
      ierr = nf90_put_att(ncid, varid_net_gy, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_net_gy, 'units', 'm')

      ierr = nf90_def_var(ncid, 'network1d_branch_order', NF90_INT, [dimid_1dnedges], varid_net_border)
      ierr = nf90_put_att(ncid, varid_net_border, 'long_name', 'Order of branches for interpolation')
      ierr = nf90_put_att(ncid, varid_net_border, 'mesh', 'network1d')
      ierr = nf90_put_att(ncid, varid_net_border, 'location', 'edge')
      ierr = nf90_def_var(ncid, 'network1d_branch_type', NF90_INT, [dimid_1dnedges], varid_net_btype)
      ierr = nf90_put_att(ncid, varid_net_btype, 'long_name', 'Type of branches')
      ierr = nf90_put_att(ncid, varid_net_btype, 'mesh', 'network1d')
      ierr = nf90_put_att(ncid, varid_net_btype, 'location', 'edge')

      ! ---- mesh1d (network-coupled) ----
      ierr = nf90_def_dim(ncid, 'mesh1d_nNodes', M1D_NNODES, dimid_m1dnodes)
      ierr = nf90_def_dim(ncid, 'mesh1d_nEdges', M1D_NEDGES, dimid_m1dedges)

      ierr = nf90_def_var(ncid, 'mesh1d', NF90_INT, varid_m1d)
      ierr = nf90_put_att(ncid, varid_m1d, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(ncid, varid_m1d, 'long_name', 'Topology data of 1D mesh')
      ierr = nf90_put_att(ncid, varid_m1d, 'topology_dimension', 1)
      ierr = nf90_put_att(ncid, varid_m1d, 'coordinate_space', 'network1d')
      ierr = nf90_put_att(ncid, varid_m1d, 'node_dimension', 'mesh1d_nNodes')
      ierr = nf90_put_att(ncid, varid_m1d, 'edge_dimension', 'mesh1d_nEdges')
      ierr = nf90_put_att(ncid, varid_m1d, 'edge_node_connectivity', 'mesh1d_edge_nodes')
      ierr = nf90_put_att(ncid, varid_m1d, 'node_coordinates', &
                          'mesh1d_node_branch mesh1d_node_offset mesh1d_node_x mesh1d_node_y')
      ierr = nf90_put_att(ncid, varid_m1d, 'edge_coordinates', 'mesh1d_edge_branch mesh1d_edge_offset')
      ierr = nf90_put_att(ncid, varid_m1d, 'node_id', 'mesh1d_node_id')
      ierr = nf90_put_att(ncid, varid_m1d, 'node_long_name', 'mesh1d_node_long_name')

      ierr = nf90_def_var(ncid, 'mesh1d_node_branch', NF90_INT, [dimid_m1dnodes], varid_m1d_nbr)
      ierr = nf90_put_att(ncid, varid_m1d_nbr, 'long_name', 'Index of branch on which mesh nodes are located')
      ierr = nf90_put_att(ncid, varid_m1d_nbr, 'start_index', 0)
      ierr = nf90_def_var(ncid, 'mesh1d_node_offset', NF90_DOUBLE, [dimid_m1dnodes], varid_m1d_noff)
      ierr = nf90_put_att(ncid, varid_m1d_noff, 'long_name', 'Offset along branch of mesh nodes')
      ierr = nf90_put_att(ncid, varid_m1d_noff, 'units', 'm')
      ierr = nf90_def_var(ncid, 'mesh1d_node_x', NF90_DOUBLE, [dimid_m1dnodes], varid_m1d_nx)
      ierr = nf90_put_att(ncid, varid_m1d_nx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, varid_m1d_nx, 'units', 'm')
      ierr = nf90_def_var(ncid, 'mesh1d_node_y', NF90_DOUBLE, [dimid_m1dnodes], varid_m1d_ny)
      ierr = nf90_put_att(ncid, varid_m1d_ny, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, varid_m1d_ny, 'units', 'm')

      ierr = nf90_def_var(ncid, 'mesh1d_edge_nodes', NF90_INT, [dimid_two, dimid_m1dedges], varid_m1d_en)
      ierr = nf90_put_att(ncid, varid_m1d_en, 'cf_role', 'edge_node_connectivity')
      ierr = nf90_put_att(ncid, varid_m1d_en, 'start_index', 1)
      ierr = nf90_def_var(ncid, 'mesh1d_edge_branch', NF90_INT, [dimid_m1dedges], varid_m1d_ebr)
      ierr = nf90_put_att(ncid, varid_m1d_ebr, 'start_index', 0)
      ierr = nf90_def_var(ncid, 'mesh1d_edge_offset', NF90_DOUBLE, [dimid_m1dedges], varid_m1d_eoff)
      ierr = nf90_put_att(ncid, varid_m1d_eoff, 'units', 'm')

      ierr = nf90_def_var(ncid, 'mesh1d_node_id', NF90_CHAR, [dimid_idlen, dimid_m1dnodes], varid_m1d_nid)
      ierr = nf90_put_att(ncid, varid_m1d_nid, 'long_name', 'ID of mesh nodes')
      ierr = nf90_put_att(ncid, varid_m1d_nid, 'cf_role', 'timeseries_id')
      ierr = nf90_def_var(ncid, 'mesh1d_node_long_name', NF90_CHAR, [dimid_lnlen, dimid_m1dnodes], varid_m1d_nln)
      ierr = nf90_put_att(ncid, varid_m1d_nln, 'long_name', 'Long name of mesh nodes')

      ierr = nf90_enddef(ncid)
      if (ierr /= nf90_noerr) then
         ierr = nf90_close(ncid)
         return
      end if

      ! ---- write 2D data ----
      ierr = nf90_put_var(ncid, varid_xn, xnodes)
      ierr = nf90_put_var(ncid, varid_yn, ynodes)
      ierr = nf90_put_var(ncid, varid_en, edge_nodes)
      ierr = nf90_put_var(ncid, varid_fn, face_nodes)

      ! ---- write network1d data ----
      ierr = nf90_put_var(ncid, varid_net_nx, net_node_x)
      ierr = nf90_put_var(ncid, varid_net_ny, net_node_y)
      ierr = nf90_put_var(ncid, varid_net_nid, net_node_id)
      ierr = nf90_put_var(ncid, varid_net_nln, net_node_ln)
      ierr = nf90_put_var(ncid, varid_net_en, net_edge_nodes)
      ierr = nf90_put_var(ncid, varid_net_elen, net_edge_length)
      ierr = nf90_put_var(ncid, varid_net_brid, net_branch_id)
      ierr = nf90_put_var(ncid, varid_net_brln, net_branch_ln)
      ierr = nf90_put_var(ncid, varid_net_gcount, net_geom_count)
      ierr = nf90_put_var(ncid, varid_net_gx, net_geom_x)
      ierr = nf90_put_var(ncid, varid_net_gy, net_geom_y)
      ierr = nf90_put_var(ncid, varid_net_border, net_branch_order)
      ierr = nf90_put_var(ncid, varid_net_btype, net_branch_type)

      ! ---- write mesh1d data ----
      ierr = nf90_put_var(ncid, varid_m1d_nbr, m1d_node_branch)
      ierr = nf90_put_var(ncid, varid_m1d_noff, m1d_node_offset)
      ierr = nf90_put_var(ncid, varid_m1d_nx, m1d_node_x)
      ierr = nf90_put_var(ncid, varid_m1d_ny, m1d_node_y)
      ierr = nf90_put_var(ncid, varid_m1d_en, m1d_edge_nodes)
      ierr = nf90_put_var(ncid, varid_m1d_ebr, m1d_edge_branch)
      ierr = nf90_put_var(ncid, varid_m1d_eoff, m1d_edge_offset)
      ierr = nf90_put_var(ncid, varid_m1d_nid, m1d_node_id)
      ierr = nf90_put_var(ncid, varid_m1d_nln, m1d_node_ln)

      ierr = nf90_close(ncid)
      ierr = 0 ! success
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
      logical :: file_exists

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
      call default_longculverts
      call timini()
      timon = .false.
      jampi = 0
      call SetMessageHandling(write2screen=.false.)
      inputfiles(1) = mdufile
      numfiles = 1
      md_japartition = 1 !> to make long culvert initialisation write the converted files
      jaSkipCmdLineArgs = 0
      call INIDAT()
      ! Write the updated net file (now includes culvert netlinks)
      netfile_local = "converted_"//trim(netfile)
      call findcells(0)
      call find1dcells()
      call unc_write_net(netfile_local, janetcell=1, janetbnd=1, jaidomain=0, &
                         jaiglobal_s=0, md_ident=md_ident) ! Save net bnds to prevent unnecessary open bnds
      md_netfile = netfile_local
      mdufile_local = "converted_"//mdufile
      md_structurefile = "converted_"//structurefile
      md_crsfile = "converted_crsdef.ini"
      md_convertlongculverts = 0
      call writeMDUfile(mdufile_local, ierr)
      mdufile = mdufile_local
      call default_longculverts !> not automatically reset
      numfiles = 0 !> not automatically reset
      md_japartition = 0 !> not automatically reset

   end subroutine convertlongculverts

end module test_longculverts
