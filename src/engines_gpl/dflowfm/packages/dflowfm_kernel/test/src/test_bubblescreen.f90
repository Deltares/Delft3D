module test_bubblescreen
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_bubblescreen
   use m_alloc, only: realloc
   use m_meteo, only: initialize_ec_module, item_sourcesink_discharge, ecInstancePtr, ec_gettimespacevalue, item_bubblescreen_discharge
   use m_cell_geometry, only: xz, yz, ndx        ! use fm_external_forcings, only: init_new
   use m_flowgeom, only: kcs
   use m_file_helpers, only: create_file
   use m_wind, only: rain
   use m_partitioninfo, only: jampi

   implicit none(type, external)

contains

   !> Sets up minimal network_data with a single rectangular netcell
    !! centered at (center_x, center_y) with given side length.
    !! This is useful for testing routines like incells that depend on network_data.
   subroutine generate_square_grid(bottom_left_x, bottom_left_y, side_length, rows, columns, array_size_margin)
      use precision, only: dp
      use network_data, only: xk, yk, zk, kc, nmk, numk, kn, nump, nump1d2d, netcell, tface, lc, numl, xzw, yzw, nod, rnod, LINK_2D
      use m_cell_geometry, only: xz, yz, ndx
      use m_alloc, only: realloc
      use m_dimens, only: kmax, lmax
      use m_set_nod_adm, only: setnodadm
      use gridoperations, only: findcells
      implicit none

      real(kind=dp), intent(in) :: bottom_left_x !< X-coordinate of cell center
      real(kind=dp), intent(in) :: bottom_left_y !< Y-coordinate of cell center
      real(kind=dp), intent(in) :: side_length !< Side length of square cell
      integer, intent(in) :: rows !< Number of rows
      integer, intent(in) :: columns !< Number of columns
      integer, optional, intent(in) :: array_size_margin
      integer :: array_size_margin_
      integer :: istat
      integer :: i, row, col, link_index, bottom_left_node_index, up_node_index, right_node_index, up_right_node_index

      array_size_margin_ = 0
      if (present(array_size_margin)) then
         array_size_margin_ = array_size_margin
      end if

      ! Set up 4 net nodes for a rectangular cell
      numk = (rows + 1)*(columns + 1)
      call realloc(xk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
      call realloc(yk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
      call realloc(zk, numk + array_size_margin_, stat=istat, fill=0.0_dp)
      call realloc(kc, numk + array_size_margin_, stat=istat, fill=1)
      call realloc(nmk, numk + array_size_margin_, stat=istat, fill=2)
      allocate (nod(numk + array_size_margin_))

      ! Place (rows+1)x(columns+1) grid nodes (the cell corners).
      do row = 0, rows
         do col = 0, columns
            xk(row*(columns + 1) + col + 1) = bottom_left_x + col*side_length
            yk(row*(columns + 1) + col + 1) = bottom_left_y + row*side_length
         end do
      end do

      ! Place links between nodes
      numl = 2*rows*columns + rows + columns
      call realloc(kn, [3, numl + array_size_margin_], stat=istat, fill=0)
      call realloc(lc, numl + array_size_margin_, stat=istat, fill=1)
      link_index = 1
      do row = 0, rows - 1
         do col = 0, columns - 1
            bottom_left_node_index = row*(columns + 1) + col + 1
            right_node_index = bottom_left_node_index + 1
            up_node_index = bottom_left_node_index + columns + 1
            up_right_node_index = bottom_left_node_index + columns + 2

            if (row == 0) then
               kn(:, link_index) = [bottom_left_node_index, right_node_index, LINK_2D]
               link_index = link_index + 1
            end if
            kn(:, link_index) = [right_node_index, up_right_node_index, LINK_2D]
            kn(:, link_index + 1) = [up_right_node_index, up_node_index, LINK_2D]
            link_index = link_index + 2
            if (col == 0) then
               kn(:, link_index) = [up_node_index, bottom_left_node_index, LINK_2D]
               link_index = link_index + 1
            end if
         end do
      end do

      ! Initializes node, face and flow geometry stuff.
      call setnodadm(0)
      call findcells(0)

      kmax = 100
      lmax = 100
   end subroutine generate_square_grid
   !> Set up a minimal 1-cell s-point grid so that get_location_target_properties
   !! and construct_target_mask do not dereference unallocated arrays.
   subroutine setup_minimal_grid()
      ndx = 1
      if (.not. allocated(xz)) allocate (xz(ndx))
      if (.not. allocated(yz)) allocate (yz(ndx))
      if (.not. allocated(kcs)) allocate (kcs(ndx))
      xz = [0.0_dp]
      yz = [0.0_dp]
      kcs = [1]
   end subroutine setup_minimal_grid

   subroutine teardown_minimal_grid()
      ndx = 0
      if (allocated(xz)) deallocate (xz)
      if (allocated(yz)) deallocate (yz)
      if (allocated(kcs)) deallocate (kcs)
      if (allocated(rain)) deallocate (rain)
   end subroutine teardown_minimal_grid

   !$f90tw TESTCODE(TEST, test_bubblescreen, test_bubble_and_normal_source_sinks, test_bubble_and_normal_source_sinks,
   !> Test bubble and normal source-sinks
   subroutine test_bubble_and_normal_source_sinks() bind(C)
      use fm_external_forcings, only: init_new
      use m_flow_geominit, only: flow_geominit
      use Timers, only: timini
      use fm_external_forcings_data, only: source_sink_all_discharges, bubblescreen_air_discharge

      integer :: iresult
      logical :: success

      call create_file("FlowFM_sourcesink.bc", [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[Forcing]", &
                       "    name                  = SourceSink01", &
                       "    function              = timeSeries", &
                       "    timeInterpolation     = linear", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2001-01-01 00:00:00", &
                       "    quantity              = sourcesink_discharge", &
                       "    unit                  = m3/s", &
                       "    0      1000", &
                       "    86400  2000" &
                       ])
      call create_file("bubble_discharge.bc", [ &
                       "[General]", &
                       "    fileVersion           = 1.01", &
                       "    fileType              = boundConds", &
                       "", &
                       "[Forcing]", &
                       "    name                  = bubbles1", &
                       "    function              = timeSeries", &
                       "    timeInterpolation     = linear", &
                       "    quantity              = time", &
                       "    unit                  = seconds since 2001-01-01 00:00:00", &
                       "    quantity              = bubblescreen_discharge", &
                       "    unit                  = m3/s", &
                       "    0      200", &
                       "    86400  600" &
                       ])

      call create_file("bubbles.pli", [ &
                       "bubbles1", &
                       "    2    2", &
                       "736  1052", &
                       "1253  1046" &
                       ])
      
      call create_file("FlowFM_bnd.ext", [ &
                       "[SourceSink]", &
                       "   id=SourceSink01", &
                       "   numCoordinates=1", &
                       "   xCoordinates=0", &
                       "   yCoordinates=0", &
                       "   discharge=FlowFM_sourcesink.bc", &
                       "", &
                       "[bubblescreen]", &
                       "   id=bubbles1", &
                       "   locationFile=bubbles.pli", &
                       "   zLevel=-5.0", &
                       "   discharge=bubble_discharge.bc" &
                       ])

      jampi = 0
      call timini()
      call generate_square_grid( &
         bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, &
         rows=1, columns=2, side_length=10.0_dp, array_size_margin=16 &
         )
      call flow_geominit(0)
      call initialize_ec_module()

      call init_new("FlowFM_bnd.ext", iresult)

      ! call get_timespace_value_by_item_and_consider_success_value(item_sourcesink_discharge, 300.0_dp)

      ! call ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)
      success = ec_gettimespacevalue(ecInstancePtr, item_sourcesink_discharge, 20010101, 0.0_dp, 1, 300.0_dp)

      call f90_expect_true(success, "ec_gettimespacevalue failed to retrieve source-sink discharge value at 300 seconds")
      call f90_expect_near(source_sink_all_discharges(1,1), 1003.47_dp, 1e-1_dp, "Source sink discharge at 300 seconds should be approximately 1003.47 m3/s")

      success = ec_gettimespacevalue(ecInstancePtr, item_bubblescreen_discharge, 20010101, 0.0_dp, 1, 300.0_dp)
      call f90_expect_true(success, "ec_gettimespacevalue failed to retrieve bubble screen air discharge value at 300 seconds")
      call f90_expect_near(bubblescreen_air_discharge(1), 201.38_dp, 1e-1_dp, "Bubble screen air discharge at 300 seconds should be approximately 201.38 m3/s")

      ! call teardown_minimal_grid()

   end subroutine test_bubble_and_normal_source_sinks
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bubblescreen, test_convert_discharge_air_to_water_default, test_convert_discharge_air_to_water_default,
   !> Test conversion of air discharge to water discharge with default parameters
   subroutine test_convert_discharge_air_to_water_default() bind(C)
      ! Local variables
      real(kind=dp) :: air_discharge
      real(kind=dp) :: expected_water_discharge
      real(kind=dp) :: computed_water_discharge

      ! Setup
      air_discharge = 0.1_dp
      expected_water_discharge = (1000.0_dp*air_discharge)**(2.0_dp/3.0_dp)

      ! Call function to test
      computed_water_discharge = convert_discharge_air_to_water(air_discharge)

      ! Compare results
      call f90_expect_true(comparereal(computed_water_discharge, expected_water_discharge) == 0, "Converted water discharge should match expected value")

   end subroutine test_convert_discharge_air_to_water_default
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bubblescreen, test_convert_discharge_air_to_water_custom, test_convert_discharge_air_to_water_custom,
   !> Test conversion of air discharge to water discharge with custom parameters
   subroutine test_convert_discharge_air_to_water_custom() bind(C)
      ! Local variables
      real(kind=dp) :: air_discharge
      real(kind=dp) :: alpha
      real(kind=dp) :: expected_water_discharge
      real(kind=dp) :: computed_water_discharge

      ! Setup
      air_discharge = 0.1_dp
      alpha = 100.0_dp
      expected_water_discharge = (alpha*air_discharge)**(2.0_dp/3.0_dp)

      ! Call function to test
      computed_water_discharge = convert_discharge_air_to_water(air_discharge, alpha)

      ! Compare results
        call f90_expect_true(comparereal(computed_water_discharge, expected_water_discharge) == 0, "Converted water discharge should match expected value")

   end subroutine test_convert_discharge_air_to_water_custom
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_bubblescreen, test_find_active_layer_interfaces, test_find_active_layer_interfaces,
   !> Test active layer interfaces computation for a flow cell with 10 layers.
    !! The bubble screen is located at z = -7.8 m, the water level is at z = -0.4 m.
    !! The subroutine should return the correct start, stop, and max velocity layer interfaces.
   subroutine test_find_active_layer_interfaces() bind(C)
      use m_flow, only: kmx, zws, kbot, ktop, s1

      ! Local variables
      character(len=:), allocatable :: bubblescreen_id !< ID of bubble screen (not used in this test but required by function signature)
      integer :: flow_cell_index !< 2D flow cell index
      integer :: expected_k_start !< Expected starting layer interface for bubble screen
      integer :: expected_k_stop !< Expected stopping layer interface for bubble screen
      integer :: expected_k_max_velocity !< Expected layer interface of maximum velocity for bubble screen
      integer :: computed_k_start !< Computed starting layer interface for bubble screen
      integer :: computed_k_stop !< Computed stopping layer interface for bubble screen
      integer :: computed_k_max_velocity !< Computed layer interface of maximum velocity for bubble screen
      real(kind=dp) :: z_bubblescreen !< z coordinate of bubble screen

      ! Setup - inputs
      bubblescreen_id = "TestBubbleScreen"
      flow_cell_index = 1
      z_bubblescreen = -7.8_dp

      ! Setup - globals
      kmx = 10
      call realloc(kbot, 1, fill=2, keepexisting=.false.)
      call realloc(ktop, 1, fill=11, keepexisting=.false.)
      call realloc(s1, 1, fill=-0.4_dp, keepexisting=.false.) ! z_top = waterlevel = -0.4 m
      call realloc(zws, 11, fill=0.0_dp, keepexisting=.false.)
      zws(1:11) = [-10.0_dp, -9.0_dp, -8.0_dp, -7.0_dp, -6.0_dp, -5.0_dp, -4.0_dp, -3.0_dp, -2.0_dp, -1.0_dp, 0.0_dp] ! Layers are 1 m thick

      ! Setup - expected values
      expected_k_start = 3 ! z_bot = z_bubblescreen = -7.8 m => closest to interface with z = -8 m
      expected_k_stop = 11 ! z_top = -0.4 m => closest to interface with z = 0 m
      expected_k_max_velocity = 9 ! z_max_velocity = -1.88 m => closest to interface with z = -2 m

      ! Call function to test
        call find_active_layer_interfaces(flow_cell_index, z_bubblescreen, bubblescreen_id, computed_k_start, computed_k_stop, computed_k_max_velocity)

      ! Compare results
      call f90_expect_true(computed_k_start == expected_k_start, "Computed k_start should match expected value")
      call f90_expect_true(computed_k_stop == expected_k_stop, "Computed k_stop should match expected value")
     call f90_expect_true(computed_k_max_velocity == expected_k_max_velocity, "Computed k_max_velocity should match expected value")

      ! Cleanup
      deallocate (kbot)
      deallocate (ktop)
      deallocate (s1)
      deallocate (zws)

   end subroutine test_find_active_layer_interfaces
   !$f90tw)

end module test_bubblescreen
