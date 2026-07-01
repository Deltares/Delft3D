module test_bubblescreen
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_bubblescreen
   use m_alloc, only: realloc

   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_bubblescreen, test_with_normal_source_sinks, test_with_normal_source_sinks,
   !> Test bubble and normal source-sinks
   subroutine test_with_normal_source_sinks() bind(C)
      use fm_external_forcings, only: init_new
      use m_flow_geominit, only: flow_geominit
      use Timers, only: timini
      use fm_external_forcings_data, only: bubblescreen_air_discharge
      use m_source_sink, only: source_sink_all_discharges
      use m_meteo, only: initialize_ec_module, item_sourcesink_discharge, ecInstancePtr, ec_gettimespacevalue, item_bubblescreen_discharge
      use m_cell_geometry, only: xz, yz, ndx        ! use fm_external_forcings, only: init_new
      use m_flowgeom, only: kcs
      use m_file_helpers, only: create_file
      use m_network_helpers, only: t_grid_helper
      use m_wind, only: rain
      use m_partitioninfo, only: jampi

      integer :: iresult
      logical :: success
      type(t_grid_helper) :: grid_helper
      grid_helper = t_grid_helper()

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
      call grid_helper%make_square_grid( &
         bottom_left_x=0.0_dp, bottom_left_y=0.0_dp, side_length=10.0_dp, &
         rows=1, columns=2, array_size_margin=2 &
         )
      call flow_geominit(0)
      call initialize_ec_module()

      call init_new("FlowFM_bnd.ext", iresult)

      success = ec_gettimespacevalue(ecInstancePtr, item_sourcesink_discharge, 20010101, 0.0_dp, 1, 300.0_dp)

      call f90_expect_true(success, "ec_gettimespacevalue failed to retrieve source-sink discharge value at 300 seconds")
      call f90_expect_near(source_sink_all_discharges(1,1), 1003.47_dp, 1e-1_dp, "Source sink discharge at 300 seconds should be approximately 1003.47 m3/s")

      success = ec_gettimespacevalue(ecInstancePtr, item_bubblescreen_discharge, 20010101, 0.0_dp, 1, 300.0_dp)
      call f90_expect_true(success, "ec_gettimespacevalue failed to retrieve bubble screen air discharge value at 300 seconds")
      call f90_expect_near(bubblescreen_air_discharge(1), 201.38_dp, 1e-1_dp, "Bubble screen air discharge at 300 seconds should be approximately 201.38 m3/s")

   end subroutine test_with_normal_source_sinks
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


end module test_bubblescreen
