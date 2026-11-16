module test_sources_sinks
   use assertions_gtest
   use fm_external_forcings, only: sourcesink_parse_coordinates
   use tree_structures, only: tree_data, tree_create, tree_create_node, tree_destroy
   use properties, only: prop_set
   use MessageHandling, only: getLastMessage, LEVEL_ERROR, resetMessageCount_MH, LEVEL_FATAL, SetMessageHandling, LEVEL_ALL
   use unstruc_messages, only: threshold_abort
   use precision_basics, only: dp
   use m_missing, only: dmiss
   use m_file_helpers, only: create_file

   implicit none(type, external)

   character(len=*), parameter :: EXT_FILENAME = "sourcesinks.ext"

contains

!$gtest
   subroutine test_polyline_file_missing() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      integer :: error_level
      character(len=256) :: error_message

      threshold_abort = LEVEL_FATAL
      call SetMessageHandling(useLog=.true., thresholdLevel=LEVEL_ALL, reset_counters=.true.)

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "this_file_does_not_exist.pliz")
      call prop_set(block_ptr, "", "zSource",      "-7.5")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, 'sourcesink', x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_expect_false(success)

      call getLastMessage(error_level, error_message)
      call f90_expect_eq(error_level, LEVEL_ERROR)

      call tree_destroy(tree)
   end subroutine test_polyline_file_missing

!$gtest
   subroutine test_double_z_data_specification() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      integer :: error_level
      character(len=256) :: error_message

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 3", &
                       "25.0 5.0 -11.5", &
                       "7.0 11.0 0.0", &
                       "17.0 9.0 -7.5"])

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "my_polyline.pliz")
      call prop_set(block_ptr, "", "zSource",      "-7.5")
      call prop_set(block_ptr, "", "zSink",        "-11.5")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      call resetMessageCount_MH()
      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_expect_false(success)

      call getLastMessage(error_level, error_message)
      call f90_expect_eq(error_level, LEVEL_ERROR)
      call f90_expect_true(error_message == "Error in source sink initialization, source/sink z information cannot be specified both in the ext file and in the polyline file. Make sure the polyline file only contains x and y columns")

      call tree_destroy(tree)
   end subroutine test_double_z_data_specification

!$gtest
   subroutine test_xy_in_polyline_file_2_columns() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 2", &
                       "25.0 5.0", &
                       "7.0 11.0", &
                       "17.0 9.0"])

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "my_polyline.pliz")
      call prop_set(block_ptr, "", "zSource",      "-7.5")
      call prop_set(block_ptr, "", "zSink",        "-11.5")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xy_in_polyline_file_2_columns

!$gtest
   subroutine test_xyz_in_polyline_file_3_columns() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 3", &
                       "25.0 5.0 -11.5", &
                       "7.0 11.0 0.0", &
                       "17.0 9.0 -7.5"])

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "my_polyline.pliz")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_3_columns

!$gtest
   subroutine test_xyz_in_polyline_file_4_columns() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 4", &
                       "25.0 5.0 -11.5 -10.0", &
                       "7.0 11.0 0.0 0.0", &
                       "17.0 9.0 -7.5 -6.0"])

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "my_polyline.pliz")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -6.0_dp)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), -10.0_dp)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_4_columns

!$gtest
   subroutine test_xyz_in_polyline_file_5_columns() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 5", &
                       "25.0 5.0 -11.5 -10.0 0", &
                       "7.0 11.0 0.0 0.0 0", &
                       "17.0 9.0 -7.5 -6.0 0"])

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",           "left")
      call prop_set(block_ptr, "", "name",         "cool_name")
      call prop_set(block_ptr, "", "locationFile", "my_polyline.pliz")
      call prop_set(block_ptr, "", "Area",         "0.0")
      call prop_set(block_ptr, "", "discharge",    "left.bc")
      call prop_set(block_ptr, "", "salinityDelta","left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 3)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -6.0_dp)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), -10.0_dp)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_5_columns

!$gtest
   subroutine test_xy_in_ext_file_single_z() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",             "left")
      call prop_set(block_ptr, "", "name",           "cool_name")
      call prop_set(block_ptr, "", "numCoordinates", "2")
      call prop_set(block_ptr, "", "xCoordinates",   "25.0 26.0")
      call prop_set(block_ptr, "", "yCoordinates",   "5.0 6.0")
      call prop_set(block_ptr, "", "zSource",        "-7.5")
      call prop_set(block_ptr, "", "zSink",          "-13.5")
      call prop_set(block_ptr, "", "Area",           "0.0")
      call prop_set(block_ptr, "", "discharge",      "left.bc")
      call prop_set(block_ptr, "", "salinityDelta",  "left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 2)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 26.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 2)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 6.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -13.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xy_in_ext_file_single_z

!$gtest
   subroutine test_xy_in_ext_file_z_ranges() bind(C)
      character(len=256) :: base_dir = ""
      character(len=256) :: file_name = ""
      character(len=256) :: group_name = ""

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      type(tree_data), pointer :: tree => null()
      type(tree_data), pointer :: block_ptr => null()

      call tree_create("sourcesinks.ext", tree)
      call tree_create_node(tree, "sourcesink", block_ptr)
      call prop_set(block_ptr, "", "id",             "left")
      call prop_set(block_ptr, "", "name",           "cool_name")
      call prop_set(block_ptr, "", "numCoordinates", "2")
      call prop_set(block_ptr, "", "xCoordinates",   "25.0 26.0")
      call prop_set(block_ptr, "", "yCoordinates",   "5.0 6.0")
      call prop_set(block_ptr, "", "zSource",        "-7.5 -3.0")
      call prop_set(block_ptr, "", "zSink",          "-10.0 -6.0")
      call prop_set(block_ptr, "", "Area",           "0.0")
      call prop_set(block_ptr, "", "discharge",      "left.bc")
      call prop_set(block_ptr, "", "salinityDelta",  "left.bc")

      success = sourcesink_parse_coordinates(block_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      if (.not. f90_assert_true(success)) then
         call tree_destroy(tree)
         return
      end if
      if (.not. f90_assert_true(allocated(x_coordinates) .and. allocated(y_coordinates), &
                                "coordinate arrays must be allocated")) then
         call tree_destroy(tree)
         return
      end if

      if (.not. f90_assert_eq(size(x_coordinates), 2)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 26.0_dp)

      if (.not. f90_assert_eq(size(y_coordinates), 2)) then
         call tree_destroy(tree)
         return
      end if
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 6.0_dp)

      call f90_expect_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -3.0_dp)

      call f90_expect_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -10.0_dp)
      call f90_expect_eq(z_range_sink(2), -6.0_dp)

      call tree_destroy(tree)
   end subroutine test_xy_in_ext_file_z_ranges

!$gtest
   subroutine test_nearfield_discharge_layer_selection() bind(C)
      call check_nearfield_discharge(-6.0_dp, -6.0_dp, 10.0_dp, [0.0_dp, 10.0_dp, 0.0_dp])
      call check_nearfield_discharge(-12.0_dp, -11.0_dp, 10.0_dp, [10.0_dp, 0.0_dp, 0.0_dp])
      call check_nearfield_discharge(-12.0_dp, -6.0_dp, 10.0_dp, [4.0_dp, 6.0_dp, 0.0_dp])
      call check_nearfield_discharge(1.0_dp, 2.0_dp, 10.0_dp, [0.0_dp, 0.0_dp, 10.0_dp])
      call check_nearfield_discharge(-8.0_dp, -8.0_dp, 10.0_dp, [0.0_dp, 10.0_dp, 0.0_dp])
      call check_nearfield_discharge(dmiss, dmiss, 10.0_dp, [2.0_dp, 3.0_dp, 5.0_dp])
      call check_nearfield_discharge(-6.0_dp, dmiss, 10.0_dp, [0.0_dp, 10.0_dp, 0.0_dp])
   end subroutine test_nearfield_discharge_layer_selection

!$gtest
   subroutine test_nearfield_discharge_reversed_and_limited() bind(C)
      call check_nearfield_discharge(-12.0_dp, -6.0_dp, -10.0_dp, [-4.0_dp, -6.0_dp, 0.0_dp])
      call check_nearfield_discharge(-12.0_dp, -6.0_dp, 2000.0_dp, [400.0_dp, 600.0_dp, 0.0_dp])
      call check_nearfield_discharge(-12.0_dp, -6.0_dp, -2000.0_dp, [-400.0_dp, -600.0_dp, 0.0_dp])
   end subroutine test_nearfield_discharge_reversed_and_limited

!$gtest
   subroutine test_nearfield_discharge_2d_and_inactive() bind(C)
      use m_source_sink, only: SourceSinks, FLOWCELL_SINK, FLOWCELL_SOURCE
      use m_flow, only: kmx

      type(SourceSinks) :: sources
      real(kind=dp), dimension(2) :: discharge

      kmx = 0
      sources%num_total = 2
      sources%num_nearfield = 2
      allocate (sources%indices(2, 6), sources%discharge(2))
      sources%indices(1, :) = [1, 1, 1, 2, 2, 2]
      sources%indices(2, :) = [0, 0, 0, 2, 2, 2]
      sources%discharge = [10.0_dp, 4.0_dp]
      call sources%get_nearfield_discharge(discharge)
      call f90_expect_near(discharge, [-10.0_dp, 14.0_dp], 1.0e-12_dp)

      sources%indices(1, FLOWCELL_SINK) = -1
      sources%indices(:, FLOWCELL_SOURCE) = -1
      call sources%get_nearfield_discharge(discharge)
      call f90_expect_near(discharge, [0.0_dp, 0.0_dp], 1.0e-12_dp, "nonlocal sides must be skipped")

      sources%indices = 0
      sources%indices(1, FLOWCELL_SINK) = 1
      call sources%get_nearfield_discharge(discharge)
      call f90_expect_near(discharge, [0.0_dp, 0.0_dp], 1.0e-12_dp, "no layer discharge before initialization")
   end subroutine test_nearfield_discharge_2d_and_inactive

!$gtest
   subroutine test_nearfield_discharge_collapsed_layers() bind(C)
      use m_source_sink, only: SourceSinks
      use m_flow, only: kmx, zws
      use m_alloc, only: realloc

      type(SourceSinks) :: sources
      real(kind=dp), dimension(4) :: discharge

      kmx = 3
      call realloc(zws, 4, keepExisting=.false., fill=-10.0_dp)
      sources%num_total = 1
      sources%num_nearfield = 1
      allocate (sources%indices(1, 6), sources%discharge(1))
      sources%indices(1, :) = [0, 0, 0, 1, 2, 4]
      sources%discharge = 6.0_dp
      call sources%get_nearfield_discharge(discharge)
      call f90_expect_near(discharge, [0.0_dp, 2.0_dp, 2.0_dp, 2.0_dp], 1.0e-12_dp)
   end subroutine test_nearfield_discharge_collapsed_layers

   subroutine check_nearfield_discharge(z_bottom, z_top, prescribed_discharge, expected)
      use m_source_sink, only: source_sinks, source_sink_all_discharges, FLOWCELL_SINK, FLOWCELL_SOURCE
      use m_flow, only: kmx, kbot, ktop, zws, vol1, qin
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi
      use m_transport, only: numconst
      use m_alloc, only: realloc

      real(kind=dp), intent(in) :: z_bottom, z_top, prescribed_discharge
      real(kind=dp), dimension(3), intent(in) :: expected

      real(kind=dp), dimension(9) :: discharge
      real(kind=dp), parameter :: tolerance = 1.0e-12_dp

      call source_sinks%reset()
      numconst = 0
      call source_sinks%initialize(2)
      source_sinks%num_total = 2
      source_sinks%num_nearfield = 1
      source_sinks%indices(1, FLOWCELL_SOURCE) = 2 ! Normal sources must not appear in nearfield output.
      source_sink_all_discharges(1, 1) = 100.0_dp
      source_sinks%indices(2, FLOWCELL_SINK) = 1
      source_sinks%indices(2, FLOWCELL_SOURCE) = 2
      source_sinks%z_bottom(2, :) = z_bottom
      source_sinks%z_top(2, :) = z_top
      source_sink_all_discharges(1, 2) = prescribed_discharge

      kmx = 3
      jampi = 0
      dts = 1.0_dp
      call realloc(kbot, 2, keepExisting=.false., fill=0)
      call realloc(ktop, 2, keepExisting=.false., fill=0)
      call realloc(zws, 9, keepExisting=.false., fill=0.0_dp)
      call realloc(vol1, 9, keepExisting=.false., fill=1000.0_dp)
      call realloc(qin, 9, keepExisting=.false., fill=0.0_dp)
      kbot = [3, 7]
      ktop = [5, 9]
      zws = [0.0_dp, -10.0_dp, -8.0_dp, -5.0_dp, 0.0_dp, -10.0_dp, -8.0_dp, -5.0_dp, 0.0_dp]

      call source_sinks%update_discharges()
      call source_sinks%get_nearfield_discharge(discharge)

      call f90_expect_near(discharge(3:5), -expected, tolerance, "sink discharge in the solver's selected layers")
      call f90_expect_near(discharge(7:9), expected, tolerance, "source discharge in the solver's selected layers")
      call f90_expect_near(discharge([1, 2, 6]), [0.0_dp, 0.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(discharge(3:5), qin(3:5), tolerance, "map sink discharge must match continuity")
      call f90_expect_near(discharge(7:9), qin(7:9) - 100.0_dp*[0.2_dp, 0.3_dp, 0.5_dp], tolerance, &
                           "map source discharge must exclude normal sources")
   end subroutine check_nearfield_discharge

end module test_sources_sinks
