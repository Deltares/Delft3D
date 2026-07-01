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

!$f90tw TESTCODE(TEST, test_sources_sinks, test_polyline_file_missing, test_polyline_file_missing,
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
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_double_z_data_specification, test_double_z_data_specification,
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

      call create_file("my_polyline.pliz", [character(len=512) :: &
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
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_polyline_file_2_columns, test_xy_in_polyline_file_2_columns,
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

      call create_file("my_polyline.pliz", [character(len=512) :: &
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 3)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      call f90_assert_eq(size(y_coordinates), 3)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xy_in_polyline_file_2_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_3_columns, test_xyz_in_polyline_file_3_columns,
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

      call create_file("my_polyline.pliz", [character(len=512) :: &
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 3)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      call f90_assert_eq(size(y_coordinates), 3)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_3_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_4_columns, test_xyz_in_polyline_file_4_columns,
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

      call create_file("my_polyline.pliz", [character(len=512) :: &
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 3)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      call f90_assert_eq(size(y_coordinates), 3)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -6.0_dp)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), -10.0_dp)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_4_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_5_columns, test_xyz_in_polyline_file_5_columns,
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

      call create_file("my_polyline.pliz", [character(len=512) :: &
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 3)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 7.0_dp)
      call f90_expect_eq(x_coordinates(3), 17.0_dp)

      call f90_assert_eq(size(y_coordinates), 3)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 11.0_dp)
      call f90_expect_eq(y_coordinates(3), 9.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -6.0_dp)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -11.5_dp)
      call f90_expect_eq(z_range_sink(2), -10.0_dp)

      call tree_destroy(tree)
   end subroutine test_xyz_in_polyline_file_5_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_ext_file_single_z, test_xy_in_ext_file_single_z,
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 2)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 26.0_dp)

      call f90_assert_eq(size(y_coordinates), 2)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 6.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -13.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)

      call tree_destroy(tree)
   end subroutine test_xy_in_ext_file_single_z
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_ext_file_z_ranges, test_xy_in_ext_file_z_ranges,
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
      call f90_expect_true(success)

      call f90_assert_eq(size(x_coordinates), 2)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)
      call f90_expect_eq(x_coordinates(2), 26.0_dp)

      call f90_assert_eq(size(y_coordinates), 2)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)
      call f90_expect_eq(y_coordinates(2), 6.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -3.0_dp)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -10.0_dp)
      call f90_expect_eq(z_range_sink(2), -6.0_dp)

      call tree_destroy(tree)
   end subroutine test_xy_in_ext_file_z_ranges
!$f90tw)

end module test_sources_sinks