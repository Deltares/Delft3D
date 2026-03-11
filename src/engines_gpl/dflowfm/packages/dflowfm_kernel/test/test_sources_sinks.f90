module test_sources_sinks
   use assertions_gtest
   use fm_external_forcings, only: sourcesink_parse_coordinates
   use tree_structures, only: tree_data, tree_create
   use properties, only: prop_inifile
   use MessageHandling, only: popLastMessage, LEVEL_ERROR, resetMessageCount_MH
   use precision_basics, only: dp
   use m_missing, only: dmiss

   implicit none

   character(len=*), parameter :: EXT_FILENAME = "sourcesinks.ext"

contains

   ! Create a file with the specified name and content. If the file already exists it will be replaced.
   subroutine create_file(file_name, lines)
      implicit none
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: lines(:)
      integer :: error_code, i, file_lun
      open (newunit=file_lun, file=file_name, status='replace', action='write', &
            form='formatted', iostat=error_code)
      if (error_code /= 0) then
         error stop "Failed to open file "//trim(file_name)
      end if
      do i = 1, size(lines)
         write (file_lun, '(A)', iostat=error_code) trim(lines(i))
         if (error_code /= 0) then
            close (file_lun)
            error stop "Failed to write to file "//trim(file_name)
         end if
      end do
      close (file_lun)
   end subroutine create_file

!$f90tw TESTCODE(TEST, test_sources_sinks, test_polyline_file_missing, test_polyline_file_missing,
   subroutine test_polyline_file_missing() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      integer :: error_level
      character(len=256) :: error_message

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = this_file_does_not_exist.pliz", &
                       "zSource        = -7.5", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      call resetMessageCount_MH()
      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_false(success)
      
      call popLastMessage(error_level, error_message)
      call f90_expect_eq(error_level, LEVEL_ERROR)
      call f90_expect_true(error_message == "Error in source sink initialization, failed to read polyline file 'this_file_does_not_exist.pliz'")

   end subroutine test_polyline_file_missing
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_double_z_data_specification, test_double_z_data_specification,
   subroutine test_double_z_data_specification() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      integer :: error_level
      character(len=256) :: error_message

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 3", &
                       "25.0 5.0 -11.5", &
                       "7.0 11.0 0.0", &
                       "17.0 9.0 -7.5"])

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = my_polyline.pliz", &
                       "zSource        = -7.5", &
                       "zSink          = -11.5", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      call resetMessageCount_MH()
      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_false(success)

      call popLastMessage(error_level, error_message)
      call f90_expect_eq(error_level, LEVEL_ERROR)
      call f90_expect_true(error_message == "Error in source sink initialization, source/sink z information cannot be specified bothin the ext file and in the polyline file. Make sure the polyline file only contains x and y columns")
   end subroutine test_double_z_data_specification
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_polyline_file_2_columns, test_xy_in_polyline_file_2_columns,
   subroutine test_xy_in_polyline_file_2_columns() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 2", &
                       "25.0 5.0", &
                       "7.0 11.0", &
                       "17.0 9.0"])

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = my_polyline.pliz", &
                       "zSource        = -7.5", &
                       "zSink          = -11.5", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

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
   end subroutine test_xy_in_polyline_file_2_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_3_columns, test_xyz_in_polyline_file_3_columns,
   subroutine test_xyz_in_polyline_file_3_columns() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 3", &
                       "25.0 5.0 -11.5", &
                       "7.0 11.0 0.0", &
                       "17.0 9.0 -7.5"])

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = my_polyline.pliz", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

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

   end subroutine test_xyz_in_polyline_file_3_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_4_columns, test_xyz_in_polyline_file_4_columns,
   subroutine test_xyz_in_polyline_file_4_columns() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 4", &
                       "25.0 5.0 -11.5 -10.0", &
                       "7.0 11.0 0.0 0.0", &
                       "17.0 9.0 -7.5 -6.0"])

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = my_polyline.pliz", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

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

   end subroutine test_xyz_in_polyline_file_4_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xyz_in_polyline_file_5_columns, test_xyz_in_polyline_file_5_columns,
   subroutine test_xyz_in_polyline_file_5_columns() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file("my_polyline.pliz", [ &
                       "L1", &
                       "3 4", &
                       "25.0 5.0 -11.5 -10.0 0", &
                       "7.0 11.0 0.0 0.0 0", &
                       "17.0 9.0 -7.5 -6.0 0"])

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "locationFile   = my_polyline.pliz", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

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

   end subroutine test_xyz_in_polyline_file_5_columns
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_ext_file_single_z, test_xy_in_ext_file_single_z,
   subroutine test_xy_in_ext_file_single_z() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "numCoordinates = 1", &
                       "xCoordinates   = 25.0", &
                       "yCoordinates   = 5.0", &
                       "zSource        = -7.5", &
                       "zSink          = -13.5", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

      call f90_assert_eq(size(x_coordinates), 1)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)

      call f90_assert_eq(size(y_coordinates), 1)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), dmiss)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -13.5_dp)
      call f90_expect_eq(z_range_sink(2), dmiss)
   end subroutine test_xy_in_ext_file_single_z
!$f90tw)

!$f90tw TESTCODE(TEST, test_sources_sinks, test_xy_in_ext_file_z_ranges, test_xy_in_ext_file_z_ranges,
   subroutine test_xy_in_ext_file_z_ranges() bind(C)
      character(len=256) :: base_dir
      character(len=256) :: file_name
      character(len=256) :: group_name

      real(kind=dp), dimension(:), allocatable :: x_coordinates
      real(kind=dp), dimension(:), allocatable :: y_coordinates
      integer, parameter :: num_range_points = 2
      real(kind=dp), dimension(num_range_points) :: z_range_source
      real(kind=dp), dimension(num_range_points) :: z_range_sink

      logical :: success
      integer :: error_code

      type(tree_data), pointer :: tree

      call create_file(EXT_FILENAME, [ &
                       "[SourceSink]", &
                       "id             = left", &
                       "name           = cool_name", &
                       "numCoordinates = 1", &
                       "xCoordinates   = 25.0", &
                       "yCoordinates   = 5.0", &
                       "zSource        = -7.5 -3.0", &
                       "zSink          = -10.0 -6.0", &
                       "Area           = 0.0", &
                       "discharge      = left.bc", &
                       "salinityDelta  = left.bc"])

      call tree_create(file_name, tree)
      call prop_inifile(EXT_FILENAME, tree, error_code)
      call f90_assert_eq(error_code, 0, "Failed to read ini file")

      success = sourcesink_parse_coordinates(tree%child_nodes(1)%node_ptr, base_dir, file_name, group_name, x_coordinates, y_coordinates, z_range_source, z_range_sink)
      call f90_assert_true(success)

      call f90_assert_eq(size(x_coordinates), 1)
      call f90_expect_eq(x_coordinates(1), 25.0_dp)

      call f90_assert_eq(size(y_coordinates), 1)
      call f90_expect_eq(y_coordinates(1), 5.0_dp)

      call f90_assert_eq(size(z_range_source), 2)
      call f90_expect_eq(z_range_source(1), -7.5_dp)
      call f90_expect_eq(z_range_source(2), -3.0_dp)

      call f90_assert_eq(size(z_range_sink), 2)
      call f90_expect_eq(z_range_sink(1), -10.0_dp)
      call f90_expect_eq(z_range_sink(2), -6.0_dp)
   end subroutine test_xy_in_ext_file_z_ranges
!$f90tw)

end module test_sources_sinks

