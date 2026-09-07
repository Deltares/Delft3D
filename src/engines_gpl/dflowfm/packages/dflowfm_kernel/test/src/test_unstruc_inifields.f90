module test_unstruc_inifields
   use assertions_gtest
   use m_file_helpers, only: create_file
   use MessageHandling, only: GetMessageCount, GetMessage_MH, LEVEL_WARN, SetMessageHandling
   use m_spatial_field, only: t_spatial_field_input, read_spatial_field_block, validate_spatial_field_input
   use properties, only: prop_file
   use timespace_parameters, only: OPERAND_OVERRIDE, OPERAND_ADD, OPERAND_MULTIPLY
   use tree_data_types, only: tree_data
   use tree_structures, only: tree_create, tree_destroy, tree_get_name

   implicit none

   character(len=*), parameter :: INI_FILENAME = 'test_data.ext'

   contains
   
!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_operand, test_read_operand,
   subroutine test_read_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      integer :: istat
      logical :: success
      type(t_spatial_field_input) :: input

      call create_file(INI_FILENAME, [ &
                       '[General]', &
                       'fileVersion         = 2.02', &
                       'fileType            = iniField', &
                       '', &
                       '[Parameter]', &
                       'quantity            = nudgeSalinityTemperature', &
                       'dataFile            = does_not_exist.nc', &
                       'dataFileType        = netcdf', &
                       'interpolationMethod = linearSpaceTime', &
                       'operand             = add'])

      call tree_create(INI_FILENAME, inifield_ptr)
      call prop_file('ini', INI_FILENAME, inifield_ptr, istat)
      call f90_expect_eq(istat, 0)

      node_ptr => inifield_ptr%child_nodes(2)%node_ptr
      groupname = trim(tree_get_name(node_ptr))
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      input = read_spatial_field_block(node_ptr)
      success = validate_spatial_field_input(input, INI_FILENAME, groupname, '')

      call f90_expect_true(success)
      call f90_expect_eq(input%oper, OPERAND_ADD)
      call f90_expect_eq(GetMessageCount(), 0)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_operand
!$f90tw)

!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_deprecated_operand, test_read_deprecated_operand,
   subroutine test_read_deprecated_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      character(len=512) :: message
      integer :: istat
      integer :: log_level
      logical :: success
      type(t_spatial_field_input) :: input

      call create_file(INI_FILENAME, [ &
                       '[General]', &
                       'fileVersion         = 2.02', &
                       'fileType            = iniField', &
                       '', &
                       '[Parameter]', &
                       'quantity            = nudgeSalinityTemperature', &
                       'dataFile            = does_not_exist.nc', &
                       'dataFileType        = netcdf', &
                       'interpolationMethod = linearSpaceTime', &
                       'operand             = *'])

      call tree_create(INI_FILENAME, inifield_ptr)
      call prop_file('ini', INI_FILENAME, inifield_ptr, istat)
      call f90_expect_eq(istat, 0)

      node_ptr => inifield_ptr%child_nodes(2)%node_ptr
      groupname = trim(tree_get_name(node_ptr))
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      input = read_spatial_field_block(node_ptr)
      success = validate_spatial_field_input(input, INI_FILENAME, groupname, '')

      call f90_expect_true(success)
      call f90_expect_eq(input%oper, OPERAND_MULTIPLY)
      call f90_expect_eq(GetMessageCount(), 1)

      log_level = GetMessage_MH(1, message)
      call f90_expect_eq(log_level, LEVEL_WARN)
      call f90_expect_true(index(message, "operand value '*'") > 0)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_deprecated_operand
!$f90tw)

!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_unknown_operand, test_read_unknown_operand,
   subroutine test_read_unknown_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      character(len=512) :: message
      integer :: istat
      integer :: log_level
      logical :: success
      type(t_spatial_field_input) :: input

      call create_file(INI_FILENAME, [ &
                       '[General]', &
                       'fileVersion         = 2.02', &
                       'fileType            = iniField', &
                       '', &
                       '[Parameter]', &
                       'quantity            = nudgeSalinityTemperature', &
                       'dataFile            = does_not_exist.nc', &
                       'dataFileType        = netcdf', &
                       'interpolationMethod = linearSpaceTime', &
                       'operand             = modus_operandi'])

      call tree_create(INI_FILENAME, inifield_ptr)
      call prop_file('ini', INI_FILENAME, inifield_ptr, istat)
      call f90_expect_eq(istat, 0)

      node_ptr => inifield_ptr%child_nodes(2)%node_ptr
      groupname = trim(tree_get_name(node_ptr))
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      input = read_spatial_field_block(node_ptr)
      success = validate_spatial_field_input(input, INI_FILENAME, groupname, '')

      call f90_expect_false(success)
      call f90_expect_eq(GetMessageCount(), 1)

      log_level = GetMessage_MH(1, message)
      call f90_expect_true(log_level /= LEVEL_WARN)
      call f90_expect_true(index(message, "Unknown operand 'modus_operandi'") > 0)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_unknown_operand
!$f90tw)

end module test_unstruc_inifields