module test_unstruc_inifields
   use assertions_gtest
   use fm_external_forcings_data, only: NTRANSFORMCOEF
   use m_file_helpers, only: create_file
   use MessageHandling, only: GetMessageCount, GetMessage_MH, LEVEL_WARN, SetMessageHandling
   use precision_basics, only: dp
   use properties, only: prop_file
   use timespace_parameters, only: OPERAND_OVERRIDE, OPERAND_ADD, OPERAND_MULTIPLY
   use tree_data_types, only: tree_data
   use tree_structures, only: tree_create, tree_destroy
   use unstruc_inifields, only: readIniFieldProvider

   implicit none

   character(len=*), parameter :: INI_FILENAME = 'test_data.ext'

   contains
   
!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_operand, test_read_operand,
   subroutine test_read_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      character(len=256) :: quantity
      character(len=256) :: filename
      character(len=256) :: varname
      real(kind=dp) :: transformcoef(NTRANSFORMCOEF)
      integer :: filetype
      integer :: method
      integer :: iloctype
      integer :: operand
      integer :: success
      integer :: istat

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
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      call readIniFieldProvider(INI_FILENAME, node_ptr, groupname, quantity, filename, filetype, method, iloctype, operand, transformcoef, success, varname)

      call f90_expect_eq(success, 1)
      call f90_expect_eq(operand, OPERAND_ADD)
      call f90_expect_eq(GetMessageCount(), 0)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_operand
!$f90tw)

!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_deprecated_operand, test_read_deprecated_operand,
   subroutine test_read_deprecated_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      character(len=256) :: quantity
      character(len=256) :: filename
      character(len=256) :: varname
      character(len=512) :: message
      real(kind=dp) :: transformcoef(NTRANSFORMCOEF)
      integer :: filetype
      integer :: method
      integer :: iloctype
      integer :: operand
      integer :: success
      integer :: istat
      integer :: log_level

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
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      call readIniFieldProvider(INI_FILENAME, node_ptr, groupname, quantity, filename, filetype, method, iloctype, operand, transformcoef, success, varname)

      call f90_expect_eq(success, 1)
      call f90_expect_eq(operand, OPERAND_MULTIPLY)
      call f90_expect_eq(GetMessageCount(), 1)

      log_level = GetMessage_MH(1, message)
      call f90_expect_eq(log_level, LEVEL_WARN)
      
      call F90_EXPECT_STREQ(trim(message)//c_null_char, "Wrong block in file 'test_data.ext': [parameter] for quantity=nudgeSalinityTemperature. Field 'operand' is set to " & 
         // "deprecated value '*'. Replace with 'override', 'overrideIfMissing', 'add', 'multiply', 'minimum' or 'maximum'."//c_null_char)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_deprecated_operand
!$f90tw)

!$f90tw TESTCODE(TEST, test_unstruc_inifields, test_read_unknown_operand, test_read_unknown_operand,
   subroutine test_read_unknown_operand() bind(C)
      type(tree_data), pointer :: inifield_ptr => null()
      type(tree_data), pointer :: node_ptr => null()

      character(len=256) :: groupname
      character(len=256) :: quantity
      character(len=256) :: filename
      character(len=256) :: varname
      character(len=512) :: message
      real(kind=dp) :: transformcoef(NTRANSFORMCOEF)
      integer :: filetype
      integer :: method
      integer :: iloctype
      integer :: operand
      integer :: success
      integer :: istat
      integer :: log_level

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
      call SetMessageHandling(write2screen=.false., useLog=.true., reset_counters=.true.)
      call readIniFieldProvider(INI_FILENAME, node_ptr, groupname, quantity, filename, filetype, method, iloctype, operand, transformcoef, success, varname)

      call f90_expect_eq(success, 0)
      call f90_expect_eq(GetMessageCount(), 1)

      log_level = GetMessage_MH(1, message)
      call f90_expect_eq(log_level, LEVEL_WARN)
      call f90_expect_true(index(message, "Field 'operand' has invalid value 'modus_operandi'") > 0)

      call tree_destroy(inifield_ptr)
   end subroutine test_read_unknown_operand
!$f90tw)

end module test_unstruc_inifields