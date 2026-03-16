module test_Readstructure
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal, equal

   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_read_weir_as_general_structure_missing_corrcoeff, test_read_weir_as_general_structure_missing_corrcoeff, test_read_weir_as_general_structure_missing_corrcoeff,
   !> Test that when a weir structure with a missing friction coefficient value (corrCoeff) is read out as a general structure, the correct default friction coefficient value is assigned to the general structure.
   subroutine test_read_weir_as_general_structure_missing_corrcoeff() bind(C)
      use m_forcinglist, only: t_forcingList
      use m_general_structure, only: t_GeneralStructure
      use m_readstructures, only: read_weir_as_general_structure
      use tree_structures, only: tree_data

      ! Local variables
      type(t_GeneralStructure), pointer :: general_structure
      type(tree_data), pointer :: input_pointer !< Pointer to the user input tree data
      character(len=256) :: structure_id
      type(t_forcingList) :: forcing_list
      logical :: success
      real(kind=dp) :: expected_cgf_pos

      ! Set up
      expected_cgf_pos = 1.0_dp
      structure_id = 'Structure01'
      call create_weir_structure_input_pointer_without_corrcoeff(input_pointer)

      ! Read weir structure input to general structure
      call read_weir_as_general_structure(general_structure, input_pointer, structure_id, forcing_list, success)

      ! Assert
      call f90_expect_eq(general_structure%cgf_pos, expected_cgf_pos, 'Default weir friction coefficient (`cgf_pos`) does not match expected value')

   end subroutine test_read_weir_as_general_structure_missing_corrcoeff
   !$f90tw)

   !> Helper subroutine to create a tree_data structure representing a weir structure input, where the corrCoeff value is missing.
   subroutine create_weir_structure_input_pointer_without_corrcoeff(input_pointer)
      use tree_structures, only: tree_data, tree_create, tree_create_node, tree_put_data

      ! Parameters
      type(tree_data), pointer, intent(out) :: input_pointer

      ! Local variables
      type(tree_data), pointer :: node_pointer
      character(1), dimension(1) :: node_value

      call tree_create('structure', input_pointer)

      call tree_create_node(input_pointer, 'type', node_pointer)
      call tree_put_data(node_pointer, transfer('weir', node_value), 'STRING')

      call tree_create_node(input_pointer, 'id', node_pointer)
      call tree_put_data(node_pointer, transfer('Structure01', node_value), 'STRING')

      call tree_create_node(input_pointer, 'locationFile', node_pointer)
      call tree_put_data(node_pointer, transfer('Structure01.pli', node_value), 'STRING')

      call tree_create_node(input_pointer, 'crestLevel', node_pointer)
      call tree_put_data(node_pointer, transfer('1', node_value), 'STRING')

      call tree_create_node(input_pointer, 'crestWidth', node_pointer)
      call tree_put_data(node_pointer, transfer('5', node_value), 'STRING')

   end subroutine create_weir_structure_input_pointer_without_corrcoeff

end module test_Readstructure
