module test_structure_bmi
   use assertions_gtest
   use m_structure_bmi
   implicit none(type, external)
   private

contains

   !$gtest
   !> The field enumerator must include only outputs available for each structure layout.
   subroutine structure_bmi_field_metadata() bind(C)
      character(len=24), dimension(6), parameter :: collections = [character(len=24) :: &
         'pumps', 'weirs', 'orifices', 'gates', 'generalstructures', 'culverts']
      integer, dimension(6), parameter :: counts = [4, 8, 9, 4, 9, 9]
      character(len=24) :: name
      integer :: i, j

      do i = 1, size(collections)
         call f90_expect_eq(structure_output_field_count(collections(i)), counts(i))
         do j = 1, counts(i)
            name = structure_output_field_name(collections(i), j)
            call f90_expect_true(len_trim(name) > 0)
            call f90_expect_true(is_structure_output_field(collections(i), name))
         end do
         call f90_expect_true(structure_output_field_name(collections(i), 0) == '')
         call f90_expect_true(structure_output_field_name(collections(i), counts(i) + 1) == '')
      end do

      call f90_expect_false(is_structure_output_field('pumps', 'flow_area'))
      call f90_expect_false(is_structure_output_field('pumps', 'velocity'))
      call f90_expect_true(is_structure_output_field('gates', 'opening_height'))
      call f90_expect_false(is_structure_output_field('gates', 'head_difference'))
      call f90_expect_false(is_structure_output_field('weirs', 'opening_height'))
      call f90_expect_true(is_structure_output_field('culverts', 'crest_level'))
      call f90_expect_true(is_structure_output_field('culverts', 'gate_lower_edge_level'))
      call f90_expect_false(is_structure_output_field('generalstructures', 'pressure_difference'))
      call f90_expect_eq(structure_output_field_count('observations'), 0)
   end subroutine structure_bmi_field_metadata

   !$gtest
   !> Hash membership must preserve every collection's supported fields and enumeration order.
   subroutine structure_bmi_named_fields() bind(C)
      call expect_fields('pumps', [character(len=24) :: &
         'discharge', 'water_level_up', 'water_level_down', 'head_difference'])
      call expect_fields('weirs', [character(len=24) :: &
         'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
         'head_difference', 'force_difference', 'water_level_at_crest'])
      call expect_fields('orifices', [character(len=24) :: &
         'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
         'head_difference', 'opening_height', 'force_difference', 'water_level_at_crest'])
      call expect_fields('gates', [character(len=24) :: &
         'discharge', 'water_level_up', 'water_level_down', 'opening_height'])
      call expect_fields('generalstructures', [character(len=24) :: &
         'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
         'head_difference', 'opening_height', 'force_difference', 'water_level_at_crest'])
      call expect_fields('culverts', [character(len=24) :: &
         'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
         'head_difference', 'opening_height', 'crest_level', 'gate_lower_edge_level'])

      call f90_expect_false(is_structure_output_field('', 'discharge'))
      call f90_expect_false(is_structure_output_field('pumps', ''))
      call f90_expect_false(is_structure_output_field('pumps', 'unknown_field'))
      call f90_expect_false(is_structure_output_field('observations', 'discharge'))
      call f90_expect_true(structure_output_field_name('observations', 1) == '')
   end subroutine structure_bmi_named_fields

   subroutine expect_fields(collection, expected)
      character(len=*), intent(in) :: collection
      character(len=*), dimension(:), intent(in) :: expected
      character(len=24), dimension(11), parameter :: all_fields = [character(len=24) :: &
         'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
         'head_difference', 'opening_height', 'force_difference', 'water_level_at_crest', &
         'crest_level', 'gate_lower_edge_level']
      integer :: i

      call f90_expect_eq(structure_output_field_count(collection), size(expected))
      do i = 1, size(expected)
         call f90_expect_true(structure_output_field_name(collection, i) == expected(i))
      end do
      do i = 1, size(all_fields)
         call f90_expect_true(is_structure_output_field(collection, all_fields(i)) .eqv. any(expected == all_fields(i)))
      end do
   end subroutine expect_fields
end module test_structure_bmi
