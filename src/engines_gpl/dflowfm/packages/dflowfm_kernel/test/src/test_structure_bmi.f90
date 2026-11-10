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
end module test_structure_bmi
