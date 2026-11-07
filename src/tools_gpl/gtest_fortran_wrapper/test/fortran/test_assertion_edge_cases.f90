module wrapper_assertion_edge_cases
   use, intrinsic :: iso_c_binding
   use assertions_gtest
   implicit none(type, external)
   private
   public :: passing_string_edge_cases, failing_array_edge_cases, failing_message_edge_cases
contains
   subroutine passing_string_edge_cases() bind(C)
      character(len=8) :: padded
      padded = 'padded'
      call f90_expect_streq('', '')
      call f90_expect_streq('plain', 'plain')
      call f90_expect_streq(padded, 'padded  ')
      call f90_expect_streq(trim(padded), 'padded')
      call f90_expect_streq('same'//c_null_char//'ignored', 'same')
      call f90_expect_streq(c_null_char, '')
      call f90_expect_strne('same', 'same ')
      call f90_expect_strcaseeq('MiXeD', 'mixed')
      call f90_expect_strcasene('different', 'MIXED')
   end subroutine passing_string_edge_cases

   subroutine failing_array_edge_cases() bind(C)
      call f90_expect_near([1.0_c_float, 2.0_c_float], [1.0_c_float, 4.0_c_float], &
                           0.1_c_float, 'vector')
      call f90_expect_double_eq(reshape([1.0_c_double, 2.0_c_double], [1, 2]), &
                                reshape([1.0_c_double, 4.0_c_double], [1, 2]), 'matrix')
      call f90_expect_true([.true., .false.], 'logical vector')
      call f90_expect_false(reshape([.false._c_bool, .true._c_bool], [1, 2]), 'logical matrix')
      call f90_expect_near([1.0_c_double], [1.0_c_double, 2.0_c_double], 0.1_c_double, 'bad vector')
      call f90_expect_double_eq(reshape([1.0_c_double, 2.0_c_double], [1, 2]), &
                                reshape([1.0_c_double, 2.0_c_double], [2, 1]), 'bad matrix')
   end subroutine failing_array_edge_cases

   subroutine failing_message_edge_cases() bind(C)
      call f90_expect_eq(1_c_int32_t, 2_c_int32_t, repeat('x', 4096)//':end'//c_null_char)
      call f90_expect_streq('left', 'right', 'visible'//c_null_char//'not-visible')
      call f90_expect_false(.true., '')
   end subroutine failing_message_edge_cases
end module wrapper_assertion_edge_cases
