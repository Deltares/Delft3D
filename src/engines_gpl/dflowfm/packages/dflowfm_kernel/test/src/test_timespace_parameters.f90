module test_timespace_parameters
   use assertions_gtest
   use timespace_parameters

   implicit none(type, external)

contains

!$f90tw TESTCODE(TEST, test_timespace_parameters, test_convert_operand_string_to_integer_known_values, test_convert_operand_string_to_integer_known_values,
   subroutine test_convert_operand_string_to_integer_known_values() bind(C)
      call f90_expect_eq(convert_operand_string_to_integer('override'), OPERAND_OVERRIDE)
      call f90_expect_eq(convert_operand_string_to_integer('overrideIfMissing'), OPERAND_OVERRIDE_IF_MISSING)
      call f90_expect_eq(convert_operand_string_to_integer('Add'), OPERAND_ADD)
      call f90_expect_eq(convert_operand_string_to_integer('MULTIPLY'), OPERAND_MULTIPLY)
      call f90_expect_eq(convert_operand_string_to_integer('minimum'), OPERAND_MINIMUM)
      call f90_expect_eq(convert_operand_string_to_integer('MaXiMuM'), OPERAND_MAXIMUM)
   end subroutine test_convert_operand_string_to_integer_known_values
!$f90tw)

!$f90tw TESTCODE(TEST, test_timespace_parameters, test_convert_operand_string_to_integer_unknown_value, test_convert_operand_string_to_integer_unknown_value,
   subroutine test_convert_operand_string_to_integer_unknown_value() bind(C)
      call f90_expect_eq(convert_operand_string_to_integer('does_not_exist'), OPERAND_UNKNOWN)
   end subroutine test_convert_operand_string_to_integer_unknown_value
!$f90tw)

!$f90tw TESTCODE(TEST, test_timespace_parameters, test_convert_legacy_operand_string_to_integer_known_values, test_convert_legacy_operand_string_to_integer_known_values,
   subroutine test_convert_legacy_operand_string_to_integer_known_values() bind(C)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('o'), OPERAND_OVERRIDE)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('A'), OPERAND_OVERRIDE_IF_MISSING)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('+'), OPERAND_ADD)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('*'), OPERAND_MULTIPLY)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('v'), OPERAND_OVERRIDE)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('N'), OPERAND_MINIMUM)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('x'), OPERAND_MAXIMUM)
   end subroutine test_convert_legacy_operand_string_to_integer_known_values
!$f90tw)

!$f90tw TESTCODE(TEST, test_timespace_parameters, test_convert_legacy_operand_string_to_integer_unknown_value, test_convert_legacy_operand_string_to_integer_unknown_value,
   subroutine test_convert_legacy_operand_string_to_integer_unknown_value() bind(C)
      call f90_expect_eq(convert_legacy_operand_string_to_integer('does_not_exist'), OPERAND_UNKNOWN)
   end subroutine test_convert_legacy_operand_string_to_integer_unknown_value
!$f90tw)

end module test_timespace_parameters
