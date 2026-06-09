module test_construct_mask
   use assertions_gtest

   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_init_spatial_field, test_parse_location_type, test_parse_location_type,
   !> parse_location_type must map all recognized strings, empty to INVALID, and unknown to ALL.
   subroutine test_parse_location_type() bind(C)
      use m_construct_mask, only: parse_location_type
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL, ILATTP_INVALID

      call f90_expect_eq(parse_location_type('1d'), ILATTP_1D, "'1d' should map to ILATTP_1D")
      call f90_expect_eq(parse_location_type('2d'), ILATTP_2D, "'2d' should map to ILATTP_2D")
      call f90_expect_eq(parse_location_type('1d2d'), ILATTP_ALL, "'1d2d' should map to ILATTP_ALL")
      call f90_expect_eq(parse_location_type('all'), ILATTP_ALL, "'all' should map to ILATTP_ALL")
      call f90_expect_eq(parse_location_type(' '), ILATTP_INVALID, "empty string should default to ILATTP_INVALID")
      call f90_expect_eq(parse_location_type('bogus'), ILATTP_ALL, "unknown string should default to ILATTP_ALL")
   end subroutine test_parse_location_type
   !$f90tw)

end module test_construct_mask
