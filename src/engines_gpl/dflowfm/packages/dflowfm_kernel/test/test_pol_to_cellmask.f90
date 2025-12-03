module test_pol_to_cellmask
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_pol_to_cellmask, test_layer_height_water_level_consistency,
   subroutine test_mixed_polygon() bind(C)
      
      call pol_to_cellmask()
      ! Check results
      call f90_expect_near(mask, expected_mask, tolerance, "mask does not match expected value")
      
   end subroutine test_mixed_polygon
   !$f90tw)
end module test_pol_to_cellmask
