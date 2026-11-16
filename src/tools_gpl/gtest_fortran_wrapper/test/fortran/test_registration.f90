module wrapper_registration
   use, intrinsic :: iso_c_binding, only: c_int
   use assertions_gtest, only: f90_expect_true
   implicit none(type, external)
   private
   public :: default_name
   integer(c_int), bind(C) :: wrapper_registration_count = 0_c_int
contains
   !$gtest
   subroutine default_name() bind(C)
      wrapper_registration_count = wrapper_registration_count + 1_c_int
      call f90_expect_true(.true.)
   end subroutine default_name

end module wrapper_registration
