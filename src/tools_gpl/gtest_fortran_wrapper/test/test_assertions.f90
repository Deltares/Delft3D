module wrapper_assertion_tests
   use, intrinsic :: iso_c_binding
   use assertions_gtest
   implicit none(type, external)
   private

   public :: passing_status_assertions, guarded_assertions, passing_status_after_failure, assertion_continued

   integer(c_int), bind(C, name="assertion_continued") :: assertion_continued = 0

contains

   subroutine passing_status_assertions() bind(C)
      call f90_expect_true(f90_assert_eq(7_c_int32_t, 7_c_int32_t))
      call f90_expect_true(f90_assert_true(.true.))
      call f90_expect_true(f90_assert_true(.true._c_bool, "C logical"))
      call f90_expect_true(f90_assert_true(-2_c_int32_t, "nonzero integer"))
   end subroutine passing_status_assertions

   subroutine guarded_assertions(scenario) bind(C)
      integer(c_int), value, intent(in) :: scenario

      assertion_continued = 0
      select case (scenario)
      case (0)
         if (.not. f90_assert_eq(7_c_int32_t, 8_c_int32_t, "integer prerequisite")) then
            return
         end if
      case (1)
         if (.not. f90_assert_true(.false., "logical prerequisite")) then
            return
         end if
      case (2)
         if (.not. f90_assert_true(.false._c_bool, "C logical prerequisite")) then
            return
         end if
      case (3)
         if (.not. f90_assert_true(0_c_int32_t, "integer condition")) then
            return
         end if
      end select
      assertion_continued = 1
   end subroutine guarded_assertions

   subroutine passing_status_after_failure() bind(C)
      assertion_continued = 0
      call f90_expect_false(f90_assert_true(.false., "earlier fatal failure"))
      if (.not. f90_assert_eq(1_c_int32_t, 1_c_int32_t)) then
         return
      end if
      if (.not. f90_assert_true(.true.)) then
         return
      end if
      assertion_continued = 1
   end subroutine passing_status_after_failure

end module wrapper_assertion_tests
