module wrapper_assertion_tests
   use, intrinsic :: iso_c_binding
   use assertions_gtest
   implicit none(type, external)
   private

   public :: passing_assertions, failing_assertions, assertion_continued
   public :: passing_status_assertions, guarded_assertions, passing_status_after_failure, assertion_cleanup_completed

   integer(c_int), bind(C, name="assertion_continued") :: assertion_continued = 0
   integer(c_int), bind(C, name="assertion_cleanup_completed") :: assertion_cleanup_completed = 0

contains

   subroutine passing_status_assertions() bind(C)
      call f90_expect_true(f90_assert_eq(7_c_int32_t, 7_c_int32_t))
      call f90_expect_true(f90_assert_true(.true.))
      call f90_expect_true(f90_assert_true(.true._c_bool, "C logical"))
      call f90_expect_true(f90_assert_true(-2_c_int32_t, "nonzero integer"))
   end subroutine passing_status_assertions

   subroutine guarded_assertions(scenario) bind(C)
      integer(c_int), value, intent(in) :: scenario
      logical :: ready

      assertion_continued = 0
      assertion_cleanup_completed = 0
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
      case (4)
         call guarded_helper(ready)
         if (.not. ready) then
            return
         end if
      end select
      assertion_continued = 1
   end subroutine guarded_assertions

   subroutine guarded_helper(ready)
      logical, intent(out) :: ready
      integer, dimension(:), allocatable :: buffer

      ready = .false.
      allocate (buffer(1))
      if (.not. f90_assert_true(.false., "helper prerequisite")) then
         deallocate (buffer)
         assertion_cleanup_completed = 1
         return
      end if
      ready = .true.
   end subroutine guarded_helper

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

   subroutine passing_assertions() bind(C)
      call f90_expect_eq(7_c_int32_t, 7_c_int32_t)
      call f90_expect_eq(1.0_c_float, 1.0_c_float)
      call f90_expect_eq(1.0_c_double, 1.0_c_double)
      call f90_expect_eq(.true., .true.)
      call f90_expect_eq(.false._c_bool, .false._c_bool)
      call f90_expect_true(.true.)
      call f90_expect_true(.true._c_bool)
      call f90_expect_false(.false.)
      call f90_expect_false(.false._c_bool)
      call f90_expect_near(1.0_c_float, 1.01_c_float, 0.1_c_float)
      call f90_expect_near(1.0_c_double, 1.01_c_double, 0.1_c_double)
      call f90_expect_double_eq(1.0_c_double, 1.0_c_double)
      call f90_expect_gt(3_c_int32_t, 2_c_int32_t)
      call f90_expect_gt(3.0_c_float, 2.0_c_float)
      call f90_expect_gt(3.0_c_double, 2.0_c_double)
      call f90_expect_streq('same', 'same')
      call f90_expect_near([1.0_c_float, 2.0_c_float], [1.01_c_float, 2.01_c_float], 0.1_c_float)
      call f90_expect_true([.true., .true.])
      call f90_expect_false([.false._c_bool, .false._c_bool])
      call f90_expect_double_eq([1.0_c_double, 2.0_c_double], [1.0_c_double, 2.0_c_double])
   end subroutine passing_assertions

   subroutine failing_assertions() bind(C)
      assertion_continued = 0
      call f90_expect_eq(7_c_int32_t, 8_c_int32_t, 'expect_eq')
      call f90_expect_true(.false., 'expect_true')
      call f90_expect_false(.true._c_bool, 'expect_false')
      call f90_expect_near(1.0_c_float, 3.0_c_float, 0.1_c_float, 'expect_near')
      call f90_expect_streq('left', 'right', 'expect_streq')
      call f90_expect_gt(1.0_c_double, 3.0_c_double, 'expect_gt')
      call f90_expect_double_eq(1.0_c_double, 3.0_c_double, 'expect_double_eq')
      assertion_continued = 1
   end subroutine failing_assertions

end module wrapper_assertion_tests
