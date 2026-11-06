!> GoogleTest checks and status-returning assertions for explicit Fortran control flow.
module assertions_gtest
   use, intrinsic :: iso_c_binding, only: gtest_wrapper_bool_t => c_int8_t, c_int32_t, c_float, c_double, c_bool, &
                                         c_char, c_size_t, c_null_char
   implicit none(type, external)
   private

   !> Record fatal integer equality failures and return whether this comparison passed.
   public :: f90_assert_eq
   interface f90_assert_eq
      module procedure assert_eq_i32
   end interface f90_assert_eq

   !> Record fatal scalar condition failures and return whether this condition is true.
   public :: f90_assert_true
   interface f90_assert_true
      module procedure assert_true_i32
      module procedure assert_true_logical
      module procedure assert_true_bool
   end interface f90_assert_true

   !> GoogleTest EXPECT_EQ, with an optional Fortran message.
   public :: f90_expect_eq
   interface f90_expect_eq
      module procedure expect_eq_i32_rank0
      module procedure expect_eq_f32_rank0
      module procedure expect_eq_f64_rank0
      module procedure expect_eq_logical_rank0
      module procedure expect_eq_bool_rank0
   end interface f90_expect_eq

   !> GoogleTest EXPECT_NE, with an optional Fortran message.
   public :: f90_expect_ne
   interface f90_expect_ne
      module procedure expect_ne_i32_rank0
      module procedure expect_ne_f32_rank0
      module procedure expect_ne_f64_rank0
      module procedure expect_ne_logical_rank0
      module procedure expect_ne_bool_rank0
   end interface f90_expect_ne

   !> GoogleTest EXPECT_LT, with an optional Fortran message.
   public :: f90_expect_lt
   interface f90_expect_lt
      module procedure expect_lt_i32_rank0
      module procedure expect_lt_f32_rank0
      module procedure expect_lt_f64_rank0
   end interface f90_expect_lt

   !> GoogleTest EXPECT_LE, with an optional Fortran message.
   public :: f90_expect_le
   interface f90_expect_le
      module procedure expect_le_i32_rank0
      module procedure expect_le_f32_rank0
      module procedure expect_le_f64_rank0
   end interface f90_expect_le

   !> GoogleTest EXPECT_GT, with an optional Fortran message.
   public :: f90_expect_gt
   interface f90_expect_gt
      module procedure expect_gt_i32_rank0
      module procedure expect_gt_f32_rank0
      module procedure expect_gt_f64_rank0
   end interface f90_expect_gt

   !> GoogleTest EXPECT_GE, with an optional Fortran message.
   public :: f90_expect_ge
   interface f90_expect_ge
      module procedure expect_ge_i32_rank0
      module procedure expect_ge_f32_rank0
      module procedure expect_ge_f64_rank0
   end interface f90_expect_ge

   !> GoogleTest EXPECT_FLOAT_EQ, with an optional Fortran message.
   public :: f90_expect_float_eq
   interface f90_expect_float_eq
      module procedure expect_float_eq_f32_rank0
      module procedure expect_float_eq_f32_rank1
      module procedure expect_float_eq_f32_rank2
   end interface f90_expect_float_eq

   !> GoogleTest EXPECT_DOUBLE_EQ, with an optional Fortran message.
   public :: f90_expect_double_eq
   interface f90_expect_double_eq
      module procedure expect_double_eq_f64_rank0
      module procedure expect_double_eq_f64_rank1
      module procedure expect_double_eq_f64_rank2
   end interface f90_expect_double_eq

   !> GoogleTest EXPECT_NEAR, with an optional Fortran message.
   public :: f90_expect_near
   interface f90_expect_near
      module procedure expect_near_f32_rank0
      module procedure expect_near_f32_rank1
      module procedure expect_near_f32_rank2
      module procedure expect_near_f64_rank0
      module procedure expect_near_f64_rank1
      module procedure expect_near_f64_rank2
   end interface f90_expect_near

   !> GoogleTest EXPECT_TRUE, with an optional Fortran message.
   public :: f90_expect_true
   interface f90_expect_true
      module procedure expect_true_i32_rank0
      module procedure expect_true_i32_rank1
      module procedure expect_true_i32_rank2
      module procedure expect_true_logical_rank0
      module procedure expect_true_logical_rank1
      module procedure expect_true_logical_rank2
      module procedure expect_true_bool_rank0
      module procedure expect_true_bool_rank1
      module procedure expect_true_bool_rank2
   end interface f90_expect_true

   !> GoogleTest EXPECT_FALSE, with an optional Fortran message.
   public :: f90_expect_false
   interface f90_expect_false
      module procedure expect_false_i32_rank0
      module procedure expect_false_i32_rank1
      module procedure expect_false_i32_rank2
      module procedure expect_false_logical_rank0
      module procedure expect_false_logical_rank1
      module procedure expect_false_logical_rank2
      module procedure expect_false_bool_rank0
      module procedure expect_false_bool_rank1
      module procedure expect_false_bool_rank2
   end interface f90_expect_false

   !> GoogleTest EXPECT_STREQ, with an optional Fortran message.
   public :: f90_expect_streq
   interface f90_expect_streq
      module procedure expect_streq_string_rank0
   end interface f90_expect_streq

   !> GoogleTest EXPECT_STRNE, with an optional Fortran message.
   public :: f90_expect_strne
   interface f90_expect_strne
      module procedure expect_strne_string_rank0
   end interface f90_expect_strne

   !> GoogleTest EXPECT_STRCASEEQ, with an optional Fortran message.
   public :: f90_expect_strcaseeq
   interface f90_expect_strcaseeq
      module procedure expect_strcaseeq_string_rank0
   end interface f90_expect_strcaseeq

   !> GoogleTest EXPECT_STRCASENE, with an optional Fortran message.
   public :: f90_expect_strcasene
   interface f90_expect_strcasene
      module procedure expect_strcasene_string_rank0
   end interface f90_expect_strcasene

   interface
      subroutine gtest_wrapper_eq_int32(val1, val2, fatal, message, message_length) &
         bind(C, name="gtest_wrapper_eq_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         integer(gtest_wrapper_bool_t), value, intent(in) :: fatal
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_eq_int32
      subroutine gtest_wrapper_eq_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_eq_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_eq_float
      subroutine gtest_wrapper_eq_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_eq_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_eq_double
      subroutine gtest_wrapper_eq_bool(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_eq_bool")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(gtest_wrapper_bool_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_eq_bool
      subroutine gtest_wrapper_ne_int32(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ne_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ne_int32
      subroutine gtest_wrapper_ne_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ne_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ne_float
      subroutine gtest_wrapper_ne_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ne_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ne_double
      subroutine gtest_wrapper_ne_bool(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ne_bool")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(gtest_wrapper_bool_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ne_bool
      subroutine gtest_wrapper_lt_int32(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_lt_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_lt_int32
      subroutine gtest_wrapper_lt_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_lt_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_lt_float
      subroutine gtest_wrapper_lt_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_lt_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_lt_double
      subroutine gtest_wrapper_le_int32(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_le_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_le_int32
      subroutine gtest_wrapper_le_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_le_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_le_float
      subroutine gtest_wrapper_le_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_le_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_le_double
      subroutine gtest_wrapper_gt_int32(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_gt_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_gt_int32
      subroutine gtest_wrapper_gt_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_gt_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_gt_float
      subroutine gtest_wrapper_gt_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_gt_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_gt_double
      subroutine gtest_wrapper_ge_int32(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ge_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ge_int32
      subroutine gtest_wrapper_ge_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ge_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ge_float
      subroutine gtest_wrapper_ge_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_ge_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_ge_double
      subroutine gtest_wrapper_float_eq_float(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_float_eq_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_float_eq_float
      subroutine gtest_wrapper_double_eq_double(val1, val2, message, message_length) &
         bind(C, name="gtest_wrapper_double_eq_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_double_eq_double
      subroutine gtest_wrapper_near_float(val1, val2, abs_error, message, message_length) &
         bind(C, name="gtest_wrapper_near_float")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_float), value, intent(in) :: val1, val2, abs_error
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_near_float
      subroutine gtest_wrapper_near_double(val1, val2, abs_error, message, message_length) &
         bind(C, name="gtest_wrapper_near_double")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         real(c_double), value, intent(in) :: val1, val2, abs_error
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_near_double
      subroutine gtest_wrapper_true_int32(condition, fatal, message, message_length) &
         bind(C, name="gtest_wrapper_true_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: condition
         integer(gtest_wrapper_bool_t), value, intent(in) :: fatal
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_true_int32
      subroutine gtest_wrapper_true_bool(condition, fatal, message, message_length) &
         bind(C, name="gtest_wrapper_true_bool")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(gtest_wrapper_bool_t), value, intent(in) :: condition
         integer(gtest_wrapper_bool_t), value, intent(in) :: fatal
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_true_bool
      subroutine gtest_wrapper_false_int32(condition, message, message_length) &
         bind(C, name="gtest_wrapper_false_int32")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(c_int32_t), value, intent(in) :: condition
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_false_int32
      subroutine gtest_wrapper_false_bool(condition, message, message_length) &
         bind(C, name="gtest_wrapper_false_bool")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         integer(gtest_wrapper_bool_t), value, intent(in) :: condition
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_false_bool
      subroutine gtest_wrapper_streq_string(str1, length1, str2, length2, message, message_length) &
         bind(C, name="gtest_wrapper_streq_string")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         character(kind=c_char), dimension(*), intent(in) :: str1, str2
         integer(c_size_t), value, intent(in) :: length1, length2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_streq_string
      subroutine gtest_wrapper_strne_string(str1, length1, str2, length2, message, message_length) &
         bind(C, name="gtest_wrapper_strne_string")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         character(kind=c_char), dimension(*), intent(in) :: str1, str2
         integer(c_size_t), value, intent(in) :: length1, length2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_strne_string
      subroutine gtest_wrapper_strcaseeq_string(str1, length1, str2, length2, message, message_length) &
         bind(C, name="gtest_wrapper_strcaseeq_string")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         character(kind=c_char), dimension(*), intent(in) :: str1, str2
         integer(c_size_t), value, intent(in) :: length1, length2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_strcaseeq_string
      subroutine gtest_wrapper_strcasene_string(str1, length1, str2, length2, message, message_length) &
         bind(C, name="gtest_wrapper_strcasene_string")
         import :: gtest_wrapper_bool_t, c_int32_t, c_float, c_double, c_char, c_size_t
         character(kind=c_char), dimension(*), intent(in) :: str1, str2
         integer(c_size_t), value, intent(in) :: length1, length2
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_strcasene_string
      subroutine gtest_wrapper_failure(message, message_length) bind(C, name="gtest_wrapper_failure")
         import :: gtest_wrapper_bool_t, c_char, c_size_t
         character(kind=c_char), dimension(*), intent(in) :: message
         integer(c_size_t), value, intent(in) :: message_length
      end subroutine gtest_wrapper_failure
   end interface

contains

   !> Check an integer prerequisite; callers must explicitly return on failure.
   logical function assert_eq_i32(val1, val2, message) result(passed)
      integer(c_int32_t), intent(in) :: val1, val2 !< Values to compare.
      character(len=*), optional, intent(in) :: message !< Failure context.
      character(len=:), allocatable :: text

      passed = val1 == val2
      text = format_error_message(message)
      call gtest_wrapper_eq_int32(val1, val2, 1_gtest_wrapper_bool_t, text, len(text, kind=c_size_t))
   end function assert_eq_i32

   !> Check an integer condition using GoogleTest's nonzero-is-true convention.
   logical function assert_true_i32(condition, message) result(passed)
      integer(c_int32_t), intent(in) :: condition !< Integer condition.
      character(len=*), optional, intent(in) :: message !< Failure context.
      character(len=:), allocatable :: text

      passed = condition /= 0_c_int32_t
      text = format_error_message(message)
      call gtest_wrapper_true_int32(condition, 1_gtest_wrapper_bool_t, text, len(text, kind=c_size_t))
   end function assert_true_i32

   !> Check a logical prerequisite; callers must explicitly return on failure.
   logical function assert_true_logical(condition, message) result(passed)
      logical, intent(in) :: condition !< Required condition.
      character(len=*), optional, intent(in) :: message !< Failure context.
      character(len=:), allocatable :: text

      passed = condition
      text = format_error_message(message)
      call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), &
                                   1_gtest_wrapper_bool_t, text, len(text, kind=c_size_t))
   end function assert_true_logical

   !> Check a C-interoperable logical prerequisite.
   logical function assert_true_bool(condition, message) result(passed)
      logical(c_bool), intent(in) :: condition !< Required condition.
      character(len=*), optional, intent(in) :: message !< Failure context.
      character(len=:), allocatable :: text

      passed = condition
      text = format_error_message(message)
      call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), &
                                   1_gtest_wrapper_bool_t, text, len(text, kind=c_size_t))
   end function assert_true_bool

   function truncate_at_c_null(value) result(text)
      character(len=*), intent(in) :: value
      character(len=:), allocatable :: text
      integer :: nul
      nul = index(value, c_null_char)
      if (nul > 0) then
         text = value(:nul - 1)
      else
         text = value
      end if
   end function truncate_at_c_null

   function format_error_message(message, i, j) result(text)
      character(len=*), optional, intent(in) :: message
      integer, optional, intent(in) :: i, j
      character(len=:), allocatable :: text
      character(len=64) :: indices
      text = ''
      if (present(message)) then
         text = truncate_at_c_null(message)
      end if
      if (present(i)) then
         if (present(j)) then
            write (indices, '(" : (", I0, ",", I0, ") INDX")') i, j
         else
            write (indices, '(" : (", I0, ") INDX")') i
         end if
         text = text//trim(indices)
      end if
   end function format_error_message

   subroutine expect_eq_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_eq_int32(val1, val2, 0_gtest_wrapper_bool_t, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_eq_i32_rank0

   subroutine expect_eq_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_eq_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_eq_f32_rank0

   subroutine expect_eq_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_eq_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_eq_f64_rank0

   subroutine expect_eq_logical_rank0(val1, val2, message)
      logical, intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_eq_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val1), &
                                 merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val2), &
                       text, len(text, kind=c_size_t))
   end subroutine expect_eq_logical_rank0

   subroutine expect_eq_bool_rank0(val1, val2, message)
      logical(c_bool), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_eq_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val1), &
                                 merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val2), &
                       text, len(text, kind=c_size_t))
   end subroutine expect_eq_bool_rank0

   subroutine expect_ne_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ne_int32(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ne_i32_rank0

   subroutine expect_ne_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ne_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ne_f32_rank0

   subroutine expect_ne_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ne_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ne_f64_rank0

   subroutine expect_ne_logical_rank0(val1, val2, message)
      logical, intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ne_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val1), &
                                 merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val2), &
                       text, len(text, kind=c_size_t))
   end subroutine expect_ne_logical_rank0

   subroutine expect_ne_bool_rank0(val1, val2, message)
      logical(c_bool), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ne_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val1), &
                                 merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, val2), &
                       text, len(text, kind=c_size_t))
   end subroutine expect_ne_bool_rank0

   subroutine expect_lt_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_lt_int32(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_lt_i32_rank0

   subroutine expect_lt_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_lt_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_lt_f32_rank0

   subroutine expect_lt_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_lt_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_lt_f64_rank0

   subroutine expect_le_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_le_int32(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_le_i32_rank0

   subroutine expect_le_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_le_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_le_f32_rank0

   subroutine expect_le_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_le_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_le_f64_rank0

   subroutine expect_gt_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_gt_int32(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_gt_i32_rank0

   subroutine expect_gt_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_gt_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_gt_f32_rank0

   subroutine expect_gt_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_gt_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_gt_f64_rank0

   subroutine expect_ge_i32_rank0(val1, val2, message)
      integer(c_int32_t), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ge_int32(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ge_i32_rank0

   subroutine expect_ge_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ge_float(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ge_f32_rank0

   subroutine expect_ge_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_ge_double(val1, val2, &
                      text, len(text, kind=c_size_t))
   end subroutine expect_ge_f64_rank0

   subroutine expect_float_eq_f32_rank0(val1, val2, message)
      real(c_float), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_float_eq_float(val1, val2, &
                            text, len(text, kind=c_size_t))
   end subroutine expect_float_eq_f32_rank0

   subroutine expect_float_eq_f32_rank1(val1, val2, message)
      real(c_float), dimension(:), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_float_eq_float(val1(i), val2(i), &
                               text, len(text, kind=c_size_t))
      end do
   end subroutine expect_float_eq_f32_rank1

   subroutine expect_float_eq_f32_rank2(val1, val2, message)
      real(c_float), dimension(:, :), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         do j = 1, size(val1, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_float_eq_float(val1(i, j), val2(i, j), &
                                  text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_float_eq_f32_rank2

   subroutine expect_double_eq_f64_rank0(val1, val2, message)
      real(c_double), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_double_eq_double(val1, val2, &
                             text, len(text, kind=c_size_t))
   end subroutine expect_double_eq_f64_rank0

   subroutine expect_double_eq_f64_rank1(val1, val2, message)
      real(c_double), dimension(:), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_double_eq_double(val1(i), val2(i), &
                                text, len(text, kind=c_size_t))
      end do
   end subroutine expect_double_eq_f64_rank1

   subroutine expect_double_eq_f64_rank2(val1, val2, message)
      real(c_double), dimension(:, :), intent(in) :: val1, val2
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         do j = 1, size(val1, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_double_eq_double(val1(i, j), val2(i, j), &
                                   text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_double_eq_f64_rank2

   subroutine expect_near_f32_rank0(val1, val2, abs_error, message)
      real(c_float), intent(in) :: val1, val2
      real(c_float), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_near_float(val1, val2, abs_error, &
                        text, len(text, kind=c_size_t))
   end subroutine expect_near_f32_rank0

   subroutine expect_near_f32_rank1(val1, val2, abs_error, message)
      real(c_float), dimension(:), intent(in) :: val1, val2
      real(c_float), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_near_float(val1(i), val2(i), abs_error, &
                           text, len(text, kind=c_size_t))
      end do
   end subroutine expect_near_f32_rank1

   subroutine expect_near_f32_rank2(val1, val2, abs_error, message)
      real(c_float), dimension(:, :), intent(in) :: val1, val2
      real(c_float), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         do j = 1, size(val1, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_near_float(val1(i, j), val2(i, j), abs_error, &
                              text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_near_f32_rank2

   subroutine expect_near_f64_rank0(val1, val2, abs_error, message)
      real(c_double), intent(in) :: val1, val2
      real(c_double), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_near_double(val1, val2, abs_error, &
                        text, len(text, kind=c_size_t))
   end subroutine expect_near_f64_rank0

   subroutine expect_near_f64_rank1(val1, val2, abs_error, message)
      real(c_double), dimension(:), intent(in) :: val1, val2
      real(c_double), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_near_double(val1(i), val2(i), abs_error, &
                           text, len(text, kind=c_size_t))
      end do
   end subroutine expect_near_f64_rank1

   subroutine expect_near_f64_rank2(val1, val2, abs_error, message)
      real(c_double), dimension(:, :), intent(in) :: val1, val2
      real(c_double), intent(in) :: abs_error
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      if (any(shape(val1) /= shape(val2))) then
         text = format_error_message(message)//' : arrays of different shape'
         call gtest_wrapper_failure(text, len(text, kind=c_size_t))
         return
      end if
      do i = 1, size(val1, 1)
         do j = 1, size(val1, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_near_double(val1(i, j), val2(i, j), abs_error, &
                              text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_near_f64_rank2

   subroutine expect_true_i32_rank0(condition, message)
      integer(c_int32_t), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_true_int32(condition, 0_gtest_wrapper_bool_t, &
                        text, len(text, kind=c_size_t))
   end subroutine expect_true_i32_rank0

   subroutine expect_true_i32_rank1(condition, message)
      integer(c_int32_t), dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_true_int32(condition(i), 0_gtest_wrapper_bool_t, &
                           text, len(text, kind=c_size_t))
      end do
   end subroutine expect_true_i32_rank1

   subroutine expect_true_i32_rank2(condition, message)
      integer(c_int32_t), dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_true_int32(condition(i, j), 0_gtest_wrapper_bool_t, &
                              text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_true_i32_rank2

   subroutine expect_true_logical_rank0(condition, message)
      logical, intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), 0_gtest_wrapper_bool_t, &
                         text, len(text, kind=c_size_t))
   end subroutine expect_true_logical_rank0

   subroutine expect_true_logical_rank1(condition, message)
      logical, dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i)), 0_gtest_wrapper_bool_t, &
                            text, len(text, kind=c_size_t))
      end do
   end subroutine expect_true_logical_rank1

   subroutine expect_true_logical_rank2(condition, message)
      logical, dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i, j)), &
                                         0_gtest_wrapper_bool_t, &
                               text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_true_logical_rank2

   subroutine expect_true_bool_rank0(condition, message)
      logical(c_bool), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), 0_gtest_wrapper_bool_t, &
                         text, len(text, kind=c_size_t))
   end subroutine expect_true_bool_rank0

   subroutine expect_true_bool_rank1(condition, message)
      logical(c_bool), dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i)), 0_gtest_wrapper_bool_t, &
                            text, len(text, kind=c_size_t))
      end do
   end subroutine expect_true_bool_rank1

   subroutine expect_true_bool_rank2(condition, message)
      logical(c_bool), dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_true_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i, j)), &
                                         0_gtest_wrapper_bool_t, &
                               text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_true_bool_rank2

   subroutine expect_false_i32_rank0(condition, message)
      integer(c_int32_t), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_false_int32(condition, &
                         text, len(text, kind=c_size_t))
   end subroutine expect_false_i32_rank0

   subroutine expect_false_i32_rank1(condition, message)
      integer(c_int32_t), dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_false_int32(condition(i), &
                            text, len(text, kind=c_size_t))
      end do
   end subroutine expect_false_i32_rank1

   subroutine expect_false_i32_rank2(condition, message)
      integer(c_int32_t), dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_false_int32(condition(i, j), &
                               text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_false_i32_rank2

   subroutine expect_false_logical_rank0(condition, message)
      logical, intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), &
                          text, len(text, kind=c_size_t))
   end subroutine expect_false_logical_rank0

   subroutine expect_false_logical_rank1(condition, message)
      logical, dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i)), &
                             text, len(text, kind=c_size_t))
      end do
   end subroutine expect_false_logical_rank1

   subroutine expect_false_logical_rank2(condition, message)
      logical, dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i, j)), &
                                text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_false_logical_rank2

   subroutine expect_false_bool_rank0(condition, message)
      logical(c_bool), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition), &
                          text, len(text, kind=c_size_t))
   end subroutine expect_false_bool_rank0

   subroutine expect_false_bool_rank1(condition, message)
      logical(c_bool), dimension(:), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i
      do i = 1, size(condition, 1)
         text = format_error_message(message, i)
         call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i)), &
                             text, len(text, kind=c_size_t))
      end do
   end subroutine expect_false_bool_rank1

   subroutine expect_false_bool_rank2(condition, message)
      logical(c_bool), dimension(:, :), intent(in) :: condition
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      integer :: i, j
      do i = 1, size(condition, 1)
         do j = 1, size(condition, 2)
            text = format_error_message(message, i, j)
            call gtest_wrapper_false_bool(merge(1_gtest_wrapper_bool_t, 0_gtest_wrapper_bool_t, condition(i, j)), &
                                text, len(text, kind=c_size_t))
         end do
      end do
   end subroutine expect_false_bool_rank2

   subroutine expect_streq_string_rank0(str1, str2, message)
      character(len=*), intent(in) :: str1, str2
      character(len=:), allocatable :: left, right
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      left = truncate_at_c_null(str1)
      right = truncate_at_c_null(str2)
      call gtest_wrapper_streq_string(left, len(left, kind=c_size_t), right, len(right, kind=c_size_t), &
                            text, len(text, kind=c_size_t))
   end subroutine expect_streq_string_rank0

   subroutine expect_strne_string_rank0(str1, str2, message)
      character(len=*), intent(in) :: str1, str2
      character(len=:), allocatable :: left, right
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      left = truncate_at_c_null(str1)
      right = truncate_at_c_null(str2)
      call gtest_wrapper_strne_string(left, len(left, kind=c_size_t), right, len(right, kind=c_size_t), &
                            text, len(text, kind=c_size_t))
   end subroutine expect_strne_string_rank0

   subroutine expect_strcaseeq_string_rank0(str1, str2, message)
      character(len=*), intent(in) :: str1, str2
      character(len=:), allocatable :: left, right
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      left = truncate_at_c_null(str1)
      right = truncate_at_c_null(str2)
      call gtest_wrapper_strcaseeq_string(left, len(left, kind=c_size_t), right, len(right, kind=c_size_t), &
                                text, len(text, kind=c_size_t))
   end subroutine expect_strcaseeq_string_rank0

   subroutine expect_strcasene_string_rank0(str1, str2, message)
      character(len=*), intent(in) :: str1, str2
      character(len=:), allocatable :: left, right
      character(len=*), optional, intent(in) :: message
      character(len=:), allocatable :: text
      text = format_error_message(message)
      left = truncate_at_c_null(str1)
      right = truncate_at_c_null(str2)
      call gtest_wrapper_strcasene_string(left, len(left, kind=c_size_t), right, len(right, kind=c_size_t), &
                                text, len(text, kind=c_size_t))
   end subroutine expect_strcasene_string_rank0

end module assertions_gtest
