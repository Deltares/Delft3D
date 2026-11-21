program fortran_result_tests
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use dflowfm_io, only: MduModel, MduIssue
   implicit none(type, external)

   type(MduModel) :: model
   type(MduIssue), dimension(:), allocatable :: issues
   real(real64), dimension(:), allocatable :: values
   integer :: value, error_count = 0
   integer(int64) :: epoch
   logical :: bool_value, has_value

   call model%create(on_error)

   call model%get_int("geometry.kmx", value, on_error)
   if (value /= 0) error stop "incorrect integer"

   call model%get_bool("geometry.usecaching", bool_value, on_error)
   if (.not. bool_value) error stop "incorrect Boolean"

   call model%get_int_enum("general.autostart", value, on_error)
   if (value /= 0) error stop "incorrect integer enum"

   call model%get_datetime("time.refdate", epoch, has_value, on_error)
   if (.not. has_value) error stop "missing reference date"
   if (epoch /= 978307200_int64) error stop "incorrect reference date"

   call model%get_datetime("time.startdatetime", epoch, has_value, on_error)
   if (has_value) error stop "unexpected start datetime"

   call model%get_double_list("wind.cdbreakpoints", values, on_error)
   if (.not. allocated(values)) error stop "missing double list"
   if (size(values) /= 2) error stop "incorrect double list size"
   if (any(abs(values - [0.00063_real64, 0.00723_real64]) > 1.0e-12_real64)) then
      error stop "incorrect double list values"
   end if

   call model%set_double_list("wind.cdbreakpoints", [real(real64) ::], on_error)
   call model%get_double_list("wind.cdbreakpoints", values, on_error)
   if (.not. allocated(values)) error stop "empty list not allocated"
   if (size(values) /= 0) error stop "empty list not empty"

   call model%get_issues(issues, on_error)
   if (.not. allocated(issues)) error stop "missing issue list"
   if (size(issues) /= 0) error stop "unexpected issues"

   call model%get_int("unknown.key", value, record_error)
   call expect_failure(1)
   call model%get_bool("unknown.key", bool_value, record_error)
   call expect_failure(2)
   call model%get_int_enum("unknown.key", value, record_error)
   call expect_failure(3)
   call model%get_datetime("unknown.key", epoch, has_value, record_error)
   call expect_failure(4)
   call model%get_double_list("unknown.key", values, record_error)
   call expect_failure(5)
   if (allocated(values)) error stop "failed list getter allocated output"

   call model%destroy(on_error)
   call model%get_issues(issues, record_error)
   call expect_failure(6)
   if (allocated(issues)) error stop "failed issue getter allocated output"

contains

   subroutine on_error(message)
      character(len=*), intent(in) :: message

      error stop "unexpected error: "//message
   end subroutine

   subroutine record_error(message)
      character(len=*), intent(in) :: message

      if (len_trim(message) == 0) then
         error stop "missing error message"
      end if
      error_count = error_count + 1
   end subroutine

   subroutine expect_failure(expected_count)
      integer, intent(in) :: expected_count

      if (error_count /= expected_count) then
         error stop "incorrect error handler invocation count"
      end if
   end subroutine

end program fortran_result_tests
