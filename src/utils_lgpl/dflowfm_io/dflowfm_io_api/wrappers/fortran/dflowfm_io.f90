module dflowfm_io
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use dflowfm_io_c
   use dflowfm_io_helpers, only: set_string_list_impl, f_to_c_string, c_string_to_f, c_string_array_to_f
   implicit none
   private

   integer, parameter, public :: DFLOWFM_IO_RESULT_SUCCESS = 0
   integer, parameter, public :: DFLOWFM_IO_RESULT_ERROR = 1

   ! Mirrors mdu_severity_t in dflowfm_io_api.h
   integer, parameter, public :: MDU_SEVERITY_DEBUG = 0
   integer, parameter, public :: MDU_SEVERITY_INFO = 1
   integer, parameter, public :: MDU_SEVERITY_WARNING = 2
   integer, parameter, public :: MDU_SEVERITY_ERROR = 3

   !> Fortran mirror of a single diagnostic issue. Uses only native Fortran types.
   type, public :: MduIssue
      integer :: line_number = -1
      integer :: severity = MDU_SEVERITY_INFO
      character(len=:), allocatable :: message
   end type MduIssue

   !> Interface for a user-supplied error handler. Every fallible model
    !! operation requires one and invokes it with the error message on failure,
    !! sparing the caller from checking every result code by hand.
   abstract interface
      subroutine mdu_error_handler_i(message)
         character(len=*), intent(in) :: message
      end subroutine
   end interface

   type, public :: MduModel
      private
      type(c_ptr) :: handle = c_null_ptr
   contains
      procedure :: create => mdu_create_f
      procedure :: destroy => mdu_destroy_f
      procedure :: has_valid_handle => mdu_has_valid_handle_f
      procedure :: get_last_error => mdu_get_last_error_f
      procedure, private :: handle_result => mdu_handle_result_f

      procedure :: load_from_file => mdu_load_from_file_f
      procedure :: load_from_string => mdu_load_from_string_f
      procedure :: save_to_file => mdu_save_to_file_f
      procedure :: save_to_string => mdu_save_to_string_f

      procedure :: get_int => mdu_get_int_f
      procedure :: get_bool => mdu_get_bool_f
      procedure :: get_double => mdu_get_double_f
      procedure :: get_string => mdu_get_string_f
      procedure :: get_path => mdu_get_path_f
      procedure :: get_datetime => mdu_get_datetime_f
      procedure :: get_int_enum => mdu_get_int_enum_f
      procedure :: get_string_enum => mdu_get_string_enum_f
      procedure :: get_string_list => mdu_get_string_list_f
      procedure :: get_path_list => mdu_get_path_list_f
      procedure :: get_double_list => mdu_get_double_list_f

      procedure :: set_int => mdu_set_int_f
      procedure :: set_bool => mdu_set_bool_f
      procedure :: set_double => mdu_set_double_f
      procedure :: set_string => mdu_set_string_f
      procedure :: set_path => mdu_set_path_f
      procedure :: set_datetime => mdu_set_datetime_f
      procedure :: set_int_enum => mdu_set_int_enum_f
      procedure :: set_string_enum => mdu_set_string_enum_f
      procedure :: set_string_list => mdu_set_string_list_f
      procedure :: set_path_list => mdu_set_path_list_f
      procedure :: set_double_list => mdu_set_double_list_f

      procedure :: get_issues => mdu_get_issues_f

      procedure, private :: mdu_assign
      generic :: assignment(=) => mdu_assign
      final :: mdu_finalizer
   end type MduModel

contains

   !---------------------------------------------------------------------------
   ! Lifetime
   !---------------------------------------------------------------------------
   subroutine mdu_create_f(self, success, error_handler)
      class(MduModel), intent(inout) :: self
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_create(self%handle))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_destroy_f(self, success, error_handler)
      class(MduModel), intent(inout) :: self
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_destroy(self%handle))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   function mdu_has_valid_handle_f(self) result(valid)
      class(MduModel), intent(in) :: self
      logical :: valid

      valid = c_associated(self%handle)
   end function

   subroutine mdu_get_last_error_f(self, msg)
      class(MduModel), intent(in) :: self
      character(len=:), allocatable, intent(out) :: msg

      msg = c_string_to_f(c_dflowfm_io_get_last_error())
   end subroutine

   !> Invoke the caller-supplied error handler with the last error message when
    !! the given result code indicates failure.
   subroutine mdu_handle_result_f(self, result_code, error_handler)
      class(MduModel), intent(in) :: self
      integer, intent(in) :: result_code
      procedure(mdu_error_handler_i) :: error_handler

      if (result_code /= DFLOWFM_IO_RESULT_SUCCESS) then
         call error_handler(c_string_to_f(c_dflowfm_io_get_last_error()))
      end if
   end subroutine

   subroutine mdu_finalizer(self)
      type(MduModel), intent(inout) :: self
      integer(c_int32_t) :: res

      res = c_mdu_destroy(self%handle)
   end subroutine

   subroutine mdu_assign(lhs, rhs)
      class(MduModel), intent(inout) :: lhs
      type(MduModel), intent(in) :: rhs
      error stop "ERROR: MduModel can't be copied or assigned"
   end subroutine

   !---------------------------------------------------------------------------
   ! Load / save
   !---------------------------------------------------------------------------
   subroutine mdu_load_from_file_f(self, filename, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: filename
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_load_from_file(self%handle, f_to_c_string(filename)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_load_from_string_f(self, data, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: data
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_load_from_string(self%handle, data, int(len(data), c_int64_t)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_save_to_file_f(self, filename, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: filename
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_save_to_file(self%handle, f_to_c_string(filename)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_save_to_string_f(self, data, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=:), allocatable, intent(out) :: data
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: cptr
      integer :: result_code

      result_code = int(c_mdu_save_to_string(self%handle, cptr))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         data = c_string_to_f(cptr)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   !---------------------------------------------------------------------------
   ! Scalar getters
   !---------------------------------------------------------------------------
   subroutine mdu_get_int_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_value
      integer :: result_code

      result_code = int(c_mdu_get_int(self%handle, f_to_c_string(key), c_value))
      value = int(c_value)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_bool_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      logical, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_value
      integer :: result_code

      result_code = int(c_mdu_get_bool(self%handle, f_to_c_string(key), c_value))
      value = (c_value /= 0)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_double_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      real(kind=real64), intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_get_double(self%handle, f_to_c_string(key), value))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_string_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: cptr
      integer :: result_code

      result_code = int(c_mdu_get_string(self%handle, f_to_c_string(key), cptr))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         value = c_string_to_f(cptr)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_path_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: cptr
      integer :: result_code

      result_code = int(c_mdu_get_path(self%handle, f_to_c_string(key), cptr))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         value = c_string_to_f(cptr)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_datetime_f(self, key, epoch, has_value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer(kind=int64), intent(out) :: epoch
      logical, intent(out) :: has_value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_has_value
      integer :: result_code

      result_code = int(c_mdu_get_datetime(self%handle, f_to_c_string(key), epoch, c_has_value))
      has_value = (c_has_value /= 0)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_int_enum_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_value
      integer :: result_code

      result_code = int(c_mdu_get_int_enum(self%handle, f_to_c_string(key), c_value))
      value = int(c_value)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_string_enum_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: cptr
      integer :: result_code

      result_code = int(c_mdu_get_string_enum(self%handle, f_to_c_string(key), cptr))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         value = c_string_to_f(cptr)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   !---------------------------------------------------------------------------
   ! List getters
   !---------------------------------------------------------------------------
   subroutine mdu_get_string_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: list_ptr
      integer(c_int64_t) :: c_size
      integer :: result_code

      result_code = int(c_mdu_get_string_list(self%handle, f_to_c_string(key), list_ptr, c_size))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         values = c_string_array_to_f(list_ptr, c_size)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_path_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=:), allocatable, intent(out) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: list_ptr
      integer(c_int64_t) :: c_size
      integer :: result_code

      result_code = int(c_mdu_get_path_list(self%handle, f_to_c_string(key), list_ptr, c_size))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS) then
         values = c_string_array_to_f(list_ptr, c_size)
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_get_double_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      real(kind=real64), allocatable, intent(out) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: list_ptr
      integer(c_int64_t) :: c_size
      real(c_double), pointer :: fptr(:)
      integer :: result_code

      result_code = int(c_mdu_get_double_list(self%handle, f_to_c_string(key), list_ptr, c_size))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS .and. c_associated(list_ptr) .and. c_size > 0) then
         call c_f_pointer(list_ptr, fptr, [int(c_size)])
         values = fptr
      else
         allocate (values(0))
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   !---------------------------------------------------------------------------
   ! Scalar setters
   !---------------------------------------------------------------------------
   subroutine mdu_set_int_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_int(self%handle, f_to_c_string(key), int(value, c_int32_t)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_bool_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      logical, intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_value
      integer :: result_code

      c_value = 0
      if (value) c_value = 1
      result_code = int(c_mdu_set_bool(self%handle, f_to_c_string(key), c_value))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_double_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      real(kind=real64), intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_double(self%handle, f_to_c_string(key), value))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_string_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_string(self%handle, f_to_c_string(key), f_to_c_string(value)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_path_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_path(self%handle, f_to_c_string(key), f_to_c_string(value)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_datetime_f(self, key, epoch, has_value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer(kind=int64), intent(in) :: epoch
      logical, intent(in) :: has_value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer(c_int32_t) :: c_has_value
      integer :: result_code

      c_has_value = 0
      if (has_value) c_has_value = 1
      result_code = int(c_mdu_set_datetime(self%handle, f_to_c_string(key), epoch, c_has_value))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_int_enum_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      integer, intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_int_enum(self%handle, f_to_c_string(key), int(value, c_int32_t)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_string_enum_f(self, key, value, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_string_enum(self%handle, f_to_c_string(key), f_to_c_string(value)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   !---------------------------------------------------------------------------
   ! List setters
   !---------------------------------------------------------------------------
   subroutine mdu_set_string_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = set_string_list_impl(.false., self%handle, key, values)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_path_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = set_string_list_impl(.true., self%handle, key, values)
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   subroutine mdu_set_double_list_f(self, key, values, success, error_handler)
      class(MduModel), intent(in) :: self
      character(len=*), intent(in) :: key
      real(kind=real64), intent(in) :: values(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      integer :: result_code

      result_code = int(c_mdu_set_double_list(self%handle, f_to_c_string(key), values, &
                                             int(size(values), c_int64_t)))
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

   !---------------------------------------------------------------------------
   ! Report
   !---------------------------------------------------------------------------
   subroutine mdu_get_issues_f(self, issues, success, error_handler)
      class(MduModel), intent(in) :: self
      type(MduIssue), allocatable, intent(out) :: issues(:)
      logical, intent(out) :: success
      procedure(mdu_error_handler_i) :: error_handler
      type(c_ptr) :: list_ptr
      integer(c_int64_t) :: c_size
      type(c_mdu_issue_t), pointer :: c_issues(:)
      integer :: i
      integer :: result_code

      result_code = int(c_mdu_get_issue_list(self%handle, list_ptr, c_size))
      if (result_code == DFLOWFM_IO_RESULT_SUCCESS .and. c_associated(list_ptr) .and. c_size > 0) then
         call c_f_pointer(list_ptr, c_issues, [int(c_size)])
         allocate (issues(int(c_size)))
         do i = 1, int(c_size)
            issues(i)%line_number = int(c_issues(i)%line_number)
            issues(i)%severity = int(c_issues(i)%severity)
            issues(i)%message = c_string_to_f(c_issues(i)%message)
         end do
      else
         allocate (issues(0))
      end if
      call self%handle_result(result_code, error_handler)
      success = (result_code == DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine

end module dflowfm_io
