module dflowfm_io
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    private

    integer, parameter, public :: DFLOWFM_IO_RESULT_SUCCESS = 0
    integer, parameter, public :: DFLOWFM_IO_RESULT_ERROR   = 1

    ! Mirrors mdu_severity_t in dflowfm_io_api.h
    integer, parameter, public :: MDU_SEVERITY_INFO    = 0
    integer, parameter, public :: MDU_SEVERITY_WARNING = 1
    integer, parameter, public :: MDU_SEVERITY_ERROR   = 2

    !> Fortran mirror of a single diagnostic issue. Uses only native Fortran types.
    type, public :: MduIssue
        integer                       :: line_number = -1
        integer                       :: severity    = MDU_SEVERITY_INFO
        character(len=:), allocatable :: message
    end type MduIssue

    type, public :: MduModel
        private
        type(c_ptr) :: handle = c_null_ptr
    contains
        procedure :: create           => mdu_create_f
        procedure :: destroy          => mdu_destroy_f
        procedure :: has_valid_handle => mdu_has_valid_handle_f
        procedure :: get_last_error   => mdu_get_last_error_f

        procedure :: load_from_file   => mdu_load_from_file_f
        procedure :: load_from_string => mdu_load_from_string_f
        procedure :: save_to_file     => mdu_save_to_file_f
        procedure :: save_to_string   => mdu_save_to_string_f

        procedure :: get_int          => mdu_get_int_f
        procedure :: get_bool         => mdu_get_bool_f
        procedure :: get_double       => mdu_get_double_f
        procedure :: get_string       => mdu_get_string_f
        procedure :: get_path         => mdu_get_path_f
        procedure :: get_datetime     => mdu_get_datetime_f
        procedure :: get_enum         => mdu_get_enum_f
        procedure :: get_string_list  => mdu_get_string_list_f
        procedure :: get_path_list    => mdu_get_path_list_f
        procedure :: get_double_list  => mdu_get_double_list_f

        procedure :: set_int          => mdu_set_int_f
        procedure :: set_bool         => mdu_set_bool_f
        procedure :: set_double       => mdu_set_double_f
        procedure :: set_string       => mdu_set_string_f
        procedure :: set_path         => mdu_set_path_f
        procedure :: set_datetime     => mdu_set_datetime_f
        procedure :: set_enum         => mdu_set_enum_f
        procedure :: set_string_list  => mdu_set_string_list_f
        procedure :: set_path_list    => mdu_set_path_list_f
        procedure :: set_double_list  => mdu_set_double_list_f

        procedure :: get_issues       => mdu_get_issues_f

        procedure, private :: mdu_assign
        generic :: assignment(=) => mdu_assign
        final     :: mdu_finalizer
    end type MduModel

    interface
        function c_mdu_create(out_handle) result(res) bind(C, name="mdu_create")
            import :: c_ptr, c_int32_t
            type(c_ptr), intent(out) :: out_handle
            integer(c_int32_t) :: res
        end function

        function c_mdu_destroy(handle) result(res) bind(C, name="mdu_destroy")
            import :: c_ptr, c_int32_t
            type(c_ptr), intent(inout) :: handle
            integer(c_int32_t) :: res
        end function

        function c_mdu_load_from_file(handle, filename) result(res) bind(C, name="mdu_load_from_file")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int32_t) :: res
        end function

        function c_mdu_load_from_string(handle, data, size) result(res) bind(C, name="mdu_load_from_string")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: data(*)
            integer(c_int64_t), value, intent(in) :: size
            integer(c_int32_t) :: res
        end function

        function c_mdu_save_to_file(handle, filename) result(res) bind(C, name="mdu_save_to_file")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int32_t) :: res
        end function

        function c_mdu_save_to_string(handle, out_data) result(res) bind(C, name="mdu_save_to_string")
            import :: c_ptr, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            type(c_ptr), intent(out) :: out_data
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_int(handle, key, out_value) result(res) bind(C, name="mdu_get_int")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_bool(handle, key, out_value) result(res) bind(C, name="mdu_get_bool")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_double(handle, key, out_value) result(res) bind(C, name="mdu_get_double")
            import :: c_ptr, c_char, c_int32_t, c_double
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            real(c_double), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_string(handle, key, out_value) result(res) bind(C, name="mdu_get_string")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_path(handle, key, out_value) result(res) bind(C, name="mdu_get_path")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_datetime(handle, key, out_value) result(res) bind(C, name="mdu_get_datetime")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int64_t), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_enum(handle, key, out_value) result(res) bind(C, name="mdu_get_enum")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), intent(out) :: out_value
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_string_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_string_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(out) :: out_list
            integer(c_int64_t), intent(out) :: out_size
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_path_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_path_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(out) :: out_list
            integer(c_int64_t), intent(out) :: out_size
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_double_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_double_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(out) :: out_list
            integer(c_int64_t), intent(out) :: out_size
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_int(handle, key, value) result(res) bind(C, name="mdu_set_int")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), value, intent(in) :: value
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_bool(handle, key, value) result(res) bind(C, name="mdu_set_bool")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), value, intent(in) :: value
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_double(handle, key, value) result(res) bind(C, name="mdu_set_double")
            import :: c_ptr, c_char, c_int32_t, c_double
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            real(c_double), value, intent(in) :: value
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_string(handle, key, value) result(res) bind(C, name="mdu_set_string")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_path(handle, key, value) result(res) bind(C, name="mdu_set_path")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_datetime(handle, key, epoch) result(res) bind(C, name="mdu_set_datetime")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int64_t), value, intent(in) :: epoch
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_enum(handle, key, value) result(res) bind(C, name="mdu_set_enum")
            import :: c_ptr, c_char, c_int32_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int32_t), value, intent(in) :: value
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_string_list(handle, key, list, size) result(res) bind(C, name="mdu_set_string_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(in) :: list(*)
            integer(c_int64_t), value, intent(in) :: size
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_path_list(handle, key, list, size) result(res) bind(C, name="mdu_set_path_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            type(c_ptr), intent(in) :: list(*)
            integer(c_int64_t), value, intent(in) :: size
            integer(c_int32_t) :: res
        end function

        function c_mdu_set_double_list(handle, key, list, size) result(res) bind(C, name="mdu_set_double_list")
            import :: c_ptr, c_char, c_int32_t, c_int64_t, c_double
            type(c_ptr), value, intent(in) :: handle
            character(kind=c_char), intent(in) :: key(*)
            real(c_double), intent(in) :: list(*)
            integer(c_int64_t), value, intent(in) :: size
            integer(c_int32_t) :: res
        end function

        function c_mdu_get_issue_list(handle, out_list, out_size) result(res) bind(C, name="mdu_get_issue_list")
            import :: c_ptr, c_int32_t, c_int64_t
            type(c_ptr), value, intent(in) :: handle
            type(c_ptr), intent(out) :: out_list
            integer(c_int64_t), intent(out) :: out_size
            integer(c_int32_t) :: res
        end function

        function c_dflowfm_io_get_last_error() result(ptr) bind(C, name="dflowfm_io_get_last_error")
            import :: c_ptr
            type(c_ptr) :: ptr
        end function
    end interface

    ! Mirror of the C mdu_issue_t struct, used only to interpret the C array.
    type, bind(C) :: c_mdu_issue_t
        integer(c_int32_t) :: line_number
        integer(c_int32_t) :: severity
        type(c_ptr)        :: message
    end type c_mdu_issue_t

contains

    !---------------------------------------------------------------------------
    ! Lifetime
    !---------------------------------------------------------------------------
    subroutine mdu_create_f(self, result_code)
        class(MduModel), intent(inout) :: self
        integer, intent(out) :: result_code

        result_code = int(c_mdu_create(self%handle))
    end subroutine

    subroutine mdu_destroy_f(self, result_code)
        class(MduModel), intent(inout) :: self
        integer, intent(out) :: result_code

        result_code = int(c_mdu_destroy(self%handle))
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
    subroutine mdu_load_from_file_f(self, filename, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: result_code

        result_code = int(c_mdu_load_from_file(self%handle, f_to_c_string(filename)))
    end subroutine

    subroutine mdu_load_from_string_f(self, data, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: data
        integer, intent(out) :: result_code

        result_code = int(c_mdu_load_from_string(self%handle, data, int(len(data), c_int64_t)))
    end subroutine

    subroutine mdu_save_to_file_f(self, filename, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: result_code

        result_code = int(c_mdu_save_to_file(self%handle, f_to_c_string(filename)))
    end subroutine

    subroutine mdu_save_to_string_f(self, data, result_code)
        class(MduModel), intent(in) :: self
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out) :: result_code
        type(c_ptr) :: cptr

        result_code = int(c_mdu_save_to_string(self%handle, cptr))
        data = c_string_to_f(cptr)
    end subroutine

    !---------------------------------------------------------------------------
    ! Scalar getters
    !---------------------------------------------------------------------------
    subroutine mdu_get_int_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: value
        integer, intent(out) :: result_code
        integer(c_int32_t) :: c_value

        result_code = int(c_mdu_get_int(self%handle, f_to_c_string(key), c_value))
        value = int(c_value)
    end subroutine

    subroutine mdu_get_bool_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, intent(out) :: value
        integer, intent(out) :: result_code
        integer(c_int32_t) :: c_value

        result_code = int(c_mdu_get_bool(self%handle, f_to_c_string(key), c_value))
        value = (c_value /= 0)
    end subroutine

    subroutine mdu_get_double_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        real(kind=real64), intent(out) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_get_double(self%handle, f_to_c_string(key), value))
    end subroutine

    subroutine mdu_get_string_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: result_code
        type(c_ptr) :: cptr

        result_code = int(c_mdu_get_string(self%handle, f_to_c_string(key), cptr))
        value = c_string_to_f(cptr)
    end subroutine

    subroutine mdu_get_path_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: result_code
        type(c_ptr) :: cptr

        result_code = int(c_mdu_get_path(self%handle, f_to_c_string(key), cptr))
        value = c_string_to_f(cptr)
    end subroutine

    subroutine mdu_get_datetime_f(self, key, epoch, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer(kind=int64), intent(out) :: epoch
        integer, intent(out) :: result_code

        result_code = int(c_mdu_get_datetime(self%handle, f_to_c_string(key), epoch))
    end subroutine

    subroutine mdu_get_enum_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(out) :: value
        integer, intent(out) :: result_code
        integer(c_int32_t) :: c_value

        result_code = int(c_mdu_get_enum(self%handle, f_to_c_string(key), c_value))
        value = int(c_value)
    end subroutine

    !---------------------------------------------------------------------------
    ! List getters
    !---------------------------------------------------------------------------
    subroutine mdu_get_string_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: values(:)
        integer, intent(out) :: result_code
        type(c_ptr) :: list_ptr
        integer(c_int64_t) :: c_size

        result_code = int(c_mdu_get_string_list(self%handle, f_to_c_string(key), list_ptr, c_size))
        values = c_string_array_to_f(list_ptr, c_size)
    end subroutine

    subroutine mdu_get_path_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: values(:)
        integer, intent(out) :: result_code
        type(c_ptr) :: list_ptr
        integer(c_int64_t) :: c_size

        result_code = int(c_mdu_get_path_list(self%handle, f_to_c_string(key), list_ptr, c_size))
        values = c_string_array_to_f(list_ptr, c_size)
    end subroutine

    subroutine mdu_get_double_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        real(kind=real64), allocatable, intent(out) :: values(:)
        integer, intent(out) :: result_code
        type(c_ptr) :: list_ptr
        integer(c_int64_t) :: c_size
        real(c_double), pointer :: fptr(:)

        result_code = int(c_mdu_get_double_list(self%handle, f_to_c_string(key), list_ptr, c_size))
        if (c_associated(list_ptr) .and. c_size > 0) then
            call c_f_pointer(list_ptr, fptr, [int(c_size)])
            values = fptr
        else
            allocate(values(0))
        end if
    end subroutine

    !---------------------------------------------------------------------------
    ! Scalar setters
    !---------------------------------------------------------------------------
    subroutine mdu_set_int_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_int(self%handle, f_to_c_string(key), int(value, c_int32_t)))
    end subroutine

    subroutine mdu_set_bool_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        integer, intent(out) :: result_code
        integer(c_int32_t) :: c_value

        c_value = 0
        if (value) c_value = 1
        result_code = int(c_mdu_set_bool(self%handle, f_to_c_string(key), c_value))
    end subroutine

    subroutine mdu_set_double_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        real(kind=real64), intent(in) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_double(self%handle, f_to_c_string(key), value))
    end subroutine

    subroutine mdu_set_string_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_string(self%handle, f_to_c_string(key), f_to_c_string(value)))
    end subroutine

    subroutine mdu_set_path_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_path(self%handle, f_to_c_string(key), f_to_c_string(value)))
    end subroutine

    subroutine mdu_set_datetime_f(self, key, epoch, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer(kind=int64), intent(in) :: epoch
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_datetime(self%handle, f_to_c_string(key), epoch))
    end subroutine

    subroutine mdu_set_enum_f(self, key, value, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_enum(self%handle, f_to_c_string(key), int(value, c_int32_t)))
    end subroutine

    !---------------------------------------------------------------------------
    ! List setters
    !---------------------------------------------------------------------------
    subroutine mdu_set_string_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: values(:)
        integer, intent(out) :: result_code

        result_code = set_string_list_impl(.false., self%handle, key, values)
    end subroutine

    subroutine mdu_set_path_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: values(:)
        integer, intent(out) :: result_code

        result_code = set_string_list_impl(.true., self%handle, key, values)
    end subroutine

    subroutine mdu_set_double_list_f(self, key, values, result_code)
        class(MduModel), intent(in) :: self
        character(len=*), intent(in) :: key
        real(kind=real64), intent(in) :: values(:)
        integer, intent(out) :: result_code

        result_code = int(c_mdu_set_double_list(self%handle, f_to_c_string(key), values, &
                                                int(size(values), c_int64_t)))
    end subroutine

    !---------------------------------------------------------------------------
    ! Report
    !---------------------------------------------------------------------------
    subroutine mdu_get_issues_f(self, issues, result_code)
        class(MduModel), intent(in) :: self
        type(MduIssue), allocatable, intent(out) :: issues(:)
        integer, intent(out) :: result_code
        type(c_ptr) :: list_ptr
        integer(c_int64_t) :: c_size
        type(c_mdu_issue_t), pointer :: c_issues(:)
        integer :: i

        result_code = int(c_mdu_get_issue_list(self%handle, list_ptr, c_size))
        if (c_associated(list_ptr) .and. c_size > 0) then
            call c_f_pointer(list_ptr, c_issues, [int(c_size)])
            allocate(issues(int(c_size)))
            do i = 1, int(c_size)
                issues(i)%line_number = int(c_issues(i)%line_number)
                issues(i)%severity    = int(c_issues(i)%severity)
                issues(i)%message     = c_string_to_f(c_issues(i)%message)
            end do
        else
            allocate(issues(0))
        end if
    end subroutine

    !---------------------------------------------------------------------------
    ! Helpers
    !---------------------------------------------------------------------------

    !> Copy a Fortran list-of-strings into a local C pointer array and call the
    !! string- or path-list setter, returning the result code.
    function set_string_list_impl(is_path, handle, key, values) result(res)
        logical, intent(in) :: is_path
        type(c_ptr), intent(in) :: handle
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: values(:)
        integer :: res
        character(len=:), allocatable, target :: buffers(:)
        type(c_ptr), allocatable :: ptrs(:)
        integer :: i, n

        n = size(values)
        allocate(character(len=len_trim_max(values) + 1) :: buffers(max(n, 1)))
        allocate(ptrs(max(n, 1)))
        do i = 1, n
            buffers(i) = trim(values(i)) // c_null_char
            ptrs(i) = c_loc(buffers(i))
        end do

        if (is_path) then
            res = int(c_mdu_set_path_list(handle, f_to_c_string(key), ptrs, int(n, c_int64_t)))
        else
            res = int(c_mdu_set_string_list(handle, f_to_c_string(key), ptrs, int(n, c_int64_t)))
        end if
    end function

    pure function len_trim_max(values) result(max_len)
        character(len=*), intent(in) :: values(:)
        integer :: max_len, i

        max_len = 0
        do i = 1, size(values)
            max_len = max(max_len, len_trim(values(i)))
        end do
    end function

    !> Append a null terminator to a Fortran string for passing to C.
    pure function f_to_c_string(str) result(c_str)
        character(len=*), intent(in) :: str
        character(len=len_trim(str) + 1, kind=c_char) :: c_str

        c_str = trim(str) // c_null_char
    end function

    !> Copy a null-terminated C string (valid until the next C call) into an
    !! allocatable Fortran string.
    function c_string_to_f(cptr) result(str)
        type(c_ptr), intent(in) :: cptr
        character(len=:), allocatable :: str
        character(kind=c_char), pointer :: chars(:)
        integer :: length, i

        if (.not. c_associated(cptr)) then
            str = ''
            return
        end if

        length = c_strlen(cptr)
        allocate(character(len=length) :: str)
        if (length > 0) then
            call c_f_pointer(cptr, chars, [length])
            do i = 1, length
                str(i:i) = chars(i)
            end do
        end if
    end function

    !> Copy a C array of null-terminated strings into a Fortran string array.
    function c_string_array_to_f(list_ptr, n) result(values)
        type(c_ptr), intent(in) :: list_ptr
        integer(c_int64_t), intent(in) :: n
        character(len=:), allocatable :: values(:)
        type(c_ptr), pointer :: ptrs(:)
        integer :: i, count, max_len

        count = int(n)
        if (.not. c_associated(list_ptr) .or. count <= 0) then
            allocate(character(len=0) :: values(0))
            return
        end if

        call c_f_pointer(list_ptr, ptrs, [count])
        max_len = 0
        do i = 1, count
            max_len = max(max_len, c_strlen(ptrs(i)))
        end do

        allocate(character(len=max_len) :: values(count))
        do i = 1, count
            values(i) = c_string_to_f(ptrs(i))
        end do
    end function

    function c_strlen(cptr) result(length)
        type(c_ptr), intent(in) :: cptr
        integer :: length
        character(kind=c_char), pointer :: chars(:)
        integer, parameter :: MAX_LEN = 1048576

        length = 0
        if (.not. c_associated(cptr)) return
        call c_f_pointer(cptr, chars, [MAX_LEN])
        do
            if (length >= MAX_LEN) exit
            if (chars(length + 1) == c_null_char) exit
            length = length + 1
        end do
    end function

end module dflowfm_io
