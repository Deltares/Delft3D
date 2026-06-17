module dflowfm_io
    use, intrinsic :: iso_c_binding
    implicit none
    private

    integer, parameter, public :: DFLOWFM_IO_RESULT_SUCCESS = 0
    integer, parameter, public :: DFLOWFM_IO_RESULT_ERROR   = 1

    type, public :: MduModel
        private
        type(c_ptr) :: handle = c_null_ptr
    contains
        procedure :: create           => mdu_model_create_f
        procedure :: destroy          => mdu_model_destroy_f
        procedure :: has_valid_handle => mdu_model_has_valid_handle_f
        procedure :: get_dummy_value  => mdu_model_get_dummy_value_f
        procedure :: get_last_error   => mdu_model_get_last_error_f
        procedure, private :: mdu_model_assign
        generic :: assignment(=) => mdu_model_assign
        final     :: mdu_model_finalizer
    end type MduModel

    interface
        function c_mdu_document_create(out_handle) result(res) bind(C, name="mdu_document_create")
            import :: c_ptr, c_int
            type(c_ptr), intent(out) :: out_handle
            integer(c_int) :: res
        end function

        function c_mdu_document_destroy(handle) result(res) bind(C, name="mdu_document_destroy")
            import :: c_ptr, c_int
            type(c_ptr), intent(inout) :: handle
            integer(c_int) :: res
        end function

        function c_mdu_model_get_dummy_value(handle, out_value) result(res) bind(C, name="mdu_model_get_dummy_value")
            import :: c_ptr, c_int
            type(c_ptr), value, intent(in) :: handle
            integer(c_int), intent(out)    :: out_value
            integer(c_int) :: res
        end function

        function c_dflowfm_io_get_last_error() result(ptr) bind(C, name="dflowfm_io_get_last_error")
            import :: c_ptr
            type(c_ptr) :: ptr
        end function
    end interface

contains

    subroutine mdu_model_create_f(self, result_code)
        class(MduModel), intent(inout) :: self
        integer, intent(out) :: result_code

        result_code = int(c_mdu_document_create(self%handle))
    end subroutine

    function mdu_model_has_valid_handle_f(self) result(valid)
        class(MduModel), intent(in) :: self
        logical :: valid

        valid = c_associated(self%handle)
    end function

    subroutine mdu_model_get_dummy_value_f(self, value, result_code)
        class(MduModel), intent(in) :: self
        integer, intent(out) :: value
        integer, intent(out) :: result_code
        integer(c_int) :: c_value

        result_code = int(c_mdu_model_get_dummy_value(self%handle, c_value))
        value = int(c_value)
    end subroutine

    subroutine mdu_model_get_last_error_f(self, msg)
        class(MduModel), intent(in) :: self
        character(len=*), intent(out) :: msg
        type(c_ptr) :: cptr
        character(kind=c_char), pointer :: fptr(:)
        integer :: i, length, max_len

        msg = ''
        cptr = c_dflowfm_io_get_last_error()
        if (.not. c_associated(cptr)) return

        max_len = len(msg)
        call c_f_pointer(cptr, fptr, [max_len])
        length = 0
        do i = 1, max_len
            if (fptr(i) == c_null_char) exit
            length = i
        end do

        do i = 1, length
            msg(i:i) = fptr(i)
        end do
    end subroutine

    subroutine mdu_model_destroy_f(self, result_code)
        class(MduModel), intent(inout) :: self
        integer, intent(out) :: result_code

        result_code = int(c_mdu_document_destroy(self%handle))
    end subroutine

    subroutine mdu_model_finalizer(self)
        type(MduModel), intent(inout) :: self
        integer(c_int) :: res

        res = c_mdu_document_destroy(self%handle)
    end subroutine

    subroutine mdu_model_assign(lhs, rhs)
        class(MduModel), intent(inout) :: lhs
        type(MduModel), intent(in) :: rhs
        error stop "ERROR: MduModel can't be copied or assigned"
    end subroutine

end module dflowfm_io
