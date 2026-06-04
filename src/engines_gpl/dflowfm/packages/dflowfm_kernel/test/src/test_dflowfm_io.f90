module test_dflowfm_io
   use assertions_gtest
   use dflowfm_io

   implicit none

   contains

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_create_and_destroy, test_create_and_destroy,
   subroutine test_create_and_destroy() bind(C)
      type(MduModel) :: model
      integer :: result_code

      call f90_expect_eq(model%has_valid_handle(), .false.)
      call model%create(result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(model%has_valid_handle(), .true.)
      
      ! Call finalizer manually
      call model%destroy(result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(model%has_valid_handle(), .false.)
   end subroutine test_create_and_destroy
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_create_and_go_out_of_scope, test_create_and_go_out_of_scope,
   subroutine test_create_and_go_out_of_scope() bind(C)
      type(MduModel) :: model
      integer :: result_code

      call f90_expect_eq(model%has_valid_handle(), .false.)
      call model%create(result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(model%has_valid_handle(), .true.)
      
      ! Object goes out of scope, finalizer should be called automatically
   end subroutine test_create_and_go_out_of_scope
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_dummy_value, test_get_dummy_value,
   subroutine test_get_dummy_value() bind(C)
      type(MduModel) :: model
      integer :: result_code
      integer :: value

      call model%create(result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)

      call model%get_dummy_value(value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
   end subroutine test_get_dummy_value
!$f90tw)

end module test_dflowfm_io
