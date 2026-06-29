module test_dflowfm_io
   use, intrinsic :: iso_c_binding, only: c_null_char
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use assertions_gtest
   use dflowfm_io

   implicit none

   ! Minimal MDU content used by the round-trip tests.
   character(len=*), parameter :: MINIMAL_MDU = &
      "[general]"   // char(10) // &
      "fileType    = modelDef"      // char(10) // &
      "fileVersion = 1.09"          // char(10) // &
      ""            // char(10) // &
      "[geometry]"  // char(10) // &
      "NetFile     = simplechannel_net.nc"  // char(10) // &
      "DryPointsFile = dry.pol dry.xyz"

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

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_load_from_string, test_load_from_string,
   subroutine test_load_from_string() bind(C)
      type(MduModel) :: model
      integer :: result_code, value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)

      call model%get_int("geometry.kmx", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(value, 0)
   end subroutine test_load_from_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_int, test_set_and_get_int,
   subroutine test_set_and_get_int() bind(C)
      type(MduModel) :: model
      integer :: result_code, value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_int("geometry.kmx", 5, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_int("geometry.kmx", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(value, 5)
   end subroutine test_set_and_get_int
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_bool, test_set_and_get_bool,
   subroutine test_set_and_get_bool() bind(C)
      type(MduModel) :: model
      integer :: result_code
      logical :: value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_bool("geometry.usecaching", .false., result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_bool("geometry.usecaching", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(value, .false.)
   end subroutine test_set_and_get_bool
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_double, test_set_and_get_double,
   subroutine test_set_and_get_double() bind(C)
      type(MduModel) :: model
      integer :: result_code
      real(kind=real64) :: value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_double("numerics.cflmax", 0.9_real64, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_double("numerics.cflmax", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_near(value, 0.9_real64, 1.0e-10_real64)
   end subroutine test_set_and_get_double
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_string, test_set_and_get_string,
   subroutine test_set_and_get_string() bind(C)
      type(MduModel) :: model
      integer :: result_code
      character(len=:), allocatable :: value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_string("general.program", "My Program", result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_string("general.program", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_streq(trim(value)//c_null_char, "My Program"//c_null_char)
   end subroutine test_set_and_get_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_enum, test_set_and_get_enum,
   subroutine test_set_and_get_enum() bind(C)
      type(MduModel) :: model
      integer :: result_code, value

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_enum("general.autostart", 1, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_enum("general.autostart", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(value, 1)
   end subroutine test_set_and_get_enum
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_datetime, test_set_and_get_datetime,
   subroutine test_set_and_get_datetime() bind(C)
      type(MduModel) :: model
      integer :: result_code
      integer(kind=int64) :: epoch

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_datetime("time.refdate", 978307200_int64, result_code) ! 2001-01-01 UTC
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_datetime("time.refdate", epoch, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(int(epoch), 978307200)
   end subroutine test_set_and_get_datetime
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_double_list, test_get_double_list,
   subroutine test_get_double_list() bind(C)
      type(MduModel) :: model
      integer :: result_code
      real(kind=real64), allocatable :: values(:)

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%get_double_list("wind.cdbreakpoints", values, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(size(values), 2)
      call f90_expect_near(values(1), 0.00063_real64, 1.0e-10_real64)
      call f90_expect_near(values(2), 0.00723_real64, 1.0e-10_real64)
   end subroutine test_get_double_list
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_path_list, test_get_path_list,
   subroutine test_get_path_list() bind(C)
      type(MduModel) :: model
      integer :: result_code
      character(len=:), allocatable :: values(:)

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%set_path_list("geometry.drypointsfile", ["pt1.pol", "pt2.pol"], result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call model%get_path_list("geometry.drypointsfile", values, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(size(values), 2)
      call f90_expect_streq(trim(values(1))//c_null_char, "pt1.pol"//c_null_char)
      call f90_expect_streq(trim(values(2))//c_null_char, "pt2.pol"//c_null_char)
   end subroutine test_get_path_list
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_save_to_string, test_save_to_string,
   subroutine test_save_to_string() bind(C)
      type(MduModel) :: model
      integer :: result_code
      character(len=:), allocatable :: text

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%save_to_string(text, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(len(text) > 0, .true.)
   end subroutine test_save_to_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_unknown_key_fails, test_get_unknown_key_fails,
   subroutine test_get_unknown_key_fails() bind(C)
      type(MduModel) :: model
      integer :: result_code, value
      character(len=:), allocatable :: error

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%get_int("nonexisting.key", value, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_ERROR)
      call model%get_last_error(error)
      call f90_expect_eq(len(error) > 0, .true.)
   end subroutine test_get_unknown_key_fails
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_issues, test_get_issues,
   subroutine test_get_issues() bind(C)
      type(MduModel) :: model
      integer :: result_code
      type(MduIssue), allocatable :: issues(:)

      call model%create(result_code)
      call model%load_from_string(MINIMAL_MDU, result_code)

      call model%get_issues(issues, result_code)
      call f90_expect_eq(result_code, DFLOWFM_IO_RESULT_SUCCESS)
      call f90_expect_eq(allocated(issues), .true.)
   end subroutine test_get_issues
!$f90tw)

end module test_dflowfm_io
