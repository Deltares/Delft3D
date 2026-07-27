module test_dflowfm_io
   use, intrinsic :: iso_c_binding, only: c_null_char
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use assertions_gtest
   use dflowfm_io

   implicit none

   ! Minimal MDU content used by the round-trip tests.
   character(len=*), parameter :: MINIMAL_MDU = &
                                  "[general]"//char(10)// &
                                  "fileType    = modelDef"//char(10)// &
                                  "fileVersion = 1.09"//char(10)// &
                                  ""//char(10)// &
                                  "[geometry]"//char(10)// &
                                  "NetFile     = simplechannel_net.nc"//char(10)// &
                                  "DryPointsFile = dry.pol dry.xyz"

contains

   subroutine terminate_on_error(message)
      character(len=*), intent(in) :: message
      error stop 'FATAL ERROR IN TEST, ABORTING: '//trim(message)
   end subroutine terminate_on_error

   subroutine ignore_error(message)
      character(len=*), intent(in) :: message
      print *, "Ignoring error in test: ", trim(message)
   end subroutine ignore_error

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_create_and_destroy, test_create_and_destroy,
   subroutine test_create_and_destroy() bind(C)
      type(MduModel) :: model
      logical :: success = .false.

      call f90_expect_eq(model%has_valid_handle(), .false.)
      call model%create(success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(model%has_valid_handle(), .true.)

      ! Call finalizer manually
      call model%destroy(success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(model%has_valid_handle(), .false.)
   end subroutine test_create_and_destroy
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_create_and_go_out_of_scope, test_create_and_go_out_of_scope,
   subroutine test_create_and_go_out_of_scope() bind(C)
      type(MduModel) :: model
      logical :: success = .false.

      call f90_expect_eq(model%has_valid_handle(), .false.)
      call model%create(success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(model%has_valid_handle(), .true.)

      ! Object goes out of scope, finalizer should be called automatically
   end subroutine test_create_and_go_out_of_scope
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_load_from_string, test_load_from_string,
   subroutine test_load_from_string() bind(C)
      type(MduModel) :: model
      integer :: value
      logical :: success = .false.

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%get_int("geometry.kmx", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(value, 0)
   end subroutine test_load_from_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_save_and_load_from_file, test_save_and_load_from_file,
   subroutine test_save_and_load_from_file() bind(C)
      character(len=*), parameter :: filename = "test_dflowfm_io_roundtrip.mdu"
      type(MduModel) :: source_model
      type(MduModel) :: loaded_model
      character(len=:), allocatable :: value
      logical :: success = .false.
      integer :: unit
      integer :: io_status

      call source_model%create(success, terminate_on_error)
      call source_model%load_from_string(MINIMAL_MDU, success, terminate_on_error)
      call source_model%set_string("general.program", "File round trip", success, terminate_on_error)
      call source_model%save_to_file(filename, success, terminate_on_error)
      call f90_expect_eq(success, .true.)

      call loaded_model%create(success, terminate_on_error)
      call loaded_model%load_from_file(filename, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call loaded_model%get_string("general.program", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_streq(trim(value)//c_null_char, "File round trip"//c_null_char)

      open (newunit=unit, file=filename, status="old", iostat=io_status)
      call f90_expect_eq(io_status, 0)
      if (io_status == 0) close (unit, status="delete")
   end subroutine test_save_and_load_from_file
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_int, test_set_and_get_int,
   subroutine test_set_and_get_int() bind(C)
      type(MduModel) :: model
      integer :: value
      logical :: success = .false.

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_int("geometry.kmx", 5, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_int("geometry.kmx", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(value, 5)
   end subroutine test_set_and_get_int
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_bool, test_set_and_get_bool,
   subroutine test_set_and_get_bool() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      logical :: value

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_bool("geometry.usecaching", .false., success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_bool("geometry.usecaching", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(value, .false.)
   end subroutine test_set_and_get_bool
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_double, test_set_and_get_double,
   subroutine test_set_and_get_double() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      real(kind=real64) :: value

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_double("numerics.cflmax", 0.9_real64, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_double("numerics.cflmax", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_near(value, 0.9_real64, 1.0e-10_real64)
   end subroutine test_set_and_get_double
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_string, test_set_and_get_string,
   subroutine test_set_and_get_string() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      character(len=:), allocatable :: value

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_string("general.program", "My Program", success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_string("general.program", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_streq(trim(value)//c_null_char, "My Program"//c_null_char)
   end subroutine test_set_and_get_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_path, test_set_and_get_path,
   subroutine test_set_and_get_path() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      character(len=:), allocatable :: value

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_path("geometry.netfile", "new_net.nc", success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_path("geometry.netfile", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_streq(trim(value)//c_null_char, "new_net.nc"//c_null_char)
   end subroutine test_set_and_get_path
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_enum, test_set_and_get_enum,
   subroutine test_set_and_get_enum() bind(C)
      type(MduModel) :: model
      integer :: value
      logical :: success = .false.

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_enum("general.autostart", 1, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_enum("general.autostart", value, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(value, 1)
   end subroutine test_set_and_get_enum
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_datetime, test_set_and_get_datetime,
   subroutine test_set_and_get_datetime() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      integer(kind=int64) :: epoch

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_datetime("time.refdate", 978307200_int64, success, terminate_on_error) ! 2001-01-01 UTC
      call f90_expect_eq(success, .true.)
      call model%get_datetime("time.refdate", epoch, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(int(epoch), 978307200)
   end subroutine test_set_and_get_datetime
!$f90tw)

! The production MDU schema currently has no string-list property. Exercise
! both API functions with a known key of another type and verify their errors.
!$f90tw TESTCODE(TEST, test_dflowfm_io, test_string_list_type_mismatch_fails, test_string_list_type_mismatch_fails,
   subroutine test_string_list_type_mismatch_fails() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      character(len=:), allocatable :: values(:)

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_string_list("general.program", ["First ", "Second"], success, ignore_error)
      call f90_expect_eq(success, .false.)
      call model%get_string_list("general.program", values, success, ignore_error)
      call f90_expect_eq(success, .false.)
      call f90_expect_eq(allocated(values), .false.)
   end subroutine test_string_list_type_mismatch_fails
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_set_and_get_double_list, test_set_and_get_double_list,
   subroutine test_set_and_get_double_list() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      real(kind=real64), allocatable :: values(:)

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_double_list("wind.cdbreakpoints", &
                                 [0.001_real64, 0.005_real64, 0.01_real64], &
                                 success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_double_list("wind.cdbreakpoints", values, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(size(values), 3)
      call f90_expect_near(values(1), 0.001_real64, 1.0e-10_real64)
      call f90_expect_near(values(2), 0.005_real64, 1.0e-10_real64)
      call f90_expect_near(values(3), 0.01_real64, 1.0e-10_real64)
   end subroutine test_set_and_get_double_list
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_path_list, test_get_path_list,
   subroutine test_get_path_list() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      character(len=:), allocatable :: values(:)

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%set_path_list("geometry.drypointsfile", ["pt1.pol", "pt2.pol"], success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call model%get_path_list("geometry.drypointsfile", values, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(size(values), 2)
      call f90_expect_streq(trim(values(1))//c_null_char, "pt1.pol"//c_null_char)
      call f90_expect_streq(trim(values(2))//c_null_char, "pt2.pol"//c_null_char)
   end subroutine test_get_path_list
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_save_to_string, test_save_to_string,
   subroutine test_save_to_string() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      character(len=:), allocatable :: text

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%save_to_string(text, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(len(text) > 0, .true.)
   end subroutine test_save_to_string
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_unknown_key_fails, test_get_unknown_key_fails,
   subroutine test_get_unknown_key_fails() bind(C)
      type(MduModel) :: model
      integer :: value
      logical :: success = .false.
      character(len=:), allocatable :: error

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%get_int("nonexisting.key", value, success, ignore_error)
      call f90_expect_eq(success, .false.)
      call model%get_last_error(error)
      call f90_expect_eq(len(error) > 0, .true.)
   end subroutine test_get_unknown_key_fails
!$f90tw)

!$f90tw TESTCODE(TEST, test_dflowfm_io, test_get_issues, test_get_issues,
   subroutine test_get_issues() bind(C)
      type(MduModel) :: model
      logical :: success = .false.
      type(MduIssue), allocatable :: issues(:)

      call model%create(success, terminate_on_error)
      call model%load_from_string(MINIMAL_MDU, success, terminate_on_error)

      call model%get_issues(issues, success, terminate_on_error)
      call f90_expect_eq(success, .true.)
      call f90_expect_eq(allocated(issues), .true.)
   end subroutine test_get_issues
!$f90tw)

end module test_dflowfm_io
