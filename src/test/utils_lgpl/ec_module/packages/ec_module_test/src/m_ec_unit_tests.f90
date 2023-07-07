!  m_ec_unit_tests.f90
!
!  FUNCTIONS:
!  TestEcGetTimesteps - Unit tests for EcGetTimeSteps
!

!****************************************************************************
!
!  MODULE: m_ec_module_test
!
!  PURPOSE:  Unit test(s) for EC-module
!
!****************************************************************************

module m_ec_unit_tests
   use m_ec_message
   use m_ec_support
   use m_ec_parameters
   use precision
   implicit none

   private

   public :: TestEcGetTimesteps, TestTimestringToUnitAndRefdate1, &
             TestTimestringToUnitAndRefdate2, TestTimestringToUnitAndRefdate3

   character(len=:), allocatable :: dumpedString  !< the last dumped error message
   integer                       :: lastLevel     !< the type of the last dumped error message
   real(kind=hp)   , parameter   :: tol = 1d-10   !< tolerance for comparing doubles

   contains

   subroutine TestEcGetTimesteps(success, errMessage)
      logical,          intent(out) :: success     !< all tests are successful or not
      character(len=*), intent(out) :: errMessage  !< error message in case an error failed

      character(len=*), parameter :: rec1 = 'TIME = 0 hours since 2006-01-01 00:00:00 +00:00'
      character(len=*), parameter :: rec2 = 'TIME = 0 hour since 2006-01-01 00:00:00 +00:00'
      character(len=:), allocatable :: s
      real(kind=hp) :: time_steps
      real(kind=hp), parameter :: time_step_expected1 = 53736.0_hp
      real(kind=hp), parameter :: time_step_expected2 = 0.0_hp

      errMessage = ' '
      call clearECMessage()

      !
      ! test 1: with conversion
      !
      success = ecGetTimesteps(rec1, time_steps, .true.)
      if (success) then
         success = comparereal(time_steps, time_step_expected1, 1d-10) == 0
      endif
      if (.not. success) then
         errMessage = 'error finding time in : ' // rec1
         return
      endif

      !
      ! test 2: without conversion
      !
      success = ecGetTimesteps(rec1, time_steps, .false.)
      if (success) then
         success = comparereal(time_steps, time_step_expected2, 1d-10) == 0
      endif
      if (.not. success) then
         errMessage = 'error finding time in : ' // rec1
         return
      endif

      !
      ! test 3: error handling
      !
      success = .not. ecGetTimesteps(rec2, time_steps)
      if (success) then
         s = dumpECMessageStack(0, copyLastMsg)
         success = (dumpedString == 'ec_support::ecGetTimesteps: can not find time step in: time = 0 hour since 2006-01-01 00:00:00 +00:00.')
      endif
      if (.not. success) then
         errMessage = 'error handling record with unknown time unit'
         return
      endif

      !
      ! test 4: error handling
      !
      success = .not. ecGetTimesteps(' ', time_steps)
      if (success) then
         s = dumpECMessageStack(0, copyLastMsg)
         success = (dumpedString == 'ec_support::ecGetTimesteps: Input string is empty.')
      endif
      if (.not. success) then
         errMessage = 'error handling empty record'
         return
      endif

      call clearECMessage()
   end subroutine TestEcGetTimesteps

   !> test normal NetCDF input for ecSupportTimestringToUnitAndRefdate
   subroutine TestTimestringToUnitAndRefdate1(success, errMessage)
      logical,          intent(out) :: success     !< all tests are successful or not
      character(len=*), intent(out) :: errMessage  !< error message in case an error failed

      character(len=*), parameter :: str1 = "TIME = 0 hours since 2006-01-01 00:00:00 -01:00"
      character(len=*), parameter :: str2 = "minutes since 1970-01-01 00:00:00.0 +0100"
      character(len=*), parameter :: str3 = "seconds since 2020-11-16T07:47:33Z"
      character(len=*), parameter :: str4 = "seconds since 2020-11-16"                 ! no time
      character(len=*), parameter :: str5 = "minutes since 1970-01-01 0:00:00.0 +0100" ! hours in 1 digit
      character(len=*), parameter :: str6 = "seconds since 20201116T074733Z"           ! no '-' and no ':' (allowed in ISO 8601)

      logical :: successArr(6)
      integer :: unit
      real(kind=hp) :: ref_date, tzone

      successArr(1) = ecSupportTimestringToUnitAndRefdate(str1, unit, ref_date, tzone)
      successArr(1) = (successArr(1) .and. unit == ec_hour)
      successArr(1) = (successArr(1) .and. comparereal(ref_date, 53736.0d0, tol) == 0)
      successArr(1) = (successArr(1) .and. comparereal(tzone, -1.0d0, tol) == 0)

      successArr(2) = ecSupportTimestringToUnitAndRefdate(str2, unit, ref_date, tzone)
      successArr(2) = (successArr(2) .and. unit == ec_minute)
      successArr(2) = (successArr(2) .and. comparereal(ref_date, 40587.0d0, tol) == 0)
      successArr(2) = (successArr(2) .and. comparereal(tzone, 1.0d0, tol) == 0)

      successArr(3) = ecSupportTimestringToUnitAndRefdate(str3, unit, ref_date, tzone)
      successArr(3) = (successArr(3) .and. unit == ec_second)
      successArr(3) = (successArr(3) .and. comparereal(ref_date, 59169.3246875d0, tol) == 0)
      successArr(3) = (successArr(3) .and. comparereal(tzone, 0d0, tol) == 0)

      successArr(4) = ecSupportTimestringToUnitAndRefdate(str4, unit, ref_date, tzone)
      successArr(4) = (successArr(4) .and. unit == ec_second)
      successArr(4) = (successArr(4) .and. comparereal(ref_date, 59169.0d0, tol) == 0)
      successArr(4) = (successArr(4) .and. comparereal(tzone, 0d0, tol) == 0)

      successArr(5) = ecSupportTimestringToUnitAndRefdate(str5, unit, ref_date, tzone)
      successArr(5) = (successArr(5) .and. unit == ec_minute)
      successArr(5) = (successArr(5) .and. comparereal(ref_date, 40587.0d0, tol) == 0)
      successArr(5) = (successArr(5) .and. comparereal(tzone, 1.0d0, tol) == 0)

      successArr(6) = ecSupportTimestringToUnitAndRefdate(str6, unit, ref_date, tzone)
      successArr(6) = (successArr(6) .and. unit == ec_second)
      successArr(6) = (successArr(6) .and. comparereal(ref_date, 59169.3246875d0, tol) == 0)
      successArr(6) = (successArr(6) .and. comparereal(tzone, 0d0, tol) == 0)

      success = all(successArr)
      errMessage = ' '
   end subroutine TestTimestringToUnitAndRefdate1

   !> test normal ArcInfo input for ecSupportTimestringToUnitAndRefdate
   subroutine TestTimestringToUnitAndRefdate2(success, errMessage)
      logical,          intent(out) :: success     !< all tests are successful or not
      character(len=*), intent(out) :: errMessage  !< error message in case an error failed

      character(len=*), parameter :: str1 = " TIME (HRS)     18.0 20000101 18"
      character(len=*), parameter :: str2 = " TIME HRS       18.0 20000101 18"
      character(len=*), parameter :: str3 = " TIME HOURS     18.0 20000101 18"

      logical :: successArr(3)
      integer :: unit
      real(kind=hp) :: ref_date, tzone

      successArr(1) = ecSupportTimestringToUnitAndRefdate(str1, unit, ref_date, tzone)
      successArr(1) = (successArr(1) .and. unit == ec_hour)
      successArr(1) = (successArr(1) .and. comparereal(ref_date, 51544.75d0, tol) == 0)
      successArr(1) = (successArr(1) .and. comparereal(tzone, 0d0, tol) == 0)

      successArr(2) = ecSupportTimestringToUnitAndRefdate(str2, unit, ref_date, tzone)
      successArr(2) = (successArr(2) .and. unit == ec_hour)
      successArr(2) = (successArr(2) .and. comparereal(ref_date, 51544.75d0, tol) == 0)
      successArr(2) = (successArr(2) .and. comparereal(tzone, 0d0, tol) == 0)

      successArr(3) = ecSupportTimestringToUnitAndRefdate(str3, unit, ref_date, tzone)
      successArr(3) = (successArr(3) .and. unit == ec_hour)
      successArr(3) = (successArr(3) .and. comparereal(ref_date, 51544.75d0, tol) == 0)
      successArr(3) = (successArr(3) .and. comparereal(tzone, 0d0, tol) == 0)

      success = all(successArr)
      errMessage = ' '
   end subroutine TestTimestringToUnitAndRefdate2

   !> test invalid input for ecSupportTimestringToUnitAndRefdate
   subroutine TestTimestringToUnitAndRefdate3(success, errMessage)
      logical,          intent(out) :: success     !< all tests are successful or not
      character(len=*), intent(out) :: errMessage  !< error message in case an error failed

      character(len=*), parameter :: str1 = "minutes since"                             ! nothing after since
      character(len=*), parameter :: str2 = "minutes since 1970-13-01 00:00:00.0 +0100" ! invalid month
      character(len=*), parameter :: str3 = "minutes since 1970-01-01 25:00:00.0 +0100" ! invalid hours

      logical :: successArr(3)
      integer :: unit
      real(kind=hp) :: ref_date, tzone

      successArr(1) = ecSupportTimestringToUnitAndRefdate(str1, unit, ref_date, tzone)
      successArr(2) = ecSupportTimestringToUnitAndRefdate(str2, unit, ref_date, tzone)
      successArr(3) = ecSupportTimestringToUnitAndRefdate(str3, unit, ref_date, tzone)

      success = all(.not. successArr)
      errMessage = ' '

   end subroutine TestTimestringToUnitAndRefdate3

   !> helper function to copy the last EC message into a module variable
   subroutine copyLastMsg(lvl, msg)
      integer, intent(in)              :: lvl  !< the type of the message
      character(len=*), intent(in)     :: msg  !< the actual message

      lastLevel = lvl
      dumpedString = msg
   end subroutine copyLastMsg

end module m_ec_unit_tests
