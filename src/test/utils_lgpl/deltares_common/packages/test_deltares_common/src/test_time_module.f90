module test_time_module
   !----- LGPL --------------------------------------------------------------------
   !                                                                               
   !  Copyright (C)  Stichting Deltares, 2011-2025.                                
   !                                                                               
   !  This library is free software; you can redistribute it and/or                
   !  modify it under the terms of the GNU Lesser General Public                   
   !  License as published by the Free Software Foundation version 2.1.            
   !                                                                               
   !  This library is distributed in the hope that it will be useful,              
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of               
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
   !  Lesser General Public License for more details.                              
   !                                                                               
   !  You should have received a copy of the GNU Lesser General Public             
   !  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
   !                                                                               
   !  contact: delft3d.support@deltares.nl                                         
   !  Stichting Deltares                                                           
   !  P.O. Box 177                                                                 
   !  2600 MH Delft, The Netherlands                                               
   !                                                                               
   !  All indications and logos of, and references to, "Delft3D" and "Deltares"    
   !  are registered trademarks of Stichting Deltares, and remain the property of  
   !  Stichting Deltares. All rights reserved.                                     
   !                                                                               
   !-------------------------------------------------------------------------------
   !  
   !  
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Tests for various time and date processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp
   use time_module
   use ftnunit
   implicit none

   private

   real(kind=hp), parameter :: tol = 1d-9

   public :: tests_time_module

   contains

      subroutine tests_time_module
         call test( test_conversion2juliandate, 'Test CalendarYearMonthDayToJulianDate' )
         call test( test_julian_gregor, 'Test Julian inverse to Gregor')
         call test( test_date2mjd2date, 'Test CalendarYearMonthDayToModifiedJulianDateAndBack')
         call test( test_mjd2date, 'Test_ModifiedJulianDateToYearMonthDayHourMinuteSecond' )
         call test( test_split_date_time, 'Test split_date_time' )
         call test( test_split_date_time_invalid, 'Test split_date_time with invalid input')
         call test( test_parse_time_valid, 'Test parse_time with valid input')
         call test( test_parse_time_invalid, 'Test parse_time with invalid input')
         call test( test_ymd2modified_jul_string_valid, 'Test ymd2modified_jul_string with valid input')
         call test( test_ymd2modified_jul_string_invalid, 'Test ymd2modified_jul_string with invalid input')
         call test( test_datetimestring_to_seconds, 'Test datetimestring_to_seconds' )
         call test( test_seconds_to_datetimestring, 'Test seconds_to_datetimestring' )
      end subroutine tests_time_module

      !> test CalendarYearMonthDayToJulianDate
      subroutine test_conversion2juliandate()

         integer :: jdn1, jdn2, jdn3, jdn4, yyyymmdd, yyyymmdd2, istat
         real(kind=hp) :: mjd, mjd2

         jdn1 = CalendarYearMonthDayToJulianDateNumber(1, 1, 1)
         jdn2 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 4)
         jdn3 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 15)
         jdn4 = CalendarYearMonthDayToJulianDateNumber(2001, 1, 1)

         call assert_equal(jdn1, 1721424, 'error for 1-1-1')
         call assert_equal(jdn2, 2299160, 'error for 4-10-1582')
         call assert_equal(jdn3, 2299161, 'error for 15-10-1582')
         call assert_equal(jdn4, 2451911, 'error for 1-1-2001')

         mjd = 55833.5_hp
         istat = mjd2date(mjd, yyyymmdd)
         mjd2 = date2mjd(yyyymmdd)
         call assert_comparable (mjd, mjd2, tol, 'error for mjd=55833.5')

         yyyymmdd = 15821015
         mjd = date2mjd(yyyymmdd)
         istat = mjd2date(mjd, yyyymmdd2)
         call assert_equal (yyyymmdd, yyyymmdd2, 'error for yyyymmdd = 15821015')

         yyyymmdd = 15821004
         mjd2 = date2mjd(yyyymmdd)
         istat = mjd2date(mjd2, yyyymmdd2)
         call assert_equal (istat, 1, 'mjd2date failed for yyyymmdd = 15821004')
         call assert_equal (yyyymmdd, yyyymmdd2, 'error for yyyymmdd = 15821004')
         call assert_false (abs(mjd - mjd2 - 1d0) > 1d-4, 'error for jump 15821004 -> 15821015')

      end subroutine test_conversion2juliandate

      !> test Julian inverse to Gregor
      subroutine test_julian_gregor

         real(kind=hp) :: jdn, dsec
         integer :: returnyear, returnmonth, returnday, hour, min, sec
         integer :: year, month, day

         jdn = julian(00010101, 0)
         call assert_comparable(jdn, -678577.0_hp + offset_modified_jd, tol, 'error for 1-1-1')

         jdn = julian(15821004, 0)
         call assert_comparable(jdn, -100841.0_hp + offset_modified_jd, tol, 'error for 4-10-1582')
         
         jdn = julian(15821015, 0)
         call assert_comparable(jdn, -100840.0_hp + offset_modified_jd, tol, 'error for 15-10-1582')

         year = 2001
         month = 01
         day = 01
         jdn = julian(year*10000 + month * 100 + day, 0)
         call assert_comparable(jdn, 51910.0_hp + offset_modified_jd, tol, 'error for 1-1-2001')         
         call gregor(jdn, returnyear, returnmonth, returnday, hour, min, sec, dsec)

         call assert_equal(year, returnyear, 'error in Julian/Gregor: incorrect year')
         call assert_equal(month, returnmonth, 'error in Julian/Gregor: incorrect month')
         call assert_equal(day, returnday, 'error in Julian/Gregor: incorrect day')
         
      end subroutine test_julian_gregor

      !> test yyyymmddToModifieldJulianDateToyyyymmdd
      subroutine test_date2mjd2date()
         implicit none
         logical       :: success_
         real(kind=hp) :: expected_mjd, refdate_mjd
         integer       :: refdate, returndate, returntime, istat
         
         refdate = 20221101
         expected_mjd = 59884.0_hp
         success_ = ymd2modified_jul(refdate, refdate_mjd)
         call assert_comparable(refdate_mjd, expected_mjd, tol, 'error in conversion ymd to modified julian date')
         istat = mjd2date(refdate_mjd, returndate)
         call assert_equal(refdate, returndate,'error in conversion modified julian date to ymd')
         istat = mjd2date(refdate_mjd, returndate, returntime)
         ! check that no time shift was introduced
         call assert_equal(returntime, 0,'error in mjd2date, unexpected timeshift')
                  
      end subroutine test_date2mjd2date

      !> test ModifiedJulianDateToYearMonthDayHourMinuteSecond
      subroutine test_mjd2date()
         implicit none
         integer       :: success
         integer       :: ymd, ymd_expected
         integer       :: hms, hms_expected
         integer       :: year, year_expected
         integer       :: month, month_expected
         integer       :: day, day_expected
         integer       :: hour, hour_expected
         integer       :: minute, minute_expected, second_expected
         real(kind=hp) :: second
         real(kind=hp) :: timestamp_mjd
 
         ! check correct date and hhmmss for 3h30 ie before noon and whole minutes
         timestamp_mjd = 55833.125_hp + 30./1440.
         ymd_expected = 20110929
         hms_expected = 33000
         success = mjd2date(timestamp_mjd,ymd)
         call assert_equal(ymd_expected, ymd,'error in conversion modified julian date to yymmdd')

         success = mjd2date(timestamp_mjd, ymd, hms)
         call assert_equal(hms_expected, hms,'error in conversion modified julian date for hhmmss')
 
         ! check correct date and hour for 18h i.e. after noon and with seconds
         timestamp_mjd = 55833.80
         year_expected = 2011
         month_expected = 9
         day_expected = 29
         hour_expected = 19
         minute_expected = 13
         second_expected = 7
         success = mjd2date(timestamp_mjd,year,month,day,hour,minute,second)
         call assert_equal(year_expected, year,'error in conversion modified julian date: year')
         call assert_equal(month_expected, month,'error in conversion modified julian date: month')
         call assert_equal(day_expected, day,'error in conversion modified julian date: day')
         call assert_equal(hour_expected, hour,'error in conversion modified julian date: hour')
         call assert_equal(minute_expected, minute,'error in conversion modified julian date: minute')
         call assert_equal(second_expected, int(second), 'error in conversion modified julian date: second')

         ! check that rounding is correct for end of year
         ! 20221231 = 59944 in mjd, 1 second = 1.157407E-5
         timestamp_mjd = 59944.999985_hp
         ymd_expected = 20221231
         hms_expected = 235959
         success = mjd2date(timestamp_mjd, ymd, hms)
         call assert_equal(hms_expected, hms, 'error in conversion modified julian date: second')
         
         timestamp_mjd = 59944.999999_hp
         ymd_expected = 20230101
         hms_expected = 0
         success = mjd2date(timestamp_mjd, ymd, hms)
         call assert_equal(hms_expected, hms, 'error in conversion modified julian date: second')

      end subroutine test_mjd2date

      !> test split_date_time with valid input
      subroutine test_split_date_time
         character(len=16) :: date, time, tz
         character(len=26) :: date_time
         logical           :: success
         character(len=26), parameter :: date_times (9) = (/ &
            "2010-04-04 00:07:02       ", &
            "2011-05-05T01:08:03       ", &
            "2012-06-06 02:09:04 +02:00", &
            "2013-07-07T03:10:05 +01:00", &
            "2014-08-08 04:11:06 -01:00", &
            "2015-09-09T05:12:07 -02:00", &
            "2016-10-10 06:13:08Z      ", &
            "2017-11-11T07:14:09Z      ", &
            "2018-12-12                " /)
         integer :: i

         do i = 1, size(date_times)
            date_time = date_times(i)
            success = split_date_time(date_time, date, time, tz)
            call assert_true(success, 'success for ' // date_time)
            call assert_equal(date, date_time(1:10), 'diff. in date for ' // date_time)
            call assert_equal(time, date_time(12:19), 'diff. in time for ' // date_time)
            call assert_equal(tz, adjustl(date_time(20:)), 'diff. in time zone for ' // date_time)
         end do

         date_time = "2014-08-08 04:11:06.123"  ! longer time: has a fractial number of seconds
         success = split_date_time(date_time, date, time, tz)
         call assert_true(success, 'success for ' // date_time)
         call assert_equal(date, date_time(1:10), 'diff. in date for ' // date_time)
         call assert_equal(time, date_time(12:), 'diff. in time for ' // date_time)
         call assert_equal(tz, ' ', 'diff. in time zone for ' // date_time)

      end subroutine test_split_date_time

      !> test split_date_time with invalid input
      subroutine test_split_date_time_invalid
         character(len=16) :: date, time, tz
         character(len=26) :: date_time
         logical           :: success
         character(len=26), parameter :: date_times (3) = (/ &
            "2010-04                   ", &  ! date to short
            "2011-05-05t01:08:03       ", &  ! t as separator (must be upper case)
            "2012-06-06 02:09:04  02:00" /)  ! no sign in time zone
         integer :: i

         do i = 1, size(date_times)
            date_time = date_times(i)
            success = split_date_time(date_time, date, time, tz)
            call assert_false(success, 'unexpected success for ' // date_time)
         end do

      end subroutine test_split_date_time_invalid

      !> test parse time with valid input
      subroutine test_parse_time_valid
         integer, parameter           :: nr_cases = 9
         character(len=16), parameter :: times(nr_cases) = (/ &
            "01:02:03        ", &
            "04:05:06.7      ", &
            "07:47:33        ", &
            "8:10:20.3       ", &    ! one digit for hour and with fractional seconds (is allowed)
            "8:10:20         ", &    ! one digit for hour (is allowed)
            "123456          ", &    ! no splitters
            "014554.44       ", &    ! no splitter, but with fractional seconds
            "14:50           ", &    ! no seconds
            "0912            "/)     ! no seconds and no splitters
         real(kind=hp), parameter :: fraction_expected(nr_cases) = (/ 0.0430902777778_hp, 0.170216435185_hp, 0.3246875_hp, &
                                                                      0.340512731481_hp, 0.340509259259_hp, 0.524259259259_hp, &
                                                                      0.073546759259_hp, 0.618055555555_hp, 0.383333333333_hp /)
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: fraction

         do i = 1, nr_cases
            fraction = parse_time(times(i), ok)
            call assert_true( ok, "unexpected success status (must succeed)")
            call assert_comparable(fraction, fraction_expected(i), tol, "difference in fraction")
         end do
      end subroutine test_parse_time_valid

      !> test parse time
      !! both valid and invalid input
      subroutine test_parse_time_invalid
         character(len=20), parameter :: times(9) = (/ &
            "                    ", &    ! no time at all
            "25:00:00            ", &    ! invalid hh
            "23:65:28            ", &    ! invalid mm
            "12:00:99            ", &    ! invalid ss
            "12:22:206577.31415  ", &    ! invalid ss combined with fractional seconds
            "250000              ", &    ! invalid hh, without ':'
            "236528              ", &    ! invalid mm, without ':'
            "120099              ", &    ! invalid ss, without ':'
            "1222206577.31415    " /)    ! invalid ss, without ':', combined with fractional seconds
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: fraction

         do i = 1, size(times)
            fraction = parse_time(times(i), ok)
            call assert_false( ok, "unexpected success status (must fail)")
         end do
      end subroutine test_parse_time_invalid

      subroutine test_ymd2modified_jul_string_valid
         integer, parameter           :: nr_cases = 9
         character(len=16), parameter :: date(nr_cases) = (/ &
            "20200904        ", &   ! no separat
            "0020200904        ", & ! no separators, six digit year
            "2020-09-04      ", &   ! - separators
            "2020-09-4       ", &   ! - separators, one digit day, two digit month
            "2020-9-4        ", &   ! - seperators, one digit day, one digit month
            "2020-9-004       ", &  ! - separator, three digit day, one digit month
            "2020 9 04       ", &   ! space separator, two digit day, one digit month
            "002020 9 04       ", & ! space separator, six digit year
            "2020/9/04       "/)    ! / separator, one digit day, two digit month
         real(kind=hp), parameter :: mjd_expected = 59096.0_hp
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: mjd

         do i = 1, nr_cases
            ok = ymd2modified_jul(date(i), mjd)
            call assert_true( ok, "unexpected success status (must succeed)")
            call assert_comparable(mjd, mjd_expected, tol, "difference in Modified Julian Day")
         end do
      end subroutine test_ymd2modified_jul_string_valid
      
      subroutine test_ymd2modified_jul_string_invalid
         integer, parameter           :: nr_cases = 7
         character(len=16), parameter :: date(nr_cases) = (/ &
            "20201904        ", &   ! no separators, month 19
            "2020-09-00      ", &   ! - separators, day zero
            "2A21-04-31      ", &   ! mutilated year
            "2021-B2-28      ", &   ! mutilated month
            "20A1-02-2C      ", &   ! mutilated day
            "202103-01       ", &   ! - seperators, one digit day, one digit month, 29 feb in no-leapyear
            "2020/0/04       "/)    ! / separator, one digit day, two digit month, month 0
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: mjd

         do i = 1, nr_cases
            ok = ymd2modified_jul(date(i), mjd)
            call assert_false( ok, "unexpected success status (must fail)")
         end do
      end subroutine test_ymd2modified_jul_string_invalid
      

      subroutine test_datetimestring_to_seconds()
        double precision    :: timsec
        integer             :: stat
      
        call datetimestring_to_seconds('20120101000100','20120101',timsec,stat)     ! easy for now, delta = 1 minute
        call assert_comparable(timsec, 60.0_hp, tol, 'error for timsec = 201201010001')
      
      end subroutine test_datetimestring_to_seconds
      
      subroutine test_seconds_to_datetimestring()
        character(len=15) :: dateandtime
        
        dateandtime = '00000000_000000'
        call seconds_to_datetimestring(dateandtime,'20120101',60.0_hp)              ! easy for now, delta = 1 minute
        call assert_equal( trim(dateandtime), '20120101_000100', 'error for datetimestring = 20120101000001' )
      
      end subroutine test_seconds_to_datetimestring
      
end module test_time_module
