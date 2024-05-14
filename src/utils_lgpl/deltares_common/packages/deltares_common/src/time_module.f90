module time_module
   !----- LGPL --------------------------------------------------------------------
   !                                                                               
   !  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
   !    Function: - Various time processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp
   use m_logger, only :  write_log_message
   implicit none

   private

   public :: time_module_info
   public :: datetime2sec
   public :: sec2ddhhmmss
   public :: ymd2jul, ymd2modified_jul
   public :: jul2mjd 
   public :: date2mjd   ! obsolete, use ymd2modified_jul
   public :: mjd2date
   public :: duration_to_string
   public :: datetime_to_string
   public :: parse_ud_timeunit
   public :: parse_time
   public :: split_date_time
   public :: CalendarYearMonthDayToJulianDateNumber
   public :: offset_modified_jd
   public :: julian, gregor, julian_with_leapyears  ! public only for testing in test_time_module.f90
   public :: datetimestring_to_seconds
   public :: seconds_to_datetimestring


   interface ymd2jul
      ! obsolete, use ymd2modified_jul
      module procedure CalendarDateToJulianDateNumber
      module procedure CalendarYearMonthDayToJulianDateNumber
   end interface ymd2jul

   interface date2mjd
      ! obsolete, use ymd2modfified_jul
      module procedure ymd2mjd
      module procedure datetime2mjd
      module procedure ymdhms2mjd
   end interface date2mjd

   interface ymd2modified_jul
      module procedure ymd2modified_jul_string
      module procedure ymd2modified_jul_int
      module procedure ymd2modified_jul_int3
   end interface ymd2modified_jul

   interface mjd2date
      module procedure mjd2datetime
      module procedure mjd2ymd
      module procedure mjd2ymdhms
   end interface mjd2date

   interface datetime_to_string
      module procedure datetime2string
      module procedure jul_frac2string
      module procedure mjd2string
   end interface datetime_to_string

   real(kind=hp), parameter :: offset_modified_jd  = 2400000.5_hp
   integer      , parameter :: firstGregorianDayNr = 2299161
   integer      , parameter :: justBeforeFirstGregorian(3) = [1582, 10, 14]
   integer      , parameter :: justAfterLastJulian(3)      = [1582, 10,  5]

   contains

      ! ------------------------------------------------------------------------------
      !   Subroutine: time_module_info
      !   Purpose:    Add info about this time module to the messages stack
      !   Summary:    Add id string and URL
      !   Arguments:
      !   messages    Stack of messages to add the info to
      ! ------------------------------------------------------------------------------
      subroutine time_module_info(messages)
          use message_module
          !
          ! Arguments
          !
          type(message_stack), pointer :: messages
          !
          !! executable statements ---------------------------------------------------
          !
          call addmessage(messages,'')
          call addmessage(messages,'$URL$')
      end subroutine time_module_info

      ! ------------------------------------------------------------------------------
      !   Subroutine: datetime2sec
      !   Purpose:    Convert a 6 integer datetime vector to a number of seconds
      !   Arguments:
      !   datetime    Integer array of length 6 containing date yr,mo,dy and time hr,mn,sc
      !   refdatetime Optional integer array of length 6 containing reference date yr,mo,dy and time hr,mn,sc
      !   sec         If refdatetime is specified: time difference in seconds
      !               Otherwise: time in seconds since julday = 0 (only to be used for time steps)
      ! ------------------------------------------------------------------------------
      function datetime2sec(datetime, refdatetime) result (sec)
          !
          ! Arguments
          !
          integer, dimension(6)           , intent(in)    :: datetime
          integer, dimension(6) , optional, intent(in)    :: refdatetime
          integer                                         :: sec
          !
          ! Local variables
          !
          integer :: jd    ! Julian date
          integer :: refjd ! Julian refernce date
          !
          !! executable statements ---------------------------------------------------
          !
          jd    = ymd2jul(datetime(1), datetime(2), datetime(3))
          !
          if (present(refdatetime)) then
             refjd = ymd2jul(refdatetime(1), refdatetime(2), refdatetime(3))
             ! difference in days
             sec   = jd - refjd
             ! difference in hours
             sec   = sec*24 + datetime(4) - refdatetime(4)
             ! difference in minutes
             sec   = sec*60 + datetime(5) - refdatetime(5)
             ! difference in seconds
             sec   = sec*60 + datetime(6) - refdatetime(6)
          else
             ! time in hours
             sec   = jd*24 + datetime(4)
             ! time in minutes
             sec   = sec*60 + datetime(5)
             ! time in seconds
             sec   = sec*60 + datetime(6)
          endif
      end function datetime2sec

      ! ------------------------------------------------------------------------------
      !   Subroutine: sec2ddhhmmss
      !   Purpose:    Convert a number of seconds to ddhhmmss integer
      !   Arguments:
      !   sec         Number of seconds
      !   ddhhmmss    Integer with days, hours, minutes and seconds formatted as ddhhmmss
      ! ------------------------------------------------------------------------------
      function sec2ddhhmmss(sec) result (ddhhmmss)
          !
          ! Arguments
          !
          integer                         , intent(in)    :: sec
          integer                                         :: ddhhmmss
          !
          ! Local variables
          !
          integer :: dd ! days
          integer :: hh ! hours
          integer :: mm ! minutes
          integer :: ss ! seconds
          !
          !! executable statements ---------------------------------------------------
          !
          ! time in seconds
          dd = sec
          ss = mod(dd,60)
          ! time in minutes
          dd = (dd - ss)/60
          mm = mod(dd,60)
          ! time in hours
          dd = (dd - mm)/60
          hh = mod(dd,24)
          ! time in days
          dd = (dd - hh)/24
          !
          ddhhmmss = ss + 100*mm + 10000*hh + 1000000*dd
      end function sec2ddhhmmss

!---------------------------------------------------------------------------------------------
! implements interface ymd2modified_jul
!---------------------------------------------------------------------------------------------
      !> calculates modified Julian Date base on a string 'yyyyddmm' with or without separators
      function ymd2modified_jul_string(date, modified_jul_date) result (success)
         use string_module, only: strsplit
         character(len=*), intent(in) :: date             !< date as string 'yyyyddmm' or 'yyyy dd mm' or 'yyyy d m'
         real(kind=hp), intent(out)   :: modified_jul_date !< returned date as modified julian
         logical                      :: success          !< function result

         integer :: year, month, day, ierr, npc, intdate
         character :: separator
         logical :: has_separators
         character(len=20) :: fmt
         character(len=12), dimension(:), allocatable :: date_elements

         success = .false.
         if (index(date,'/') > 0) then
             separator = '/'
         elseif (index(date,'-') > 0) then
             separator = '-'
         else
             separator = ' '
         endif
         call strsplit(date,1,date_elements,1,separator)

         npc = size(date_elements)
         if (npc >= 3) then
            read(date_elements(1),*,iostat=ierr) year
            if (ierr /= 0) return
            read(date_elements(2),*,iostat=ierr) month
            if (ierr /= 0) return
            read(date_elements(3),*,iostat=ierr) day
            if (ierr /= 0) return
         elseif (npc == 1) then
            read(date_elements(1),*,iostat=ierr) intdate
            if (ierr /= 0) return
            year = int(intdate/10000)
            month = int(mod(intdate,10000)/100)
            day = mod(intdate,100)
         else
            return
         endif

         if (month>=1 .and. month <= 12 .and. day>=1 .and. year>=1) then
            modified_jul_date = julian(year*10000 + month * 100 + day, 0)
            if (modified_jul_date == -1) return 
            modified_jul_date = modified_jul_date - offset_modified_jd 
         else
            return
         endif
         success = .true. 
      end function ymd2modified_jul_string

      !> calculates modified Julian Date base on a integer yyyyddmm
      function ymd2modified_jul_int(yyyymmdd, modified_jul_date) result(success)
         integer,       intent(in)  :: yyyymmdd          !< date as integer yyyymmdd
         real(kind=hp), intent(out) :: modified_jul_date  !< output modified Julian Date number
         logical                    :: success           !< function result

         integer :: year, month, day

         call splitDate(yyyymmdd, year, month, day)

         success = ymd2modified_jul_int3(year, month, day, modified_jul_date)

      end function ymd2modified_jul_int

      !> calculates modified Julian Date base on integers year, month and day
      function ymd2modified_jul_int3(year, month, day, modified_jul_date) result(success)
         integer      , intent(in)  :: year              !< year
         integer      , intent(in)  :: month             !< month
         integer      , intent(in)  :: day               !< day
         real(kind=hp), intent(out) :: modified_jul_date !< output modified Julian Date number
         logical                    :: success           !< function result

         integer :: jdn       
         real(kind=hp) :: jd 
         
         jdn = CalendarYearMonthDayToJulianDateNumber(year, month, day)
         ! jdn is an integer value (and the result of an integer computation). 
         ! To compute the Julian date at YYYYMMDDhhmmss as a real number for a moment 
         ! after 12:00 noon one must add (hh - 12)/24 + mm/1440 + sec/86400 (real divisions). 
         ! 
         ! In this function, only calendar days starting at midnight, are assumed. 
         ! For midnight, exactly 12 hours before noon, one must add (0-12)/24 + 0 + 0 = -0.5
         jd = real(jdn, hp) - real(0.5, hp)
         
         if (jdn == 0) then
            modified_jul_date = 0.0_hp
            success = .false.
         else
            modified_jul_date = jd - offset_modified_jd
            success = .true.
         endif

      end function ymd2modified_jul_int3

!---------------------------------------------------------------------------------------------
! implements interface ymd2jul
!---------------------------------------------------------------------------------------------
      !> Calculates the Julian Date Number from a calender date.
      !! Returns 0 in case of failure.
      function CalendarDateToJulianDateNumber(yyyymmdd) result(jdn)
         integer             :: jdn      !< calculated Julian Date Number
         integer, intent(in) :: yyyymmdd !< Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         call splitDate(yyyymmdd, year, month, day)
         jdn = CalendarYearMonthDayToJulianDateNumber(year, month, day)
      end function CalendarDateToJulianDateNumber

      !> helper function to split integer yyyyddmm in 3 integers year, month and day
      subroutine splitDate(yyyymmdd, year, month, day)
         integer, intent(in)  :: yyyymmdd !< calender date
         integer, intent(out) :: year     !< year
         integer, intent(out) :: month    !< month
         integer, intent(out) :: day      !< day

         year = yyyymmdd/10000
         month = yyyymmdd/100 - year*100
         day = yyyymmdd - month*100 - year*10000
      end subroutine splitDate

      !> Calculates the Julian Date Number from a year, month and day.
      !! Returns 0 in case of failure.
      function CalendarYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
         integer :: jdn               !< calculated Julian Date Number
         integer, intent(in) :: year  !< year
         integer, intent(in) :: month !< month
         integer, intent(in) :: day   !< day

         if (compareDates([year, month, day], justBeforeFirstGregorian) == 1) then
            jdn = GregorianYearMonthDayToJulianDateNumber(year, month, day)
         else if (compareDates([year, month, day], justAfterLastJulian) == -1) then
            jdn = JulianYearMonthDayToJulianDateNumber(year, month, day)
         else
            jdn = 0
         endif
      end function CalendarYearMonthDayToJulianDateNumber

      !> helper function to compare dates
      !> return 0 if equal, -1 if x is earlier than y, +1 otherwise
      integer function compareDates(x, y)
         integer, intent(in) :: x(3)  !< date 1 : [year, month, day]
         integer, intent(in) :: y(3)  !< date 2 (idem)

         integer :: i

         compareDates = 0
         do i = 1, 3
            if (x(i) < y(i)) then
               compareDates = -1
               exit
            else if (x(i) > y(i)) then
               compareDates = 1
               exit
            endif
         enddo
      end function compareDates

      !> calculates Julian Date Number based on date before oktober 1582
      function JulianYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
      integer, intent(in) :: year  !< year
      integer, intent(in) :: month !< month
      integer, intent(in) :: day   !< day
      integer             :: jdn   !< function result: Juliun day number

      integer :: y, m, d
      real(kind=hp) :: jd

      jd = JulianYearMonthDayToJulianDateRealNumber(year, month, day) + 0.5_hp
      jdn = nint(jd)

      !
      ! Calculate backwards to test if the assumption is correct
      !
      call JulianDateNumberToCalendarYearMonthDay(jdn, y, m, d)
      !
      ! Test if calculation is correct
      !
      if (CompareDates([y, m, d], [year, month, day]) /= 0) then
         jdn = 0
      endif

      end function JulianYearMonthDayToJulianDateNumber

      !> from https://quasar.as.utexas.edu/BillInfo/JulianDateCalc.html
      function JulianYearMonthDayToJulianDateRealNumber(year, month, day) result(jdn)
      integer, intent(in) :: year  !< year
      integer, intent(in) :: month !< month
      integer, intent(in) :: day   !< day
      real(kind=hp)       :: jdn   !< function result: Julian day number

      integer       :: y, m
      real(kind=hp) :: e, f

      y = year
      m = month
      if (m < 3) then
         y = y-1
         m = m + 12
      endif
      e = floor(365.25_hp  * real(y + 4716, hp))
      f = floor(30.6001_hp * real(m + 1, hp))

      jdn = day + e + f - 1524.5_hp
      end function JulianYearMonthDayToJulianDateRealNumber

      !> Calculates the Julian Date Number from a Gregorian calender date.
      !! Returns 0 in case of failure.
      function GregorianDateToJulianDateNumber(yyyymmdd) result(jdn)
         integer             :: jdn      !< calculated Julian Date Number
         integer, intent(in) :: yyyymmdd !< Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         call splitDate(yyyymmdd, year, month, day)
         !
         jdn = GregorianYearMonthDayToJulianDateNumber(year, month, day)
      end function GregorianDateToJulianDateNumber

      ! =======================================================================

      !> Calculates the Julian Date Number from a Gregorian year, month and day.
      !! Returns 0 in case of failure.
      function GregorianYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
         integer :: jdn               !< calculated Julian Date Number
         integer, intent(in) :: year  !< Gregorian year
         integer, intent(in) :: month !< Gregorian month
         integer, intent(in) :: day   !< Gregorian day
         !
         integer :: month1 !< helper variable
         integer :: y      !< helper variable
         integer :: m      !< helper variable
         integer :: d      !< helper variable
         !
         ! Calculate Julian day assuming the given month is correct.
         ! This is an integer computation, divisions are integer divisions towards zero.
         !
         month1 = (month - 14)/12
         jdn = day - 32075 + 1461*(year + 4800 + month1)/4 &
                           + 367*(month - 2 - month1*12)/12 &
                           - 3*((year + 4900 + month1)/100)/4
         !
         ! Calculate backwards to test if the assumption is correct
         !
         call JulianDateNumberToCalendarYearMonthDay(jdn, y, m, d)
         !
         ! Test if calculation is correct
         !
         if (CompareDates([y, m, d], [year, month, day]) /= 0) then
            jdn = 0
         endif
      end function GregorianYearMonthDayToJulianDateNumber

      ! =======================================================================

      subroutine JulianDateNumberToCalendarDate(jdn, yyyymmdd)
         integer, intent(in) :: jdn       !< Julian Date Number
         integer, intent(out) :: yyyymmdd !< calculated calender date

         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable

         if (jdn >= firstGregorianDayNr) then
            call JulianDateNumberToGregorianDate(jdn, yyyymmdd)
         else
            call JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
            yyyymmdd = year*10000 + month*100 + day
         endif
      end subroutine JulianDateNumberToCalendarDate

      subroutine JulianDateNumberToCalendarYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated year
         integer, intent(out) :: month !< calculated month
         integer, intent(out) :: day   !< calculated day

         if (jdn >= firstGregorianDayNr) then
            call JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         else
            call JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
         endif
      end subroutine JulianDateNumberToCalendarYearMonthDay

      !> calculates (year, month, day) based on Julian date number before oktober 1582
      subroutine JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated year
         integer, intent(out) :: month !< calculated month
         integer, intent(out) :: day   !< calculated day

         real(kind=hp) :: jd

         jd = jdn-0.5_hp

         call JulianDateRealNumberToJulianYearMonthDay(jd, year, month, day)
      end subroutine JulianDateNumberToJulianYearMonthDay

      !> from https://quasar.as.utexas.edu/BillInfo/JulianDateCalc.html
      subroutine JulianDateRealNumberToJulianYearMonthDay(jdn, year, month, day)
         real(kind=hp), intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out)       :: year  !< calculated year
         integer, intent(out)       :: month !< calculated month
         integer, intent(out)       :: day   !< calculated day

         real(kind=hp) :: a, b, z, f
         integer       :: c, d, e

         z = jdn + 0.5_hp
         f = z - floor(z)
         a = z
         b = a + 1524
         c = floor((b - 122.1_hp)/365.25_hp)
         d = floor(365.25_hp*c)
         e = floor((b - d)/30.6001_hp)

         month = merge(e-13, e-1, e > 13)
         day   = b - d - floor(30.6001_hp * real(e, hp)) + f
         year  = merge(c-4715, c-4716, month < 3)

      end subroutine JulianDateRealNumberToJulianYearMonthDay

      !> Calculates the Gregorian calender date from a Julian Date Number.
      subroutine JulianDateNumberToGregorianDate(jdn, yyyymmdd)
         integer, intent(in) :: jdn       !< Julian Date Number
         integer, intent(out) :: yyyymmdd !< calculated Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         call JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         yyyymmdd = year*10000 + month*100 + day
      end subroutine JulianDateNumberToGregorianDate

      ! =======================================================================

      !> Calculates the Gregorian year, month, day triplet from a Julian Date Number.
      subroutine JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated Gregorian year
         integer, intent(out) :: month !< calculated Gregorian month
         integer, intent(out) :: day   !< calculated Gregorian day
         !
         integer :: j !< helper variable
         integer :: l !< helper variable
         integer :: m !< helper variable
         integer :: n !< helper variable
         !
         l      = jdn + 68569
         n      = 4 * l / 146097
         l      = l - (146097*n + 3)/4
         j      = 4000 * (l + 1) / 1461001
         l      = l - 1461*j/4 + 31
         m      = 80 * l / 2447
         day    = l - 2447*m/80
         l      = m / 11
         month  = m + 2 - 12*l
         year   = 100*(n-49) + j + l
      end subroutine JulianDateNumberToGregorianYearMonthDay

      ! =======================================================================

      !> Parses an UDUnit-conventions datetime unit string.
      !! TODO: replace this by calling C-API from UDUnits(-2).
      function parse_ud_timeunit(timeunitstr, iunit, iyear, imonth, iday, ihour, imin, isec) result(ierr)
         character(len=*), intent(in)  :: timeunitstr !< Time unit by UDUnits conventions, e.g. 'seconds since 2012-01-01 00:00:00.0 +0000'.
         integer,          intent(out) :: iunit       !< Unit in seconds, i.e. 'hours since..' has iunit=3600.
         integer,          intent(out) :: iyear       !< Year in reference datetime.
         integer,          intent(out) :: imonth      !< Month in reference datetime.
         integer,          intent(out) :: iday        !< Day in reference datetime.
         integer,          intent(out) :: ihour       !< Hour in reference datetime.
         integer,          intent(out) :: imin        !< Minute in reference datetime.
         integer,          intent(out) :: isec        !< Seconds in reference datetime.
         integer                       :: ierr        !< Error status, only 0 when successful.
         !
         integer          :: i
         integer          :: n
         integer          :: ifound
         integer          :: iostat
         character(len=7) :: unitstr
         !
         ierr    = 0
         unitstr = ' '
         !
         n = len_trim(timeunitstr)
         ifound = 0
         do i = 1,n
            if (timeunitstr(i:i) == ' ') then ! First space found
               if (timeunitstr(i+1:min(n, i+5)) == 'since') then
                  unitstr = timeunitstr(1:i-1)
                  ifound = 1
               else
                  ierr = 1
               end if
               exit ! Found or error, look no further.
            end if
         end do
         !
         if (ifound == 1) then
            select case(trim(unitstr))
            case('seconds')
               iunit = 1
            case('minutes')
               iunit = 60
            case('hours')
               iunit = 3600
            case('days')
               iunit = 86400
            case('weeks')
               iunit = 604800
            case default
               iunit = -1
            end select
            !
            read (timeunitstr(i+7:n), '(I4,1H,I2,1H,I2,1H,I2,1H,I2,1H,I2)', iostat = iostat) iyear, imonth, iday, ihour, imin, isec
         end if
      end function parse_ud_timeunit


      !> Creates a string representation of a duration, in the ISO 8601 format.
      !!
      !! NOTE: for durations less than a month, the extended format
      !! P[YYYY]-[MM]-[DD]T[hh]:[mm]:[ss] is returned. For longer durations,
      !! the basic format P[n]Y[n]M[n]DT[n]H[n]M[n]S is returned.
      function duration_to_string(seconds_total) result(duration_string)
         double precision,  intent(in   ) :: seconds_total   !< Seconds numeric value for the duration period.
         character(len=20)                :: duration_string !< Resulting duration string, consider trimming at call site.

         integer :: days, hours, mins, secs
         double precision :: seconds_remaining

         days  = int(seconds_total / 86400d0)
         seconds_remaining = seconds_total - days*86400d0
         hours = int(seconds_remaining / 3600d0)
         seconds_remaining = seconds_remaining - hours*3600d0
         mins  = int(seconds_remaining / 60d0)
         secs = seconds_remaining - mins*60d0
         if (days > 31) then
            ! No support > 1 month yet, so fall back to basic format, which allows unlimited days: P[n]Y[n]M[n]DT[n]H[n]M[n]S
            write (duration_string, '("P", i0,"DT", i0, "H", i0, "M", i0, "S")') days, hours, mins, secs
         else
            ! Preferred extended format: P[YYYY]-[MM]-[DD]T[hh]:[mm]:[ss]
            write (duration_string, '("P", i4.4,"-",i2.2,"-",i2.2,"T", i2.2, ":", i2.2, ":", i2.2)') 0, 0, days, hours, mins, secs
         end if
      end function duration_to_string


!---------------------------------------------------------------------------------------------
! implements interface datetime_to_string
!---------------------------------------------------------------------------------------------
      !> Creates a string representation of a date time, in the ISO 8601 format.
      !! Example: 2015-08-07T18:30:27Z
      !! 2015-08-07T18:30:27+00:00
      !! Performs no check on validity of input numbers!
      function datetime2string(iyear, imonth, iday, ihour, imin, isec, ioffsethour, ioffsetmin, ierr) result(datetimestr)
         integer,           intent(in   )  :: iyear, imonth, iday
         integer, optional, intent(in   )  :: ihour, imin, isec !< Time is optional, will be printed as 00:00:00 if omitted.
         integer, optional, intent(in   )  :: ioffsethour       !< UTC offset hours, optional, will only be printed as [+-]HH:** when given.
         integer, optional, intent(in   )  :: ioffsetmin        !< UTC offset minutes, optional, will be printed as [+-]HH:00 if omitted. Requires also ioffsethour when given.
         integer, optional, intent(  out)  :: ierr              !< Error status, 0 if success, nonzero in case of format error.

         character(len=25) :: datetimestr !< The resulting date time string. Considering using trim() on it.

         integer :: ihour_, imin_, isec_, ioffsethour_, ioffsetmin_, ierr_
         character(len=20) :: offsetformat

         if (.not. present(ihour)) then
            ihour_ = 0
         else
            ihour_ = ihour
         end if
         if (.not. present(imin)) then
            imin_ = 0
         else
            imin_ = imin
         end if
         if (.not. present(isec)) then
            isec_ = 0
         else
            isec_ = isec
         end if

         ! Only print UTC offset "[+-]HH:MM" when at least ioffsethour is given (and optionally also ioffsetmin)
         ! Otherwise end string with the zero offset "Z".
         if (.not. present(ioffsethour)) then
            ioffsethour_ = 0
            offsetformat = ',"Z"'
         else
            ioffsethour_ = ioffsethour
            offsetformat = ',SP,i3.2,":",SS,i2.2'
         end if
         if (.not. present(ioffsetmin)) then
            ioffsetmin_ = 0
         else
            ioffsetmin_ = ioffsetmin
         end if

         write (datetimestr, '(i4,"-",i2.2,"-",i2.2,"T",i2.2,":",i2.2,":",i2.2'//trim(offsetformat)//')', iostat=ierr_) &
                               iyear, imonth, iday, ihour_, imin_, isec_, ioffsethour_, ioffsetmin_

         if (present(ierr)) then
            ierr = ierr_
         end if
      end function datetime2string

      function jul_frac2string(jul, dayfrac, ioffsethour, ioffsetmin, ierr) result(datetimestr)
         implicit none
         integer                , intent(in   )  :: jul
         real(kind=hp), optional, intent(in   )  :: dayfrac
         integer,       optional, intent(in   )  :: ioffsethour !< UTC offset hours, optional, will only be printed as [+-]HH:** when given.
         integer,       optional, intent(in   )  :: ioffsetmin  !< UTC offset minutes, optional, will be printed as [+-]HH:00 if omitted. Requires also ioffsethour when given.
         integer      , optional, intent(  out)  :: ierr        !< Error status, 0 if success, nonzero in case of format error.
         character(len=25)                       :: datetimestr !< The resulting date time string. Considering using trim() on it.

         real(kind=hp) :: days
         real(kind=hp) :: dayfrac_
         integer       :: ierr_

         if (present(dayfrac)) then
             dayfrac_ = dayfrac
         else
             dayfrac_ = 0.0_hp
         end if
         datetimestr = mjd2string(jul2mjd(jul,dayfrac_), ioffsethour=ioffsethour, ioffsetmin=ioffsetmin, ierr=ierr_)
         if (present(ierr)) then
            ierr = ierr_
         end if
      end function jul_frac2string

      function mjd2string(days, ioffsethour, ioffsetmin, ierr) result(datetimestr)
         implicit none
         real(kind=hp)    , intent(in   )  :: days        !< Modified Julian date, including fractional time part.
         integer, optional, intent(in   )  :: ioffsethour !< UTC offset hours, optional, will only be printed as [+-]HH:** when given.
         integer, optional, intent(in   )  :: ioffsetmin  !< UTC offset minutes, optional, will be printed as [+-]HH:00 if omitted. Requires also ioffsethour when given.
         integer, optional, intent(  out)  :: ierr        !< Error status, 0 if success, nonzero in case of format error.
         character(len=25)                 :: datetimestr !< The resulting date time string. Considering using trim() on it.

         integer       :: iyear, imonth, iday, ihour, imin, isec
         real(kind=hp) :: second
         integer       :: ierr_

         ierr_ = -1
         if (mjd2datetime(days,iyear,imonth,iday,ihour,imin,second)/=0) then
            isec = nint(second) ! unfortunately rounding instead of truncating requires all of the following checks
            if (isec == 60) then
               imin = imin+1
               isec = 0
            endif
            if (imin == 60) then
               ihour = ihour+1
               imin = 0
            endif
            if (ihour == 24) then
               iday = iday+1
               ihour = 0
            endif
            select case(imonth)
            case (1,3,5,7,8,10) ! 31 days
               if (iday == 32) then
                  imonth = imonth+1
                  iday = 1
               endif
            case (12) ! 31 days
               if (iday == 32) then
                  iyear = iyear+1
                  imonth = 1
                  iday = 1
               endif
            case (4,6,9,11) ! 30 days
               if (iday == 31) then
                  imonth = imonth+1
                  iday = 1
               endif
            case default ! February
                if (leapYear(iyear)) then
                   if (iday == 30) then
                      imonth = 3
                      iday = 1
                   endif
                else ! 28 days
                   if (iday == 29) then
                      imonth = 3
                      iday = 1
                   endif
                endif
            end select
            datetimestr = datetime2string(iyear, imonth, iday, ihour, imin, isec, &
                        ioffsethour=ioffsethour, ioffsetmin=ioffsetmin, ierr=ierr_)
         else
            ierr_ = -1
            datetimestr = ' '
         endif
         if (present(ierr)) then
            ierr = ierr_
         end if
      end function mjd2string

      !> helper function to find out if a year is a leap year or not
      logical function leapYear(iyear)
         integer, intent(in) :: iyear

         integer :: jdn

         if (mod(iyear, 4) /= 0) then
            ! basic check: if year is not a multiple of 4 it is certainly NOT a leap year
            leapYear = .false.
         else
            ! if it is a multiple of 4, it is quite complex,
            ! so check if 29 February is a valid date in that year:
            jdn = CalendarYearMonthDayToJulianDateNumber(iyear, 2, 29)
            leapYear = (jdn /= 0)
         endif
      end function leapYear

      ! implements (obsolete!) interface date2mjd
      function ymd2mjd(ymd) result(days)
         implicit none
         integer, intent(in)       :: ymd
         real(kind=hp)             :: days

         integer       :: year, month, day

         call splitDate(ymd, year, month, day)
         days = datetime2mjd(year,month,day,0,0,0.d0)
      end function ymd2mjd

      function ymdhms2mjd(ymd,hms) result(days)
         implicit none
         integer, intent(in)       :: ymd
         real(kind=hp), intent(in) :: hms
         real(kind=hp)             :: days
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         year   = int(ymd/10000)
         month  = int(mod(ymd,10000)/100)
         day    = mod(ymd,100)
         hour   = int(hms/10000)
         minute = int(mod(hms,10000.d0)/100)
         second = mod(hms,100.d0)
         days = datetime2mjd(year,month,day,hour,minute,second)
      end function ymdhms2mjd

      function datetime2mjd(year,month,day,hour,minute,second) result(days)
         implicit none
         integer, intent(in)   :: year, month, day
         integer, intent(in)   :: hour, minute
         real(kind=hp):: second
         real(kind=hp) :: days
         real(kind=hp) :: dayfrac
         dayfrac = (hour*3600+minute*60+second)/(24*3600)
         days = jul2mjd(CalendarYearMonthDayToJulianDateNumber(year,month,day),dayfrac)
      end function datetime2mjd

!---------------------------------------------------------------------------------------------
! private: convert Julian day to Modified Julian 
!---------------------------------------------------------------------------------------------
      function jul2mjd(jul,frac) result(days)
         implicit none
         integer                , intent(in)  :: jul
         real(kind=hp), optional, intent(in)  :: frac
         real(kind=hp)                        :: days

         days = real(jul,hp)-offset_modified_jd
         if (present(frac)) then
             days = days + frac
         endif
      end function jul2mjd

!---------------------------------------------------------------------------------------------
! implements interface mjd2date
!---------------------------------------------------------------------------------------------
      function mjd2ymd(days,ymd) result(success)
         implicit none
         real(kind=hp), intent(in)  :: days
         integer, intent(out)       :: ymd
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         integer       :: success

         success = 0
         if (mjd2datetime(days,year,month,day,hour,minute,second)==0) return
         ymd = year*10000 + month*100 + day
         success = 1
      end function mjd2ymd

      function mjd2ymdhms(days,ymd,hms) result(success)
         implicit none
         real(kind=hp), intent(in)       :: days
         integer, intent(out)            :: ymd
         integer, intent(out)            :: hms
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         integer       :: success

         success = 0
         if (mjd2datetime(days,year,month,day,hour,minute,second)==0) return
         ymd = year*10000 + month*100 + day
         hms = nint(hour*10000 + minute*100 + second)
         success = 1
      end function mjd2ymdhms

      function mjd2datetime(days,year,month,day,hour,minute,second) result(success)
         implicit none
         real(kind=hp), intent(in)  :: days
         integer,  intent(out)      :: year, month, day
         integer,  intent(out)      :: hour, minute
         real(kind=hp), intent(out) :: second
         real(kind=hp) :: mjd, dayfrac
         real(kind=hp) :: jul
         integer       :: success, ntry

         success = 0
         ntry = 1
         mjd = days
         do while (ntry <= 2)
            jul = mjd + offset_modified_jd
            dayfrac = mjd - floor(mjd)         
            hour = int(dayfrac*24)
            minute = int(mod(dayfrac*1440,60._hp))
            second = mod(dayfrac*86400,60._hp)            
            if (nint(second) >= 60) then
               ! add less than 0.5 second to mjd (1/86400 = 1.157E-5) and retry
               mjd = mjd + 0.000005_hp
               ntry = ntry + 1
            else 
               exit
            endif
         enddo
         call JulianDateNumberToCalendarYearMonthDay(nint(jul),year,month,day)
         success = 1
      end function mjd2datetime

      !> split a string in date, time and time zone part
      function split_date_time(string, date, time, tz) result(success)
         character(len=*), intent(in)  :: string  !< input string like 2020-01-01 00:00:00; with or without time
                                                  !<                or 2020-01-01T00:00:00 as in ISO_8601
                                                  !< input string may contains a time zone indication
         character(len=*), intent(out) :: date    !< output date, in this case 2020-01-01
         character(len=*), intent(out) :: time    !< output time, in this case 00:00:00
         character(len=*), intent(out) :: tz      !< output time zone indication
         logical                       :: success !< function result

         character(len=:), allocatable :: date_time
         integer                       :: ipos, i, iposTZ, ipos2, correct_short_date_time
         character, parameter          :: splitters1(2) = (/ 'T', ' ' /)
         character, parameter          :: splitters2(3) = (/ '+', '-', 'Z' /)
         character                     :: splitter
         integer  , parameter          :: size_date = len('2020-01-01')
         integer  , parameter          :: size_time = len('00:00:00')

         date_time = trim(adjustl(string))

         ! search for the character splitting the date and the time
         do i = 1, size(splitters1)
            ipos = index(date_time, splitters1(i))
            if (ipos > 0) exit
         end do

         if (ipos == 9) then
            ! no splitters in date and time
            correct_short_date_time = 2
         else
            correct_short_date_time = 0
         end if

         ! search for time zone indication
         iposTZ = 0
         do i = 1, size(splitters2)
            splitter = splitters2(i)
            ipos2 = index(date_time, splitter, back=.true.)
            if (ipos2 > size_date + size_time + 1 - 2*correct_short_date_time) then           ! the minus can be part of the date
               iposTZ = max(iposTZ, ipos2)
            else if (ipos2 > 0 .and. splitter /= '-') then
               success = .false.
               return
            end if
         end do

         if (ipos > 0) then
            date = date_time(1:ipos-1)
            if (iposTZ > 0) then
               time = adjustl(date_time(ipos+1:iposTZ-1))
               tz = date_time(iposTZ:)
            else
               time = adjustl(date_time(ipos+1:))
               tz   = ' '
            endif
            if (len_trim(time) > size_time - correct_short_date_time) then
               success = index(time, '.') > 0 ! allow longer time string if it includes a dot
               return
            end if
         else if (len(date_time) == size_date - correct_short_date_time) then
            date = date_time
            time = ' '
            tz   = ' '
         else ! format not recoqnized
            success = .false.
            return
         endif
         success = .true.
      end function split_date_time

      !> parse a time string of the form "23:59:59.123" or "23:59:59" and return it as fraction of a day
      !! also no splitter : is allowed ("235959.123" or "235959")
      !! ms and seconds are optional
      function parse_time(time, ok) result (fraction)
         use string_module, only : strsplit
         character(len=*), intent(in)  :: time      !< input time string
         logical         , intent(out) :: ok        !< success flag
         real(kind=hp)                 :: fraction  !< function result

         integer, parameter            :: maxParts = 3
         integer                       :: iPart, ierr
         real(kind=hp)                 :: scalefactor, temp
         integer                       :: nParts

         character(len=16), allocatable :: times(:)
         real(kind=hp), parameter      :: invalidValues(maxParts) = (/ 24.01_hp, 60.01_hp, 61.1_hp /) ! accept leap second

         ok = .false.
         scalefactor = 24.0_hp
         fraction = 0.0_hp

         if (index(time, ':') > 0) then
            call strsplit(time, 1, times, 1, ':')
            nParts = min(maxParts, size(times))
         else if (len_trim(time) == 4 .or. len_trim(time) >= 6) then
            nParts = min(maxParts, len_trim(time) / 2)
            allocate(times(nParts))
            do iPart = 1, nParts-1
               times(iPart) = time(2*iPart-1:2*iPart)
            end do
            times(nParts) = time(2*nParts-1:)
         else
            nParts = 0 ! results in an error
         end if

         do iPart = 1, nParts
              read(times(iPart), *, iostat=ierr) temp
              if (ierr /= 0 .or. temp >= invalidValues(iPart) .or. temp<0) exit
              fraction = fraction + temp/scalefactor
              scalefactor = scalefactor * 60.0_hp
              ok = (iPart == nParts)
         enddo
      end function parse_time
      
      !> Given datetime string, compute time in seconds from refdat
     subroutine datetimestring_to_seconds(dateandtime,refdat,timsec,stat)
         implicit none

         character,         intent(in)  :: dateandtime*(*) !< Input datetime string, format '201201010000', note that seconds are ignored.
         character (len=8), intent(in)  :: refdat          !< reference date
         integer,           intent(out) :: stat
 

         double precision              :: timmin
         double precision, intent(out) :: timsec

         integer          :: iday ,imonth ,iyear ,ihour , imin, isec
         integer          :: ierr

         stat = 0
         read(dateandtime( 1:4 ),'(i4)'  ,iostat=ierr) iyear
         if (ierr /= 0) goto 999
         read(dateandtime( 5:6 ),'(i2.2)',iostat=ierr) imonth
         if (ierr /= 0) goto 999
         read(dateandtime( 7:8 ),'(i2.2)',iostat=ierr) iday
         if (ierr /= 0) goto 999
         read(dateandtime( 9:10),'(i2.2)',iostat=ierr) ihour
         if (ierr /= 0) goto 999
         read(dateandtime(11:12),'(i2.2)',iostat=ierr) imin
         if (ierr /= 0) goto 999
         read(dateandtime(13:14),'(i2.2)',iostat=ierr) isec
         if (ierr /= 0) goto 999
         
         call seconds_since_refdat(iyear, imonth, iday, ihour, imin, isec, refdat, timsec)

         timmin  = timsec/60d0
         !timmin = (jul - jul0)*24d0*60d0      + ihour*60d0      + imin

         return
999      continue
         stat = ierr
         return
     end subroutine datetimestring_to_seconds
      
     !> Given time in seconds from refdat, fill dateandtime string
     !! NOTE: seconds_to_datetimestring and datetimestring_to_seconds are not compatible, because of minutes versus seconds, and different format string.
     subroutine seconds_to_datetimestring(dateandtime,refdat,tim)
         implicit none

         character,        intent(out) :: dateandtime*(*) !< Output datetime string, format '20000101_000000', note: includes seconds.
         double precision, intent(in)  :: tim             !< Input time in seconds since refdat.
        character (len=8), intent(in)  :: refdat          !< reference date

         integer          :: iday, imonth, iyear, ihour, imin, isec

         dateandtime = '20000101_000000'
         ! TODO: AvD: seconds_to_datetimestring and datetimestring_to_seconds are now inconsistent since the addition of this '_'

         call datetime_from_refdat(tim, refdat, iyear, imonth, iday, ihour, imin, isec)

         write(dateandtime( 1:4 ),'(i4)')   iyear
         write(dateandtime( 5:6 ),'(i2.2)') imonth
         write(dateandtime( 7:8 ),'(i2.2)') iday
         write(dateandtime(10:11),'(i2.2)') ihour
         write(dateandtime(12:13),'(i2.2)') imin
         write(dateandtime(14:15),'(i2.2)') isec

         return
     end subroutine seconds_to_datetimestring
 
      DOUBLE PRECISION FUNCTION JULIAN ( IDATE , ITIME )
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the so called Julian day of a date, or
!        the value -1.0 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differences between dates. ( See SUBROUTINE GREGOR
!        for the reverse proces ).
!
!         If idate is before 15821005 Julian calendar is assumed,
!         otherwise Gregorian.
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     IDATE  integer  in     -               Date as YYYYMMDD
!     ITIME  integer  in     -               Time as HHMMSS
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     IYEAR  integer  -      Year   ( -4713-.. )
!     IMONTH integer  -      Month  ( 1-12 )
!     IDAY   integer  -      Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  -      Hour   ( 0-23 )
!     IMIN   integer  -      Minute ( 0-59 )
!     ISEC   integer  -      Second ( 0-59 )
!     MONLEN integer  12     Length of month in days
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      IMPLICIT NONE !!!

      INTEGER          IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IDATE, ITIME, MONLEN(12)
      DOUBLE PRECISION TEMP1, TEMP2
!
!***********************************************************************
!
!     Initialize lenghts of months :
!
      DATA MONLEN / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
!***********************************************************************
!
!
!
      IYEAR  = IDATE/10000
      IMONTH = IDATE/100 - IYEAR*100
      IDAY   = IDATE - IYEAR*10000 - IMONTH*100
      IHOUR  = ITIME/10000
      IMIN   = ITIME/100 - IHOUR*100
      ISEC   = ITIME - IHOUR*10000 - IMIN*100
      
      IF (( IYEAR  .LT. -4713 ) .OR. ( IMONTH .LT.  1 ) .OR. &
          ( IMONTH .GT.    12 ) .OR. ( IDAY   .LT.  1 ) .OR. &
          ( IDAY   .GT. MONLEN(IMONTH) ) .OR. &
          ( IHOUR  .LT.     0 ) .OR. ( IHOUR  .GT. 23 ) .OR. &
          ( IMIN   .LT.     0 ) .OR. ( IMIN   .GT. 59 ) .OR. &
          ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 60 )) THEN
!!!GP original check was op 59 seconden, gaf afrondingsproblemen; daarom nu op 60 gezet
!!!  5    ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 59 )) THEN
         JULIAN = -1.0
         GOTO 999
      ELSE if (compareDates([iyear, imonth, iday], justAfterLastJulian) == -1) then
         TEMP2 = JulianYearMonthDayToJulianDateNumber(iyear, imonth, iday)
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + &
                  FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 )
      ELSE
         TEMP1  = INT (( IMONTH-14.0) / 12.0 )
         TEMP2  = IDAY - 32075.0 + &
                INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 ) + &
                INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 ) - &
                INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) / &
                4.0 )
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + &
                  FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 )
      ENDIF
  999 RETURN
      END FUNCTION JULIAN

      DOUBLE PRECISION FUNCTION julian_with_leapyears ( IDATE , ITIME )
!
!     +----------------------------------------------------------------+
!     |    W A T E R L O O P K U N D I G   L A B O R A T O R I U M     |
!     |               Sector Waterbeheer & Milieu                      |
!     +----------------------------------------------------------------+
!
!***********************************************************************
!
!     Project : T0467
!     Author  : Andre Hendriks
!     Date    : 891215             Version : 1.00
!
!     Changes in this module :
!
!     Date    Author          Description
!     ------  --------------  -----------------------------------
!     ......  ..............  ..............................
!     891215  Andre Hendriks  Version 1.00
!
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the so called Julian day of a date, or
!        the value -1.0 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See SUBROUTINE GREGOR
!        for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     IDATE  integer  in     -               Date as YYYYMMDD
!     ITIME  integer  in     -               Time as HHMMSS
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     IYEAR  integer  -      Year   ( -4713-.. )
!     IMONTH integer  -      Month  ( 1-12 )
!     IDAY   integer  -      Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  -      Hour   ( 0-23 )
!     IMIN   integer  -      Minute ( 0-59 )
!     ISEC   integer  -      Second ( 0-59 )
!     MONLEN integer  12     Length of month in days
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      INTEGER          IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC  , &
                       IDATE , ITIME , MONLEN(12)
      DOUBLE PRECISION TEMP1 , TEMP2
      CHARACTER*48     LINE
!
!***********************************************************************
!
!     Initialize lenghts of months :
!
      DATA MONLEN / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
!***********************************************************************
!
!
!
      IYEAR  = IDATE/10000
      IMONTH = IDATE/100 - IYEAR*100
      IDAY   = IDATE - IYEAR*10000 - IMONTH*100
      IHOUR  = ITIME/10000
      IMIN   = ITIME/100 - IHOUR*100
      ISEC   = ITIME - IHOUR*10000 - IMIN*100

      IF (MOD(IYEAR, 4) .NE. 0) THEN
!        IT IS A COMMON YEAR
         MONLEN(2) = 28
      ELSE IF (MOD(IYEAR,100) .NE. 0) THEN
!        IT IS A LEAP YEAR
         MONLEN(2) = 29
      ELSE IF (MOD(IYEAR,400) .NE. 0) THEN
!        IT IS A COMMON YEAR
         MONLEN(2) = 28
      ELSE 
!        IT IS A LEAP YEAR
         MONLEN(2) = 29
      END IF
         
      
      IF (( IYEAR  .LT. -4713 ) .OR. ( IMONTH .LT.  1 ) .OR. &
          ( IMONTH .GT.    12 ) .OR. ( IDAY   .LT.  1 ) .OR. &
          ( IDAY   .GT. MONLEN(IMONTH) ) .OR. &
          ( IHOUR  .LT.     0 ) .OR. ( IHOUR  .GT. 24 ) .OR. &
          ( IMIN   .LT.     0 ) .OR. ( IMIN   .GT. 60 ) .OR. &
          ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 60 )) THEN
         julian_with_leapyears = -1.0
         WRITE(LINE,'(A33,I8,''-'',I6)') 'ERROR in JULIAN interpreting time:',IDATE,ITIME
         CALL write_log_message(LINE)
         GOTO 999
      ELSE
         TEMP1  = INT (( IMONTH-14.0) / 12.0 )
         TEMP2  = IDAY - 32075.0 + &
                INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 ) + &
                INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 ) - &
                INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) / 4.0 )
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + FLOAT ( ISEC  ) - 43200.0
         julian_with_leapyears = TEMP2 + ( TEMP1 / 86400.0 )
      ENDIF
  999 RETURN
      END FUNCTION julian_with_leapyears

      SUBROUTINE GREGOR ( JULIAN, IYEAR , IMONTH, IDAY  , IHOUR , &
                        IMIN  , ISEC  , DSEC)
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the Gregorian date and the time of a so
!        called Julian day, or iyear -9999 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See DOUBLE PRECISION
!        FUNCTION JULIAN for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     JULIAN real*8   in     -               Julian day
!     IYEAR  integer  out    -               Year   ( -4713-.. )
!     IMONTH integer  out    -               Month  ( 1-12 )
!     IDAY   integer  out    -               Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  out    -               Hour   ( 0-23 )
!     IMIN   integer  out    -               Minute ( 0-59 )
!     ISEC   integer  out    -               Second ( 0-59 )
!     DSEC   real*8   out    -               Second as double
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     TEMP3  real*8   -      Temporary variable
!     TEMP4  real*8   -      Temporary variable, JULIAN
!     TEMP5  real*8   -      Temporary variable, fractional part JULIAN
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      INTEGER           :: IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
      DOUBLE PRECISION  :: JULIAN, TEMP1 , TEMP2 , TEMP3 , TEMP4 , TEMP5
      DOUBLE PRECISION  :: DSEC
      DOUBLE PRECISION  :: myJULIAN, delta
      integer           :: nTry
!
!***********************************************************************
!
      delta = 0.0D+00
 
      IF ( JULIAN .LT. 0.0 ) THEN
         IYEAR = -9999
      ELSE IF ( JULIAN < real(firstGregorianDayNr, hp)) then
         IYEAR = -9999 ! not yet implemented
      ELSE
         nTry = 1
         DO WHILE ( nTry <= 2 )
             myJULIAN= JULIAN + delta
             TEMP4 = myJULIAN
             TEMP5 = DMOD ( myJULIAN, 1.0D0 )
             IF ( TEMP5 .LT. 0.5 ) THEN
                TEMP3  = 0.5 + TEMP5
                TEMP4  = DINT ( TEMP4 )
             ELSE
                TEMP3  = TEMP5 - 0.5
                TEMP4  = DINT ( TEMP4 ) + 1.0
             ENDIF
             TEMP1  = TEMP4 + 68569.0
             TEMP2  = DINT  ( 4.0 * TEMP1 / 146097.0 )
             TEMP1  = TEMP1 - DINT ( ( 146097.0 * TEMP2 + 3.0 ) / 4.0 )
             IYEAR  = INT   ( 4000.0 * ( TEMP1 + 1.0 ) / 1461001.0 )
             TEMP1  = TEMP1 - DINT ( (1461.0D0 * IYEAR) / 4.0 ) + 31.0
             IMONTH = INT   ( 80.0 * TEMP1 / 2447.0 )
             IDAY   = INT   ( TEMP1 - AINT ( 2447.0 * IMONTH / 80.0 ) )
             TEMP1  = DINT  ( dble(IMONTH / 11.0D0) )
             IMONTH = INT   ( IMONTH + 2.0 - 12.0 * TEMP1 )
             IYEAR  = INT   ( 100.0 * ( TEMP2 - 49.0 ) + IYEAR + TEMP1 )
             IHOUR  = INT   ( TEMP3 * 24.0 )
             IMIN   = INT   ( TEMP3 * 1440.0 - 60.0 * IHOUR )
             DSEC   =         TEMP3 * 86400.0 - 3600.0 * IHOUR - 60.0*IMIN
             ISEC   = NINT  ( DSEC )

             if ( isec >= 60 ) then
                 if ( nTry < 2 ) then
                     delta = 0.49999D+00 / 86400.0D+00
                     nTry = nTry + 1
                 else
                     IYEAR = -9999
                     exit
                 endif
             else
                 exit
             endif
         ENDDO

      ENDIF

      END SUBROUTINE GREGOR

end module time_module
