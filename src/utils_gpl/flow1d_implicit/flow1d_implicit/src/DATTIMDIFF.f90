double precision function dattimdiff (date1, date2)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Parse New Model Database (V2.0)
!
! Programmer:         P.R. Evans
!
! Module:             Calculate difference in date
!
! Module description: This function calculates the difference between
!                     the start date/time and the end date/time for
!                     sobek, including leap years. The result is
!                     returned as a double precision real value in
!                     seconds. The date/time inputs are read as
!                     integers.
!
!=======================================================================
!
!     Declaration of Parameters:
!
   integer         date1(6), date2(6)
!
!     Declaration of local variables:
!
   integer          aday(12), nyear, nleap, nday, endday, startday,&
   &year1, month1, day1, hour1, min1, sec1, hun1,&
   &year2, month2, day2, hour2, min2, sec2, hun2,&
   &iyear
   real             endtime, starttime
   double precision rday
!
   data             aday /0, 31, 59, 90, 120, 151, 181, 212, 243,&
   &273, 304, 334/
!
!     initialise variables
!
   nyear       = 0
   nleap       = 0
   nday        = 0
   endday      = 0
   startday    = 0
   endtime     = 0
   starttime   = 0
   dattimdiff  = 0D0
!
   year1  = date1(1)
   month1 = date1(2)
   day1   = date1(3)
   hour1  = date1(4)
   min1   = date1(5)
   sec1   = date1(6)
   hun1   = 0
!
   year2  = date2(1)
   month2 = date2(2)
   day2   = date2(3)
   hour2  = date2(4)
   min2   = date2(5)
   sec2   = date2(6)
   hun2 = 0
!
!     calculate number of years and then number of leap years
!     year including end date is added later
   nyear = year2 - year1
!
   do 10 iyear=year1, year2-1
      if ((MOD (iyear, 4) .eq. 0&
      &.and. MOD (iyear, 100) .ne. 0)&
      &.or. MOD (iyear, 400) .eq. 0) then
         nleap = nleap + 1
      endif
10 continue
!
!     now determine number of days in start year
   startday = aday(month1) + day1
!
!     is start year a leap year and month after february?
   if (month1 .gt. 2&
   &.and. ((MOD (year1, 4) .eq. 0&
   &.and. MOD (year1, 100) .ne. 0)&
   &.or. MOD (year1, 400) .eq. 0)) then
      startday = startday + 1
   endif
!
!     now determine number of days in end year
   endday = aday(month2) + day2
!
!     is end year a leap year and month after february?
   if (month2 .gt. 2&
   &.and. ((MOD (year2, 4) .eq. 0&
   &.and. MOD (year2, 100) .ne. 0)&
   &.or. MOD (year2, 400) .eq. 0)) then
      endday = endday + 1
   endif
!
!     calculate total number of days
   nday = nyear*365 + nleap - startday + endday
!
!     calculate difference in times (exclude date)
   starttime = hour1*3600 + min1*60 + sec1 + hun1/100
   endtime = hour2*3600 + min2*60 + sec2 + hun2/100
   dattimdiff = endtime - starttime
!
!     add up total time difference
   rday       = nday
   dattimdiff = rday*86400. + dattimdiff
!
end
