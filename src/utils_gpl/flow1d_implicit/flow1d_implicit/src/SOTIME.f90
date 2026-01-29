subroutine SOTIME ( itim, dt )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             SOTIME (SObek TIME)
!
! Module description: Calculated new time in YYYYMMDD HHMMSSHH format.
!
!                     Sobek uses two methods to keep track of time. One
!                     is a real variable which contains the time in
!                     seconds. The other one is the time in year-
!                     -month-day hour-minute-seconds-hundredth of se-
!                     conds format. This last format is used to store
!                     results and restart files. This routine receives a
!                     time in YYYYMMDD etc. format and a delta time in
!                     seconds. It is possible to count up or down by
!                     signing delta time.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dt                I  Computational time step dt [sec].
!  1 itim(2)           IO Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sotime.pf,v $
! Revision 1.3  1999/03/15  15:51:25  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:02:32  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:35  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:22:50  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:00  kuipe_j
! Initial version
!
!
!***********************************************************************
!

   integer  itim(2)
   double   precision  dt, dta, dtju
   integer  year, month, day
   integer  hour, minute, second
   integer  days, time
   integer  aday(12), mday, idate, nday
   integer  minunit(3),hourunit(3),sechunit(3)
   integer  junit
   integer,parameter::hundsec=1,onesec=2,onemin=3
!
!     Number of days for each month (jan-dec)
!
   data     aday    /31,28,31,30,31,30,31,31,30,31,30,31/
   data     minunit /6000,60,1/
   data     hourunit/360000,3600,60/
   data     sechunit/1,100,6000/
!
!     Extract date
!
   year   = int (itim(1) / 10000)
   month  = mod (int (itim(1) / 100), 100)
   day    = mod (itim(1), 100)
!
!     Extract time
!
   hour   = int (itim(2) / 1000000)
   minute = mod (int (itim(2) / 10000), 100)
   second = mod (itim(2), 10000 )
!     second in hundredths of seconds
!
!     Calculate time in time units (hundredths of seconds,
!     seconds or minutes)
!
   dta = abs(dt)
   if (dta.le.1.d7) then
      junit   = hundsec
   else if (dta.le.1.d9) then
      junit   = onesec
   else
      junit   = onemin
   endif
   time   =  second / sechunit(junit)  +&
   &minute * minunit(junit)  +&
   &hour   * hourunit(junit)
!
!     Add time (in time unit)
!
   dtju = dt*100.d0/sechunit(junit)
   time = time + nint(dtju)
!
!     Check for negative time (count back)
!
   nday = 0
!
100 continue
   if (time .lt. 0) then
      nday = nday + 1
      time = time + 24*hourunit(junit)
      goto 100
   endif
!
!     Create time format
!
!     Second in time units
   second = mod (time,minunit(junit))
   hour   = time / hourunit(junit)
   time   = time - (hour*hourunit(junit)) - second
   minute = time / minunit(junit)
!     Second in hundredths of seconds
   second = second * sechunit(junit)
!
!     Determine number of days in case of positive direction
!
   if (dt .gt. 0) then
      days = hour / 24
   else
      days = nday
   endif
!
!     Calculate hour
!
   hour = mod (hour,24)
!
!     Assemble time word
!
   itim(2) = second + minute * 10000 + hour * 1000000
!
!     Now update date
!
   if (dt .gt. 0) then
!
!        Date updating positive direction
!
      do 200 idate = 1, days
!
!           Calculate next day in sequence
!
         day  = day + 1
!
!           Calculate maximum number of days in month
!
         mday = aday(month)
         if (((month          .eq. 2)  .and.&
         &(mod(year,4)    .eq. 0)  .and.&
         &(mod(year,100)  .ne. 0)) .or.&
         &((month          .eq. 2)  .and.&
         &(mod(year,1000) .eq. 0)))&
         &then
            mday = mday + 1
         endif
!
!           Check for max day
!
         if (day .gt. mday) then
            day   = 1
            month = month + 1
            if (month .gt. 12) then
               month = 1
               year  = year + 1
            endif
         endif
200   continue
   else
!
!        Date updating negative direction
!
      do 300 idate = 1, days
!
!           Calculate previous day in sequence
!
         day  = day - 1
         if (day .lt. 1) then
            month = month - 1
            if (month .lt. 1) then
               month = 12
               year  = year - 1
            endif
!
!              Calculate maximum number of days in month
!
            mday = aday(month)
            if (((month          .eq. 2)  .and.&
            &(mod(year,4)    .eq. 0)  .and.&
            &(mod(year,100)  .ne. 0)) .or.&
            &((month          .eq. 2)  .and.&
            &(mod(year,1000) .eq. 0)))&
            &then
               mday = mday + 1
            endif
!
!              Assign max day
!
            day = mday
         endif
300   continue
   endif
!
!     Assemble date word
!
   itim(1) = day + month * 100 + year * 10000
!
   return
end
