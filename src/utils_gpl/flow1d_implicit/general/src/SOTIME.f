      subroutine SOTIME ( itim, dt )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             SOTIME (SObek TIME)
c
c Module description: Calculated new time in YYYYMMDD HHMMSSHH format.
c
c                     Sobek uses two methods to keep track of time. One
c                     is a real variable which contains the time in
c                     seconds. The other one is the time in year-
c                     -month-day hour-minute-seconds-hundredth of se-
c                     conds format. This last format is used to store
c                     results and restart files. This routine receives a
c                     time in YYYYMMDD etc. format and a delta time in
c                     seconds. It is possible to count up or down by
c                     signing delta time.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dt                I  Computational time step dt [sec].
c  1 itim(2)           IO Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sotime.pf,v $
c Revision 1.3  1999/03/15  15:51:25  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:02:32  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:35  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  09:22:50  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:44:00  kuipe_j
c Initial version
c
c
c***********************************************************************
c

      integer  itim(2)
      double   precision  dt, dta, dtju
      integer  year, month, day
      integer  hour, minute, second
      integer  days, time
      integer  aday(12), mday, idate, nday
      integer  minunit(3),hourunit(3),sechunit(3)
      integer  junit
      integer,parameter::hundsec=1,onesec=2,onemin=3
c
c     Number of days for each month (jan-dec)
c
      data     aday    /31,28,31,30,31,30,31,31,30,31,30,31/
      data     minunit /6000,60,1/
      data     hourunit/360000,3600,60/
      data     sechunit/1,100,6000/ 
c
c     Extract date
c
      year   = int (itim(1) / 10000)
      month  = mod (int (itim(1) / 100), 100)
      day    = mod (itim(1), 100)
c
c     Extract time
c
      hour   = int (itim(2) / 1000000)
      minute = mod (int (itim(2) / 10000), 100)
      second = mod (itim(2), 10000 )
c     second in hundredths of seconds 
c
c     Calculate time in time units (hundredths of seconds,
c     seconds or minutes)
c
      dta = abs(dt)
      if (dta.le.1.d7) then
         junit   = hundsec
      else if (dta.le.1.d9) then
         junit   = onesec
      else   
         junit   = onemin
      endif   
      time   =  second / sechunit(junit)  +
     +          minute * minunit(junit)  +
     +          hour   * hourunit(junit)
c
c     Add time (in time unit)
c
      dtju = dt*100.d0/sechunit(junit)
      time = time + nint(dtju)
c
c     Check for negative time (count back)
c
      nday = 0
c
  100 continue
      if (time .lt. 0) then
         nday = nday + 1
         time = time + 24*hourunit(junit)
         goto 100
      endif
c
c     Create time format
c
c     Second in time units
      second = mod (time,minunit(junit))
      hour   = time / hourunit(junit)
      time   = time - (hour*hourunit(junit)) - second
      minute = time / minunit(junit)
c     Second in hundredths of seconds      
      second = second * sechunit(junit)
c
c     Determine number of days in case of positive direction
c
      if (dt .gt. 0) then
         days = hour / 24
      else
         days = nday
      endif
c
c     Calculate hour
c
      hour = mod (hour,24)
c
c     Assemble time word
c
      itim(2) = second + minute * 10000 + hour * 1000000
c
c     Now update date
c
      if (dt .gt. 0) then
c
c        Date updating positive direction
c
         do 200 idate = 1, days
c
c           Calculate next day in sequence
c
            day  = day + 1
c
c           Calculate maximum number of days in month
c
            mday = aday(month)
            if (((month          .eq. 2)  .and.
     +           (mod(year,4)    .eq. 0)  .and.
     +           (mod(year,100)  .ne. 0)) .or.
     +          ((month          .eq. 2)  .and.
     +           (mod(year,1000) .eq. 0)))
     +      then
               mday = mday + 1
            endif
c
c           Check for max day
c
            if (day .gt. mday) then
               day   = 1
               month = month + 1
               if (month .gt. 12) then
                  month = 1
                  year  = year + 1
               endif
            endif
 200     continue
      else
c
c        Date updating negative direction
c
         do 300 idate = 1, days
c
c           Calculate previous day in sequence
c
            day  = day - 1
            if (day .lt. 1) then
               month = month - 1
               if (month .lt. 1) then
                  month = 12
                  year  = year - 1
               endif
c
c              Calculate maximum number of days in month
c
               mday = aday(month)
               if (((month          .eq. 2)  .and.
     +              (mod(year,4)    .eq. 0)  .and.
     +              (mod(year,100)  .ne. 0)) .or.
     +             ((month          .eq. 2)  .and.
     +              (mod(year,1000) .eq. 0)))
     +         then
                  mday = mday + 1
               endif
c
c              Assign max day
c
               day = mday
            endif
 300     continue
      endif
c
c     Assemble date word
c
      itim(1) = day + month * 100 + year * 10000
c
      return
      end
