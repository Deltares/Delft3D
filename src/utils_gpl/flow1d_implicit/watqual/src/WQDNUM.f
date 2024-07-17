      subroutine wqdnum ( date, dayno )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQDNUM (Water Quality Day NUMber)
c
c Module description: Calculate daynumber for a given date YYYYMMDD
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 date              I  date in YYYYMMDD format
c  2 dayno             IO daynumber of date (1..366)
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqdnum.pf,v $
c Revision 1.3  1999/03/15  15:53:50  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:22  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:43  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  date, dayno
c
c     Variables
c
      integer  year, month, day
      integer  aday(12), mday, idate
c
c     Number of days for each month (jan-dec)
c
      data     aday /31,28,31,30,31,30,31,31,30,31,30,31/
c
c     Extract date
c
      year   = int (date / 10000)
      month  = mod (int(date / 100), 100)
      day    = mod (date , 100)
c
c     Set day number to zero
c
      dayno  = 0
c
c     Calculate day number
c
      do 100 idate = 1, month-1
c
c        Calculate maximum number of days in month
c
         mday = aday(idate)
         if (((idate          .eq. 2)  .and.
     +        (mod(year,4)    .eq. 0)  .and.
     +        (mod(year,100)  .ne. 0)) .or.
     +       ((idate          .eq. 2)  .and.
     +        (mod(year,1000) .eq. 0)))
     +   then
            mday = mday + 1
         endif
c
c        Increment day number
c
         dayno = dayno + mday
 100  continue
c
c     Add actual number of days of current month
c
      dayno = dayno + day

      return
      end
