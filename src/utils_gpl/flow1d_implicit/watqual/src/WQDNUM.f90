subroutine wqdnum ( date, dayno )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQDNUM (Water Quality Day NUMber)
!
! Module description: Calculate daynumber for a given date YYYYMMDD
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 date              I  date in YYYYMMDD format
!  2 dayno             IO daynumber of date (1..366)
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqdnum.pf,v $
! Revision 1.3  1999/03/15  15:53:50  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:22  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:43  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  date, dayno
!
!     Variables
!
   integer  year, month, day
   integer  aday(12), mday, idate
!
!     Number of days for each month (jan-dec)
!
   data     aday /31,28,31,30,31,30,31,31,30,31,30,31/
!
!     Extract date
!
   year   = int (date / 10000)
   month  = mod (int(date / 100), 100)
   day    = mod (date , 100)
!
!     Set day number to zero
!
   dayno  = 0
!
!     Calculate day number
!
   do 100 idate = 1, month-1
!
!        Calculate maximum number of days in month
!
      mday = aday(idate)
      if (((idate          .eq. 2)  .and.&
      &(mod(year,4)    .eq. 0)  .and.&
      &(mod(year,100)  .ne. 0)) .or.&
      &((idate          .eq. 2)  .and.&
      &(mod(year,1000) .eq. 0)))&
      &then
         mday = mday + 1
      endif
!
!        Increment day number
!
      dayno = dayno + mday
100 continue
!
!     Add actual number of days of current month
!
   dayno = dayno + day

   return
end
