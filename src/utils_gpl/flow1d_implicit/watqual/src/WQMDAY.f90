subroutine wqmday ( date, mday )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQMDAY (Water Quality Maximum DAY number)
!
! Module description: Calculate maximum day number for a given year
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 date              I  date in YYYYMMDD format
!  2 mday              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqdnum  Water Quality Day NUMber
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqmday.pf,v $
! Revision 1.2  1995/05/30  07:08:40  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:02  hoeks_a
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
   integer  date, mday
!
!     Variables
!
   integer  year, month, day, ndate
!
!     Extract year
!
   year   = int (date / 10000)
!
!     Assemble new date 31-12-year
!
   day     = 31
   month   = 12
   ndate   = day + month * 100 + year * 10000
!
!     Calculate daynumber
!
   call wqdnum ( ndate, mday )
!
   return
end
