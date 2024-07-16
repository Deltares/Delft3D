      subroutine wqmday ( date, mday )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQMDAY (Water Quality Maximum DAY number)
c
c Module description: Calculate maximum day number for a given year
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 date              I  date in YYYYMMDD format
c  2 mday              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqdnum  Water Quality Day NUMber
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqmday.pf,v $
c Revision 1.2  1995/05/30  07:08:40  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:02  hoeks_a
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
      integer  date, mday
c
c     Variables
c
      integer  year, month, day, ndate
c
c     Extract year
c
      year   = int (date / 10000)
c
c     Assemble new date 31-12-year
c
      day     = 31
      month   = 12
      ndate   = day + month * 100 + year * 10000
c
c     Calculate daynumber
c
      call wqdnum ( ndate, mday )
c
      return
      end
