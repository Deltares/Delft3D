      subroutine wqhsec ( itime, rtime )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQHSEC (Water Quality Hundredth of SEConds)
c
c Module description: Convert time HHMMSSHH to hundredth of seconds
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 itime             I  time in HHMMSSHH format
c  2 rtime             O  time in hundredth of seconds
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqhsec.pf,v $
c Revision 1.2  1995/05/30  07:08:39  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:00  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer  itime
      integer  rtime
c
c     Local variables
c
      integer  hour, minute, second
c
c     Extract time
c
      hour   = int (itime / 1000000)
      minute = mod (int (itime / 10000), 100)
      second = mod (itime, 10000 )
c
c     Calculate time in seconds
c
      rtime   =  second          +
     +         (minute *   6000) +
     +         (hour   * 360000)

      end
