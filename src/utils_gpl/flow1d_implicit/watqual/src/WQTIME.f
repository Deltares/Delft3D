      subroutine wqtime ( itim1, itim2, dt )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQTIME (Water Quality TIME conversion)
c
c Module description: Calculate the delta time between two water quality
c                     time steps in seconds
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 dt                O  Computational time step dt [sec].
c  1 itim1             P  -
c  2 itim2             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqdnum  Water Quality Day NUMber
c wqhsec  Water Quality Hundredth of SEConds
c wqmday  Water Quality Maximum DAY number
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqtime.pf,v $
c Revision 1.5  1997/01/20  13:14:21  kuipe_j
c Remove auz. output
c
c Revision 1.4  1996/12/20  15:44:30  kuipe_j
c rounding error in time step
c
c Revision 1.3  1995/10/18  09:00:50  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:08:45  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:11:06  hoeks_a
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
      integer itim1(2), itim2(2)
      integer dt
c
c     Local variables
c
      integer day1, day2, hsec1, hsec2, nhsec, diff, mday, difday
c
c     Calculate daynumber of date 1 and 2
c
      call wqdnum ( itim1(1), day1 )
      call wqdnum ( itim2(1), day2 )
c
c     Calculate time in hundreds of seconds
c
      call wqhsec ( itim1(2), hsec1 )
      call wqhsec ( itim2(2), hsec2 )
c
c     Number of seconds between days
c
      if (day2 - day1 .lt. 0) then
         call wqmday ( itim1(1), mday )
         difday = mday - day1 + day2
      else
         difday = day2 - day1
      endif
c
c     Calculate number of 100 th seconds
c
      nhsec = difday * 8640000
c
c     Difference
c
      diff = hsec2 + nhsec - hsec1
c
c     Calculate dt [s]
c
      dt = (diff + 50) / 100
c
      end
