      subroutine KAINS1(cpredn ,predin ,lfilt  ,ngrid  ,wt1    ,wt     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAINS1 (KAlman Initialize Next Step 1)
c
c Module description: Shift predicted covariances to new time step if the
c                     previous step was not a filter step.
c                     Increment current prediction interval.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 cpredn            IO Current prediction interval.
c  3 lfilt             O  = True if a filter step must be performed.
c  4 ngrid             I  Number of grid points in network.
c  2 predin            I  Prediction interval.
c  6 wt(ngrid)         I  Actual total width at every grid point.
c  5 wt1(ngrid)        O  Total width in every grid point i on time n
c                         (previous time).
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kains1.pf,v $
c Revision 1.3  1999/03/15  15:51:55  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:04  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:38  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer cpredn, predin,  ngrid
      real    wt1(ngrid), wt(ngrid)
      logical lfilt
c
c     Declaration of local variable:
c
      integer i

      cpredn = cpredn + 1
      if ( cpredn .gt. predin ) then
c
c        Set current prediction interval to zero.
c
         cpredn = 1
      endif
      if ( cpredn .eq. predin ) then
         lfilt = .true.
      else
         lfilt = .false.
      endif
c
      do 30 i = 1, ngrid
         wt1(i) = wt(i)
   30 continue
c
      end
