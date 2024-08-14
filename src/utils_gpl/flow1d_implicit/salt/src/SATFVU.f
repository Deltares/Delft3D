      subroutine satfvu (im     ,nbran  ,ngrid  ,dt   ,lslack ,mouth  ,
     &                   branch ,q1     ,q2     ,af   ,timout ,mouqpu )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATFVU (SAlt Th. harl. Flood Vol. and max. vel.(U))
c
c Module description: Calculate flood volume and maximum flood velocity
c                     for current tide at a mouth.
c
c                     If the flow has changed from outflowing to inflo-
c                     wing the last fraction is added to the tidal sum
c                     of the flood volume. If the flow changes from
c                     inflowing to outflowing the last fraction will be
c                     added to the flood volume. In a tidal period more
c                     than one flood periods are allowed.
c
c                     At the start of a tidal period ,provided it is not
c                     the first one, the tidal sum is multiplied by the
c                     time step. The obtained flood volume and the
c                     maximum flood velocity are stored for the last
c                     tide and the values of the last tide are stored
c                     for the one but last tide. Initialization takes
c                     place at start of a new tidal period.
c
c                     If no change in flow direction occured the volume
c                     is updated by adding the discharge in case it is
c                     flood. Also the maximum velocity is updated.
c
c                     During ebb nothing is done.
c
c Precondition:       It is already determined if it is the start of a
c                     new tidal period.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c  7 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  4 dt                I  Computational time step dt [sec].
c  1 im                I  Mouth number.
c  5 lslack            I  = .true.   :  Slack water before flood encoun-
c                                       tered
c                         = .false.  :  Otherwise.
c 12 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
c        nmouth)          Harleman or ZWENDL dispersion formulation.
c                         - First index:
c                         (1,,) = Fresh water discharge.
c                         (2,,) = Flood volume.
c                         (3,,) = Maximum flood velocity.
c                         - Second index:
c                         (,0,) = For current tide. Mouqpu(i,0,j) con-
c                                 tains the actual sum or maximum on the
c                                 current time.
c                         (,1,) = For the last tide.
c                         (,2,) = For the tide before the last tide.
c                         - Third index:
c                         (,,i) = Mouth number.
c  6 mouth(2,nmouth)   I  Node numbers which are mouths:
c                         (1,i) = Node number j which is a mouth.
c                         (2,i) = Number of the branch that contains the
c                                 mouth.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  8 q1(ngrid)         I  Discharge in every grid point at time t(n).
c  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 11 timout(2,nmouth)  IO Administration for the calculation of fresh
c                         water discharge, flood volume and maximum
c                         flood velocity for every mouth:
c                         (1,i) = Starting time of current tide.
c                         (2,i) = 0 : Fist tidal period not started yet.
c                                 1 : Fist tidal period has started.
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
c $Log: satfvu.pf,v $
c Revision 1.2  1995/05/30  07:06:19  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:00  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  09:17:23  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer   im    ,nbran ,ngrid
      integer   mouth (2,*)     ,branch(4,nbran)
      real      af(ngrid), timout(2,*), mouqpu(3,0:2,*)
      double    precision        dt, q1(ngrid), q2(ngrid)
      logical   lslack
c
c     Declaration of local parameters
c
      integer   ibr   ,igr
      real      s     ,frac
      logical   start ,sum
c
c     Determine if mouth is at begin or end of branch.
c
      ibr = mouth(2,im)
      if (branch(1,ibr) .eq. mouth(1,im)) then
         igr = branch(3,ibr)
         s   = 1.
      else
         igr = branch(4,ibr)
         s   = -1.
      endif
c
      start = int(timout(2,im)) .eq. 1
      if (lslack) then
c
c        Change from outflow to inflow (slack water before flood).
c
         if (start) then
c
c           Shift flood volume.
            mouqpu(2,2,im) = mouqpu(2,1,im)
            mouqpu(2,1,im) = mouqpu(2,0,im) * sngl(dt) * s
c
c           Shift maximum velocity.
            mouqpu(3,2,im) = mouqpu(3,1,im)
            mouqpu(3,1,im) = mouqpu(3,0,im)
         else
c
c           First tidal period is started.
            timout(2,im) = 1.
         endif
c
c        Initialize.
         mouqpu(2,0,im) = 0.
         mouqpu(3,0,im) = 0.
      endif
c
      sum   = .false.
      if (q2(igr)*s .gt. 0. .and. q1(igr)*s .le. 0.) then
c
c        Change from outflow to inflow (slack water before flood).
c        This may occur more than one time in a tidal period.
c        Calculate first fraction of a flood volume.
         frac   = q2(igr) - q1(igr)
         if (frac*s .lt. 1.e-10) then
            frac = 1.
         else
            frac = q1(igr) / frac
         endif
         mouqpu(2,0,im) = mouqpu(2,0,im) + q2(igr) * .5 * frac
c
c        Calculation of maximum velocity for first point of a
c        flood period
         mouqpu(3,0,im) = max(mouqpu(3,0,im),q2(igr)/af(igr)*s)

      else if (q2(igr)*s .lt. 0. .and. q1(igr)*s .ge. 0.) then
c
c        Change from inflow to outflow (slack water before ebb).
c        Calculate last fraction of flood volume.
c
         frac   = q2(igr) - q1(igr)
         if (frac*s .gt. -1.e-10) then
            frac = 1.
         else
            frac = q1(igr) / frac
         endif
         mouqpu(2,0,im) = mouqpu(2,0,im) + .5 * q1(igr) * (1. - frac)
      else
         sum = start
      endif

      if (sum .and. q2(igr)*s .gt. 0. ) then
c
c        Flood after determined low tide slack.
c
         mouqpu(2,0,im) = mouqpu(2,0,im) + q1(igr)
         mouqpu(3,0,im) = max(mouqpu(3,0,im),q2(igr)/af(igr)*s)
      endif
c
      end
