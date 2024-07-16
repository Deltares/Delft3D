      subroutine satfwd (im     ,nbran  ,ngrid  ,time  ,dt     ,tp     ,
     &                   mouth  ,branch ,q1     ,q2    ,lslack ,timout ,
     &                   mouqpu )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATFWD (SAlt Thatcher harl. Fresh Water Discharge)
c
c Module description: Calculate fresh water discharge for current tide
c                     at a mouth.
c
c                     First the subroutine checks if the flow has chan-
c                     ged (from outflowing to inflowing). If this is the
c                     case the last fraction is added to the tidal sum
c                     and averaged over the tidal period. After this the
c                     obtained discharge is stored for the last tide and
c                     the discharge of the last tide is stored for the
c                     one but last tide. A new tidal sum is initialised
c                     with the new flow fraction.
c
c                     If the flow did not change from outflow to inflow,
c                     but this did occur previously, the flow is added
c                     to the tidal sum. At the first change of flow
c                     direction no shifting of discharges will take
c                     place.
c
c Postcondition:      A flag is set that determines if it is the start
c                     of a new tidal period.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 dt                I  Computational time step dt [sec].
c  1 im                I  Mouth number.
c 11 lslack            IO = .true.   :  Slack water before flood encoun-
c                                       tered
c                         = .false.  :  Otherwise.
c 13 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
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
c  7 mouth(2,nmouth)   I  Node numbers which are mouths:
c                         (1,i) = Node number j which is a mouth.
c                         (2,i) = Number of the branch that contains the
c                                 mouth.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
c 10 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  4 time              I  Actual time level tn+1. in sec.
c 12 timout(2,nmouth)  IO Administration for the calculation of fresh
c                         water discharge, flood volume and maximum
c                         flood velocity for every mouth:
c                         (1,i) = Starting time of current tide.
c                         (2,i) = 0 : Fist tidal period not started yet.
c                                 1 : Fist tidal period has started.
c  6 tp                I  Tidal period   (salt module).
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
c $Log: satfwd.pf,v $
c Revision 1.4  1999/03/15  15:53:28  kuipe_j
c tabs removed
c
c Revision 1.3  1995/08/23  14:29:46  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.2  1995/05/30  07:06:20  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:01  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  09:17:25  kuipe_j
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
      real      timout(2,*)     ,mouqpu(3,0:2,*)
      double    precision  time, dt, tp, q1(ngrid), q2(ngrid)
      logical   lslack
c
c     Declaration of local parameters
c
      integer   ibr   ,igr
      real      s     ,frac  ,tcursl
      logical   start
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
      start  = int(timout(2,im)) .eq. 1
      lslack = .false.
      if (q2(igr)*s .gt. 0. .and. q1(igr)*s .le. 0.) then
c
c        Change from outflow to inflow (slack water before flood).
c
         frac   = q2(igr) - q1(igr)
         if (frac*s .lt. 1.e-10) then
            frac = -.5
         else
            frac   = q1(igr) / frac
         endif
         tcursl = sngl(time - dt - dble(frac)*dt)
         if (start) then
c
c           Check if this slack water is about one tidal period later
c           than the previous one.
c
            if (sngl(time) .gt. timout(1,im)+sngl(.75*tp)) then
c
c              Shift fresh water discharge.
               mouqpu(1,2,im) = mouqpu(1,1,im)
               mouqpu(1,1,im) = -(mouqpu(1,0,im) + .5*q1(igr)*(1.-frac))
     &                          * sngl(dt) * s / (tcursl-timout(1,im))
               if (mouqpu(1,1,im) .lt. 1.) then
                  mouqpu(1,1,im) = mouqpu(1,2,im)
               endif
               lslack = .true.
            endif
         else
c
c           First tidal period is started.
            lslack = .true.
         endif
c
         if (lslack) then
c           Store starting time of tide.
            timout(1,im)   = tcursl
c
c           Calculate first fraction of volume of tide.
            mouqpu(1,0,im) = q2(igr) * .5 * frac
         endif
c
      endif
c
      if (.not.lslack.and.start) then
         mouqpu(1,0,im) = mouqpu(1,0,im) + q1(igr)
      endif
c
      end
