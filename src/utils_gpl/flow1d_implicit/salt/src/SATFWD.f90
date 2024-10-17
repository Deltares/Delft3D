subroutine satfwd (im     ,nbran  ,ngrid  ,time  ,dt     ,tp     ,&
&mouth  ,branch ,q1     ,q2    ,lslack ,timout ,&
&mouqpu )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATFWD (SAlt Thatcher harl. Fresh Water Discharge)
!
! Module description: Calculate fresh water discharge for current tide
!                     at a mouth.
!
!                     First the subroutine checks if the flow has chan-
!                     ged (from outflowing to inflowing). If this is the
!                     case the last fraction is added to the tidal sum
!                     and averaged over the tidal period. After this the
!                     obtained discharge is stored for the last tide and
!                     the discharge of the last tide is stored for the
!                     one but last tide. A new tidal sum is initialised
!                     with the new flow fraction.
!
!                     If the flow did not change from outflow to inflow,
!                     but this did occur previously, the flow is added
!                     to the tidal sum. At the first change of flow
!                     direction no shifting of discharges will take
!                     place.
!
! Postcondition:      A flag is set that determines if it is the start
!                     of a new tidal period.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 dt                I  Computational time step dt [sec].
!  1 im                I  Mouth number.
! 11 lslack            IO = .true.   :  Slack water before flood encoun-
!                                       tered
!                         = .false.  :  Otherwise.
! 13 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
!        nmouth)          Harleman or ZWENDL dispersion formulation.
!                         - First index:
!                         (1,,) = Fresh water discharge.
!                         (2,,) = Flood volume.
!                         (3,,) = Maximum flood velocity.
!                         - Second index:
!                         (,0,) = For current tide. Mouqpu(i,0,j) con-
!                                 tains the actual sum or maximum on the
!                                 current time.
!                         (,1,) = For the last tide.
!                         (,2,) = For the tide before the last tide.
!                         - Third index:
!                         (,,i) = Mouth number.
!  7 mouth(2,nmouth)   I  Node numbers which are mouths:
!                         (1,i) = Node number j which is a mouth.
!                         (2,i) = Number of the branch that contains the
!                                 mouth.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 10 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  4 time              I  Actual time level tn+1. in sec.
! 12 timout(2,nmouth)  IO Administration for the calculation of fresh
!                         water discharge, flood volume and maximum
!                         flood velocity for every mouth:
!                         (1,i) = Starting time of current tide.
!                         (2,i) = 0 : Fist tidal period not started yet.
!                                 1 : Fist tidal period has started.
!  6 tp                I  Tidal period   (salt module).
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: satfwd.pf,v $
! Revision 1.4  1999/03/15  15:53:28  kuipe_j
! tabs removed
!
! Revision 1.3  1995/08/23  14:29:46  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.2  1995/05/30  07:06:20  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:01  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:17:25  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer   im    ,nbran ,ngrid
   integer   mouth (2,*)     ,branch(4,nbran)
   real      timout(2,*)     ,mouqpu(3,0:2,*)
   double    precision  time, dt, tp, q1(ngrid), q2(ngrid)
   logical   lslack
!
!     Declaration of local parameters
!
   integer   ibr   ,igr
   real      s     ,frac  ,tcursl
   logical   start
!
!     Determine if mouth is at begin or end of branch.
!
   ibr = mouth(2,im)
   if (branch(1,ibr) .eq. mouth(1,im)) then
      igr = branch(3,ibr)
      s   = 1.
   else
      igr = branch(4,ibr)
      s   = -1.
   endif
!
   start  = int(timout(2,im)) .eq. 1
   lslack = .false.
   if (q2(igr)*s .gt. 0. .and. q1(igr)*s .le. 0.) then
!
!        Change from outflow to inflow (slack water before flood).
!
      frac   = q2(igr) - q1(igr)
      if (frac*s .lt. 1.e-10) then
         frac = -.5
      else
         frac   = q1(igr) / frac
      endif
      tcursl = sngl(time - dt - dble(frac)*dt)
      if (start) then
!
!           Check if this slack water is about one tidal period later
!           than the previous one.
!
         if (sngl(time) .gt. timout(1,im)+sngl(.75*tp)) then
!
!              Shift fresh water discharge.
            mouqpu(1,2,im) = mouqpu(1,1,im)
            mouqpu(1,1,im) = -(mouqpu(1,0,im) + .5*q1(igr)*(1.-frac))&
            &* sngl(dt) * s / (tcursl-timout(1,im))
            if (mouqpu(1,1,im) .lt. 1.) then
               mouqpu(1,1,im) = mouqpu(1,2,im)
            endif
            lslack = .true.
         endif
      else
!
!           First tidal period is started.
         lslack = .true.
      endif
!
      if (lslack) then
!           Store starting time of tide.
         timout(1,im)   = tcursl
!
!           Calculate first fraction of volume of tide.
         mouqpu(1,0,im) = q2(igr) * .5 * frac
      endif
!
   endif
!
   if (.not.lslack.and.start) then
      mouqpu(1,0,im) = mouqpu(1,0,im) + q1(igr)
   endif
!
end
