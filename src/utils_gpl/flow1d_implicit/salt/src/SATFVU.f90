subroutine satfvu (im     ,nbran  ,ngrid  ,dt   ,lslack ,mouth  ,&
&branch ,q1     ,q2     ,af   ,timout ,mouqpu )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATFVU (SAlt Th. harl. Flood Vol. and max. vel.(U))
!
! Module description: Calculate flood volume and maximum flood velocity
!                     for current tide at a mouth.
!
!                     If the flow has changed from outflowing to inflo-
!                     wing the last fraction is added to the tidal sum
!                     of the flood volume. If the flow changes from
!                     inflowing to outflowing the last fraction will be
!                     added to the flood volume. In a tidal period more
!                     than one flood periods are allowed.
!
!                     At the start of a tidal period ,provided it is not
!                     the first one, the tidal sum is multiplied by the
!                     time step. The obtained flood volume and the
!                     maximum flood velocity are stored for the last
!                     tide and the values of the last tide are stored
!                     for the one but last tide. Initialization takes
!                     place at start of a new tidal period.
!
!                     If no change in flow direction occured the volume
!                     is updated by adding the discharge in case it is
!                     flood. Also the maximum velocity is updated.
!
!                     During ebb nothing is done.
!
! Precondition:       It is already determined if it is the start of a
!                     new tidal period.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 af(ngrid)         I  Flow area at every grid point at time t(n+1)
!  7 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  4 dt                I  Computational time step dt [sec].
!  1 im                I  Mouth number.
!  5 lslack            I  = .true.   :  Slack water before flood encoun-
!                                       tered
!                         = .false.  :  Otherwise.
! 12 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
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
!  6 mouth(2,nmouth)   I  Node numbers which are mouths:
!                         (1,i) = Node number j which is a mouth.
!                         (2,i) = Number of the branch that contains the
!                                 mouth.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  8 q1(ngrid)         I  Discharge in every grid point at time t(n).
!  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 11 timout(2,nmouth)  IO Administration for the calculation of fresh
!                         water discharge, flood volume and maximum
!                         flood velocity for every mouth:
!                         (1,i) = Starting time of current tide.
!                         (2,i) = 0 : Fist tidal period not started yet.
!                                 1 : Fist tidal period has started.
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
! $Log: satfvu.pf,v $
! Revision 1.2  1995/05/30  07:06:19  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:00  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:17:23  kuipe_j
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
   real      af(ngrid), timout(2,*), mouqpu(3,0:2,*)
   double    precision        dt, q1(ngrid), q2(ngrid)
   logical   lslack
!
!     Declaration of local parameters
!
   integer   ibr   ,igr
   real      s     ,frac
   logical   start ,sum
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
   start = int(timout(2,im)) .eq. 1
   if (lslack) then
!
!        Change from outflow to inflow (slack water before flood).
!
      if (start) then
!
!           Shift flood volume.
         mouqpu(2,2,im) = mouqpu(2,1,im)
         mouqpu(2,1,im) = mouqpu(2,0,im) * sngl(dt) * s
!
!           Shift maximum velocity.
         mouqpu(3,2,im) = mouqpu(3,1,im)
         mouqpu(3,1,im) = mouqpu(3,0,im)
      else
!
!           First tidal period is started.
         timout(2,im) = 1.
      endif
!
!        Initialize.
      mouqpu(2,0,im) = 0.
      mouqpu(3,0,im) = 0.
   endif
!
   sum   = .false.
   if (q2(igr)*s .gt. 0. .and. q1(igr)*s .le. 0.) then
!
!        Change from outflow to inflow (slack water before flood).
!        This may occur more than one time in a tidal period.
!        Calculate first fraction of a flood volume.
      frac   = q2(igr) - q1(igr)
      if (frac*s .lt. 1.e-10) then
         frac = 1.
      else
         frac = q1(igr) / frac
      endif
      mouqpu(2,0,im) = mouqpu(2,0,im) + q2(igr) * .5 * frac
!
!        Calculation of maximum velocity for first point of a
!        flood period
      mouqpu(3,0,im) = max(mouqpu(3,0,im),q2(igr)/af(igr)*s)

   else if (q2(igr)*s .lt. 0. .and. q1(igr)*s .ge. 0.) then
!
!        Change from inflow to outflow (slack water before ebb).
!        Calculate last fraction of flood volume.
!
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
!
!        Flood after determined low tide slack.
!
      mouqpu(2,0,im) = mouqpu(2,0,im) + q1(igr)
      mouqpu(3,0,im) = max(mouqpu(3,0,im),q2(igr)/af(igr)*s)
   endif
!
end
