subroutine FLUPDG(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,&
&q      ,q1     ,q2     ,af     ,wf     ,rho    ,&
&crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,&
&ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,&
&idown  ,sign   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLUPDG (FLow UP/Downstream near General structure)
!
! Module description: For up/down stream several parameters are compu-
!                     ted. Water level at t(n) and t(n+1), velocity and
!                     the stream direction.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 21 au                O  -
! 12 crest             I  crest.
!  1 g                 I  Acceleration of gravity.
!  6 h1(ngrid)         I  Water level in every grid point at time t(n).
!  5 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 16 hdn               O  Downstream water level at t=(n).
! 14 hdnp1             IO Downstream water level at t=(n+1).
! 15 hun               O  Upstream water level at t=(n).
! 13 hunp1             IO Upstream water level at t=(n+1).
! 24 idown             IO Index of gridpoint downstream the structure.
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                I  Grid point on right side of structure (upper
!                         index).
! 23 iup               IO Index of gridpoint upstream the structure.
!  4 ngrid             I  Number of grid points in network.
!  8 q1(ngrid)         I  Discharge in every grid point at time t(n).
!  7 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 19 qun               O  -
! 20 qunp1             O  -
! 11 rho(ngrid)        I  Density of diluted water per grid point.
! 25 sign              O  Flow direction (+/-).
! 18 ud                IO Downstream velocity.
! 17 uu                IO Upstream velocity.
! 10 wf(ngrid)         I  Actual flow width at every grid point.
! 22 wu                O  -
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flupdg.pf,v $
! Revision 1.7  1999/03/15  15:51:01  kuipe_j
! tabs removed
!
! Revision 1.6  1997/10/03  06:39:38  kuipe_j
! criterium for flow drection changed
!
! Revision 1.5  1996/04/12  13:04:24  kuipe_j
! headers, minor changes
!
! Revision 1.4  1996/04/11  08:23:56  kuipe_j
! Kalman module added
!
! Revision 1.3  1996/01/17  14:38:56  kuipe_j
! header update
!
! Revision 1.2  1995/11/21  11:08:06  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.1  1995/09/22  10:02:29  kuipe_j
! variable dimensions, new headers
!
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer il, ir, iup, idown, ngrid
   double precision h(ngrid), h1(ngrid),&
   &q(ngrid), q1(ngrid), q2(ngrid)
   real    af(ngrid),wf(ngrid), rho(ngrid)
   double precision  g    ,hunp1, hdnp1, hun, hdn, uu, ud, sign ,&
   &elu  ,eld  , crest, qun, qunp1, wu, au
   real   fac
!
   iup   = il
   idown = ir
!
   hunp1 = h(iup)
   hdnp1 = h(idown)
!
   uu = q(iup)   / af(iup)
   ud = q(idown) / af(idown)

   fac = (q2(iup) + q2(idown))
   if (abs(fac).le.1e-10) then
      fac = 0.5
   else
      fac = (q(iup) + q(idown)) / fac
   endif
   if (fac.lt..8) then
!
!        The direction of flow is determined by the sign of
!        the pressure difference
!
      elu = hunp1 + (uu*uu) / (2.*g)
      eld = hdnp1 + (ud*ud) / (2.*g)
!
!     Methode "van Velzen" (8 sept '95)
!
      elu = (elu-crest)*dble(rho(iup))
      eld = (eld-crest)*dble(rho(idown))

      if ( elu .ge. eld ) then
         iup   = il
         idown = ir
         sign  = 1.0D0
      else
         iup   = ir
         idown = il
         sign  = -1.0D0
      endif
   else
      if ( q(iup) .gt. 0.) then
!
!        The direction of flow is assumed to be equal to the
!        direction in the previous iteration step
!
         iup   = il
         idown = ir
         sign  = 1.0D0
      else
         iup   = ir
         idown = il
         sign  = -1.0D0
      endif
   endif
!
!     Discharge and water levels on time n
!
   hun   = h1(iup)
   hdn   = h1(idown)
   qun   = q1(iup)

!
!     Discharges and water levels on time *
!
   hunp1 = h(iup)
   hdnp1 = h(idown)
   qunp1 = q(iup)
!
!     Velocities on time *
!
   uu = q(iup)   / af(iup)
   ud = q(idown) / af(idown)
!
!     Upstream Flow area and width
!
   wu = wf(iup)
   au = af(iup)
!
end
