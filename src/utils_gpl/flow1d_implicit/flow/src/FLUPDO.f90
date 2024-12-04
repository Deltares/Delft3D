subroutine FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,&
&q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,&
&hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
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
! Module:             FLUPDO (FLow UP- and DOwnstream near structure)
!
! Module description: For up/down stream several parameters are compu-
!                     ted. Water level at t(n) and t(n+1), velocity and
!                     the stream direction.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 10 crest             I  crest.
!  1 g                 I  Acceleration of gravity.
!  6 h1(ngrid)         I  Water level in every grid point at time t(n).
!  5 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 14 hdn               O  Downstream water level at t=(n).
! 12 hdnp1             IO Downstream water level at t=(n+1).
! 13 hun               O  Upstream water level at t=(n).
! 11 hunp1             IO Upstream water level at t=(n+1).
! 18 idown             IO Index of gridpoint downstream the structure.
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                I  Grid point on right side of structure (upper
!                         index).
! 17 iup               IO Index of gridpoint upstream the structure.
!  4 ngrid             I  Number of grid points in network.
!  7 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
!  9 rho(ngrid)        I  Density of diluted water per grid point.
! 19 sign              O  Flow direction (+/-).
! 16 ud                IO Downstream velocity.
! 15 uu                IO Upstream velocity.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flupdo.pf,v $
! Revision 1.11  1999/03/15  15:51:02  kuipe_j
! tabs removed
!
! Revision 1.10  1997/10/03  06:39:39  kuipe_j
! criterium for flow drection changed
!
! Revision 1.9  1996/04/12  13:04:25  kuipe_j
! headers, minor changes
!
! Revision 1.8  1996/04/11  08:23:57  kuipe_j
! Kalman module added
!
! Revision 1.7  1995/09/22  10:02:30  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:05  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:37:00  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:32  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:35  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:37  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:15  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer il, ir, iup, idown, ngrid
   real    g,  af(ngrid)
   real    rho(ngrid)
   real    hunp1, hun, hdn, uu, ud, sign, hdnp1
   real    elu, eld
   real    crest, fac
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
!
   iup   = il
   idown = ir
!
   hunp1 = sngl(h(iup))
   hdnp1 = sngl(h(idown))
!
   uu = q(iup)   / af(iup)
   ud = q(idown) / af(idown)
!
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
      elu = (elu-crest)*rho(iup)
      eld = (eld-crest)*rho(idown)

      if ( elu .ge. eld ) then
         iup   = il
         idown = ir
         sign  = 1.0
      else
         iup   = ir
         idown = il
         sign  = -1.0
      endif

   else
      if ( q(iup) .gt. 0.) then
!
!        The direction of flow is assumed to be equal to the
!        direction in the previous iteration step
!
         iup   = il
         idown = ir
         sign  = 1.0
      else
         iup   = ir
         idown = il
         sign  = -1.0
      endif
   endif
!
!     Water levels on time n
!
   hun   = sngl(h1(iup))
   hdn   = sngl(h1(idown))
!
!     Water levels on time *
!
   hunp1 = sngl(h(iup))
   hdnp1 = sngl(h(idown))
!
!     Velocities on time *
!
   uu = sngl(q(iup)   / af(iup))
   ud = sngl(q(idown) / af(idown))
!
end
