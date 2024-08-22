      subroutine FLUPDG(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +                  q      ,q1     ,q2     ,af     ,wf     ,rho    ,
     +                  crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,
     +                  ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,
     +                  idown  ,sign   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLUPDG (FLow UP/Downstream near General structure)
c
c Module description: For up/down stream several parameters are compu-
c                     ted. Water level at t(n) and t(n+1), velocity and
c                     the stream direction.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 21 au                O  -
c 12 crest             I  crest.
c  1 g                 I  Acceleration of gravity.
c  6 h1(ngrid)         I  Water level in every grid point at time t(n).
c  5 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 16 hdn               O  Downstream water level at t=(n).
c 14 hdnp1             IO Downstream water level at t=(n+1).
c 15 hun               O  Upstream water level at t=(n).
c 13 hunp1             IO Upstream water level at t=(n+1).
c 24 idown             IO Index of gridpoint downstream the structure.
c  2 il                I  Grid point on left side of structure (lower
c                         index).
c  3 ir                I  Grid point on right side of structure (upper
c                         index).
c 23 iup               IO Index of gridpoint upstream the structure.
c  4 ngrid             I  Number of grid points in network.
c  8 q1(ngrid)         I  Discharge in every grid point at time t(n).
c  7 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 19 qun               O  -
c 20 qunp1             O  -
c 11 rho(ngrid)        I  Density of diluted water per grid point.
c 25 sign              O  Flow direction (+/-).
c 18 ud                IO Downstream velocity.
c 17 uu                IO Upstream velocity.
c 10 wf(ngrid)         I  Actual flow width at every grid point.
c 22 wu                O  -
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flupdg.pf,v $
c Revision 1.7  1999/03/15  15:51:01  kuipe_j
c tabs removed
c
c Revision 1.6  1997/10/03  06:39:38  kuipe_j
c criterium for flow drection changed
c
c Revision 1.5  1996/04/12  13:04:24  kuipe_j
c headers, minor changes
c
c Revision 1.4  1996/04/11  08:23:56  kuipe_j
c Kalman module added
c
c Revision 1.3  1996/01/17  14:38:56  kuipe_j
c header update
c
c Revision 1.2  1995/11/21  11:08:06  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.1  1995/09/22  10:02:29  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer il, ir, iup, idown, ngrid
      double precision h(ngrid), h1(ngrid), 
     +                 q(ngrid), q1(ngrid), q2(ngrid)
      real    af(ngrid),wf(ngrid), rho(ngrid)
      double precision  g    ,hunp1, hdnp1, hun, hdn, uu, ud, sign ,
     +                  elu  ,eld  , crest, qun, qunp1, wu, au
      real   fac
c
      iup   = il
      idown = ir
c
      hunp1 = h(iup)
      hdnp1 = h(idown)
c
      uu = q(iup)   / af(iup)
      ud = q(idown) / af(idown)

      fac = (q2(iup) + q2(idown))
      if (abs(fac).le.1e-10) then
         fac = 0.5
      else
         fac = (q(iup) + q(idown)) / fac
      endif
      if (fac.lt..8) then
c
c        The direction of flow is determined by the sign of
c        the pressure difference
c
         elu = hunp1 + (uu*uu) / (2.*g)
         eld = hdnp1 + (ud*ud) / (2.*g)
c
c     Methode "van Velzen" (8 sept '95)
c
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
c
c        The direction of flow is assumed to be equal to the 
c        direction in the previous iteration step
c
            iup   = il
            idown = ir
            sign  = 1.0D0
         else
            iup   = ir
            idown = il
            sign  = -1.0D0
         endif
      endif
c
c     Discharge and water levels on time n
c
      hun   = h1(iup)
      hdn   = h1(idown)
      qun   = q1(iup)

c
c     Discharges and water levels on time *
c
      hunp1 = h(iup)
      hdnp1 = h(idown)
      qunp1 = q(iup)
c
c     Velocities on time *
c
      uu = q(iup)   / af(iup)
      ud = q(idown) / af(idown)
c
c     Upstream Flow area and width
c
      wu = wf(iup)
      au = af(iup)
c
      end
