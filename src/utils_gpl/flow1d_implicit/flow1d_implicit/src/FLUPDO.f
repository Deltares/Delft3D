      subroutine FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +                  q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,
     +                  hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
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
c Module:             FLUPDO (FLow UP- and DOwnstream near structure)
c
c Module description: For up/down stream several parameters are compu-
c                     ted. Water level at t(n) and t(n+1), velocity and
c                     the stream direction.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 10 crest             I  crest.
c  1 g                 I  Acceleration of gravity.
c  6 h1(ngrid)         I  Water level in every grid point at time t(n).
c  5 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 14 hdn               O  Downstream water level at t=(n).
c 12 hdnp1             IO Downstream water level at t=(n+1).
c 13 hun               O  Upstream water level at t=(n).
c 11 hunp1             IO Upstream water level at t=(n+1).
c 18 idown             IO Index of gridpoint downstream the structure.
c  2 il                I  Grid point on left side of structure (lower
c                         index).
c  3 ir                I  Grid point on right side of structure (upper
c                         index).
c 17 iup               IO Index of gridpoint upstream the structure.
c  4 ngrid             I  Number of grid points in network.
c  7 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c  9 rho(ngrid)        I  Density of diluted water per grid point.
c 19 sign              O  Flow direction (+/-).
c 16 ud                IO Downstream velocity.
c 15 uu                IO Upstream velocity.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flupdo.pf,v $
c Revision 1.11  1999/03/15  15:51:02  kuipe_j
c tabs removed
c
c Revision 1.10  1997/10/03  06:39:39  kuipe_j
c criterium for flow drection changed
c
c Revision 1.9  1996/04/12  13:04:25  kuipe_j
c headers, minor changes
c
c Revision 1.8  1996/04/11  08:23:57  kuipe_j
c Kalman module added
c
c Revision 1.7  1995/09/22  10:02:30  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:05  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:37:00  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:32  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:35  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:37  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:15  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer il, ir, iup, idown, ngrid
      real    g,  af(ngrid)
      real    rho(ngrid)
      real    hunp1, hun, hdn, uu, ud, sign, hdnp1
      real    elu, eld
      real    crest, fac
	double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
c
      iup   = il
      idown = ir
c
      hunp1 = sngl(h(iup))
      hdnp1 = sngl(h(idown))
c
      uu = q(iup)   / af(iup)
      ud = q(idown) / af(idown)
c
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
c
c        The direction of flow is assumed to be equal to the 
c        direction in the previous iteration step
c
            iup   = il
            idown = ir
            sign  = 1.0
         else
            iup   = ir
            idown = il
            sign  = -1.0
         endif
      endif
c
c     Water levels on time n
c
      hun   = sngl(h1(iup))
      hdn   = sngl(h1(idown))
c
c     Water levels on time *
c
      hunp1 = sngl(h(iup))
      hdnp1 = sngl(h(idown))
c
c     Velocities on time *
c
      uu = sngl(q(iup)   / af(iup))
      ud = sngl(q(idown) / af(idown))
c
      end
