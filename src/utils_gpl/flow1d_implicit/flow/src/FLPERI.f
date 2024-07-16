      subroutine FLPERI(ngrid  ,i      ,lslot  ,
     +                  maxlev ,nlev   ,hlev   ,
     +                  hact   ,ilev   ,
     +                  wft    ,wf     ,
     +                  of     ,o      ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLPERI (FLow PERImeter)
c
c Module description: Subroutine FLPERI interpolates in the provided
c                     table for the actual water level parameter the
c                     wetted perimeter.
c
c                     Interpolate in table according to [S-FO-001.5KV]
c                     (4-2 and 4-3)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 hact              I  Actual water level at gridpoint in branch.
c  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  2 i                 I  Gridpoint index in branch.
c  8 ilev              I  Level number in table.
c  3 lslot             I  True if Preissmann slot is present,
c                         otherwise false.
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 ngrid             I  Number of grid points in network.
c  5 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c 12 o(ngrid)          O  Wetted perimeter for total cross section.
c 11 of(ngrid)         I  Actual wetted perimeter at every cross secti-
c                         on.
c 13 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c 10 wf(ngrid)         I  Actual flow width at every grid point.
c  9 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flperi.pf,v $
c Revision 1.6  1999/03/15  15:50:22  kuipe_j
c tabs removed
c
c Revision 1.5  1997/02/17  10:20:52  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.4  1997/01/23  08:29:15  kuipe_j
c Make flow module robust
c
c Revision 1.3  1995/05/30  09:55:17  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:17  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:58  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:20  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  ngrid, i, maxlev, ilev,
     +         nlev(ngrid)
      logical  lslot
	real     wft (ngrid,maxlev), wf,
     +         of  (ngrid,maxlev),  o,
     +         psltvr(7,ngrid)
	double precision hlev(ngrid,maxlev), hact
ccc	double precision hact
c
c     Declaration of local variables:
c
      real  w1, w2, dh, dw, hdown, hster, wdown
ccc	double precision w1, w2, dh, dw, hdown, hster, wdown 

c
c     Compute wetted perimeter O for the actual water level (hact)
c
      if ( lslot .and. (hact .lt. psltvr(5,i) ) ) then
c
c        Preismann slot defined and waterlevel inside slot
c
         hdown = psltvr(3,i)
         hster = psltvr(4,i)
c        htop  = psltvr(5,i)
         wdown = psltvr(6,i)
c        wtop  = psltvr(7,i)
         if ( hact .gt. psltvr(4,i) ) then
            dh = hact - hster
            dw = (wf-wdown) / 2.
            o = wdown + 2. * ( hster-hdown ) + 
     +          2. * sqrt(dh*dh + dw*dw)
         else
            o = wdown + 2. * ( hact-hdown )
         endif
      else
         if (ilev .gt. 0) then
            w1 = wft(i,ilev)
            w2 = wf
c
            dh = hact - hlev(i,ilev)
            dw = (w2-w1) / 2.
c
            o = of(i,ilev) + 2. * sqrt(dh*dh + dw*dw)
         else
            o = of(i,nlev(i)) + 2. * ( hact-hlev(i,nlev(i)) )
         endif
      endif
      end
