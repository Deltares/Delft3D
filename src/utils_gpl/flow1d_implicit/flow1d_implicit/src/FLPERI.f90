subroutine FLPERI(ngrid  ,i      ,lslot  ,&
&maxlev ,nlev   ,hlev   ,&
&hact   ,ilev   ,&
&wft    ,wf     ,&
&of     ,o      ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLPERI (FLow PERImeter)
!
! Module description: Subroutine FLPERI interpolates in the provided
!                     table for the actual water level parameter the
!                     wetted perimeter.
!
!                     Interpolate in table according to [S-FO-001.5KV]
!                     (4-2 and 4-3)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 hact              I  Actual water level at gridpoint in branch.
!  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  2 i                 I  Gridpoint index in branch.
!  8 ilev              I  Level number in table.
!  3 lslot             I  True if Preissmann slot is present,
!                         otherwise false.
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  5 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
! 12 o(ngrid)          O  Wetted perimeter for total cross section.
! 11 of(ngrid)         I  Actual wetted perimeter at every cross secti-
!                         on.
! 13 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
! 10 wf(ngrid)         I  Actual flow width at every grid point.
!  9 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flperi.pf,v $
! Revision 1.6  1999/03/15  15:50:22  kuipe_j
! tabs removed
!
! Revision 1.5  1997/02/17  10:20:52  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.4  1997/01/23  08:29:15  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/05/30  09:55:17  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:17  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:58  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:20  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, i, maxlev, ilev,&
   &nlev(ngrid)
   logical  lslot
   real     wft (ngrid,maxlev), wf,&
   &of  (ngrid,maxlev),  o,&
   &psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), hact
!cc	double precision hact
!
!     Declaration of local variables:
!
   real  w1, w2, dh, dw, hdown, hster, wdown
!cc	double precision w1, w2, dh, dw, hdown, hster, wdown

!
!     Compute wetted perimeter O for the actual water level (hact)
!
   if ( lslot .and. (hact .lt. psltvr(5,i) ) ) then
!
!        Preismann slot defined and waterlevel inside slot
!
      hdown = psltvr(3,i)
      hster = psltvr(4,i)
!        htop  = psltvr(5,i)
      wdown = psltvr(6,i)
!        wtop  = psltvr(7,i)
      if ( hact .gt. psltvr(4,i) ) then
         dh = hact - hster
         dw = (wf-wdown) / 2.
         o = wdown + 2. * ( hster-hdown ) +&
         &2. * sqrt(dh*dh + dw*dw)
      else
         o = wdown + 2. * ( hact-hdown )
      endif
   else
      if (ilev .gt. 0) then
         w1 = wft(i,ilev)
         w2 = wf
!
         dh = hact - hlev(i,ilev)
         dw = (w2-w1) / 2.
!
         o = of(i,ilev) + 2. * sqrt(dh*dh + dw*dw)
      else
         o = of(i,nlev(i)) + 2. * ( hact-hlev(i,nlev(i)) )
      endif
   endif
end
