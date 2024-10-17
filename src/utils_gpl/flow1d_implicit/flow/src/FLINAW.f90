subroutine FLINAW (ngrid  ,i      ,lslot  ,&
&maxlev ,nlev   ,hlev   ,&
&hact   ,ilev   ,wght   ,&
&width  ,area   ,&
&w      ,a      ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLINAW (FLow INterpolate Area and Width)
!
! Module description: Subroutine FLINAW interpolates in the provided
!                     table for the actual water level parameter the
!                     area and width.
!
!                     Interpolate in table according to [S-FO-001.5KV]
!                     (4-2 and 4-3)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 a                 O  computed area at actual water level.
! 11 area(ngrid,       I  Flow/total area.
!       maxlev)
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
! 14 psltvr(8,ngrid)   I  -
! 12 w                 IO computed width at actual water level.
!  9 wght              I  Weight factor.
! 10 width(ngrid,      I  Flow/total width.
!        maxlev)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flinaw.pf,v $
! Revision 1.5  1999/03/15  15:50:05  kuipe_j
! tabs removed
!
! Revision 1.4  1997/01/23  08:29:05  kuipe_j
! Make flow module robust
!
! Revision 1.3  1995/05/30  09:55:09  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:08  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:51  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:07  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid, i, maxlev,&
   &nlev(ngrid)
   logical lslot
   real    width(ngrid,maxlev), area(ngrid,maxlev),&
   &w, a, psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), hact, wght
!
!     Declaration of local variables:
!
   integer ilev, nmax
   real    wmax, hster, hmax, hdown, htop, wdown, wtop
!
!     **************************************
!     * Compute flow/total width Wf/Wt and *
!     *         flow/total area Af/At      *
!     * at level h = hact                    *
!     **************************************
!
   if ( lslot .and. ( sngl(hact) .lt. psltvr(5,i) ) ) then
!
!        Preismann slot defined and waterlevel inside slot
!
      hdown = psltvr(3,i)
      hster = psltvr(4,i)
      htop  = psltvr(5,i)
      wdown = psltvr(6,i)
      wtop  = psltvr(7,i)
      if ( sngl(hact) .le. hster ) then
         w = wdown
         a = w * ( sngl(hact)-hdown)
      else
         w = wdown + (wtop-wdown)*(sngl(hact)-hster)/(htop-hster)
         a = wdown * (hster-hdown) + (sngl(hact)-hster)*&
         &(wdown+w)/2.
      endif
   else
!
!        Remark: ilev < 0 means: table overflow
!
      if (ilev .gt. 0) then
!
!           Compute actual flow width
!           [Doc. S-FO-001.5KV   Eq. 4-3]
!
         w = (1.-sngl(wght)) * width(i,ilev) +&
         &sngl(wght) * width(i,ilev+1)
!
!           Compute actual flow area
!           [Doc. S-FO-001.5KV   Eq. 4-3]
!
         a = area(i,ilev) + sngl(hact-hlev(i,ilev)) *&
         &( width(i,ilev) + 0.5 * (w-width(i,ilev) ) )
      else
!
!           Situation actual water level exceeds table.
!           [Doc. S-FO-001.5KV   Eq. 4-2]
!
!           nmax  = dimension of table of flow widths at grid point i
!           hmax  = highest water level in table of flow widths at
!                   grid point i
!           wmax  = flow width at z=hmax
!
         nmax  = nlev(i)
         hmax  = hlev(i,nmax)
!
!           Compute flow/total width and area for table overflow
!
         wmax = width(i,nmax)
!
         w = wmax
         a = area(i,nmax) + (hact-hmax) * wmax
      endif
!
   endif
end
