      subroutine FLINAW (ngrid  ,i      ,lslot  ,
     +                   maxlev ,nlev   ,hlev   ,
     +                   hact   ,ilev   ,wght   ,
     +                   width  ,area   ,
     +                   w      ,a      ,psltvr ) 

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLINAW (FLow INterpolate Area and Width)
c
c Module description: Subroutine FLINAW interpolates in the provided
c                     table for the actual water level parameter the
c                     area and width.
c
c                     Interpolate in table according to [S-FO-001.5KV]
c                     (4-2 and 4-3)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 a                 O  computed area at actual water level.
c 11 area(ngrid,       I  Flow/total area.
c       maxlev)
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
c 14 psltvr(8,ngrid)   I  -
c 12 w                 IO computed width at actual water level.
c  9 wght              I  Weight factor.
c 10 width(ngrid,      I  Flow/total width.
c        maxlev)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flinaw.pf,v $
c Revision 1.5  1999/03/15  15:50:05  kuipe_j
c tabs removed
c
c Revision 1.4  1997/01/23  08:29:05  kuipe_j
c Make flow module robust
c
c Revision 1.3  1995/05/30  09:55:09  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:08  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:51  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:07  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer ngrid, i, maxlev,
     +        nlev(ngrid)
      logical lslot
      real    width(ngrid,maxlev), area(ngrid,maxlev),
     +        w, a, psltvr(7,ngrid)
	double precision hlev(ngrid,maxlev), hact, wght
c
c     Declaration of local variables:
c
      integer ilev, nmax
      real    wmax, hster, hmax, hdown, htop, wdown, wtop
c
c     **************************************
c     * Compute flow/total width Wf/Wt and *
c     *         flow/total area Af/At      *
c     * at level h = hact                    *
c     **************************************
c
      if ( lslot .and. ( real(hact) < psltvr(5,i) ) ) then
c       
c        Preismann slot defined and waterlevel inside slot
c
         hdown = psltvr(3,i)
         hster = psltvr(4,i)
         htop  = psltvr(5,i)
         wdown = psltvr(6,i)
         wtop  = psltvr(7,i)
         if ( real(hact, kind=kind(hster)) .le. hster ) then
            w = wdown
            a = w * ( real(hact, kind=kind(a))-hdown)
         else
            w = wdown + (wtop-wdown)*(real(hact) - hster)/(htop-hster)
            a = wdown * (hster-hdown) + (real(hact)-hster)*
     +          (wdown+w)/2.
         endif
      else
c
c        Remark: ilev < 0 means: table overflow
c
         if (ilev .gt. 0) then
c
c           Compute actual flow width
c           [Doc. S-FO-001.5KV   Eq. 4-3]
c
            w = (1.-real(wght, kind=kind(w))) * width(i,ilev) +
     +          real(wght, kind=kind(w)) * width(i,ilev+1)
c
c           Compute actual flow area
c           [Doc. S-FO-001.5KV   Eq. 4-3]
c
            a = area(i,ilev) + real(hact-hlev(i,ilev), kind=kind(a)) *
     +             ( width(i,ilev) + 0.5 * (w-width(i,ilev) ) )
         else
c
c           Situation actual water level exceeds table.
c           [Doc. S-FO-001.5KV   Eq. 4-2]
c
c           nmax  = dimension of table of flow widths at grid point i
c           hmax  = highest water level in table of flow widths at
c                   grid point i
c           wmax  = flow width at z=hmax
c
            nmax  = nlev(i)
            hmax  = hlev(i,nmax)
c
c           Compute flow/total width and area for table overflow
c
            wmax = width(i,nmax)
c
            w = wmax
            a = area(i,nmax) + (hact-hmax) * wmax
         endif
c
      endif
      end
