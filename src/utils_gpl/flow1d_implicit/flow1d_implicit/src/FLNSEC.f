      subroutine FLNSEC(hi   ,igr ,asubsc ,subsec,secth0 ,secth1 ,wfh0 ,
     +                  wfh1 ,af  ,afh0   ,afh1  ,ngrid  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             FLNSEC (FLow determine Number of SECtions)
c
c Module description: Given water level hi subroutine FLNSEC
c                     determines the actual number of sub sections in a
c                     cross section.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c  9 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
c                         grid point.
c 10 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
c                         grid point.
c                         Calculated with underrelaxation.
c                         Input parameter on the former iteration level.
c                         Output parameter on the actual iteration level
c  2 asubsc            O  Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c  1 hi                I  Actual water level in grid point.
c  4 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
c                         grid point.
c  5 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
c                         point.
c  3 subsec(ngrid)     I  Defines the number of sub sections for every
c                         cross section:
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c                         (For a circle cross section   : 0 ;
c                          For a sedredge cross section : 1 )
c  6 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
c                         grid point.
c  7 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
c                         grid point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flnsec.pf,v $
c Revision 1.1  1999/03/15  14:31:24  kuipe_j
c bed friction table general
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer ngrid, igr
	real    hi
      real    asubsc
      real    subsec(ngrid) , af(ngrid)
      real    afh0(ngrid)   , afh1(ngrid)
      real    secth0(ngrid) , secth1(ngrid)
      real    wfh0(ngrid)   , wfh1(ngrid)
c
c     Declaration of local variables:
c
      real    h0, hh1, af0, af1, af2
c
c     Defined subsections:
c     subsec = 0 : only main section
c            = 1 : 1 sub section  (main + sub section 1)
c            = 2 : 2 sub sections (main + sub sections 1 + 2 )
c
c     Actual subsections:
c     asubsc = 0 : only main section
c            = 1 : 1 sub section  (main + sub section 1)
c            = 2 : 2 sub sections (main + sub sections 1 + 2 )
c
c     secth0(igr)  :  h0-value for grid point i
c     secth1(igr)  : hh1-value for grid point i
c
      if      ( int( subsec(igr) ) .eq. 0 ) then
         asubsc = 0.
      else if ( int( subsec(igr) ) .eq. 1 ) then
         h0  = secth0(igr)
         if (hi .le. h0) then
            asubsc = 0.
         else
c           Wen the difference between the two water levels
c           is very small Af1 can become < 0.
c
            Af0 = Afh0(igr) + (hi-h0) * Wfh0(igr)
            Af1 = Af(igr) - Af0
            if (Af1 .le. 0) then
               asubsc = 0.
            else
               asubsc = 1.
            endif
         endif
      else if ( int( subsec(igr) ) .eq. 2 ) then
         h0  = secth0(igr)
         hh1 = secth1(igr)
         if      (hi .le. h0) then
            asubsc = 0.
         else if (hi .le. hh1) then
            Af0 = Afh0(igr) + (hi-h0) * Wfh0(igr)
            Af1 = Af(igr) - Af0
            if (Af1 .le. 0) then
               asubsc = 0.
            else
               asubsc = 1.
            endif
         else
            Af0 = Afh0(igr) + (hi-h0 ) * Wfh0(igr)
            Af1 = Afh1(igr) + (hi-hh1) * Wfh1(igr) - Af0
            Af2 = Af(igr) - Af1 - Af0
            if (Af1 .le. 0) then
               asubsc = 0.
            else if (Af2 .le. 0) then
               asubsc = 1.
            else
               asubsc = 2.
            endif
         endif
      endif
c
      end
