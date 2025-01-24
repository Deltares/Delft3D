subroutine FLNSEC(hi   ,igr ,asubsc ,subsec,secth0 ,secth1 ,wfh0 ,&
&wfh1 ,af  ,afh0   ,afh1  ,ngrid  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLNSEC (FLow determine Number of SECtions)
!
! Module description: Given water level hi subroutine FLNSEC
!                     determines the actual number of sub sections in a
!                     cross section.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 af(ngrid)         I  Flow area at every grid point at time t(n+1)
!  9 afh0(ngrid)       I  Flow area Af at water level h=h0 for every
!                         grid point.
! 10 afh1(ngrid)       I  Flow area Af at water level h=h1 for every
!                         grid point.
!                         Calculated with underrelaxation.
!                         Input parameter on the former iteration level.
!                         Output parameter on the actual iteration level
!  2 asubsc            O  Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!  1 hi                I  Actual water level in grid point.
!  4 secth0(ngrid)     I  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
!  5 secth1(ngrid)     I  H0-value (for 2 sub section) for every grid
!                         point.
!  3 subsec(ngrid)     I  Defines the number of sub sections for every
!                         cross section:
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!                         (For a circle cross section   : 0 ;
!                          For a sedredge cross section : 1 )
!  6 wfh0(ngrid)       I  Flow width Wf at water level h=h0 for every
!                         grid point.
!  7 wfh1(ngrid)       I  Flow width Wf at water level h=h1 for every
!                         grid point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flnsec.pf,v $
! Revision 1.1  1999/03/15  14:31:24  kuipe_j
! bed friction table general
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid, igr
   real    hi
   real    asubsc
   real    subsec(ngrid) , af(ngrid)
   real    afh0(ngrid)   , afh1(ngrid)
   real    secth0(ngrid) , secth1(ngrid)
   real    wfh0(ngrid)   , wfh1(ngrid)
!
!     Declaration of local variables:
!
   real    h0, hh1, af0, af1, af2
!
!     Defined subsections:
!     subsec = 0 : only main section
!            = 1 : 1 sub section  (main + sub section 1)
!            = 2 : 2 sub sections (main + sub sections 1 + 2 )
!
!     Actual subsections:
!     asubsc = 0 : only main section
!            = 1 : 1 sub section  (main + sub section 1)
!            = 2 : 2 sub sections (main + sub sections 1 + 2 )
!
!     secth0(igr)  :  h0-value for grid point i
!     secth1(igr)  : hh1-value for grid point i
!
   if      ( int( subsec(igr) ) .eq. 0 ) then
      asubsc = 0.
   else if ( int( subsec(igr) ) .eq. 1 ) then
      h0  = secth0(igr)
      if (hi .le. h0) then
         asubsc = 0.
      else
!           Wen the difference between the two water levels
!           is very small Af1 can become < 0.
!
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
!
end
