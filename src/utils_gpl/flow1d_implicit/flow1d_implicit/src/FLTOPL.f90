function FLTOPL (igr    ,ngrid  ,maxlev ,hlev   ,nlev)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLTOPL (FLow determine TOP Level)
!
! Module description: Determine the highest water level in a cross
!                     section.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 fltopl            O  Function value of function FLTOPL (=highest
!                         water level in a cross section).
!  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 igr               I  Gridpoint index.
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 ngrid             I  Number of grid points in network.
!  5 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fltopl.pf,v $
! Revision 1.4  1995/09/22  10:02:22  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:55:33  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:34  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:13  hoeks_a
! Initial check-in
!
! Revision 1.1  1994/12/02  13:24:43  kuipe_j
! Initial / added for improvement of autostart.
!
! Revision 1.2  1993/11/26  15:30:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Function declaration:
!
   real FLTOPL
!
!     Declaration of Parameters:
!
   integer igr, ngrid, maxlev
   integer nlev(ngrid)
   double precision hlev(ngrid,maxlev)
!
   FLTOPL = hlev(igr,nlev(igr) )
!
end
