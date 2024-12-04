subroutine secsed (isec   ,igp    ,igm    ,ngrid  ,maxlev ,relden,&
&d50    ,eb     ,chezy  ,u      ,depth  ,hlev  ,&
&x      ,factor )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SECSED (SEdiment Correct SEDiment transport)
!
! Module description: Correction of calculated sediment transport and
!                     celerity in a grid point in a channel for longitu-
!                     dinal bed slope effects.
!
!                     The correction is according to:
!                     [ Doc. S-FO-002.2KV / Eq. 6.10 and 6.11 ]
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 chezy             I  Chezy value
!  7 d50               I  D50
! 11 depth             I  avarage depth
!  8 eb(7)             I  E coefficients for a sedredge branch:
!                         (1) = E1 coefficient.
!                         (2) = E2 coefficient.
!                         (3) = E3 coefficient.
!                         (4) = E4 coefficient.
!                         (5) = E5 coefficient.
!                         (6) = E6 coefficient.
!                         (7) = E7 coefficient.
! 14 factor            O  Sediment reduction factor (sedredge)
! 12 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  3 igm               I  igr-1 (at branch begin = igr)
!  2 igp               I  igr+1 (at branch end = igr)
!  1 isec              I  Section number.
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  4 ngrid             I  Number of grid points in network.
!  6 relden            I  relative density
! 10 u                 I  velocity
! 13 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: secsed.pf,v $
! Revision 1.2  1995/05/30  07:07:13  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:15  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:36  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    isec   ,igp   ,igm    ,ngrid ,maxlev
   real       relden ,d50   ,chezy  ,u     ,depth  ,factor
   real       x(ngrid)      ,eb(7)
   double precision hlev(ngrid,maxlev)
!
!     Declaration of local parameters
!
   real       dzdx   ,shiel
!
   dzdx   = (hlev(igp,isec) - hlev(igm,isec)) / (x(igp) - x(igm))
   shiel  = (u/chezy)**2 / (relden * d50)
   factor = 1. - eb(5) /((shiel**eb(3))*((depth / d50)**eb(4)))* dzdx
!
end
