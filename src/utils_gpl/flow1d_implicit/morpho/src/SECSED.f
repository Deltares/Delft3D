      subroutine secsed (isec   ,igp    ,igm    ,ngrid  ,maxlev ,relden,
     &                   d50    ,eb     ,chezy  ,u      ,depth  ,hlev  ,
     &                   x      ,factor )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SECSED (SEdiment Correct SEDiment transport)
c
c Module description: Correction of calculated sediment transport and
c                     celerity in a grid point in a channel for longitu-
c                     dinal bed slope effects.
c
c                     The correction is according to:
c                     [ Doc. S-FO-002.2KV / Eq. 6.10 and 6.11 ]
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 chezy             I  Chezy value
c  7 d50               I  D50
c 11 depth             I  avarage depth
c  8 eb(7)             I  E coefficients for a sedredge branch:
c                         (1) = E1 coefficient.
c                         (2) = E2 coefficient.
c                         (3) = E3 coefficient.
c                         (4) = E4 coefficient.
c                         (5) = E5 coefficient.
c                         (6) = E6 coefficient.
c                         (7) = E7 coefficient.
c 14 factor            O  Sediment reduction factor (sedredge)
c 12 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  3 igm               I  igr-1 (at branch begin = igr)
c  2 igp               I  igr+1 (at branch end = igr)
c  1 isec              I  Section number.
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  4 ngrid             I  Number of grid points in network.
c  6 relden            I  relative density
c 10 u                 I  velocity
c 13 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: secsed.pf,v $
c Revision 1.2  1995/05/30  07:07:13  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:15  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:36  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    isec   ,igp   ,igm    ,ngrid ,maxlev
      real       relden ,d50   ,chezy  ,u     ,depth  ,factor
      real       x(ngrid)      ,eb(7)
	double precision hlev(ngrid,maxlev)
c
c     Declaration of local parameters
c
      real       dzdx   ,shiel
c
      dzdx   = (hlev(igp,isec) - hlev(igm,isec)) / (x(igp) - x(igm))
      shiel  = (u/chezy)**2 / (relden * d50)
      factor = 1. - eb(5) /((shiel**eb(3))*((depth / d50)**eb(4)))* dzdx
c
      end
