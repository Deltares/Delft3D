      function FLBOTT (igr    ,ngrid  ,maxlev ,hlev   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLBOTT (FLow determine BOTTom)
c
c Module description: Determine the lowest bed level for a sedredge
c                     branch.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 flbott            O  Function value.
c  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 igr               I  Gridpoint index.
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 ngrid             I  Number of grid points in network.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flbott.pf,v $
c Revision 1.3  1995/05/30  09:54:47  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:44  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:30  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Function declaration:
c
      real FLBOTT
c
c     Declaration of Parameters:
c
      integer igr, ngrid, maxlev
      double precision hlev(ngrid,maxlev)
c
      FLBOTT = min ( hlev(igr,1), hlev(igr,2) )
c
      end
