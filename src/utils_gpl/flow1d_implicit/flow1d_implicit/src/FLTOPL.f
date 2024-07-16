      function FLTOPL (igr    ,ngrid  ,maxlev ,hlev   ,nlev)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLTOPL (FLow determine TOP Level)
c
c Module description: Determine the highest water level in a cross
c                     section.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 fltopl            O  Function value of function FLTOPL (=highest
c                         water level in a cross section).
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
c  5 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fltopl.pf,v $
c Revision 1.4  1995/09/22  10:02:22  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:55:33  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:34  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:13  hoeks_a
c Initial check-in
c
c Revision 1.1  1994/12/02  13:24:43  kuipe_j
c Initial / added for improvement of autostart.
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
      real FLTOPL
c
c     Declaration of Parameters:
c
      integer igr, ngrid, maxlev
      integer nlev(ngrid)
      double precision hlev(ngrid,maxlev)
c
      FLTOPL = hlev(igr,nlev(igr) )
c
      end
