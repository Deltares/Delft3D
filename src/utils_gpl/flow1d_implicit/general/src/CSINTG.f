      subroutine CSINTG (ngrid,
     +                   maxlev, nlev  , hlev  ,
     +                   width , area  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Brouwer
c
c Module:             CSINTG (Cross Section INTeGration of tables)
c
c Module description: Subroutine CSINTG performs the numerical integra-
c                     tion of tables.
c
c                     In subroutine CSINTG from the passed table of
c                     flow/total widths, tables will be computed for
c                     flow/total areas. As a consequence of this in the
c                     SOBEK time integration the actual flow/ total
c                     areas can easily be computed from the generated
c                     tables.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 area(ngrid,       IO Flow/total area.
c       maxlev)
c  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 ngrid             I  Number of grid points in network.
c  3 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  5 width(ngrid,      I  Flow/total width.
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
c $Log: csintg.pf,v $
c Revision 1.3  1996/05/30  09:59:50  kuipe_j
c comment char
c
c Revision 1.2  1995/05/30  06:55:26  hoeks_a
c files changed from dos-file to unix-files
c
c Revision 1.1  1995/04/13  06:58:30  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:29:48  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer ngrid, maxlev
      integer nlev(ngrid)
      real    width(ngrid,maxlev), area(ngrid,maxlev)
      double precision hlev (ngrid,maxlev)
c
c     Declaration of local variables:
c
      integer i, j
      real    z1, z2, w1, w2
c
c     Loop over network
c
      do 100 i = 1, ngrid
c
c        Numerical integration by trapezoidal rule
c
         z2  = hlev(i,1)
         w2  = width(i,1)
c
         area(i,1) = 0.0
c
         do 10 j = 2, nlev(i)
            z1  = z2
            w1  = w2
c
            z2  = hlev(i,j)
            w2  = width(i,j)
c
            area(i,j) = area(i,j-1) + 0.5*(z2-z1)*(w1+w2)
   10    continue
c
  100 continue
c
      end
