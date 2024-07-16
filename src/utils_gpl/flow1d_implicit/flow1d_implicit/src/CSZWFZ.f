       subroutine CSZWFZ (ngrid  ,
     +                    maxlev ,nlev   ,hlev   ,
     +                    width  ,izwft  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Cross Sectional Table Module
c
c Programmer:         J.Brouwer
c
c Module:             CSZWFZ (Cross Section z Wf(z) dz)
c
c Module description: Compute a table for each defined water level con-
c                     taining integrated widths.
c
c                     Compute a table for each defined water level con-
c                     taining integrated widths. This table is used to
c                     compute the A1m integral in case of salt. For this
c                     routine the table with flow widths is required.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  6 izwft(ngrid,      IO Table containing integrated widths.
c       maxlev)
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
c $Log: cszwfz.pf,v $
c Revision 1.4  1999/03/15  15:49:11  kuipe_j
c tabs removed
c
c Revision 1.3  1996/05/30  09:59:56  kuipe_j
c comment char
c
c Revision 1.2  1995/05/30  06:55:33  hoeks_a
c files changed from dos-file to unix-files
c
c Revision 1.1  1995/04/13  06:58:36  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:29:57  kuipe_j
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
      real width(ngrid,maxlev), izwft(ngrid,maxlev)
      double precision hlev (ngrid,maxlev)
c
c     Declaration of local variables:
c
      integer i, j
      real    h1, h2, w1, w2
c
c     Loop over network
c
      do 100 i = 1, ngrid
c
c        Numerical integration by trapezoidal rule
c
         h2  = hlev(i,1)
         w2  = width(i,1)
c
         izwft(i,1) = 0.0
c
         do 10 j = 2, nlev(i)
            h1  = h2
            w1  = w2
c
            h2  = hlev(i,j)
            w2  = width(i,j)
c
            izwft(i,j) = izwft(i,j-1) +
     *                   ( h2*h2*h2/6. + h1*h1*h1/3. - h2*h1*h1/2. ) /
     *                   ( h2 - h1 ) * w1 +
     *                   ( h1*h1*h1/6. + h2*h2*h2/3. - h2*h2*h1/2. ) /
     *                   ( h2 - h1 ) * w2
c               
   10    continue
c
  100 continue
c
      end
