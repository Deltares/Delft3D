      subroutine gmiflh ( igpbou ,igpcel ,ngrid ,nfrac ,alphac ,alphad ,
     +                    alphae ,dtm    ,celer ,celert,sedtr  ,source ,
     +                    x      ,dfrac ,ds     ,spredc,cela1  ,intiph ,
     +                    jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:
c
c Module:             GMIFLH (Graded MOrphology Integral on First
c                             or Last Half point)
c
c Module description: This subroutine calculates the change in area for
c                     an internal point. If a branch has grid points
c                     numbered from 1..n internal grid points will be
c                     located between 2 1/2, .., n-3/2. Sometimes the
c                     points 1/2 and n-1/2 are internal too. This will
c                     be evaluated on a higher level. Notice that the
c                     integral Ii+1/2 will be returned to the calling
c                     routine. This integral value will be used in the
c                     next call as Ii-1/2. Notice that on the first
c                     internal point the lateral sediment from i-1/2
c                     will be assigned completely.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 alphac            I  Stability factor for bottom scheme (>1)
c  8 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 13 deltaa            O  Calculated change in area
c  7 dtm               I  Morphology time step
c  2 igp               I  Gridpoint number
c  1 igpm1             I  Gridpoint number - 1
c  3 igpp1             I  Gridpoint number + 1
c 12 intiph            IO Calculated integral value on i + 1/2
c  5 ngrid             I  Number of grid points in network.
c  9 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmiflh.F,v $
c Revision 1.3  1996/01/05  15:43:24  kuipe_j
c Lateral sediment and structures
c
c Revision 1.2  1995/09/27  10:11:35  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Parameters
c
      integer  igpbou ,igpcel ,ngrid  ,nfrac  ,jugralg
      real     alphac ,alphad ,alphae ,dtm
      real     celer (ngrid,nfrac,5)  ,celert (ngrid)       ,
     +         sedtr (ngrid,nfrac+2)  ,
     +         source(ngrid,nfrac+2)  ,
     +         x     (ngrid)          ,cela1  (nfrac,nfrac) ,
     +         ds    (nfrac)          ,spredc (nfrac)       ,
     +         intiph(nfrac)          ,
     +         dfrac (nfrac)
c
c     Local variables
c
      integer  igp   ,igpp1  ,jf
      real     dx    ,dtm2   ,sediph
c
c     Calculate dx
c
      if (igpcel .gt. igpbou) then
         igp   = igpbou
         igpp1 = igpcel
      else
         igp   = igpcel
         igpp1 = igpbou
      endif

      dx = abs(x(igpp1) - x(igp))
c
c     Calculate predicted transport
c
      call gmpred (ngrid  ,nfrac  ,igp    ,dx     ,dtm    ,alphac ,
c                                                 <cela0>   
     &             alphad ,alphae ,sedtr  ,source ,celer(1,1,1)   ,
c                 <cela1a>        <cela1b>        <cela2>         
     &             celer(1,1,2)   ,celer(1,1,3)   ,celer(1,1,4)   ,
c                 <cela3>
     &             celer(1,1,5)   ,dfrac  ,cela1  ,ds     ,celert ,
     &             spredc         ,jugralg) 
c
c     Calculate dx for lateral sediment and delta A calculation
c
      dtm2 = dtm * .5

      do 10 jf=1,nfrac
c
c        Calculate sediment on i+1/2
c
         sediph = (sedtr(igp,jf) + sedtr (igpp1,jf)) / 2.
c         
c        Calculate integral on i+1/2
c
         intiph(jf) = ( sediph + spredc(jf)) * dtm2
c
  10  continue
c
      end
