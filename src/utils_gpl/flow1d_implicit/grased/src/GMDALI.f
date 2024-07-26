      subroutine GMDALI ( igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,
     +                    intiph ,intimh ,source ,x     ,deltaa )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             GMDALI (Graded Morphology Delta Area for
c                             Last Internal point)
c
c Module description: This subroutine calculates the change in area for
c                     the last internal point. This point uses the cal-
c                     culated integral values Ii+1/2 (last value of
c                     MODAIP) and In-1/2 (value from MODAEP). This is
c                     shown below:
c
c                     |         |         |
c                     -----x---------x-----
c                     |         |         |
c                        i+1/2 n-1  n-1/2 n
c                         /     |     \
c                        /      |      \
c                     MODAIP    MODALI   MODAEP
c
c                     The lateral sediment on point n-1/2 will be assig-
c                     ned to the point n-1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 deltaa            O  Calculated change in area
c  6 dtm               I  Morphology time step
c  2 igp               I  Gridpoint number
c  1 igpm1             I  Gridpoint number - 1
c  3 igpp1             I  Gridpoint number + 1
c  7 intiph            I  Calculated integral value on i + 1/2
c  8 intnmh            I  Calculated integral value on n - 1/2
c  4 isec              I  Section number (1 or 2)
c  5 ngrid             I  Number of grid points in network.
c  9 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmdali.F,v $
c Revision 1.2  1995/09/27  10:11:31  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Parameters
c
      integer  igpm1  ,igp    ,igpp1  ,ngrid  ,nfrac
      real     source (ngrid,nfrac+2) ,
     +         x      (ngrid)         ,
     +         intiph (nfrac)         ,intimh (nfrac)
      double precision dtm, deltaa (ngrid,nfrac+1)
c
c     Local variables
c
      integer  jf
      real     rdx    ,ili    ,sum  ,dtm2
c
c     Calculate dx
c
      rdx  = 2. / (x(igpp1) - x(igpm1))
      dtm2 = sngl(dtm) * .5

      do 10 jf=1,nfrac
c
         ili    = (source(igpm1,jf)+source(igp,jf)) * dtm2
c
c        Calculate delta A
c
         deltaa(igp,jf) = ( intimh(jf)-intiph(jf) - ili ) * rdx
  10  continue
c
      sum = 0.
      do 20 jf=1,nfrac
         sum = sum + deltaa(igp,jf)
  20  continue
      deltaa(igp,nfrac+1) = sum

      end
