      subroutine MODALI ( igpm1  ,igp    ,igpp1  ,
     +                    isec   , ngrid ,
     +                    dtm    ,
     +                    intiph ,intnmh ,
     +                    slat   ,x      ,
     +                    deltaa
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MODALI (MOrphology Delta Area for Last Internal point)
c
c Module description: This subroutine calculates the change in area for
c                     the last internal point. This point uses the cal-
c                     culated integral values Ii+1/2 (last value of
c                     MODAIP) and In-1/2 (value from MODAEP or MODAST).
c                     This is shown below:
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
c  6 dtm               I  Morphology time step.
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
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: modali.pf,v $
c Revision 1.5  1997/02/17  10:23:13  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.4  1996/03/08  09:39:02  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.3  1995/10/18  08:59:55  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:04:39  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:08  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:52:29  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  igpm1  ,igp    ,igpp1  ,isec   , ngrid
      real     intiph ,intnmh 
      real     slat (ngrid,*),
     +         x    (ngrid)
      double  precision  dtm, deltaa
c
c     Local variables
c
      real     dx,
     +         ili

c
c     Calculate dx
c
      dx = x(igpp1) - x(igpm1)
c
c     Calculate lateral sediment integral
c
      ili = slat(igp,isec) * sngl(dtm)
c
c     Calculate delta A
c
      deltaa = ( intnmh - intiph - ili ) / ( 0.5 * dx )

      return
      end
