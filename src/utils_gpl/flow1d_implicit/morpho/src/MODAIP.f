      subroutine MODAIP ( igp    ,isec   ,ngrid  ,
     +                    alphac ,dtm    ,alphad ,
     +                    celer  ,sedtr  ,slat   ,x     ,
     +                    intiph ,deltaa
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
c Module:             MODAIP (MOrphology Delta Area for Internal Points)
c
c Module description: This subroutine calculates the change in area for
c                     an internal point. If a branch has grid points
c                     numbered from 1..n internal grid points will be
c                     located between 2 1/2, .., n-3/2.
c                     The integral value I i+1/2 will be returned to
c                     the calling routine.
c                     This integral value will be used in the
c                     next call as Ii-1/2. Notice that on the first
c                     internal point the lateral sediment from i-1/2
c                     will be assigned completely.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 alphac            P  -
c  6 alphad            P  -
c  7 celer             P  -
c 12 deltaa            O  Calculated change in area
c  5 dtm               I  Morphology time step.
c  1 igp               I  Gridpoint number
c 11 intiph            IO Calculated integral value on i + 1/2
c  2 isec              I  Section number (1 or 2)
c  3 ngrid             I  Number of grid points in network.
c  8 sedtr             P  -
c  9 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moitip  MOrphology InTegral on Intermediate grid Point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: modaip.pf,v $
c Revision 1.7  1997/02/17  10:23:12  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.6  1996/05/28  13:30:06  kuipe_j
c Error message courant nr added
c
c Revision 1.5  1996/03/08  09:39:01  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.4  1996/03/07  10:44:13  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/10/18  08:59:54  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:04:38  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:07  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:27  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:32  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  igp    ,isec   , ngrid
      real     intiph ,alphac , alphad
      real     celer (ngrid,*),
     +         sedtr (ngrid,*),
     +         slat  (ngrid,*),
     +         x     (ngrid)
      double  precision     dtm, deltaa
c
c     Local variables
c
      real     inthp,
     +         dx,
     +         ili,
     +         dtms

      dtms = sngl(dtm)
c
c     Calculate integral on i+1/2
c
      CALL MOITIP ( igp    ,isec   ,ngrid  ,
     +              x      ,dtm    ,alphac ,
     +              celer  ,sedtr  ,
     +              alphad ,inthp
     +            )
c
c     Calculate dx for lateral sediment and delta A calculation
c
      dx = x(igp+1) - x(igp-1)
c
      ili = slat(igp,isec) * dtms
c
c     Calculate delta A
c
      deltaa = ( inthp - intiph - ili ) / ( 0.5 * dx )
c
c     return value of I at i+1/2 
c
      intiph = inthp          
c
      end
