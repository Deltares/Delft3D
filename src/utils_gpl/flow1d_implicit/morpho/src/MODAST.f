      subroutine MODAST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,
     +                    maxtab ,ntabm  ,ntab   ,table  ,h      ,
     +                    wf     ,wfh0   ,ws     ,wft    ,afs    ,
     +                    x      ,dtm    ,alphac ,alphad ,
     +                    celer  ,sedtr  ,dissed ,time   ,flwdir ,
     +                    mopta  ,moptb  ,moptc  ,moptd  ,mopte  ,
     +                    moptf  ,slat   ,intstr ,int1   ,int2   ,
     +                    delta1 ,delta2
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
c Module:             MODAST (MOrphology Delta Area for STructures)
c
c Module description: Calculate change in area for grid points around
c                     structures.
c
c                     This routine calculates the change in area for
c                     the left and right cross sections around a struc-
c                     ture. For the left grid point of a structure the
c                     integrals Istr-1/2 and Istr are used. For the
c                     right grid point of a structure the integrals
c                     Istr+1/2 and Istr are used. The inegrals on the
c                     halve point will be returned to the calling rou-
c                     tine. These integral values will be used to cal-
c                     culate the last internal point (MODALI) and the
c                     first following internal point (MODAIP). The
c                     structure integral Istr will be calculated by
c                     routine MOITST.
c Module:             MODAST (MOrphology Delta Area for STructures)
c
c Module description: Calculate change in area for grid points around
c                     structures.
c
c                     This subroutine calculates the change in area for
c                     the left or right cross section around a structu-
c                     re. For the left grid point of a structure the
c                     points I_sl-1/2 and I_stru are used. For the right
c                     grid point of the structure the points I_sr+1/2
c                     and I_stru are used. The integral on the halve
c                     point will be returned to the calling routine.
c                     This integral value will be used to calculate the
c                     last internal point (MODALI) or the first follo-
c                     wing internal point (MODAIP). The structure inte-
c                     gral I_stru will be calculated by routine MOISTR.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 afs               P  -
c 28 alphac            P  -
c 29 alphad            P  -
c  9 branch            P  -
c 30 celer             P  -
c 45 delta1            O  Calculated change in area at left side of
c                         structure
c 46 delta2            O  Calculated change in arear at right side
c                         of structure
c 32 dissed            P  -
c 27 dtm               I  Morphology time step.
c 34 flwdir            P  -
c 15 grid              P  -
c 20 h                 P  -
c 14 hlev              P  -
c 10 ibr               P  -
c 12 ibrtyp            P  -
c  2 icel              I  First non structure point after structure
c  1 igp               I  Gridpoint number
c 43 int1              I  Calculated integral value left side of structure
c 44 int2              I  Calculated integral value right side of structure
c 42 intstr            I  Calculated integral value for a structure
c  3 isec              I  Section number (1 or 2)
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 16 maxtab            I  Maximum number of defined tables.
c 13 mbdpar            P  -
c 35 mopta             P  -
c 36 moptb             P  -
c 37 moptc             P  -
c 38 moptd             P  -
c 39 mopte             P  -
c 40 moptf             P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  7 nnode             I  Number of nodes.
c 11 node              P  -
c 18 ntab              P  -
c 17 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 31 sedtr             P  -
c 41 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 19 table             P  -
c 33 time              P  -
c 21 wf                P  -
c 22 wfh0              P  -
c 24 wft               P  -
c 23 ws                P  -
c 26 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moitst  MOrphology InTegral for STructure point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: modast.pf,v $
c Revision 1.7  1997/02/17  10:23:14  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.6  1996/03/08  09:39:04  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.5  1996/03/07  10:44:14  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/10/18  08:59:56  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:49  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:40  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:09  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:31  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:34  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer     igp    ,icel   ,isec   ,ngrid  ,ibr    ,nbran  ,
     +            nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,
     +            maxlev

      integer     branch (4,nbran),
     +            grid   (ngrid),
     +            node   (4,nnode),
     +            mbdpar (5,nboun),
     +            ntab   (4,maxtab),
     +            flwdir (ngrid)

      real        alphac ,alphad ,intstr ,int1   ,int2
     
      double precision  delta1 ,delta2

      real        x      (ngrid),
     +            table  (ntabm),
     +            celer  (ngrid,*),
     +            sedtr  (ngrid,*),
     +            dissed (4,nbran),
     +            wf     (ngrid),
     +            wfh0   (ngrid),
     +            ws     (ngrid),
     +            wft    (ngrid),
     +            slat   (ngrid,*),
     +            afs    (ngrid)

      double  precision   time, dtm, hlev   (ngrid,maxlev)
      double precision    h(ngrid)

      logical mopta ,moptb ,moptc ,moptd ,mopte ,moptf
c
c     Local variables
c
      real       ili1, ili2, dx1, dx2
c
c
c        Calculate I_structure
c
      CALL MOITST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,
     +              nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +              node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,
     +              maxtab ,ntabm  ,ntab   ,table  ,h      ,
     +              wf     ,wfh0   ,ws     ,wft    ,afs    ,
     +              x      ,dtm    ,alphac ,alphad ,
     +              celer  ,sedtr  ,dissed ,time   ,flwdir ,
     +              mopta  ,moptb  , moptc ,moptd  ,mopte  ,
     +              moptf  ,intstr ,int1   ,int2
     +            )
c
c     Calculate dx
c
      dx1 = abs ( x(igp) - x(igp-1) )
      dx2 = abs ( x(icel+1) - x(icel) )
c
c     Calculate Slat integral
c
      ili1 = slat(igp,isec) * sngl(dtm)
      ili2 = slat(igp,isec) * sngl(dtm)
c
c     Calculate delta A
c
      delta1 = ( intstr - int1 - ili1 ) / ( 0.5 * dx1 )
      delta2 = ( int2 - intstr - ili2 ) / ( 0.5 * dx2 )
c
      return
      end
