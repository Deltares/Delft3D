      subroutine MODABP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,
     +                    nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +                    node   ,ibrtyp ,mbdpar ,hlev   ,
     +                    grid   ,maxtab ,ntabm  ,ntab   ,
     +                    table  ,h      ,wf     ,wfh0   ,
     +                    ws     ,wft    ,afs    ,dissed ,
     +                    x      ,time   ,dtm    ,alphac ,
     +                    celer  ,sedtr  ,intbou ,flwdir ,
     +                    alphad ,moptd  ,mopte  ,slat   ,
     +                    intiph ,iextra ,deltaa ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MODABP (MOrphology Delta Area for Begin Point)
c
c Module description: Calculate delta area for begin point of branch.
c
c                     For the calculation of the delta area on the first
c                     point of a branch the values of the integral I1
c                     and I3/2 are needed. The integral value I1 is
c                     known from the call to MOINOD or MOIBOU.
c
c                     The value of delta area is returned as well as the
c                     value of I3/2. This integral value is used as a
c                     starting point for the internal points. On the
c                     first point no lateral sediment can be defined.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 afs               P  -
c 30 alphac            P  -
c 35 alphad            P  -
c  9 branch            P  -
c 31 celer             P  -
c 42 deltaa            O  Calculated change in area
c 26 dissed            P  -
c 29 dtm               I  Morphology time step.
c 34 flwdir            P  -
c 15 grid              P  -
c 20 h                 P  -
c 14 hlev              P  -
c 10 ibr               P  -
c 12 ibrtyp            P  -
c  1 igpbou            I  Calculated integral value on boundary
c  2 igpcel            I  Calculated integral for first or last cell in
c                         branch
c 33 intbou            I  Integral value for begin or end point of a
c                         branch
c 41 intiph            I  Calculated integral value on i + 1/2
c  3 isec              I  Section number (1 or 2)
c 39 juer              P  -
c 40 ker               P  -
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 16 maxtab            I  Maximum number of defined tables.
c 13 mbdpar            P  -
c 36 moptd             P  -
c 37 mopte             P  -
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c  7 nnode             I  Number of nodes.
c 11 node              P  -
c 18 ntab              P  -
c 17 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 32 sedtr             P  -
c 38 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 19 table             P  -
c 28 time              P  -
c 21 wf                P  -
c 22 wfh0              P  -
c 24 wft               P  -
c 23 ws                P  -
c 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moitbp  MORPHology InTegral on Begin Point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: modabp.pf,v $
c Revision 1.7  1998/06/11  11:47:07  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1997/02/17  10:23:10  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.5  1996/03/08  09:38:59  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.4  1996/03/07  10:44:11  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.3  1995/10/18  08:59:52  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:04:36  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:05  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:52:23  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:30  kuipe_j
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
      integer    igpbou ,igpcel ,isec   ,ngrid  ,ibr    ,nbran  ,
     +           nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,
     +           maxlev ,juer   ,ker
      integer    branch (4,nbran),
     +            grid   (ngrid),
     +            node   (4,nnode),
     +            mbdpar (5,nboun),
     +            ntab   (4,maxtab),
     +            flwdir (ngrid)

      real       alphac ,intiph, alphad, intbou , iextra

      real       x      (ngrid),
     +           table  (ntabm),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*),
     +           dissed (4,nbran),
     +           wf     (ngrid),
     +           wfh0   (ngrid),
     +           ws     (ngrid),
     +           wft    (ngrid,maxlev),
     +           slat   (ngrid,*),
     +           afs    (ngrid)

      double precision time, dtm, hlev (ngrid,maxlev), deltaa, h(ngrid)


      logical    moptd, mopte
c
c     Local variables
c
      real       ili, dx
c
      CALL MOITBP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,
     +              nboun  ,nnode  ,maxlev ,branch ,ibr    ,
     +              node   ,ibrtyp ,mbdpar ,hlev   ,
     +              grid   ,maxtab ,ntabm  ,ntab   ,
     +              table  ,h      ,wf     ,wfh0   ,
     +              ws     ,wft    ,afs    ,dissed ,
     +              x      ,time   ,dtm    ,alphac ,
     +              celer  ,sedtr  ,intbou ,flwdir ,
     +              alphad ,moptd  ,mopte  ,intiph ,
     +              iextra ,juer   ,ker    )
c
c     Calculate dx
c
      dx = x(igpcel) - x(igpbou)
c
c     Calculate Slat integral
c
      ili = slat(igpbou,isec) * sngl(dtm) 
c
c     Calculate delta A
c
      deltaa = ( intiph - intbou - ili ) / ( 0.5 * dx )
c
      return
      end
