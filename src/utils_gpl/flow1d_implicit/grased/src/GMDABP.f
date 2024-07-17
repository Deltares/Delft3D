      subroutine GMDABP (igpbou ,ngrid  ,nfrac  ,dtm   ,alphac ,
     +                   alphad ,alphae ,x      ,celer ,celert ,sedtr  ,
     +                   source ,intiph ,intbou ,dfrac ,ds     ,cela1  ,
     +                   spredc ,deltaa ,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             GMDABP (Graded Morphology Delta Area for
c                             Begin Point)
c
c Module description: Calculate delta area for begin point of branch.
c
c                     For the calculation of the delta area on the first
c                     point of a branch the values of the integral I1
c                     and I3/2 are needed. The integrals are calculated
c                     by routine MOIBOU and MOIIPH. After the call to
c                     MOIBOU the value of I1 is known. The integral
c                     value is passed to MOIIPH because it is possible
c                     that the integral value is needed to calculated
c                     the integral on i3/2. This is true in case a node
c                     is connected to point 1.
c
c                     The value of delta area is returned as well as the
c                     value of I3/2. This integral is value is used as a
c                     starting point for the internal points. On the
c                     first point no lateral sediment can be defined.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 29 afs               P  -
c 23 alphac            P  -
c 11 branch            P  -
c 30 celer             P  -
c 35 deltaa            O  Calculated change in area
c 32 dissed            P  -
c 22 dtm               I  Morphology time step
c 10 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 24 h                 P  -
c 16 hlev              P  -
c  1 ibr               P  -
c 13 ibrtyp            P  -
c  2 igpbou            I  Calculated integral value on boundary
c  4 intbou            I  Integral value for begin or end point of a
c                         branch
c 34 intiph            I  Calculated integral value on i + 1/2
c  3 isec              I  Section number (1 or 2)
c  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 17 maxtab            I  Maximum number of defined tables.
c 14 mbdpar            P  -
c  7 nboun             I  Number of boundary nodes.
c  6 nbran             I  Number of branches.
c  5 ngrid             I  Number of grid points in network.
c  8 nnode             I  Number of nodes.
c 12 node              P  -
c 19 ntab              P  -
c 18 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 31 sedtr             P  -
c 33 slat(ngrid,*)     I  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 20 table             P  -
c 21 time              P  -
c 25 wf                P  -
c 26 wfh0              P  -
c 28 wft               P  -
c 27 ws                P  -
c 15 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moiflh  MOrphology Integral on First or Last halve point
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmdabp.F,v $
c Revision 1.2  1995/09/27  10:11:27  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Parameters
c
      integer    igpbou ,ngrid  ,nfrac ,jugralg
      real       alphac ,alphad ,alphae
      real       x      (ngrid)        ,
     +           celer  (ngrid,nfrac,5),celert (ngrid)       ,
     +           sedtr  (ngrid,nfrac+2),
     +           source (ngrid,nfrac+2),
     +           cela1  (nfrac,nfrac)  ,
     +           ds     (nfrac)        ,spredc (nfrac)       ,
     +           intiph (nfrac)        ,intbou (nfrac)       ,
     +           dfrac  (nfrac)
      double precision   dtm, deltaa (ngrid,nfrac+1)
c
c     Local variables
c
      integer    igpcel  ,jf
      real       ili     ,dtm2  ,rdx ,sum
c
c     Calculate point on location x = 1.5
c
      igpcel = igpbou+1            
c
      call gmiflh ( igpbou ,igpcel ,ngrid ,nfrac ,alphac ,alphad ,
     +              alphae ,sngl(dtm)     ,celer ,celert ,sedtr  ,
     +              source ,x      ,dfrac ,ds    ,spredc ,cela1  ,
     +              intiph ,jugralg)
c
c     Calculate dx
c
      dtm2 = sngl(dtm) * .5
      rdx  = 2. / (x(igpcel) - x(igpbou))
c
      do 10 jf=1,nfrac

c        Calculate lateral integral

         ili = source(igpbou,jf) * dtm2
c
c        Calculate delta A
c
         deltaa(igpbou,jf) = ( intiph(jf) - intbou(jf)- ili) * rdx

  10  continue
cf
      sum = 0.
      do 20 jf=1,nfrac
         sum = sum + deltaa(igpbou,jf)
  20  continue
      deltaa(igpbou,nfrac+1) = sum

      end
