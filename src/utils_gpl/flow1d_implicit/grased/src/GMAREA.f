      subroutine GMAREA (ibr    ,ngrid  ,nbran  ,nfrac  ,dtm    ,alphac,
     +                   alphad ,alphae ,grid   ,branch ,x      ,intgr ,
     +                   celer  ,celert ,sedtr  ,source ,flwdir ,dfrac ,
     +                   ds     ,cela1  ,spredc ,intimh ,intiph ,intstr,
     +                   deltaa ,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         J.Kuipers         
c
c Module:             GMAREA (Graded Morphology AREA)
c
c Module description: Calculate delta A by solving a continuity equation
c                     for each gridpoint in the branch to adapt cross
c                     sectional dimensions. Also the change in area
c                     per fraction is calculated.
c
c                     First routine GMADBP is called to determine the
c                     change in area for grid point 1. This routine also
c                     delivers the value of the integral at point x =
c                     3/2. The integral is used as a starting point for
c                     the calculation of the internal grid points. The
c                     internal grid points from i1+1 until i2-2 are pro-
c                     cessed by routine GMDAIP. The last point is calcu-
c                     lated by routine GMDAEP. Point i2-1 is calculated
c                     by routine GMDALI using the last results from
c                     routines GMDAIP and GMDAEP. See also figure one in
c                     chapter three of [S-DO-004].
c                     A structure including the preceding cell is
c                     processed by routine GMDAST.
c
c                     Each of the above described routines return a
c                     change in the area. The calculated changes are
c                     used to adapt the cross sectional tables in routi-
c                     ne GMCROS.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 31 afs               P  -
c 11 alphac            P  -
c 13 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 32 celer             P  -
c 34 dissed            P  -
c  8 dtm               P  -
c 12 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 26 h                 P  -
c 25 hlev              P  -
c  1 ibr               I  Branch number
c 15 ibrtyp            P  -
c 22 intgr(ngrid,*)    I  Integral values for grid
c  2 isec              I  Section number (1 or 2)
c 23 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 16 maxtab            I  Maximum number of defined tables.
c 20 mbdpar            P  -
c  9 moropt            P  -
c  5 nboun             I  Number of boundary nodes.
c  4 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 24 nlev              P  -
c  6 nnode             I  Number of nodes.
c 14 node              P  -
c 18 ntab              P  -
c 17 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 33 sedtr             P  -
c 35 slat              P  -
c 19 table             P  -
c  7 time              P  -
c 27 wf                P  -
c 30 wfh0              P  -
c 28 wft               P  -
c 29 ws                P  -
c 21 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c moadcs  MORPHology Adapt Cross Sections
c modabp  MOrphology Delta Area for Begin Point
c modaep  MOrphology Delta Area for End Point
c modaip  MOrphology Delta Area for Internal Points
c modali  MOrphology Delta Area for Last Internal point
c modast  MOrphology Delta Area for STructures
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmarea.F,v $
c Revision 1.5  1996/06/07  11:55:09  kuipe_j
c multi  +  fixed layer
c
c Revision 1.4  1996/01/08  13:29:33  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.3  1996/01/05  15:43:18  kuipe_j
c Lateral sediment and structures
c
c Revision 1.2  1995/09/27  10:11:25  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c
c     Parameters
c

      integer   ibr    ,ngrid  ,nbran  ,nfrac ,jugralg 
      integer   grid   (ngrid),
     +          branch (4,nbran),
     +          flwdir (ngrid)
      real      alphac ,alphad, alphae
      real      x      (ngrid),
     +          celer  (ngrid,nfrac,5) ,celert (ngrid)       ,
     +          sedtr  (ngrid,nfrac+2) ,
     +          source (ngrid,nfrac+2) ,
     +          intgr  (nfrac,2,nbran) ,
     +          intimh (nfrac)         ,intiph (nfrac)       ,
     +          intstr (nfrac)         ,
     +          spredc (nfrac)         ,cela1  (nfrac,nfrac) ,
     +          ds     (nfrac)         ,dfrac  (nfrac)  
      double precision  dtm, deltaa (ngrid,nfrac+1)
c
c     Local variables
c
      integer   i1     ,i2     ,igp    ,igpm1  ,igpp1 
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c      
c     Read first and last grid point of branch
c
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
c 
c     Determine delta area on point 1
c
      call GMDABP (i1     ,ngrid  ,nfrac ,dtm    ,alphac ,
     +             alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,
     +             source ,intiph ,intgr(1,1,ibr),dfrac  ,ds     ,
     +             cela1  ,spredc ,deltaa,jugralg)
c
c     Determine first internal point
c
      igp = i1 + 1
c
c     Loop over internal points
c
  100 continue
c
c     Stop if igp = i2 - 1
c
      if ( igp .lt. i2-1 ) then
c
c        Check if next grid point = structure point
c
         igpm1 = igp - 1              
         igpp1 = igp + 1        
c
         if ( grid (igpp1) .eq. cstrcl) then
c
c           Calculate I_str-1/2 , I_structure and I_str+1/2
c           and DELTA-a before and after the structure  
c
            call GMDAST (igp    ,ngrid  ,nfrac  ,dtm    ,alphac ,alphad,
     +                   alphae ,grid   ,x      ,celer  ,celert ,sedtr ,
     +                   source ,flwdir ,dfrac  ,ds     ,cela1  ,spredc,
     +                   intimh ,intiph ,intstr ,deltaa ,jugralg)

         else
c
c           Normal grid cell.
c           intiph [I(i+1/2)] is used from previous point
c
            call GMDAIP (igpm1  ,igp    ,igpp1  ,ngrid ,nfrac ,alphac ,
     +                   alphad ,alphae ,dtm    ,celer ,celert,sedtr  ,
     +                   source ,x      ,dfrac  ,ds    ,spredc,cela1  ,
     +                   intiph ,deltaa ,jugralg)
c
         endif
         igp   = igp + 1
         goto 100
      endif
c
c     Last point of branch
c
c     intimh [I(n-1/2)] will be calculated
c
      call GMDAEP (i2     ,ngrid  ,nfrac ,dtm    ,alphac ,
     +             alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,
     +             source ,intimh ,intgr(1,2,ibr),dfrac  ,ds     ,
     +             cela1  ,spredc ,deltaa,jugralg)
c
c     One but last point of branch
c
c     intiph [I(i+1/2)] is used from previous point
c     intimh [I(n-1/2)] is used from next point
c
      igpm1 = igp - 1
      igpp1 = igp + 1

      call GMDALI ( igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,
     +              intiph ,intimh ,source ,x     ,deltaa )
c

      return
      end
