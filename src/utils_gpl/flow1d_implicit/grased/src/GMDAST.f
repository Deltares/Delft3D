      subroutine GMDAST (igp    ,ngrid  ,nfrac  ,dtm    ,alphac ,alphad,
     +                   alphae ,grid   ,x      ,celer  ,celert ,sedtr ,
     +                   source ,flwdir ,dfrac  ,ds     ,cela1  ,spredc,
     +                   intimh ,intiph ,intstr ,deltaa ,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         J.Kuipers            
c
c Module:             GMDAST  (Graded Morphology Delta area structures)
c
c Module description: Calculate delta A by solving a continuity equation
c                     for a cell containing structures and the        
c                     preceeding cell.
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

      integer   igp    ,ngrid  ,nfrac  ,jugralg
      integer   grid   (ngrid) ,flwdir (ngrid)
      real      alphac ,alphad ,alphae
      real      x      (ngrid) ,
     +          celer  (ngrid,nfrac,5) ,celert (ngrid)       ,
     +          sedtr  (ngrid,nfrac+2) ,
     +          source (ngrid,nfrac+2) ,
     +          intimh (nfrac)         ,intiph (nfrac)       ,
     +          intstr (nfrac)         ,
     +          spredc (nfrac)         ,cela1  (nfrac,nfrac) ,
     +          ds     (nfrac)         ,dfrac  (nfrac)       
      double precision  dtm, deltaa (ngrid,nfrac+1)
c
c     Local variables
c
      integer   igpm1  ,igpp1  ,igps  ,istep  ,icel  ,igpcel
      logical   flwpos
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c      
c     Read first and last grid point of branch
c
      flwpos = flwdir(igp+1).ge.0
c
c     Determine how many structures are present on consecutive cells.
c
      istep = 1
   10 continue
         if (grid(igp+1+istep) .eq. cstrcl) then
            istep=istep+1
      goto 10
         endif
      icel = igp + istep + 1
c
c     Determine flow direction
c
      if (flwpos) then
        igps   = igp + 1
        igpcel = igp
      else
        igps   = icel
        igpcel = igps + 1
      endif
c
c     Calculate sediment integral on grid point flow upwards
c     of structure. This value will also be used at the downwards 
c     side.
c
      call gmiflp (igps   ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,
     +             alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,
     +             source ,x      ,dfrac ,ds     ,spredc ,cela1  ,
     +             intstr ,jugralg)
c
      igpm1 = igp - 1              
      igpp1 = igp + 1        
c
c     Determine change in area at left side of structure
c     as well a sediment integral on DX/2 leftwards.
c
      call GMDAEP (igpp1  ,ngrid  ,nfrac ,dtm    ,alphac ,
     +             alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,
cu   +             source ,intiph ,intstr        ,dfrac  ,ds     ,
ci1
     +             source ,intimh ,intstr        ,dfrac  ,ds     ,
     +             cela1  ,spredc ,deltaa,jugralg)
c
c     Determine change in area of DX left of structure
c
      call GMDALI (igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,
cu   +             intimh ,intiph ,source ,x     ,deltaa )
ci1
     +             intiph ,intimh ,source ,x     ,deltaa )
c
c     Determine change in area at right side of structure
c     as well a sediment integral on DX/2 on the right.
c
      call GMDABP (icel   ,ngrid  ,nfrac ,dtm    ,alphac ,
     +             alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,
cu   +             source ,intimh ,intstr        ,dfrac  ,ds     ,
ci1
     +             source ,intiph ,intstr        ,dfrac  ,ds     ,
     +             cela1  ,spredc ,deltaa,jugralg)
c
      igp = icel
c
      return
      end
