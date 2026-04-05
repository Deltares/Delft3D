subroutine GMAREA (ibr    ,ngrid  ,nbran  ,nfrac  ,dtm    ,alphac,&
&alphad ,alphae ,grid   ,branch ,x      ,intgr ,&
&celer  ,celert ,sedtr  ,source ,flwdir ,dfrac ,&
&ds     ,cela1  ,spredc ,intimh ,intiph ,intstr,&
&deltaa ,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         J.Kuipers
!
! Module:             GMAREA (Graded Morphology AREA)
!
! Module description: Calculate delta A by solving a continuity equation
!                     for each gridpoint in the branch to adapt cross
!                     sectional dimensions. Also the change in area
!                     per fraction is calculated.
!
!                     First routine GMADBP is called to determine the
!                     change in area for grid point 1. This routine also
!                     delivers the value of the integral at point x =
!                     3/2. The integral is used as a starting point for
!                     the calculation of the internal grid points. The
!                     internal grid points from i1+1 until i2-2 are pro-
!                     cessed by routine GMDAIP. The last point is calcu-
!                     lated by routine GMDAEP. Point i2-1 is calculated
!                     by routine GMDALI using the last results from
!                     routines GMDAIP and GMDAEP. See also figure one in
!                     chapter three of [S-DO-004].
!                     A structure including the preceding cell is
!                     processed by routine GMDAST.
!
!                     Each of the above described routines return a
!                     change in the area. The calculated changes are
!                     used to adapt the cross sectional tables in routi-
!                     ne GMCROS.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 31 afs               P  -
! 11 alphac            P  -
! 13 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 32 celer             P  -
! 34 dissed            P  -
!  8 dtm               P  -
! 12 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 26 h                 P  -
! 25 hlev              P  -
!  1 ibr               I  Branch number
! 15 ibrtyp            P  -
! 22 intgr(ngrid,*)    I  Integral values for grid
!  2 isec              I  Section number (1 or 2)
! 23 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 16 maxtab            I  Maximum number of defined tables.
! 20 mbdpar            P  -
!  9 moropt            P  -
!  5 nboun             I  Number of boundary nodes.
!  4 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 24 nlev              P  -
!  6 nnode             I  Number of nodes.
! 14 node              P  -
! 18 ntab              P  -
! 17 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 33 sedtr             P  -
! 35 slat              P  -
! 19 table             P  -
!  7 time              P  -
! 27 wf                P  -
! 30 wfh0              P  -
! 28 wft               P  -
! 29 ws                P  -
! 21 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moadcs  MORPHology Adapt Cross Sections
! modabp  MOrphology Delta Area for Begin Point
! modaep  MOrphology Delta Area for End Point
! modaip  MOrphology Delta Area for Internal Points
! modali  MOrphology Delta Area for Last Internal point
! modast  MOrphology Delta Area for STructures
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmarea.F,v $
! Revision 1.5  1996/06/07  11:55:09  kuipe_j
! multi  +  fixed layer
!
! Revision 1.4  1996/01/08  13:29:33  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.3  1996/01/05  15:43:18  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:11:25  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!
!     Parameters
!

   integer   ibr    ,ngrid  ,nbran  ,nfrac ,jugralg
   integer   grid   (ngrid),&
   &branch (4,nbran),&
   &flwdir (ngrid)
   real      alphac ,alphad, alphae
   real      x      (ngrid),&
   &celer  (ngrid,nfrac,5) ,celert (ngrid)       ,&
   &sedtr  (ngrid,nfrac+2) ,&
   &source (ngrid,nfrac+2) ,&
   &intgr  (nfrac,2,nbran) ,&
   &intimh (nfrac)         ,intiph (nfrac)       ,&
   &intstr (nfrac)         ,&
   &spredc (nfrac)         ,cela1  (nfrac,nfrac) ,&
   &ds     (nfrac)         ,dfrac  (nfrac)
   double precision  dtm, deltaa (ngrid,nfrac+1)
!
!     Local variables
!
   integer   i1     ,i2     ,igp    ,igpm1  ,igpp1
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Read first and last grid point of branch
!
   i1 = branch(3,ibr)
   i2 = branch(4,ibr)
!
!     Determine delta area on point 1
!
   call GMDABP (i1     ,ngrid  ,nfrac ,dtm    ,alphac ,&
   &alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,&
   &source ,intiph ,intgr(1,1,ibr),dfrac  ,ds     ,&
   &cela1  ,spredc ,deltaa,jugralg)
!
!     Determine first internal point
!
   igp = i1 + 1
!
!     Loop over internal points
!
100 continue
!
!     Stop if igp = i2 - 1
!
   if ( igp .lt. i2-1 ) then
!
!        Check if next grid point = structure point
!
      igpm1 = igp - 1
      igpp1 = igp + 1
!
      if ( grid (igpp1) .eq. cstrcl) then
!
!           Calculate I_str-1/2 , I_structure and I_str+1/2
!           and DELTA-a before and after the structure
!
         call GMDAST (igp    ,ngrid  ,nfrac  ,dtm    ,alphac ,alphad,&
         &alphae ,grid   ,x      ,celer  ,celert ,sedtr ,&
         &source ,flwdir ,dfrac  ,ds     ,cela1  ,spredc,&
         &intimh ,intiph ,intstr ,deltaa ,jugralg)

      else
!
!           Normal grid cell.
!           intiph [I(i+1/2)] is used from previous point
!
         call GMDAIP (igpm1  ,igp    ,igpp1  ,ngrid ,nfrac ,alphac ,&
         &alphad ,alphae ,dtm    ,celer ,celert,sedtr  ,&
         &source ,x      ,dfrac  ,ds    ,spredc,cela1  ,&
         &intiph ,deltaa ,jugralg)
!
      endif
      igp   = igp + 1
      goto 100
   endif
!
!     Last point of branch
!
!     intimh [I(n-1/2)] will be calculated
!
   call GMDAEP (i2     ,ngrid  ,nfrac ,dtm    ,alphac ,&
   &alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,&
   &source ,intimh ,intgr(1,2,ibr),dfrac  ,ds     ,&
   &cela1  ,spredc ,deltaa,jugralg)
!
!     One but last point of branch
!
!     intiph [I(i+1/2)] is used from previous point
!     intimh [I(n-1/2)] is used from next point
!
   igpm1 = igp - 1
   igpp1 = igp + 1

   call GMDALI ( igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,&
   &intiph ,intimh ,source ,x     ,deltaa )
!

   return
end
