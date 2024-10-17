subroutine GMDAST (igp    ,ngrid  ,nfrac  ,dtm    ,alphac ,alphad,&
&alphae ,grid   ,x      ,celer  ,celert ,sedtr ,&
&source ,flwdir ,dfrac  ,ds     ,cela1  ,spredc,&
&intimh ,intiph ,intstr ,deltaa ,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         J.Kuipers
!
! Module:             GMDAST  (Graded Morphology Delta area structures)
!
! Module description: Calculate delta A by solving a continuity equation
!                     for a cell containing structures and the
!                     preceeding cell.
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

   integer   igp    ,ngrid  ,nfrac  ,jugralg
   integer   grid   (ngrid) ,flwdir (ngrid)
   real      alphac ,alphad ,alphae
   real      x      (ngrid) ,&
   &celer  (ngrid,nfrac,5) ,celert (ngrid)       ,&
   &sedtr  (ngrid,nfrac+2) ,&
   &source (ngrid,nfrac+2) ,&
   &intimh (nfrac)         ,intiph (nfrac)       ,&
   &intstr (nfrac)         ,&
   &spredc (nfrac)         ,cela1  (nfrac,nfrac) ,&
   &ds     (nfrac)         ,dfrac  (nfrac)
   double precision  dtm, deltaa (ngrid,nfrac+1)
!
!     Local variables
!
   integer   igpm1  ,igpp1  ,igps  ,istep  ,icel  ,igpcel
   logical   flwpos
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Read first and last grid point of branch
!
   flwpos = flwdir(igp+1).ge.0
!
!     Determine how many structures are present on consecutive cells.
!
   istep = 1
10 continue
   if (grid(igp+1+istep) .eq. cstrcl) then
      istep=istep+1
      goto 10
   endif
   icel = igp + istep + 1
!
!     Determine flow direction
!
   if (flwpos) then
      igps   = igp + 1
      igpcel = igp
   else
      igps   = icel
      igpcel = igps + 1
   endif
!
!     Calculate sediment integral on grid point flow upwards
!     of structure. This value will also be used at the downwards
!     side.
!
   call gmiflp (igps   ,igpcel ,ngrid ,nfrac  ,alphac ,alphad ,&
   &alphae ,sngl(dtm)     ,celer  ,celert ,sedtr  ,&
   &source ,x      ,dfrac ,ds     ,spredc ,cela1  ,&
   &intstr ,jugralg)
!
   igpm1 = igp - 1
   igpp1 = igp + 1
!
!     Determine change in area at left side of structure
!     as well a sediment integral on DX/2 leftwards.
!
   call GMDAEP (igpp1  ,ngrid  ,nfrac ,dtm    ,alphac ,&
   &alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,&
!u   +             source ,intiph ,intstr        ,dfrac  ,ds     ,
!i1
   &source ,intimh ,intstr        ,dfrac  ,ds     ,&
   &cela1  ,spredc ,deltaa,jugralg)
!
!     Determine change in area of DX left of structure
!
   call GMDALI (igpm1  ,igp    ,igpp1  ,ngrid ,nfrac  ,dtm   ,&
!u   +             intimh ,intiph ,source ,x     ,deltaa )
!i1
   &intiph ,intimh ,source ,x     ,deltaa )
!
!     Determine change in area at right side of structure
!     as well a sediment integral on DX/2 on the right.
!
   call GMDABP (icel   ,ngrid  ,nfrac ,dtm    ,alphac ,&
   &alphad ,alphae ,x     ,celer  ,celert ,sedtr  ,&
!u   +             source ,intimh ,intstr        ,dfrac  ,ds     ,
!i1
   &source ,intiph ,intstr        ,dfrac  ,ds     ,&
   &cela1  ,spredc ,deltaa,jugralg)
!
   igp = icel
!
   return
end
