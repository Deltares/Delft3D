subroutine GMDABP (igpbou ,ngrid  ,nfrac  ,dtm   ,alphac ,&
&alphad ,alphae ,x      ,celer ,celert ,sedtr  ,&
&source ,intiph ,intbou ,dfrac ,ds     ,cela1  ,&
&spredc ,deltaa ,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             GMDABP (Graded Morphology Delta Area for
!                             Begin Point)
!
! Module description: Calculate delta area for begin point of branch.
!
!                     For the calculation of the delta area on the first
!                     point of a branch the values of the integral I1
!                     and I3/2 are needed. The integrals are calculated
!                     by routine MOIBOU and MOIIPH. After the call to
!                     MOIBOU the value of I1 is known. The integral
!                     value is passed to MOIIPH because it is possible
!                     that the integral value is needed to calculated
!                     the integral on i3/2. This is true in case a node
!                     is connected to point 1.
!
!                     The value of delta area is returned as well as the
!                     value of I3/2. This integral is value is used as a
!                     starting point for the internal points. On the
!                     first point no lateral sediment can be defined.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 29 afs               P  -
! 23 alphac            P  -
! 11 branch            P  -
! 30 celer             P  -
! 35 deltaa            O  Calculated change in area
! 32 dissed            P  -
! 22 dtm               I  Morphology time step
! 10 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 24 h                 P  -
! 16 hlev              P  -
!  1 ibr               P  -
! 13 ibrtyp            P  -
!  2 igpbou            I  Calculated integral value on boundary
!  4 intbou            I  Integral value for begin or end point of a
!                         branch
! 34 intiph            I  Calculated integral value on i + 1/2
!  3 isec              I  Section number (1 or 2)
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 17 maxtab            I  Maximum number of defined tables.
! 14 mbdpar            P  -
!  7 nboun             I  Number of boundary nodes.
!  6 nbran             I  Number of branches.
!  5 ngrid             I  Number of grid points in network.
!  8 nnode             I  Number of nodes.
! 12 node              P  -
! 19 ntab              P  -
! 18 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 31 sedtr             P  -
! 33 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 20 table             P  -
! 21 time              P  -
! 25 wf                P  -
! 26 wfh0              P  -
! 28 wft               P  -
! 27 ws                P  -
! 15 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moiflh  MOrphology Integral on First or Last halve point
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmdabp.F,v $
! Revision 1.2  1995/09/27  10:11:27  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igpbou ,ngrid  ,nfrac ,jugralg
   real       alphac ,alphad ,alphae
   real       x      (ngrid)        ,&
   &celer  (ngrid,nfrac,5),celert (ngrid)       ,&
   &sedtr  (ngrid,nfrac+2),&
   &source (ngrid,nfrac+2),&
   &cela1  (nfrac,nfrac)  ,&
   &ds     (nfrac)        ,spredc (nfrac)       ,&
   &intiph (nfrac)        ,intbou (nfrac)       ,&
   &dfrac  (nfrac)
   double precision   dtm, deltaa (ngrid,nfrac+1)
!
!     Local variables
!
   integer    igpcel  ,jf
   real       ili     ,dtm2  ,rdx ,sum
!
!     Calculate point on location x = 1.5
!
   igpcel = igpbou+1
!
   call gmiflh ( igpbou ,igpcel ,ngrid ,nfrac ,alphac ,alphad ,&
   &alphae ,sngl(dtm)     ,celer ,celert ,sedtr  ,&
   &source ,x      ,dfrac ,ds    ,spredc ,cela1  ,&
   &intiph ,jugralg)
!
!     Calculate dx
!
   dtm2 = sngl(dtm) * .5
   rdx  = 2. / (x(igpcel) - x(igpbou))
!
   do 10 jf=1,nfrac

!        Calculate lateral integral

      ili = source(igpbou,jf) * dtm2
!
!        Calculate delta A
!
      deltaa(igpbou,jf) = ( intiph(jf) - intbou(jf)- ili) * rdx

10 continue
!f
   sum = 0.
   do 20 jf=1,nfrac
      sum = sum + deltaa(igpbou,jf)
20 continue
   deltaa(igpbou,nfrac+1) = sum

end
