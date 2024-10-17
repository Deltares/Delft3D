subroutine MODAST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,&
&nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
&node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,&
&maxtab ,ntabm  ,ntab   ,table  ,h      ,&
&wf     ,wfh0   ,ws     ,wft    ,afs    ,&
&x      ,dtm    ,alphac ,alphad ,&
&celer  ,sedtr  ,dissed ,time   ,flwdir ,&
&mopta  ,moptb  ,moptc  ,moptd  ,mopte  ,&
&moptf  ,slat   ,intstr ,int1   ,int2   ,&
&delta1 ,delta2&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MODAST (MOrphology Delta Area for STructures)
!
! Module description: Calculate change in area for grid points around
!                     structures.
!
!                     This routine calculates the change in area for
!                     the left and right cross sections around a struc-
!                     ture. For the left grid point of a structure the
!                     integrals Istr-1/2 and Istr are used. For the
!                     right grid point of a structure the integrals
!                     Istr+1/2 and Istr are used. The inegrals on the
!                     halve point will be returned to the calling rou-
!                     tine. These integral values will be used to cal-
!                     culate the last internal point (MODALI) and the
!                     first following internal point (MODAIP). The
!                     structure integral Istr will be calculated by
!                     routine MOITST.
! Module:             MODAST (MOrphology Delta Area for STructures)
!
! Module description: Calculate change in area for grid points around
!                     structures.
!
!                     This subroutine calculates the change in area for
!                     the left or right cross section around a structu-
!                     re. For the left grid point of a structure the
!                     points I_sl-1/2 and I_stru are used. For the right
!                     grid point of the structure the points I_sr+1/2
!                     and I_stru are used. The integral on the halve
!                     point will be returned to the calling routine.
!                     This integral value will be used to calculate the
!                     last internal point (MODALI) or the first follo-
!                     wing internal point (MODAIP). The structure inte-
!                     gral I_stru will be calculated by routine MOISTR.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 afs               P  -
! 28 alphac            P  -
! 29 alphad            P  -
!  9 branch            P  -
! 30 celer             P  -
! 45 delta1            O  Calculated change in area at left side of
!                         structure
! 46 delta2            O  Calculated change in arear at right side
!                         of structure
! 32 dissed            P  -
! 27 dtm               I  Morphology time step.
! 34 flwdir            P  -
! 15 grid              P  -
! 20 h                 P  -
! 14 hlev              P  -
! 10 ibr               P  -
! 12 ibrtyp            P  -
!  2 icel              I  First non structure point after structure
!  1 igp               I  Gridpoint number
! 43 int1              I  Calculated integral value left side of structure
! 44 int2              I  Calculated integral value right side of structure
! 42 intstr            I  Calculated integral value for a structure
!  3 isec              I  Section number (1 or 2)
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 16 maxtab            I  Maximum number of defined tables.
! 13 mbdpar            P  -
! 35 mopta             P  -
! 36 moptb             P  -
! 37 moptc             P  -
! 38 moptd             P  -
! 39 mopte             P  -
! 40 moptf             P  -
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  7 nnode             I  Number of nodes.
! 11 node              P  -
! 18 ntab              P  -
! 17 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 31 sedtr             P  -
! 41 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 19 table             P  -
! 33 time              P  -
! 21 wf                P  -
! 22 wfh0              P  -
! 24 wft               P  -
! 23 ws                P  -
! 26 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moitst  MOrphology InTegral for STructure point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: modast.pf,v $
! Revision 1.7  1997/02/17  10:23:14  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.6  1996/03/08  09:39:04  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.5  1996/03/07  10:44:14  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/10/18  08:59:56  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:49  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:40  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:09  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:31  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:34  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer     igp    ,icel   ,isec   ,ngrid  ,ibr    ,nbran  ,&
   &nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,&
   &maxlev

   integer     branch (4,nbran),&
   &grid   (ngrid),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab),&
   &flwdir (ngrid)

   real        alphac ,alphad ,intstr ,int1   ,int2

   double precision  delta1 ,delta2

   real        x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid),&
   &slat   (ngrid,*),&
   &afs    (ngrid)

   double  precision   time, dtm, hlev   (ngrid,maxlev)
   double precision    h(ngrid)

   logical mopta ,moptb ,moptc ,moptd ,mopte ,moptf
!
!     Local variables
!
   real       ili1, ili2, dx1, dx2
!
!
!        Calculate I_structure
!
   CALL MOITST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,&
   &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
   &node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,&
   &maxtab ,ntabm  ,ntab   ,table  ,h      ,&
   &wf     ,wfh0   ,ws     ,wft    ,afs    ,&
   &x      ,dtm    ,alphac ,alphad ,&
   &celer  ,sedtr  ,dissed ,time   ,flwdir ,&
   &mopta  ,moptb  , moptc ,moptd  ,mopte  ,&
   &moptf  ,intstr ,int1   ,int2&
   &)
!
!     Calculate dx
!
   dx1 = abs ( x(igp) - x(igp-1) )
   dx2 = abs ( x(icel+1) - x(icel) )
!
!     Calculate Slat integral
!
   ili1 = slat(igp,isec) * sngl(dtm)
   ili2 = slat(igp,isec) * sngl(dtm)
!
!     Calculate delta A
!
   delta1 = ( intstr - int1 - ili1 ) / ( 0.5 * dx1 )
   delta2 = ( int2 - intstr - ili2 ) / ( 0.5 * dx2 )
!
   return
end
