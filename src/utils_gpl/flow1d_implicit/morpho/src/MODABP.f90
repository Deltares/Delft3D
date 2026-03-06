subroutine MODABP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,&
&nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
&node   ,ibrtyp ,mbdpar ,hlev   ,&
&grid   ,maxtab ,ntabm  ,ntab   ,&
&table  ,h      ,wf     ,wfh0   ,&
&ws     ,wft    ,afs    ,dissed ,&
&x      ,time   ,dtm    ,alphac ,&
&celer  ,sedtr  ,intbou ,flwdir ,&
&alphad ,moptd  ,mopte  ,slat   ,&
&intiph ,iextra ,deltaa ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MODABP (MOrphology Delta Area for Begin Point)
!
! Module description: Calculate delta area for begin point of branch.
!
!                     For the calculation of the delta area on the first
!                     point of a branch the values of the integral I1
!                     and I3/2 are needed. The integral value I1 is
!                     known from the call to MOINOD or MOIBOU.
!
!                     The value of delta area is returned as well as the
!                     value of I3/2. This integral value is used as a
!                     starting point for the internal points. On the
!                     first point no lateral sediment can be defined.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 afs               P  -
! 30 alphac            P  -
! 35 alphad            P  -
!  9 branch            P  -
! 31 celer             P  -
! 42 deltaa            O  Calculated change in area
! 26 dissed            P  -
! 29 dtm               I  Morphology time step.
! 34 flwdir            P  -
! 15 grid              P  -
! 20 h                 P  -
! 14 hlev              P  -
! 10 ibr               P  -
! 12 ibrtyp            P  -
!  1 igpbou            I  Calculated integral value on boundary
!  2 igpcel            I  Calculated integral for first or last cell in
!                         branch
! 33 intbou            I  Integral value for begin or end point of a
!                         branch
! 41 intiph            I  Calculated integral value on i + 1/2
!  3 isec              I  Section number (1 or 2)
! 39 juer              P  -
! 40 ker               P  -
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 16 maxtab            I  Maximum number of defined tables.
! 13 mbdpar            P  -
! 36 moptd             P  -
! 37 mopte             P  -
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
!  7 nnode             I  Number of nodes.
! 11 node              P  -
! 18 ntab              P  -
! 17 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 32 sedtr             P  -
! 38 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 19 table             P  -
! 28 time              P  -
! 21 wf                P  -
! 22 wfh0              P  -
! 24 wft               P  -
! 23 ws                P  -
! 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moitbp  MORPHology InTegral on Begin Point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: modabp.pf,v $
! Revision 1.7  1998/06/11  11:47:07  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1997/02/17  10:23:10  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.5  1996/03/08  09:38:59  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.4  1996/03/07  10:44:11  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/10/18  08:59:52  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:04:36  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:05  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:23  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:30  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igpbou ,igpcel ,isec   ,ngrid  ,ibr    ,nbran  ,&
   &nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,&
   &maxlev ,juer   ,ker
   integer    branch (4,nbran),&
   &grid   (ngrid),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab),&
   &flwdir (ngrid)

   real       alphac ,intiph, alphad, intbou , iextra

   real       x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid,maxlev),&
   &slat   (ngrid,*),&
   &afs    (ngrid)

   double precision time, dtm, hlev (ngrid,maxlev), deltaa, h(ngrid)


   logical    moptd, mopte
!
!     Local variables
!
   real       ili, dx
!
   CALL MOITBP ( igpbou ,igpcel ,isec   ,ngrid  ,nbran  ,&
   &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
   &node   ,ibrtyp ,mbdpar ,hlev   ,&
   &grid   ,maxtab ,ntabm  ,ntab   ,&
   &table  ,h      ,wf     ,wfh0   ,&
   &ws     ,wft    ,afs    ,dissed ,&
   &x      ,time   ,dtm    ,alphac ,&
   &celer  ,sedtr  ,intbou ,flwdir ,&
   &alphad ,moptd  ,mopte  ,intiph ,&
   &iextra ,juer   ,ker    )
!
!     Calculate dx
!
   dx = x(igpcel) - x(igpbou)
!
!     Calculate Slat integral
!
   ili = slat(igpbou,isec) * sngl(dtm)
!
!     Calculate delta A
!
   deltaa = ( intiph - intbou - ili ) / ( 0.5 * dx )
!
   return
end
