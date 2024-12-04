subroutine MOAREA ( isec   ,ngrid  ,nbran  ,&
&nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
&node   ,ibrtyp ,mbdpar ,hlev   ,nlev   ,&
&grid   ,maxtab ,ntabm  ,ntab   ,&
&table  ,h      ,wf     ,wfh0   ,&
&ws     ,wft    ,afs    ,dissed ,&
&x      ,time   ,dtm    ,alphac ,&
&celer  ,sedtr  ,intgr  ,flwdir ,sumda  ,&
&alphad ,mopta  ,moptb  ,moptc  ,moptd  ,&
&mopte  ,moptf  ,slat   ,moropt ,&
&juer   ,ker&
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
! Module:             MOAREA (MOrphology AREA)
!
! Module description: Calculate delta A by solving a continuity equation
!                     for each gridpoint in the branch and adapt cross
!                     sectional dimensions. For sedredge branches this
!                     routine will be called once for the left channel
!                     and once for the right channel.
!
!                     First routine MOADBP is called to determine the
!                     change in area for grid point 1. This routine also
!                     delivers the value of the integral at point x =
!                     3/2. The integral is used as a starting point for
!                     the calculation of the internal grid points. The
!                     internal grid points from i1+1 until i2-2 are pro-
!                     cessed by routine MODAIP. The last point is calcu-
!                     lated by routine MOADEP. Point i2-1 is calculated
!                     by routine MODALI using the last results from
!                     routines MODAIP and MODAEP. See also figure one in
!                     chapter three of [S-DO-004].
!
!                     In case a structure is found, the last point
!                     before the structure will be calculated as the
!                     last point of a branch. The structure grid points
!                     are processed by MODAST.
!
!                     Each of the above described routines return a
!                     change in the area. The calculated changes are
!                     used to adapt the cross sectional tables in routi-
!                     ne MOADCS.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 24 afs               P  -
! 29 alphac            P  -
! 34 alphad            P  -
!  7 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 30 celer             P  -
! 25 dissed            P  -
! 28 dtm               P  -
! 33 flwdir            P  -
! 14 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 19 h                 P  -
! 12 hlev              P  -
!  8 ibr               I  Branch number
! 10 ibrtyp            P  -
! 32 intgr(ngrid,*)    I  Integral values for grid
!  1 isec              I  Section number (1 or 2)
! 44 juer              P  -
! 45 ker               P  -
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 15 maxtab            I  Maximum number of defined tables.
! 11 mbdpar            P  -
! 35 mopta             P  -
! 36 moptb             P  -
! 37 moptc             P  -
! 38 moptd             P  -
! 39 mopte             P  -
! 40 moptf             P  -
! 42 moropt            P  -
!  4 nboun             I  Number of boundary nodes.
!  3 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
! 13 nlev              P  -
!  5 nnode             I  Number of nodes.
!  9 node              P  -
! 17 ntab              P  -
! 16 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 31 sedtr             P  -
! 41 slat              P  -
!    sumda             IO Increase in cross sectional area since start
! 18 table             P  -
! 27 time              P  -
! 20 wf                P  -
! 21 wfh0              P  -
! 23 wft               P  -
! 22 ws                P  -
! 26 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moadcs  MORPHology Adapt Cross Sections
! modabp  MOrphology Delta Area for Begin Point
! modaep  MOrphology Delta Area for End Point
! modaip  MOrphology Delta Area for Internal Points
! modali  MOrphology Delta Area for Last Internal point
! modast  MOrphology Delta Area for STructures
! modast  MOrphology Delta Area for STructures
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moarea.pf,v $
! Revision 1.8  1998/06/11  11:47:06  kuipe_j
! Estuary special integrated
!
! Revision 1.7  1997/06/17  11:18:26  kuipe_j
! Remove undefined vars
!
! Revision 1.6  1996/09/03  14:48:47  kuipe_j
! frequency time hist,Improved sed distribution at nodes
!
! Revision 1.5  1996/03/08  09:38:57  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.4  1996/03/07  10:44:10  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/05/30  09:55:46  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:02  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:26:23  kuipe_j
! Branches with 2 or 3 grid points are also possible now.
!
! Revision 1.3  1994/11/28  08:52:21  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:26  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:05  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer    isec   ,ngrid  ,ibr    ,nbran  ,&
   &nboun  ,nnode  ,maxtab ,ntabm  ,ibrtyp ,moropt ,&
   &maxlev ,juer   ,ker

   integer    branch (4,nbran),&
   &grid   (ngrid),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab),&
   &nlev   (ngrid),&
   &flwdir (ngrid)

   real       alphac ,alphad

   real       x      (ngrid),&
   &intgr  (ngrid,2,*),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid,maxlev),&
   &slat   (ngrid,*),&
   &afs    (ngrid)  ,sumda (ngrid)

   double precision  time, dtm, hlev   (ngrid,maxlev)
   double precision  h(ngrid)

   logical    mopta ,moptb ,moptc ,moptd ,mopte ,moptf

!     Local variables
!
   integer   i1     ,i2     ,igp    ,icel   ,istep

   real      intbou ,intimh ,intiph ,intstr ,&
   &int1   ,int2   ,intnmh ,&
   &iextra

   double precision deltaa, delta1, delta2
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
   intbou = intgr(i1,1,isec)
   iextra = intgr(i1,2,isec)
!
   call MODABP ( i1 ,i1+1 ,isec   ,ngrid  ,nbran  ,&
   &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
   &node   ,ibrtyp ,mbdpar ,hlev   ,&
   &grid   ,maxtab ,ntabm  ,ntab   ,&
   &table  ,h      ,wf     ,wfh0   ,&
   &ws     ,wft    ,afs    ,dissed ,&
   &x      ,time   ,dtm    ,alphac ,&
   &celer  ,sedtr  ,intbou ,flwdir ,&
   &alphad ,moptd  ,mopte  ,slat   ,&
   &intiph ,iextra ,deltaa ,juer   ,ker    )
!
!  Aanpassing Kees Sloff 8-8-1997: ibr toegvoegd aan MODACS
!     Adapt cross section
!
   call MOADCS ( i1     ,isec   ,ibrtyp ,&
   &deltaa ,time   ,moropt ,&
   &nboun  ,ngrid  ,nnode  ,&
   &branch ,ibr    ,node   ,mbdpar ,&
   &maxtab ,ntabm  ,ntab   ,table  ,&
   &maxlev ,nlev   ,hlev   ,&
   &wft    ,ws     ,&
   &flwdir&
   &)
   sumda(i1) = sumda(i1) + deltaa
!
!     Determine first internal point and save integral on halve point
!
   igp = i1 + 1
   intimh = intiph
!
!     Loop over internal points
!
100 continue
!
!     Stop if igp >= i2 - 1
!
   if ( igp .lt. i2-1 ) then
!
!        Check if next grid point = structure point
!
      if ( grid (igp+1) .eq. cstrcl) then
!
!           Calculate I_n-1/2 , I_structure and I_n+1/2
!
!
!           locate end point of structure
!
         istep = 1
200      continue
         if (grid(igp+1+istep) .eq. cstrcl) then
            istep=istep+1
            goto 200
         endif
         icel = igp + istep + 1
         call MODAST ( igp+1  ,icel   ,isec   ,ngrid  ,nbran  ,&
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
!
!           Adapt cross section for left side of structure
!
         call MOADCS ( igp+1  ,isec   ,ibrtyp ,&
         &delta1 ,time   ,moropt ,&
         &nboun  ,ngrid  ,nnode  ,&
         &branch ,ibr    ,node   ,mbdpar ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &maxlev ,nlev   ,hlev   ,&
         &wft    ,ws     ,&
         &flwdir&
         &)
         sumda(igp+1) = sumda(igp+1) + real(delta1)
!
!           Adapt cross section for right side of structure
!
         call MOADCS ( icel   ,isec   ,ibrtyp ,&
         &delta2 ,time   ,moropt ,&
         &nboun  ,ngrid  ,nnode  ,&
         &branch ,ibr    ,node   ,mbdpar ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &maxlev ,nlev   ,hlev   ,&
         &wft    ,ws     ,&
         &flwdir&
         &)
         sumda(icel) = sumda(icel) + real(delta2)
!
!           Process last point before structure
!
         intnmh = int1
         call MODALI ( igp-1  ,igp    ,igp+1  ,&
         &isec   ,ngrid  ,&
         &dtm    ,&
         &intimh ,intnmh ,&
         &slat   ,x      ,&
         &deltaa&
         &)
!
!           Adapt cross section
!
         call MOADCS ( igp    ,isec   ,ibrtyp ,&
         &deltaa ,time   ,moropt ,&
         &nboun  ,ngrid  ,nnode  ,&
         &branch ,ibr    ,node   ,mbdpar ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &maxlev ,nlev   ,hlev   ,&
         &wft    ,ws     ,&
         &flwdir&
         &)
         sumda(igp) = sumda(igp) + real(deltaa)
!
!           Determine next grid point and save integral on halve point
!
         igp   = icel + 1
         intimh = int2
      else
!
!           Next point is a normal grid cell
!
!           intiph [I(i+1/2)] from previous point is used
!
         intiph = intimh
         call MODAIP ( igp    ,isec   ,ngrid  ,&
         &alphac ,dtm    ,alphad ,&
         &celer  ,sedtr  ,slat   ,x     ,&
         &intiph ,deltaa&
         &)
!
!           Adapt cross section
!
         call MOADCS ( igp    ,isec   ,ibrtyp ,&
         &deltaa ,time   ,moropt ,&
         &nboun  ,ngrid  ,nnode  ,&
         &branch ,ibr    ,node   ,mbdpar ,&
         &maxtab ,ntabm  ,ntab   ,table  ,&
         &maxlev ,nlev   ,hlev   ,&
         &wft    ,ws     ,&
         &flwdir&
         &)
         sumda(igp) = sumda(igp) + deltaa
!
!           Determine next grid point and save integral at halve point
!
         igp   = igp + 1
         intimh = intiph
!
      endif
      goto 100
   endif
!
!     Last point of branch
!
!       * intnmh [I(n-1/2)] will be calculated
!
   intbou = intgr(i2,1,isec)
   iextra = intgr(i2,2,isec)
!
   call MODAEP ( i2     ,i2-1   ,isec   ,ngrid  ,nbran  ,&
   &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
   &node   ,ibrtyp ,mbdpar ,hlev   ,&
   &grid   ,maxtab ,ntabm  ,ntab   ,&
   &table  ,h      ,wf     ,wfh0   ,&
   &ws     ,wft    ,afs    ,dissed ,&
   &x      ,time   ,dtm    ,alphac ,&
   &celer  ,sedtr  ,intbou ,flwdir ,&
   &alphad ,moptc  ,moptf  ,slat   ,&
   &intnmh ,iextra ,deltaa ,juer   ,ker    )
!
!     Adapt cross section
!
   call MOADCS ( i2     ,isec   ,ibrtyp ,&
   &deltaa ,time   ,moropt ,&
   &nboun  ,ngrid  ,nnode  ,&
   &branch ,ibr    ,node   ,mbdpar ,&
   &maxtab ,ntabm  ,ntab   ,table  ,&
   &maxlev ,nlev   ,hlev   ,&
   &wft    ,ws     ,&
   &flwdir&
   &)
   sumda(i2) = sumda(i2) + real( deltaa )
!
!     One but last point of branch
!
!       * intimh [I(i+1/2)] from previous point is used
!       * intnmh [I(n-1/2)] from end point is used
!
   if (igp .lt. i2) then
      call MODALI ( igp-1  ,igp    ,igp+1  ,&
      &isec   ,ngrid  ,&
      &dtm    ,&
      &intimh ,intnmh ,&
      &slat   ,x      ,&
      &deltaa&
      &)
!
!        Adapt cross section
!
      call MOADCS ( igp    ,isec   ,ibrtyp ,&
      &deltaa ,time   ,moropt ,&
      &nboun  ,ngrid  ,nnode  ,&
      &branch ,ibr    ,node   ,mbdpar ,&
      &maxtab ,ntabm  ,ntab   ,table  ,&
      &maxlev ,nlev   ,hlev   ,&
      &wft    ,ws     ,&
      &flwdir&
      &)
      sumda(igp) = sumda(igp) + real( deltaa )
   endif

   return
end
