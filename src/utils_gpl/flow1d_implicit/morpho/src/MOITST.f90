subroutine MOITST ( igp    ,icel   ,isec   ,ngrid  ,nbran  ,&
&nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
&node   ,ibrtyp ,mbdpar ,hlev   ,grid   ,&
&maxtab ,ntabm  ,ntab   ,table  ,h      ,&
&wf     ,wfh0   ,ws     ,wft    ,afs    ,&
&x      ,dtm    ,alphac ,alphad ,&
&celer  ,sedtr  ,dissed ,time   ,flwdir ,&
&mopta  ,moptb  , moptc ,moptd  ,mopte  ,&
&moptf  ,intstr ,int1   ,int2&
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
! Module:             MOITST (MOrphology InTegral for STructure point)
!
! Module description: Calculate structure integral
!
!                     The integral for a structure  is determined at
!                     the outflowing side of the structure. This
!                     because the sum of the integrals must be zero.
!                     Notice that a structure has a begin and end point
!                     This routine will be called with the left grid
!                     point number igp and the right point number icel.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 afs               P  -
! 28 alphac            P  -
! 29 alphad            P  -
!  9 branch            P  -
! 30 celer             P  -
! 32 dissed            P  -
! 27 dtm               P  -
! 34 flwdir(ngrid)     I  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
! 15 grid              P  -
! 20 h                 P  -
! 14 hlev              P  -
! 10 ibr               P  -
! 12 ibrtyp            P  -
!  2 icel              I  First non structure point after structure
!  1 igp               I  Gridpoint number
! 42 int1              P  -
! 43 int2              P  -
! 41 intstr            O  Calculated integral value for a structure
!  3 isec              P  -
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
! 19 table             P  -
! 33 time              P  -
! 21 wf                P  -
! 22 wfh0              P  -
! 24 wft               P  -
! 23 ws                P  -
! 26 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moitbp  MORPHology InTegral on Begin Point
! moitep  MOrphology InTegral on End Point
! moitno  MOrphology InTegral at a NOde
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moitst.pf,v $
! Revision 1.3  1998/06/11  11:47:18  kuipe_j
! Estuary special integrated
!
! Revision 1.2  1996/03/08  09:39:14  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.1  1996/03/07  10:44:23  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/10/18  09:00:01  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:04:51  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:19  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:39  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:49  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
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
   &maxlev ,juer   ,ker

   integer     branch (4,nbran),&
   &grid   (ngrid),&
   &node   (4,nnode),&
   &mbdpar (5,nboun),&
   &ntab   (4,maxtab),&
   &flwdir (ngrid)

   real        alphac ,alphad ,intstr ,int1   ,int2

   real        x      (ngrid),&
   &table  (ntabm),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &wf     (ngrid),&
   &wfh0   (ngrid),&
   &ws     (ngrid),&
   &wft    (ngrid),&
   &afs    (ngrid)

   double  precision   time, dtm, hlev(ngrid,maxlev), h(ngrid)


   logical mopta ,moptb ,moptc ,moptd ,mopte ,moptf
!
!     Local variables
!
   real    iextra
!
!     Determine flow direction at i=istruct
!
! wijziging zie MOITEP
!
!     if ( flwdir(igp) .ge. 0) then
   if ( flwdir(igp) .gt. 0) then
!
!        Left side of structure is inflowing
!        ------------------------------------

!
!     Calculate Istr
!
      if (flwdir(igp) .eq. 0) then
         intstr = 0.
      else
         CALL MOITNO ( igp    ,isec   ,ngrid  ,x      ,&
         &dtm    ,alphac ,0      ,branch ,&
         &ibr    ,nbran  ,grid   ,&
         &mopta  ,moptb  ,moptc  ,moptd  ,&
         &celer  ,sedtr  ,alphad ,flwdir ,&
         &juer   ,ker    ,intstr&
         &)
      endif
!
!     Calculate I-extra
!
      call MOIEXT ( igp    ,-1     ,isec   ,ngrid  ,&
      &dtm    ,x      ,celer  ,sedtr  ,iextra )
!
!     Calculate Istr-1/2
!
      CALL MOITEP ( igp    ,igp-1  ,isec   ,ngrid  ,nbran  ,&
      &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
      &node   ,ibrtyp ,mbdpar ,hlev   ,&
      &grid   ,maxtab ,ntabm  ,ntab   ,&
      &table  ,h      ,wf     ,wfh0   ,&
      &ws     ,wft    ,afs    ,dissed ,&
      &x      ,time   ,dtm    ,alphac ,&
      &celer  ,sedtr  ,intstr ,flwdir ,&
      &alphad ,moptc  ,moptf  ,int1   ,&
      &iextra ,juer   ,ker    )
!
!     Calculate Istr+1/2
!
      CALL MOITBP ( icel   ,icel+1 ,isec   ,ngrid  ,nbran  ,&
      &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
      &node   ,ibrtyp ,mbdpar ,hlev   ,&
      &grid   ,maxtab ,ntabm  ,ntab   ,&
      &table  ,h      ,wf     ,wfh0   ,&
      &ws     ,wft    ,afs    ,dissed ,&
      &x      ,time   ,dtm    ,alphac ,&
      &celer  ,sedtr  ,intstr ,flwdir ,&
      &alphad ,moptd  ,mopte  ,int2   ,&
      &iextra ,juer   ,ker    )

   else
!
!        Left side of structure is outflowing
!        -------------------------------------
!
!
!     Calculate Istr
!
      CALL MOITNO ( icel   ,isec   ,ngrid  ,x      ,&
      &dtm    ,alphac ,0      ,branch ,&
      &ibr    ,nbran  ,grid   ,&
      &mopta  ,moptb  ,moptc  ,moptd  ,&
      &celer  ,sedtr  ,alphad ,flwdir ,&
      &juer   ,ker    ,intstr&
      &)
!
!     Calculate I-extra
!
      call MOIEXT ( icel   , 1     ,isec   ,ngrid  ,&
      &dtm    ,x      ,celer  ,sedtr  ,iextra )
!
!     Calculate Istr-1/2
!
      CALL MOITEP ( igp    ,igp-1  ,isec   ,ngrid  ,nbran  ,&
      &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
      &node   ,ibrtyp ,mbdpar ,hlev   ,&
      &grid   ,maxtab ,ntabm  ,ntab   ,&
      &table  ,h      ,wf     ,wfh0   ,&
      &ws     ,wft    ,afs    ,dissed ,&
      &x      ,time   ,dtm    ,alphac ,&
      &celer  ,sedtr  ,intstr ,flwdir ,&
      &alphad ,moptc  ,moptf  ,int1   ,&
      &iextra ,juer   ,ker    )
!
      CALL MOITBP ( icel   ,icel+1 ,isec   ,ngrid  ,nbran  ,&
      &nboun  ,nnode  ,maxlev ,branch ,ibr    ,&
      &node   ,ibrtyp ,mbdpar ,hlev   ,&
      &grid   ,maxtab ,ntabm  ,ntab   ,&
      &table  ,h      ,wf     ,wfh0   ,&
      &ws     ,wft    ,afs    ,dissed ,&
      &x      ,time   ,dtm    ,alphac ,&
      &celer  ,sedtr  ,intstr ,flwdir ,&
      &alphad ,moptd  ,mopte  ,int2   ,&
      &iextra ,juer   ,ker    )

   endif
!
   return
end
