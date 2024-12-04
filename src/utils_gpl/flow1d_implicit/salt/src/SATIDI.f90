subroutine satidi (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,&
&ntabm  ,ngrid ,dsopt  ,juer   ,time   ,dt     ,&
&tp     ,mouth  ,branch,node   ,bramrl ,hbdpar ,&
&ntab   ,table  ,grid  ,x      ,q1     ,q2     ,&
&af     ,csa1   ,dcdx   ,cdcdx0,cdcdx1 ,cdcdx2 ,&
&thasca ,qfloq  ,emppar ,mouqpu,thcsum ,timout ,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATIDI (SAlt calculate TIDal Information)
!
! Module description: Calculate information for Thather Harleman or
!                     Zwendl formulation that is constant over a tidal
!                     period.
!
!                     Both methods use different ways to calculate the
!                     fresh water discharge, the flood volume and the
!                     maximum flood velocity.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 af                P  -
! 17 bramrl            P  -
! 15 branch            P  -
! 28 cdcdx0            P  -
! 29 cdcdx1            P  -
! 30 cdcdx2            P  -
! 26 csa1              P  -
! 27 dcdx              P  -
!  9 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
! 12 dt                P  -
! 33 emppar            P  -
! 21 grid              P  -
! 18 hbdpar            P  -
! 10 juer              P  -
! 37 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  6 maxtab            I  Maximum number of defined tables.
! 34 mouqpu            P  -
! 14 mouth             P  -
!  2 nbran             I  Number of branches.
!  8 ngrid             I  Number of grid points in network.
!  4 nhstat            I  Number of H-boundary stations.
!  1 nmouth            I  Maximum number of mouths in the network.
!  3 nnode             I  Number of nodes.
! 16 node              P  -
!  5 nqfloc            P  -
! 19 ntab              P  -
!  7 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 23 q1                P  -
! 24 q2                P  -
! 32 qfloq             P  -
! 20 table             P  -
! 31 thasca            P  -
! 35 thcsum            P  -
! 11 time              P  -
! 36 timout            P  -
! 13 tp                P  -
! 22 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! saccda  SAlt calculation of C/C0*Dc/dx Averaged
! satqpu  SA Tha. h. Q(fresh wd),P(flood vol),U(vel)
! sazqpu  SA Zwendl Q(fresh wd),P(flood vol),U(vel)
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: satidi.pf,v $
! Revision 1.5  1995/10/18  09:00:32  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/30  12:37:21  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:18  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:21  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:02  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:27  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:34:12  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer  nmouth  ,nbran ,nnode  ,nhstat  ,nqfloc  ,maxtab,&
   &ntabm   ,ngrid ,dsopt  ,juer    ,ker
   integer  mouth (2,*)    ,branch(4,nbran) ,node(4,nnode),&
   &bramrl(nmouth+1,nbran) ,hbdpar(3,nhstat) ,ntab(4,maxtab),&
   &grid  (ngrid)
   real     table (ntabm)    ,x     (ngrid)    ,&
   &af    (ngrid)    ,csa1  (ngrid)    ,dcdx  (ngrid)      ,&
   &cdcdx0(ngrid)    ,cdcdx1(ngrid)    ,cdcdx2(ngrid)      ,&
   &thasca(3)        ,qfloq (2,*)      ,&
   &emppar(4,*)      ,mouqpu(3,0:2,*)  ,&
   &thcsum(2,nbran)  ,timout(2,*)
   double   precision&
   &time  ,dt ,tp, q1(ngrid),q2(ngrid)
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   call saccda (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt     ,&
   &tp     ,mouth  ,branch ,bramrl ,q1     ,q2     ,&
   &grid   ,csa1   ,x      ,dcdx   ,cdcdx0 ,cdcdx1 ,&
   &cdcdx2 ,thasca ,ker    )
!
   if (ker.lt.fatal) then
      if      (dsopt .eq. cdsthh) then
!
!           Thatcher Harleman formulation
!
         call satqpu (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt    ,&
         &tp     ,mouth  ,branch ,bramrl ,q1     ,q2    ,&
         &af     ,timout ,mouqpu ,thcsum ,ker    )
      else if (dsopt .eq. cdsemp) then
!
!           Empirical formulation
!
         call sazqpu (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,&
         &ntabm  ,ngrid ,juer   ,time   ,dt     ,tp     ,&
         &mouth  ,branch ,node  ,bramrl ,hbdpar ,ntab   ,&
         &table  ,q1     ,q2    ,qfloq  ,emppar ,mouqpu ,&
         &thcsum ,timout ,ker   )
      endif
   endif
!
end
