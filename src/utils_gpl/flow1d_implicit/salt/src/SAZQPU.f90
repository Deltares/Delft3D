subroutine sazqpu (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,&
&ntabm  ,ngrid ,juer   ,time   ,dt     ,tp     ,&
&mouth  ,branch ,node  ,bramrl ,hbdpar ,ntab   ,&
&table  ,q1     ,q2    ,qfloq   ,emppar ,mouqpu,&
&thcsum ,timout ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAZQPU (SA Zwendl Q(fresh wd),P(flood vol),U(vel))
!
! Module description: Calculation of new fresh water discharges, flood
!                     volumes, maximum velocities and the constant part
!                     in the Thather-Harleman sum, for usage in the
!                     Zwendl dispersion formulation. A restriction to
!                     this formulation is that only one mouth can be
!                     defined in the first release. This mouth is used
!                     to determine changes in tide.
!
!                     The quantities Q, P and U are calculated by SAZFWD
!                     and SAZFVU. If SATZWD detects the start of a tidal
!                     period in the mouth, routine SATBPA will be called
!                     to recalculate for each branch the constant part
!                     of the Thather Harleman sum.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 bramrl            P  -
! 14 branch            P  -
! 11 dt                P  -
! 23 emppar            P  -
! 17 hbdpar            P  -
!  9 juer              P  -
! 27 ker               P  -
!  6 maxtab            I  Maximum number of defined tables.
! 24 mouqpu            P  -
! 13 mouth             P  -
!  2 nbran             I  Number of branches.
!  8 ngrid             I  Number of grid points in network.
!  4 nhstat            I  Number of H-boundary stations.
!  1 nmouth            I  Maximum number of mouths in the network.
!  3 nnode             I  Number of nodes.
! 15 node              P  -
!  5 nqfloc            P  -
! 18 ntab              P  -
!  7 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 20 q1                P  -
! 21 q2                P  -
! 22 qfloq             P  -
! 19 table             P  -
! 25 thcsum            P  -
! 10 time              P  -
! 26 timout            P  -
! 12 tp                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! satbpa  SAlt Thatcher harleman Branch PArameters
! sazfvu  SAlt Zwendl Flood Vol. and max. vel.(U)
! sazfwd  SAlt Zwendl Fresh Water Discharge
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
! $Log: sazqpu.pf,v $
! Revision 1.2  1995/05/30  07:06:29  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:09  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:17:35  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer  nmouth  ,nbran ,nnode  ,nhstat  ,nqfloc  ,maxtab,&
   &ntabm   ,ngrid ,juer   ,ker
   integer  mouth (2,*)            ,branch(4,nbran)  ,node(4,nnode) ,&
   &bramrl(nmouth+1,nbran) ,hbdpar(3,nhstat) ,ntab(4,maxtab)
   real     table (ntabm)    ,qfloq (2,*)      ,&
   &timout(2,*)      ,emppar(4,*)      ,mouqpu(3,0:2,*)     ,&
   &thcsum(2,nbran)
   double precision   time  ,dt       ,tp
   double precision   q1(ngrid), q2(ngrid)

!
!     Declaration of local parameters
!
   integer   im
   logical   change,lslack
!
   change = .false.
   do 10 im = 1,nmouth
      call sazfwd (im     ,nbran  ,nqfloc ,ngrid  ,time  ,dt     ,&
      &tp     ,mouth  ,branch ,qfloq  ,q1    ,q2     ,&
      &lslack ,timout ,mouqpu )
!
!        At the first slack water before flood the flood volume and
!        the maximum flood velocity are determined. So the
!        Thatcher-Harleman sum has to be recalculated.
!
      if (lslack) change = .true.
!
      call sazfvu (im     ,nnode  ,nhstat ,maxtab ,ntabm ,time   ,&
      &dt     ,tp     ,lslack ,mouth  ,node  ,hbdpar ,&
      &ntab   ,table  ,emppar ,mouqpu )
10 continue
!
   if (change) then
      call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum, ker)
   endif
!
end
