subroutine satqpu (nmouth ,nbran  ,ngrid  ,juer   ,time  ,dt    ,&
&tp     ,mouth  ,branch ,bramrl ,q1    ,q2    ,&
&af     ,timout ,mouqpu ,thcsum ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATQPU (SA Tha. h. Q(fresh wd),P(flood vol),U(vel))
!
! Module description: Calculation of new fresh water discharges, flood
!                     volumes and maximum velocities and the constant
!                     part in the  Thatcher-Harleman sum.
!
!                     For this tidal information is used. The quantities
!                     Q, P and U are calculated for each mouth (SATFWD
!                     and SATFVU). If SATFWD detects the start of a
!                     tidal period in a mouth, provided it is not the
!                     first one, routine SATBPA will be called to recal-
!                     culate for each branch the constant part of the
!                     Thather Harleman sum.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 af                P  -
! 10 bramrl            P  -
!  9 branch            P  -
!  6 dt                P  -
!  4 juer              P  -
! 17 ker               P  -
! 15 mouqpu            P  -
!  8 mouth             P  -
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  1 nmouth            I  Maximum number of mouths in the network.
! 11 q1                P  -
! 12 q2                P  -
! 16 thcsum            P  -
!  5 time              P  -
! 14 timout(2,nmouth)  I  Administration for the calculation of fresh
!                         water discharge, flood volume and maximum
!                         flood velocity for every mouth:
!                         (1,i) = Starting time of current tide.
!                         (2,i) = 0 : Fist tidal period not started yet.
!                                 1 : Fist tidal period has started.
!  7 tp                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! satbpa  SAlt Thatcher harleman Branch PArameters
! satfvu  SAlt Th. harl. Flood Vol. and max. vel.(U)
! satfwd  SAlt Thatcher harl. Fresh Water Discharge
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
! $Log: satqpu.pf,v $
! Revision 1.4  1995/10/18  09:00:33  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/08/23  14:29:47  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.2  1995/05/30  07:06:23  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:04  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:17:29  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration parameters
!
   integer   nmouth  ,nbran   ,ngrid ,juer   ,ker

   integer   mouth (2,*)      ,branch(4,nbran),&
   &bramrl(nmouth+1  ,nbran)
   real      af (ngrid), timout(2,*), mouqpu(3,0:2,*),&
   &thcsum(2,nbran)
   double precision     time, dt, tp, q1(ngrid), q2(ngrid)
!
!     Declaration of local parameters
!
   integer   im
   logical   change,lslack
!
   change = .false.
   do 10 im = 1, nmouth
      call satfwd (im     ,nbran  ,ngrid ,time  ,dt     ,tp     ,&
      &mouth  ,branch ,q1    ,q2    ,lslack ,timout ,&
      &mouqpu )
!
!        At the first slack water before flood no Thatcher-
!        Harleman sum has to be calculated.
!
      if (lslack .and. int(timout(2,im)).eq.1) change = .true.
!
      call satfvu (im     ,nbran ,ngrid  ,dt    ,lslack ,mouth  ,&
      &branch ,q1    ,q2     ,af    ,timout ,mouqpu )
10 continue
!
   if (change) then
! WRITE (11,*) 'TIME=',time do 112 k=1,nmouth
! WRITE (11,*) 'MOUTH=',k
!do 111 j=0,2
!WRITE (11,*) j,(mouqpu(i,j,k),i=1,3)
!111      continue
!12      continue
      call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum ,ker)
   endif
!
end
