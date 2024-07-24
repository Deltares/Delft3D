      subroutine satqpu (nmouth ,nbran  ,ngrid  ,juer   ,time  ,dt    ,
     &                   tp     ,mouth  ,branch ,bramrl ,q1    ,q2    ,
     &                   af     ,timout ,mouqpu ,thcsum ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATQPU (SA Tha. h. Q(fresh wd),P(flood vol),U(vel))
c
c Module description: Calculation of new fresh water discharges, flood
c                     volumes and maximum velocities and the constant
c                     part in the  Thatcher-Harleman sum.
c
c                     For this tidal information is used. The quantities
c                     Q, P and U are calculated for each mouth (SATFWD
c                     and SATFVU). If SATFWD detects the start of a
c                     tidal period in a mouth, provided it is not the
c                     first one, routine SATBPA will be called to recal-
c                     culate for each branch the constant part of the
c                     Thather Harleman sum.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 af                P  -
c 10 bramrl            P  -
c  9 branch            P  -
c  6 dt                P  -
c  4 juer              P  -
c 17 ker               P  -
c 15 mouqpu            P  -
c  8 mouth             P  -
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  1 nmouth            I  Maximum number of mouths in the network.
c 11 q1                P  -
c 12 q2                P  -
c 16 thcsum            P  -
c  5 time              P  -
c 14 timout(2,nmouth)  I  Administration for the calculation of fresh
c                         water discharge, flood volume and maximum
c                         flood velocity for every mouth:
c                         (1,i) = Starting time of current tide.
c                         (2,i) = 0 : Fist tidal period not started yet.
c                                 1 : Fist tidal period has started.
c  7 tp                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c satbpa  SAlt Thatcher harleman Branch PArameters
c satfvu  SAlt Th. harl. Flood Vol. and max. vel.(U)
c satfwd  SAlt Thatcher harl. Fresh Water Discharge
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: satqpu.pf,v $
c Revision 1.4  1995/10/18  09:00:33  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/08/23  14:29:47  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.2  1995/05/30  07:06:23  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:04  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  09:17:29  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration parameters
c
      integer   nmouth  ,nbran   ,ngrid ,juer   ,ker

      integer   mouth (2,*)      ,branch(4,nbran),
     &          bramrl(nmouth+1  ,nbran)
      real      af (ngrid), timout(2,*), mouqpu(3,0:2,*),
     &          thcsum(2,nbran)
      double precision     time, dt, tp, q1(ngrid), q2(ngrid)
c
c     Declaration of local parameters
c
      integer   im
      logical   change,lslack
c
      change = .false.
      do 10 im = 1, nmouth
         call satfwd (im     ,nbran  ,ngrid ,time  ,dt     ,tp     ,
     &                mouth  ,branch ,q1    ,q2    ,lslack ,timout ,
     &                mouqpu )
c
c        At the first slack water before flood no Thatcher-
c        Harleman sum has to be calculated.
c
         if (lslack .and. int(timout(2,im)).eq.1) change = .true.
c
         call satfvu (im     ,nbran ,ngrid  ,dt    ,lslack ,mouth  ,
     &                branch ,q1    ,q2     ,af    ,timout ,mouqpu )
   10 continue
c
      if (change) then
C WRITE (11,*) 'TIME=',time do 112 k=1,nmouth
C WRITE (11,*) 'MOUTH=',k
Cdo 111 j=0,2
CWRITE (11,*) j,(mouqpu(i,j,k),i=1,3)
C111      continue
C12      continue
         call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum ,ker)
      endif
c
      end
