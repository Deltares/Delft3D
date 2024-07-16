      subroutine sazqpu (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,
     &                   ntabm  ,ngrid ,juer   ,time   ,dt     ,tp     ,
     &                   mouth  ,branch ,node  ,bramrl ,hbdpar ,ntab   ,
     &                   table  ,q1     ,q2    ,qfloq   ,emppar ,mouqpu,
     &                   thcsum ,timout ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAZQPU (SA Zwendl Q(fresh wd),P(flood vol),U(vel))
c
c Module description: Calculation of new fresh water discharges, flood
c                     volumes, maximum velocities and the constant part
c                     in the Thather-Harleman sum, for usage in the
c                     Zwendl dispersion formulation. A restriction to
c                     this formulation is that only one mouth can be
c                     defined in the first release. This mouth is used
c                     to determine changes in tide.
c
c                     The quantities Q, P and U are calculated by SAZFWD
c                     and SAZFVU. If SATZWD detects the start of a tidal
c                     period in the mouth, routine SATBPA will be called
c                     to recalculate for each branch the constant part
c                     of the Thather Harleman sum.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 bramrl            P  -
c 14 branch            P  -
c 11 dt                P  -
c 23 emppar            P  -
c 17 hbdpar            P  -
c  9 juer              P  -
c 27 ker               P  -
c  6 maxtab            I  Maximum number of defined tables.
c 24 mouqpu            P  -
c 13 mouth             P  -
c  2 nbran             I  Number of branches.
c  8 ngrid             I  Number of grid points in network.
c  4 nhstat            I  Number of H-boundary stations.
c  1 nmouth            I  Maximum number of mouths in the network.
c  3 nnode             I  Number of nodes.
c 15 node              P  -
c  5 nqfloc            P  -
c 18 ntab              P  -
c  7 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 20 q1                P  -
c 21 q2                P  -
c 22 qfloq             P  -
c 19 table             P  -
c 25 thcsum            P  -
c 10 time              P  -
c 26 timout            P  -
c 12 tp                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c satbpa  SAlt Thatcher harleman Branch PArameters
c sazfvu  SAlt Zwendl Flood Vol. and max. vel.(U)
c sazfwd  SAlt Zwendl Fresh Water Discharge
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
c $Log: sazqpu.pf,v $
c Revision 1.2  1995/05/30  07:06:29  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:09  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  09:17:35  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer  nmouth  ,nbran ,nnode  ,nhstat  ,nqfloc  ,maxtab,
     &         ntabm   ,ngrid ,juer   ,ker
      integer  mouth (2,*)            ,branch(4,nbran)  ,node(4,nnode) ,
     &         bramrl(nmouth+1,nbran) ,hbdpar(3,nhstat) ,ntab(4,maxtab)
      real     table (ntabm)    ,qfloq (2,*)      ,
     &         timout(2,*)      ,emppar(4,*)      ,mouqpu(3,0:2,*)     ,
     &         thcsum(2,nbran)
      double precision   time  ,dt       ,tp
      double precision   q1(ngrid), q2(ngrid)

c
c     Declaration of local parameters
c
      integer   im
      logical   change,lslack
c
      change = .false.
      do 10 im = 1,nmouth
         call sazfwd (im     ,nbran  ,nqfloc ,ngrid  ,time  ,dt     ,
     &                tp     ,mouth  ,branch ,qfloq  ,q1    ,q2     ,
     &                lslack ,timout ,mouqpu )
c
c        At the first slack water before flood the flood volume and
c        the maximum flood velocity are determined. So the
c        Thatcher-Harleman sum has to be recalculated.
c
         if (lslack) change = .true.
c
         call sazfvu (im     ,nnode  ,nhstat ,maxtab ,ntabm ,time   ,
     &                dt     ,tp     ,lslack ,mouth  ,node  ,hbdpar ,
     &                ntab   ,table  ,emppar ,mouqpu )
   10 continue
c
      if (change) then
         call satbpa (nmouth ,nbran ,juer ,bramrl ,mouqpu ,thcsum, ker)
      endif
c
      end
