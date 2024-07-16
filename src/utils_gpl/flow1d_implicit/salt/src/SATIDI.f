      subroutine satidi (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,
     &                   ntabm  ,ngrid ,dsopt  ,juer   ,time   ,dt     ,
     &                   tp     ,mouth  ,branch,node   ,bramrl ,hbdpar ,
     &                   ntab   ,table  ,grid  ,x      ,q1     ,q2     ,
     &                   af     ,csa1   ,dcdx   ,cdcdx0,cdcdx1 ,cdcdx2 ,
     &                   thasca ,qfloq  ,emppar ,mouqpu,thcsum ,timout ,
     &                   ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATIDI (SAlt calculate TIDal Information)
c
c Module description: Calculate information for Thather Harleman or
c                     Zwendl formulation that is constant over a tidal
c                     period.
c
c                     Both methods use different ways to calculate the
c                     fresh water discharge, the flood volume and the
c                     maximum flood velocity.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 25 af                P  -
c 17 bramrl            P  -
c 15 branch            P  -
c 28 cdcdx0            P  -
c 29 cdcdx1            P  -
c 30 cdcdx2            P  -
c 26 csa1              P  -
c 27 dcdx              P  -
c  9 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c 12 dt                P  -
c 33 emppar            P  -
c 21 grid              P  -
c 18 hbdpar            P  -
c 10 juer              P  -
c 37 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  6 maxtab            I  Maximum number of defined tables.
c 34 mouqpu            P  -
c 14 mouth             P  -
c  2 nbran             I  Number of branches.
c  8 ngrid             I  Number of grid points in network.
c  4 nhstat            I  Number of H-boundary stations.
c  1 nmouth            I  Maximum number of mouths in the network.
c  3 nnode             I  Number of nodes.
c 16 node              P  -
c  5 nqfloc            P  -
c 19 ntab              P  -
c  7 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 23 q1                P  -
c 24 q2                P  -
c 32 qfloq             P  -
c 20 table             P  -
c 31 thasca            P  -
c 35 thcsum            P  -
c 11 time              P  -
c 36 timout            P  -
c 13 tp                P  -
c 22 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c saccda  SAlt calculation of C/C0*Dc/dx Averaged
c satqpu  SA Tha. h. Q(fresh wd),P(flood vol),U(vel)
c sazqpu  SA Zwendl Q(fresh wd),P(flood vol),U(vel)
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
c $Log: satidi.pf,v $
c Revision 1.5  1995/10/18  09:00:32  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/30  12:37:21  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:18  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:21  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:02  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:27  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:34:12  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer  nmouth  ,nbran ,nnode  ,nhstat  ,nqfloc  ,maxtab,
     &         ntabm   ,ngrid ,dsopt  ,juer    ,ker
      integer  mouth (2,*)    ,branch(4,nbran) ,node(4,nnode),
     &         bramrl(nmouth+1,nbran) ,hbdpar(3,nhstat) ,ntab(4,maxtab),
     &         grid  (ngrid)
      real     table (ntabm)    ,x     (ngrid)    ,
     &         af    (ngrid)    ,csa1  (ngrid)    ,dcdx  (ngrid)      ,
     &         cdcdx0(ngrid)    ,cdcdx1(ngrid)    ,cdcdx2(ngrid)      ,
     &         thasca(3)        ,qfloq (2,*)      ,
     &         emppar(4,*)      ,mouqpu(3,0:2,*)  ,
     &         thcsum(2,nbran)  ,timout(2,*)
      double   precision   
     &         time  ,dt ,tp, q1(ngrid),q2(ngrid)
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
      call saccda (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt     ,
     &             tp     ,mouth  ,branch ,bramrl ,q1     ,q2     ,
     &             grid   ,csa1   ,x      ,dcdx   ,cdcdx0 ,cdcdx1 ,
     &             cdcdx2 ,thasca ,ker    )
c
      if (ker.lt.fatal) then
         if      (dsopt .eq. cdsthh) then
c
c           Thatcher Harleman formulation
c
            call satqpu (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt    ,
     &                   tp     ,mouth  ,branch ,bramrl ,q1     ,q2    ,
     &                   af     ,timout ,mouqpu ,thcsum ,ker    )
         else if (dsopt .eq. cdsemp) then
c
c           Empirical formulation
c
            call sazqpu (nmouth ,nbran ,nnode  ,nhstat ,nqfloc ,maxtab ,
     &                   ntabm  ,ngrid ,juer   ,time   ,dt     ,tp     ,
     &                   mouth  ,branch ,node  ,bramrl ,hbdpar ,ntab   ,
     &                   table  ,q1     ,q2    ,qfloq  ,emppar ,mouqpu ,
     &                   thcsum ,timout ,ker   )
         endif
      endif
c
      end
