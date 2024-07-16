      subroutine selsed(ngrid  ,nqlat  ,nmlat  ,nbran  ,nsedrd ,maxtab ,
     &                  ntabm  ,time   ,dt     ,branch ,sedinf ,x      ,
     &                  sedtr  ,qltpar ,mltpar ,qlat   ,ntab   ,table  ,
     &                  slata  ,slat   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SELSED (SEdiment Lateral SEDiment)
c
c Module description: Calculate the amount of lateral sediment addition
c                     and/or withdrawal.
c
c                     A lateral addition or withdrawal can be defined
c                     as:
c
c                     1)  A sediment concentration as a function of time
c                         in a lateral discharge (Qlat)
c                     2)  An amount as a function of time (load).
c
c                     Sediment can be "pumped"  between 2 locations.
c                     Both locations must be defined as lateral sediment
c                     stations (having defined functions in that case).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  9 dt                I  Computational time step dt [sec].
c  6 maxtab            I  Maximum number of defined tables.
c 15 mltpar(9,nslat)   I  Definition of lateral morpho stations:
c                         (1,i) = Branch number.
c                         (2,i) = Link to corresponding lateral dischar-
c                                 ge station:
c                                 0     A load value.
c                                 >0    Index of corresponding station
c                                       in qltpar.
c                         (3,i) = Table pointer to concentration functi-
c                                 on of time.
c                         The following variables are defined for the
c                         loads:
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                                 ctd1gc (3) : Traject discharge 1 cell
c                                 ctdmgc (4) : Traject discharge over
c                                              more grid cells
c                         (5,i) = First gridpoint of cell/trajectory
c                                 (types 1,2,3,4).
c                         (6,i) = Last gridpoint of cell/trajectory
c                                 (types 2,3,4).
c                         (7,i) = Lb coordinate for cell/trajectory
c                                 (types 3,4).
c                         (8,i) = Le coordinate for cell/trajectory
c                                 (types 3,4).
c                         (9,i) = Section number that is connected to
c                                 the station:
c                                 Sedredge branch:
c                                       1 = Left channel
c                                       2 = Right channel
c                                 Normal branch:
c                                       1 = Main section
c  4 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  3 nmlat             I  Number of lateral morpho stations.
c  2 nqlat             P  -
c  5 nsedrd            I  Number of defined sedredge branches.
c 17 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  7 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 16 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 14 qltpar            P  -
c 11 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c 13 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 20 slat(ngrid,*)     IO Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c 19 slata             IO -
c 18 table             P  -
c  8 time              I  Actual time level tn+1. in sec.
c 12 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cqlatg  Calculate Q LaTeral per Grid point
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: selsed.pf,v $
c Revision 1.4  1999/03/15  15:53:38  kuipe_j
c tabs removed
c
c Revision 1.3  1997/02/17  10:28:40  kuipe_j
c lateral Q in m3/s in cont equation now
c
c Revision 1.2  1995/05/30  07:07:25  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:25  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:37  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:55  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ngrid ,nqlat   ,nmlat ,nbran,nsedrd,maxtab  ,ntabm
      integer ntab  (4,maxtab),branch(4,nbran),sedinf(2,nbran)
      real    x     (ngrid)   ,sedtr (ngrid,*),slat(ngrid,*)  ,
     &        qltpar(9,*)     ,mltpar(9,*)    ,qlat(*)        ,
     &        slata (ngrid,*) ,table(ntabm)
      double  precision  time ,dt
c
c     Declaration of local variables
c
      integer mstat ,qstat ,itab  ,i   ,n  ,ibr ,igr ,igr1 ,igr2 ,isec
      real    loadst,sestat,dx
c
c     Initialize at zero values.
c
      if (nsedrd.gt.0) then
         n = 2
      else
         n = 1
      endif
c
c     Clear slat and slat'
c
      do 20 i=1,n
         do 10 igr = 1,ngrid
            slat (igr,i) = 0.
   10    continue
   20 continue
c
c     Add lateral sediments to source term array SLAT
c     (Time level n+1/2,Place level i+1/2).
c
      do 30 mstat = 1 , nmlat
         isec  = int(mltpar(9,mstat))
         qstat = int(mltpar(2,mstat))
         if (qstat .eq. 0) then
c
c           Get current sediment load from a user defined table.
c
c           itab       : TABLE number load=f(t)
c           time       : t(n+1/2)
c           loadst     : m2/s for diffusive load
c                        m3/s for point load
c
            itab  = int(mltpar(3,mstat))
c
            call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                   table(ntab(2,itab)),
     &                   table(ntab(3,itab)),
     &                   time-dt*.5D0       ,loadst      )
c
c           Distribution of load over grid points.
c           (Array MLTPAR has the same structure as QLTPAR, so
c           routine CQLATG can be used).
c
            call cqlatg(ngrid  ,nmlat  ,mstat  ,loadst   ,mltpar  ,
     &                  x      ,slat(1,isec)   )
         else
c
c           Get current sediment concentration from a user defined table.
c
c           itab       : TABLE number sestat=f(t)
c           time       : t(n+1/2)
c
            itab  = int(mltpar(3,mstat))
c
            call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                   table(ntab(2,itab)),
     &                   table(ntab(3,itab)),
     &                   time-dt*.5D0       ,sestat      )
c
c           Calculate load at station.
c           qlat       : m2/s for diffusive load
c                        m3/s for point load
c           sestat     : m3/m3
c
            loadst = qlat(qstat) * sestat
c
c           Distribution of load over grid points.
c
            call cqlatg(ngrid  ,nqlat  ,qstat  ,loadst ,qltpar  ,
     &                  x      ,slat(1,isec)   )
c
         endif
   30 continue
c
c     If nmlat > 0 move sediment transport from i+1/2 to i and i+1
c
      if (nmlat .gt. 0) then
c
c        Copy slat to slat'
c
         do 50 i=1,n
            do 40 igr = 1,ngrid
               slata (igr,i) = slat(igr,i)
   40       continue
   50    continue
c
c        Now move slat to points i, i+1, ..
c
         do 80 ibr = 1, nbran
            if (sedinf(1,ibr) .gt. 0) then
               n = 2
            else
               n = 1
            endif
c
            igr1 = branch(3,ibr)
            igr2 = branch(4,ibr)
c
            do 70 i = 1, n
c
c              First point receives 1/2 from i+1/2
c
               slat(igr1,i) = .5 * slata(igr1,i)
c
               do 60 igr = igr1+1, igr2-1
                  slat(igr,i) = .5*slata(igr-1,i) + .5*slata(igr,i)
   60          continue
c
c              Last point receives 1/2 from n-1/2
c
               slat(igr2,i) = .5 * slata(igr2-1,i)
c
   70       continue
   80    continue
      endif
c
c     Add sediment exchange between sedredge channels in source term
c     SLAT (Time level n+1,Place level i, i+1).
c
      do 100 ibr = 1,nbran
         if (sedinf(1,ibr) .gt. 0) then
            igr1 = branch(3,ibr)
            igr2 = branch(4,ibr)
            do 90 igr = igr1+1,igr2-1
               dx          = (x(igr+1)-x(igr-1))*0.5
               slat(igr,1) = slat(igr,1) + sedtr(igr,3)*dx
               slat(igr,2) = slat(igr,2) - sedtr(igr,3)*dx
   90       continue
            dx          = (x(igr2)-x(igr2-1))*0.5
            slat(igr2,1) = slat(igr2,1) + sedtr(igr2,3)*dx
            slat(igr2,2) = slat(igr2,2) - sedtr(igr2,3)*dx
            dx          = (x(igr1+1)-x(igr1))*0.5
            slat(igr1,1) = slat(igr1,1) + sedtr(igr1,3)*dx
            slat(igr1,2) = slat(igr1,2) - sedtr(igr1,3)*dx
         endif
  100 continue
c
      end
