      subroutine gslsed(ngrid  ,nqlat  ,nmlat  ,nbran  ,nfrac  ,maxtab ,
     &                  ntabm  ,time   ,dt     ,branch ,x      ,
     &                  qltpar ,mltpar ,qlat   ,ntab   ,table  ,
     &                  source ,dfrac  ,sedtr  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSLSED (Graded Sediment Lateral SEDiment)
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
c $Log: gslsed.F,v $
c Revision 1.4  1996/01/08  13:29:56  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.3  1996/01/05  15:43:33  kuipe_j
c Lateral sediment and structures
c
c Revision 1.2  1995/09/27  10:12:35  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Declaration of parameters
c

      integer ngrid ,nqlat   ,nmlat ,nbran,nfrac ,maxtab  ,ntabm
      integer ntab  (4,maxtab),branch(4,nbran)
      real    x     (ngrid)   ,
     &        qltpar(9,*)     ,mltpar(9,*)    ,qlat(*)        ,
     &        source(ngrid,nfrac+2) ,table(ntabm),dfrac(nfrac),
     &        sedtr (ngrid,nfrac+2)
      double  precision    time  ,dt
c
c     Declaration of local variables
c
      integer mstat ,qstat ,itab  ,i   ,ibr ,igr ,igr1 ,igr2
      integer type  ,iopt  ,i1    ,i2  ,mstat2 ,j    ,itabtb
      real    loadst,sestat
      real    lbt  , let  , percen
c
c     Include sobek error code file
c
      include '..\include\sobcon.i'
c
c     Initialize at zero values.
c

      do 20 j=1,nfrac+2
         do 10 i=1,ngrid
            source (i,j) = 0.
   10    continue
   20 continue

c
c     Add lateral sediments to source term array SOURCE
c     (Time level n+1/2,Place level i+1/2).
c     First the lateral sediments in the stations are stored in a
c     workarray source(mstat,j+2). Afterwards the Slat's are distributed
c     over the grid points.
c
      do 30 mstat = 1 , nmlat
c
c        iopt = 1 : Slat defined as time series
c        iopt = 2 : Slat follows from S-local-table
c        iopt = 3 : Not implemented
c        iopt = 4 : Slat follows from lateral sediment discharge in
c                   2nd lateral discharge station

         iopt  = int(mltpar(9,mstat))
         qstat = int(mltpar(2,mstat))

         if (iopt .eq. 1) then

           if (qstat .eq. 0) then

c
c            Get current sediment load from a user defined table.
c
c            itab       : TABLE number load=f(t)
c            time       : t(n+1/2)
c            loadst     : m2/s for diffusive load
c                         m3/s for point load
c
             itabtb  = int(mltpar(3,mstat))
c
             do 24 j = 1 , nfrac
               itab = int(table(ntab(3,itabtb) + j - 1))
               call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                      table(ntab(2,itab)),
     &                      table(ntab(3,itab)),
     &                      time-dt*.5D0       ,loadst      )
c
c            Distribution of load over grid points.
c            (Array MLTPAR has the same structure as QLTPAR, so
c            routine CQLATG can be used).
c
               source(mstat,j+2) = loadst
   24        continue
           else
c
c           Get current sediment concentration from a user defined table
c
c            itab       : TABLE number sestat=f(t)
c            time       : t(n+1/2)
c
             itabtb  = int(mltpar(3,mstat))
c
             do 25 j = 1 , nfrac
               itab = int(table(ntab(3,itabtb) + j - 1))
               call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                      table(ntab(2,itab)),
     &                      table(ntab(3,itab)),
     &                      time-dt*.5D0       ,sestat      )
c
c            Calculate load at station.
c            qlat       : m2/s for diffusive load
c                         m3/s for point load
c            sestat     : m3/m3
c
               loadst = qlat(qstat) * sestat
c
c            Distribution of load over grid points.
c
               source(mstat,j+2) = loadst
c
   25        continue
           endif

         else if (iopt .eq. 2) then

           itabtb = int(mltpar(3,mstat))
           type   = int(mltpar(4,mstat))
           if (type .eq. cpd1gp) then
             igr = int(mltpar(5,mstat))
             do 26 j=1,nfrac
               source(mstat,j+2) = sedtr(igr,j)
   26        continue
           else if (type .eq. cpd1gc) then
             igr = int(mltpar(5,mstat))
             do 27 j=1,nfrac
               source(mstat,j+2) = (sedtr(igr,j)+sedtr(igr+1,j))/2.
   27        continue
           else if (type .eq. ctd1gc .or. type .eq. ctdmgc) then
             i1  = int(mltpar(5,mstat))
             i2  = int(mltpar(6,mstat))
             lbt = mltpar(7,mstat)
             let = mltpar(8,mstat)
             do 28 j=1,nfrac
               call FLHAVG(ngrid,x,sedtr(1,j),sedtr(1,j),lbt,let,
     &                     i1,i2,source(mstat,j+2),0.5)
   28        continue
           endif
           do 29 j=1,nfrac
             itab = int(table(ntab(3,itabtb)+j-1))
             call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                    table(ntab(2,itab)),
     &                    table(ntab(3,itab)),
     &                    dble(source(mstat,j+2)) ,percen )
             source(mstat,j+2) = percen*source(mstat,j+2)
   29      continue
         endif
   30 continue

c     rondpompen:
      do 35 mstat = 1,nmlat
         iopt = int(mltpar(9,mstat))
         if (iopt .eq. 4) then
c
c           **********************************************
c           * Slat from other sediment discharge station *
c           **********************************************
c
c           mstat2       = number of 2nd Slat station
c           slat(mstat2) = Slat at 2nd station
c
            mstat2 = int(mltpar(3,mstat))
            do 34 j=1,nfrac
               source(mstat,j+2) = -source(mstat2,j+2)
   34       continue
         endif
   35 continue
c
c           *****************************************
c           * Distribution of Slat over grid points *
c           *****************************************
c
      do 37 j=1,nfrac
         do 36 mstat = 1,nmlat
            qstat = int(mltpar(2,mstat))
            if (qstat .eq. 0) then
c     Sediment load from a user defined table
                call cqlatg(ngrid,nmlat,mstat,source(mstat,j+2),mltpar,
     &                      x,source(1,j))
            else
c     Sediment concentration from a user defined table related to Qlat
                call cqlatg(ngrid,nqlat,qstat,source(mstat,j+2),qltpar,
     &                      x,source(1,j))
            endif
            source(mstat,j+2) = 0.
   36    continue
   37 continue
c
c     Calculate source(i,nfrac+1) and source(i,nfrac+2)
c
      do 100 ibr = 1,nbran
         igr1 = branch(3,ibr)
         igr2 = branch(4,ibr)
         do 90 igr = igr1,igr2
            source(igr,nfrac+1) = 0.
            source(igr,nfrac+2) = 0.
            do 80 j = 1 , nfrac
               source(igr,nfrac+1) = source(igr,nfrac+1) + source(igr,j)
               source(igr,nfrac+2) = source(igr,nfrac+2) + source(igr,j)
     &                               * dfrac(j)
   80       continue
   90    continue
  100 continue
c
      end
