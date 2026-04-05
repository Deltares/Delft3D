subroutine gslsed(ngrid  ,nqlat  ,nmlat  ,nbran  ,nfrac  ,maxtab ,&
&ntabm  ,time   ,dt     ,branch ,x      ,&
&qltpar ,mltpar ,qlat   ,ntab   ,table  ,&
&source ,dfrac  ,sedtr  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSLSED (Graded Sediment Lateral SEDiment)
!
! Module description: Calculate the amount of lateral sediment addition
!                     and/or withdrawal.
!
!                     A lateral addition or withdrawal can be defined
!                     as:
!
!                     1)  A sediment concentration as a function of time
!                         in a lateral discharge (Qlat)
!                     2)  An amount as a function of time (load).
!
!                     Sediment can be "pumped"  between 2 locations.
!                     Both locations must be defined as lateral sediment
!                     stations (having defined functions in that case).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  9 dt                I  Computational time step dt [sec].
!  6 maxtab            I  Maximum number of defined tables.
! 15 mltpar(9,nslat)   I  Definition of lateral morpho stations:
!                         (1,i) = Branch number.
!                         (2,i) = Link to corresponding lateral dischar-
!                                 ge station:
!                                 0     A load value.
!                                 >0    Index of corresponding station
!                                       in qltpar.
!                         (3,i) = Table pointer to concentration functi-
!                                 on of time.
!                         The following variables are defined for the
!                         loads:
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                                 ctd1gc (3) : Traject discharge 1 cell
!                                 ctdmgc (4) : Traject discharge over
!                                              more grid cells
!                         (5,i) = First gridpoint of cell/trajectory
!                                 (types 1,2,3,4).
!                         (6,i) = Last gridpoint of cell/trajectory
!                                 (types 2,3,4).
!                         (7,i) = Lb coordinate for cell/trajectory
!                                 (types 3,4).
!                         (8,i) = Le coordinate for cell/trajectory
!                                 (types 3,4).
!                         (9,i) = Section number that is connected to
!                                 the station:
!                                 Sedredge branch:
!                                       1 = Left channel
!                                       2 = Right channel
!                                 Normal branch:
!                                       1 = Main section
!  4 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  3 nmlat             I  Number of lateral morpho stations.
!  2 nqlat             P  -
!  5 nsedrd            I  Number of defined sedredge branches.
! 17 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  7 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 16 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 14 qltpar            P  -
! 11 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
! 13 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 20 slat(ngrid,*)     IO Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 19 slata             IO -
! 18 table             P  -
!  8 time              I  Actual time level tn+1. in sec.
! 12 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cqlatg  Calculate Q LaTeral per Grid point
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gslsed.F,v $
! Revision 1.4  1996/01/08  13:29:56  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.3  1996/01/05  15:43:33  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:12:35  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Declaration of parameters
!

   integer ngrid ,nqlat   ,nmlat ,nbran,nfrac ,maxtab  ,ntabm
   integer ntab  (4,maxtab),branch(4,nbran)
   real    x     (ngrid)   ,&
   &qltpar(9,*)     ,mltpar(9,*)    ,qlat(*)        ,&
   &source(ngrid,nfrac+2) ,table(ntabm),dfrac(nfrac),&
   &sedtr (ngrid,nfrac+2)
   double  precision    time  ,dt
!
!     Declaration of local variables
!
   integer mstat ,qstat ,itab  ,i   ,ibr ,igr ,igr1 ,igr2
   integer type  ,iopt  ,i1    ,i2  ,mstat2 ,j    ,itabtb
   real    loadst,sestat
   real    lbt  , let  , percen
!
!     Include sobek error code file
!
   include '..\include\sobcon.i'
!
!     Initialize at zero values.
!

   do 20 j=1,nfrac+2
      do 10 i=1,ngrid
         source (i,j) = 0.
10    continue
20 continue

!
!     Add lateral sediments to source term array SOURCE
!     (Time level n+1/2,Place level i+1/2).
!     First the lateral sediments in the stations are stored in a
!     workarray source(mstat,j+2). Afterwards the Slat's are distributed
!     over the grid points.
!
   do 30 mstat = 1 , nmlat
!
!        iopt = 1 : Slat defined as time series
!        iopt = 2 : Slat follows from S-local-table
!        iopt = 3 : Not implemented
!        iopt = 4 : Slat follows from lateral sediment discharge in
!                   2nd lateral discharge station

      iopt  = int(mltpar(9,mstat))
      qstat = int(mltpar(2,mstat))

      if (iopt .eq. 1) then

         if (qstat .eq. 0) then

!
!            Get current sediment load from a user defined table.
!
!            itab       : TABLE number load=f(t)
!            time       : t(n+1/2)
!            loadst     : m2/s for diffusive load
!                         m3/s for point load
!
            itabtb  = int(mltpar(3,mstat))
!
            do 24 j = 1 , nfrac
               itab = int(table(ntab(3,itabtb) + j - 1))
               call inttab (ntab (1,itab)      ,ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time-dt*.5D0       ,loadst      )
!
!            Distribution of load over grid points.
!            (Array MLTPAR has the same structure as QLTPAR, so
!            routine CQLATG can be used).
!
               source(mstat,j+2) = loadst
24          continue
         else
!
!           Get current sediment concentration from a user defined table
!
!            itab       : TABLE number sestat=f(t)
!            time       : t(n+1/2)
!
            itabtb  = int(mltpar(3,mstat))
!
            do 25 j = 1 , nfrac
               itab = int(table(ntab(3,itabtb) + j - 1))
               call inttab (ntab (1,itab)      ,ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &time-dt*.5D0       ,sestat      )
!
!            Calculate load at station.
!            qlat       : m2/s for diffusive load
!                         m3/s for point load
!            sestat     : m3/m3
!
               loadst = qlat(qstat) * sestat
!
!            Distribution of load over grid points.
!
               source(mstat,j+2) = loadst
!
25          continue
         endif

      else if (iopt .eq. 2) then

         itabtb = int(mltpar(3,mstat))
         type   = int(mltpar(4,mstat))
         if (type .eq. cpd1gp) then
            igr = int(mltpar(5,mstat))
            do 26 j=1,nfrac
               source(mstat,j+2) = sedtr(igr,j)
26          continue
         else if (type .eq. cpd1gc) then
            igr = int(mltpar(5,mstat))
            do 27 j=1,nfrac
               source(mstat,j+2) = (sedtr(igr,j)+sedtr(igr+1,j))/2.
27          continue
         else if (type .eq. ctd1gc .or. type .eq. ctdmgc) then
            i1  = int(mltpar(5,mstat))
            i2  = int(mltpar(6,mstat))
            lbt = mltpar(7,mstat)
            let = mltpar(8,mstat)
            do 28 j=1,nfrac
               call FLHAVG(ngrid,x,sedtr(1,j),sedtr(1,j),lbt,let,&
               &i1,i2,source(mstat,j+2),0.5)
28          continue
         endif
         do 29 j=1,nfrac
            itab = int(table(ntab(3,itabtb)+j-1))
            call inttab (ntab (1,itab)      ,ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &dble(source(mstat,j+2)) ,percen )
            source(mstat,j+2) = percen*source(mstat,j+2)
29       continue
      endif
30 continue

!     rondpompen:
   do 35 mstat = 1,nmlat
      iopt = int(mltpar(9,mstat))
      if (iopt .eq. 4) then
!
!           **********************************************
!           * Slat from other sediment discharge station *
!           **********************************************
!
!           mstat2       = number of 2nd Slat station
!           slat(mstat2) = Slat at 2nd station
!
         mstat2 = int(mltpar(3,mstat))
         do 34 j=1,nfrac
            source(mstat,j+2) = -source(mstat2,j+2)
34       continue
      endif
35 continue
!
!           *****************************************
!           * Distribution of Slat over grid points *
!           *****************************************
!
   do 37 j=1,nfrac
      do 36 mstat = 1,nmlat
         qstat = int(mltpar(2,mstat))
         if (qstat .eq. 0) then
!     Sediment load from a user defined table
            call cqlatg(ngrid,nmlat,mstat,source(mstat,j+2),mltpar,&
            &x,source(1,j))
         else
!     Sediment concentration from a user defined table related to Qlat
            call cqlatg(ngrid,nqlat,qstat,source(mstat,j+2),qltpar,&
            &x,source(1,j))
         endif
         source(mstat,j+2) = 0.
36    continue
37 continue
!
!     Calculate source(i,nfrac+1) and source(i,nfrac+2)
!
   do 100 ibr = 1,nbran
      igr1 = branch(3,ibr)
      igr2 = branch(4,ibr)
      do 90 igr = igr1,igr2
         source(igr,nfrac+1) = 0.
         source(igr,nfrac+2) = 0.
         do 80 j = 1 , nfrac
            source(igr,nfrac+1) = source(igr,nfrac+1) + source(igr,j)
            source(igr,nfrac+2) = source(igr,nfrac+2) + source(igr,j)&
            &* dfrac(j)
80       continue
90    continue
100 continue
!
end
