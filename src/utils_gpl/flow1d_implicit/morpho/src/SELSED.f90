subroutine selsed(ngrid  ,nqlat  ,nmlat  ,nbran  ,nsedrd ,maxtab ,&
&ntabm  ,time   ,dt     ,branch ,sedinf ,x      ,&
&sedtr  ,qltpar ,mltpar ,qlat   ,ntab   ,table  ,&
&slata  ,slat   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SELSED (SEdiment Lateral SEDiment)
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
! $Log: selsed.pf,v $
! Revision 1.4  1999/03/15  15:53:38  kuipe_j
! tabs removed
!
! Revision 1.3  1997/02/17  10:28:40  kuipe_j
! lateral Q in m3/s in cont equation now
!
! Revision 1.2  1995/05/30  07:07:25  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:25  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:37  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:55  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:22  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ngrid ,nqlat   ,nmlat ,nbran,nsedrd,maxtab  ,ntabm
   integer ntab  (4,maxtab),branch(4,nbran),sedinf(2,nbran)
   real    x     (ngrid)   ,sedtr (ngrid,*),slat(ngrid,*)  ,&
   &qltpar(9,*)     ,mltpar(9,*)    ,qlat(*)        ,&
   &slata (ngrid,*) ,table(ntabm)
   double  precision  time ,dt
!
!     Declaration of local variables
!
   integer mstat ,qstat ,itab  ,i   ,n  ,ibr ,igr ,igr1 ,igr2 ,isec
   real    loadst,sestat,dx
!
!     Initialize at zero values.
!
   if (nsedrd.gt.0) then
      n = 2
   else
      n = 1
   endif
!
!     Clear slat and slat'
!
   do 20 i=1,n
      do 10 igr = 1,ngrid
         slat (igr,i) = 0.
10    continue
20 continue
!
!     Add lateral sediments to source term array SLAT
!     (Time level n+1/2,Place level i+1/2).
!
   do 30 mstat = 1 , nmlat
      isec  = int(mltpar(9,mstat))
      qstat = int(mltpar(2,mstat))
      if (qstat .eq. 0) then
!
!           Get current sediment load from a user defined table.
!
!           itab       : TABLE number load=f(t)
!           time       : t(n+1/2)
!           loadst     : m2/s for diffusive load
!                        m3/s for point load
!
         itab  = int(mltpar(3,mstat))
!
         call inttab (ntab (1,itab)      ,ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time-dt*.5D0       ,loadst      )
!
!           Distribution of load over grid points.
!           (Array MLTPAR has the same structure as QLTPAR, so
!           routine CQLATG can be used).
!
         call cqlatg(ngrid  ,nmlat  ,mstat  ,loadst   ,mltpar  ,&
         &x      ,slat(1,isec)   )
      else
!
!           Get current sediment concentration from a user defined table.
!
!           itab       : TABLE number sestat=f(t)
!           time       : t(n+1/2)
!
         itab  = int(mltpar(3,mstat))
!
         call inttab (ntab (1,itab)      ,ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time-dt*.5D0       ,sestat      )
!
!           Calculate load at station.
!           qlat       : m2/s for diffusive load
!                        m3/s for point load
!           sestat     : m3/m3
!
         loadst = qlat(qstat) * sestat
!
!           Distribution of load over grid points.
!
         call cqlatg(ngrid  ,nqlat  ,qstat  ,loadst ,qltpar  ,&
         &x      ,slat(1,isec)   )
!
      endif
30 continue
!
!     If nmlat > 0 move sediment transport from i+1/2 to i and i+1
!
   if (nmlat .gt. 0) then
!
!        Copy slat to slat'
!
      do 50 i=1,n
         do 40 igr = 1,ngrid
            slata (igr,i) = slat(igr,i)
40       continue
50    continue
!
!        Now move slat to points i, i+1, ..
!
      do 80 ibr = 1, nbran
         if (sedinf(1,ibr) .gt. 0) then
            n = 2
         else
            n = 1
         endif
!
         igr1 = branch(3,ibr)
         igr2 = branch(4,ibr)
!
         do 70 i = 1, n
!
!              First point receives 1/2 from i+1/2
!
            slat(igr1,i) = .5 * slata(igr1,i)
!
            do 60 igr = igr1+1, igr2-1
               slat(igr,i) = .5*slata(igr-1,i) + .5*slata(igr,i)
60          continue
!
!              Last point receives 1/2 from n-1/2
!
            slat(igr2,i) = .5 * slata(igr2-1,i)
!
70       continue
80    continue
   endif
!
!     Add sediment exchange between sedredge channels in source term
!     SLAT (Time level n+1,Place level i, i+1).
!
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
!
end
