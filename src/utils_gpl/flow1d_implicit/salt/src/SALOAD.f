      subroutine saload(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,
     &                  dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,
     &                  qltpar ,sltpar ,qlat   ,ntab   ,table  ,source)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SALOAD (SAlt LOAD)
c
c Module description: Calculate and divide loads over the gridpoints
c                     according to the definition of outflowing lateral
c                     discharges and connected lateral discharge stati-
c                     ons.
c
c                     2 connected lateral discharge stations result in 2
c                     loads (inflowing and outflowing). These loads are
c                     defined in one cel or at one grid point. Each
c                     station can have a different definition.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 csa1(ngrid)       I  Salt concentration in every grid point at time
c                         t(n).
c 12 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c  7 dt                I  Computational time step dt [sec].
c  4 maxtab            I  Maximum number of defined tables.
c  1 ngrid             I  Number of grid points in network.
c  2 nqlat             I  Number of lateral discharge stations.
c  3 nslat             I  Number of lateral salt stations.
c 16 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  9 psi               I  Space weight factor in Preissmann scheme.
c 15 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 13 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
c                         (1,i) = Branch number.
c                         (2,i) = Type of Qlateral definition:
c                                 cqlftm (1) : Qlat = f(t)
c                                 cqlfh  (2) : Qlat = Q(h)
c                                 cqlstr (3) : Qlat from structure
c                                 cqlcon (4) : Qlat from other lateral
c                                              discharge station
c                         - For types 1 and 2 (functns of time or Q(h)):
c                         (3,i) = Table number.
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
c                         - For type 3 (structure):
c                         (3,i) = Table number of outside water level
c                                 table.
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                         (5,i) = First gridpoint of cell.
c                         (6,i) = Last gridpoint of cell.
c                         (7,i) = Structure type.
c                         (8,i) = Structure number.
c                         - For type 4 (connection point):
c                         (3,i) = Second index of qltpar which is the
c                                 connection point.
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                         (5,i) = First gridpoint of cell.
c                         (6,i) = Last gridpoint of cell.
c                         Below a drawing is given which defines the
c                         locations of the grid points and trajectory
c                         (Lb , Le):
c                            x=Lb                    x=Le
c                             |                       |
c                         -+-------+--------------+--------+-----
c                         i1  |   i1+1            i2  |   i2+1
c 14 sltpar(9,nslat)   I  Definition of lateral salt stations:
c                         (1,i) = Branch number.
c                         (2,i) = Link to corresponding lateral dischar-
c                                 ge station:
c                                 0     Dry waste load
c                                 >0    Index of corresponding station
c                                       in qltpar
c                         (3,i) = Table pointer to concentration functi-
c                                 on of time.
c                         The following variables are defined for dry
c                         waste loads:
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
c 18 source(ngrid)     O  Load due to inflow at salt stations, wast
c                         loads and connected salt stations. One value
c                         for every grid point at time t(n+1).
c 17 table             P  -
c  8 theta             I  Time weight factor in Preissmann scheme.
c  6 time              I  Actual time level tn+1. in sec.
c 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cqlatg  Calculate Q LaTeral per Grid point
c inttab  INTerpolate in TABle
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
c $Log: saload.pf,v $
c Revision 1.3  1995/05/30  09:56:12  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:09  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:51  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:16  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:51  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      !DEC$ if defined (_DLL)
      use SobekRE_OpenMI
   !DEC$ endif
c     Declaration of parameters
c
      integer ngrid ,nqlat   ,nslat ,maxtab  ,ntabm
      real    theta ,psi
      integer ntab  (4,maxtab)
      real    x     (ngrid)  ,csa1  (ngrid)  ,csa2(ngrid),source(ngrid),
     &        qltpar(9,*)    ,sltpar(9,*)    ,qlat(*)    ,table (ntabm)
      double  precision time ,dt
c
c     Declaration of local variables
c
      integer sstat ,qstat ,itab   ,staout ,stain  ,ncels  ,i  ,j  ,
     +        j1, qlatid
      real    cstat ,loadst,sourcj ,sum
c
c     External functions
c
      integer gtcpnt
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\mempool.i'
c
c     Calculation of loads at lateral discharge stations with salt load.
c     Treatment of unconnected stations with inflow.
c
      !DEC$ if defined (_DLL)
      if (OpenMIactive()) then
      qlatid = max(1,gtcpnt ('QLATNM'))
      endif
   !DEC$ endif

      do 10 sstat = 1 , nslat
         qstat = int(sltpar(2,sstat))
         if (qstat .ne. 0) then
            if (qlat(qstat) .gt. 0.) then
c
c              Inflow.
c              Get current salt concentration from a user defined table.
c
c              itab       : TABLE number cstat=f(t)
c              time-dt/2. : t(n+1/2)
c
               itab  = int(sltpar(3,sstat))
c
               call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                      table(ntab(2,itab)),
     &                      table(ntab(3,itab)),
     &                      time-dt/2d0        ,cstat       )
c
c              Calculate load at station.
c              [ Doc. S-FO-001.5KV / Eq. 22-10 ]
c              loadst     : kg/m/s for diffusive load
c                           kg/s for point load
c
      !DEC$ if defined (_DLL)
               if (OpenMIactive()) then
                  call GetLateralsSalt(cp(qlatid), qstat, nqlat, cstat)
                  slat(qstat) = cstat
               endif
      !DEC$ endif
               loadst = qlat(qstat) * cstat
c
c              Distribution of load over grid points.
c
               call cqlatg(ngrid  ,nqlat  ,qstat  ,loadst ,qltpar  ,
     &                     x      ,source )
      !DEC$ if defined (_DLL)
         elseif (OpenMIactive()) then

c              Outgoing flow, use concentration

               slat(qstat) = csa1(sltpar(5, sstat))
   !DEC$ endif
            endif
         endif
   10 continue
c
c     Calculation of loads due to connected lateral stations.
c
      do 40 qstat = 1 , nqlat
         if (int(qltpar(2,qstat)) .eq. cqlcon) then
            if (qlat(qstat) .lt. 0.) then
c              Outflow at station QSTAT.
               staout = qstat
               stain  = int(qltpar(3,qstat))
            else
c              Inflow at station QSTAT.
               stain  = qstat
               staout = int(qltpar(3,qstat))
            endif
c
c           Determine number of grid cells involved (NCELS) at outgoing
c           station:
c           ncels = 1: Point discharge in grid cell
c           ncels = 2: Point discharge in grid point
c
            ncels = 3 - int(qltpar(4,staout))
            sum   = 0.
            do 20 i = 1 , ncels
c              Calculate outgoing load in a cell.
c              In this implementation CSA2 contains the concentrations
c              at the previous step (i.e. t(n))
c              [ Doc. S-FO-001.5KV / Eq. 22-11 (sign !) ]
               j  = int(qltpar(5,staout)) + i - ncels
               j1 = j + 1
               sourcj = qlat(staout) / ((x(j1) - x(j)) * real(ncels)) *
     &                  (     theta  *     psi  * csa2(j1)
     &                  +     theta  * (1.-psi) * csa2(j )
     &                  + (1.-theta) *     psi  * csa1(j1)
     &                  + (1.-theta) * (1.-psi) * csa1(j ) )
               source(j) = sourcj
               sum       = sum + sourcj
   20       continue
c
c           Determine number of grid cells involved (NCELS) at ingoing
c           station:
c           ncels = 1: Point discharge in grid cell
c           ncels = 2: Point discharge in grid point
c
            ncels = 3 - int(qltpar(4,stain))
            do 30 i = 1 , ncels
c              Calculate incoming load in a cell.
c              Incoming load = - outgoing load of connected station.
               j = int(qltpar(5,stain)) - i + 1
               source(j) = -sum / real(ncels)
   30       continue
         endif
   40 continue
c
      end
