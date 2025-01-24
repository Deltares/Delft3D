subroutine saload(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,&
&dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,&
&qltpar ,sltpar ,qlat   ,ntab   ,table  ,source)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SALOAD (SAlt LOAD)
!
! Module description: Calculate and divide loads over the gridpoints
!                     according to the definition of outflowing lateral
!                     discharges and connected lateral discharge stati-
!                     ons.
!
!                     2 connected lateral discharge stations result in 2
!                     loads (inflowing and outflowing). These loads are
!                     defined in one cel or at one grid point. Each
!                     station can have a different definition.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
! 12 csa2(ngrid)       I  Salt concentration in every grid point at time
!                         t(n+1).
!  7 dt                I  Computational time step dt [sec].
!  4 maxtab            I  Maximum number of defined tables.
!  1 ngrid             I  Number of grid points in network.
!  2 nqlat             I  Number of lateral discharge stations.
!  3 nslat             I  Number of lateral salt stations.
! 16 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  9 psi               I  Space weight factor in Preissmann scheme.
! 15 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 13 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
!                         (1,i) = Branch number.
!                         (2,i) = Type of Qlateral definition:
!                                 cqlftm (1) : Qlat = f(t)
!                                 cqlfh  (2) : Qlat = Q(h)
!                                 cqlstr (3) : Qlat from structure
!                                 cqlcon (4) : Qlat from other lateral
!                                              discharge station
!                         - For types 1 and 2 (functns of time or Q(h)):
!                         (3,i) = Table number.
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
!                         - For type 3 (structure):
!                         (3,i) = Table number of outside water level
!                                 table.
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                         (5,i) = First gridpoint of cell.
!                         (6,i) = Last gridpoint of cell.
!                         (7,i) = Structure type.
!                         (8,i) = Structure number.
!                         - For type 4 (connection point):
!                         (3,i) = Second index of qltpar which is the
!                                 connection point.
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                         (5,i) = First gridpoint of cell.
!                         (6,i) = Last gridpoint of cell.
!                         Below a drawing is given which defines the
!                         locations of the grid points and trajectory
!                         (Lb , Le):
!                            x=Lb                    x=Le
!                             |                       |
!                         -+-------+--------------+--------+-----
!                         i1  |   i1+1            i2  |   i2+1
! 14 sltpar(9,nslat)   I  Definition of lateral salt stations:
!                         (1,i) = Branch number.
!                         (2,i) = Link to corresponding lateral dischar-
!                                 ge station:
!                                 0     Dry waste load
!                                 >0    Index of corresponding station
!                                       in qltpar
!                         (3,i) = Table pointer to concentration functi-
!                                 on of time.
!                         The following variables are defined for dry
!                         waste loads:
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
! 18 source(ngrid)     O  Load due to inflow at salt stations, wast
!                         loads and connected salt stations. One value
!                         for every grid point at time t(n+1).
! 17 table             P  -
!  8 theta             I  Time weight factor in Preissmann scheme.
!  6 time              I  Actual time level tn+1. in sec.
! 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cqlatg  Calculate Q LaTeral per Grid point
! inttab  INTerpolate in TABle
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
! $Log: saload.pf,v $
! Revision 1.3  1995/05/30  09:56:12  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:09  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:51  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:16  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:51  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   !DEC$ if defined (_DLL)
   use SobekRE_OpenMI
   !DEC$ endif
!     Declaration of parameters
!
   integer ngrid ,nqlat   ,nslat ,maxtab  ,ntabm
   real    theta ,psi
   integer ntab  (4,maxtab)
   real    x     (ngrid)  ,csa1  (ngrid)  ,csa2(ngrid),source(ngrid),&
   &qltpar(9,*)    ,sltpar(9,*)    ,qlat(*)    ,table (ntabm)
   double  precision time ,dt
!
!     Declaration of local variables
!
   integer sstat ,qstat ,itab   ,staout ,stain  ,ncels  ,i  ,j  ,&
   &j1, qlatid
   real    cstat ,loadst,sourcj ,sum
!
!     External functions
!
   integer gtcpnt
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\mempool.i'
!
!     Calculation of loads at lateral discharge stations with salt load.
!     Treatment of unconnected stations with inflow.
!
   !DEC$ if defined (_DLL)
   if (OpenMIactive()) then
      qlatid = max(1,gtcpnt ('QLATNM'))
   endif
   !DEC$ endif

   do 10 sstat = 1 , nslat
      qstat = int(sltpar(2,sstat))
      if (qstat .ne. 0) then
         if (qlat(qstat) .gt. 0.) then
!
!              Inflow.
!              Get current salt concentration from a user defined table.
!
!              itab       : TABLE number cstat=f(t)
!              time-dt/2. : t(n+1/2)
!
            itab  = int(sltpar(3,sstat))
!
            call inttab (ntab (1,itab)      ,ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &time-dt/2d0        ,cstat       )
!
!              Calculate load at station.
!              [ Doc. S-FO-001.5KV / Eq. 22-10 ]
!              loadst     : kg/m/s for diffusive load
!                           kg/s for point load
!
            !DEC$ if defined (_DLL)
            if (OpenMIactive()) then
               call GetLateralsSalt(cp(qlatid), qstat, nqlat, cstat)
               slat(qstat) = cstat
            endif
            !DEC$ endif
            loadst = qlat(qstat) * cstat
!
!              Distribution of load over grid points.
!
            call cqlatg(ngrid  ,nqlat  ,qstat  ,loadst ,qltpar  ,&
            &x      ,source )
            !DEC$ if defined (_DLL)
         elseif (OpenMIactive()) then

!              Outgoing flow, use concentration

            slat(qstat) = csa1(sltpar(5, sstat))
            !DEC$ endif
         endif
      endif
10 continue
!
!     Calculation of loads due to connected lateral stations.
!
   do 40 qstat = 1 , nqlat
      if (int(qltpar(2,qstat)) .eq. cqlcon) then
         if (qlat(qstat) .lt. 0.) then
!              Outflow at station QSTAT.
            staout = qstat
            stain  = int(qltpar(3,qstat))
         else
!              Inflow at station QSTAT.
            stain  = qstat
            staout = int(qltpar(3,qstat))
         endif
!
!           Determine number of grid cells involved (NCELS) at outgoing
!           station:
!           ncels = 1: Point discharge in grid cell
!           ncels = 2: Point discharge in grid point
!
         ncels = 3 - int(qltpar(4,staout))
         sum   = 0.
         do 20 i = 1 , ncels
!              Calculate outgoing load in a cell.
!              In this implementation CSA2 contains the concentrations
!              at the previous step (i.e. t(n))
!              [ Doc. S-FO-001.5KV / Eq. 22-11 (sign !) ]
            j  = int(qltpar(5,staout)) + i - ncels
            j1 = j + 1
            sourcj = qlat(staout) / ((x(j1) - x(j)) * real(ncels)) *&
            &(     theta  *     psi  * csa2(j1)&
            &+     theta  * (1.-psi) * csa2(j )&
            &+ (1.-theta) *     psi  * csa1(j1)&
            &+ (1.-theta) * (1.-psi) * csa1(j ) )
            source(j) = sourcj
            sum       = sum + sourcj
20       continue
!
!           Determine number of grid cells involved (NCELS) at ingoing
!           station:
!           ncels = 1: Point discharge in grid cell
!           ncels = 2: Point discharge in grid point
!
         ncels = 3 - int(qltpar(4,stain))
         do 30 i = 1 , ncels
!              Calculate incoming load in a cell.
!              Incoming load = - outgoing load of connected station.
            j = int(qltpar(5,stain)) - i + 1
            source(j) = -sum / real(ncels)
30       continue
      endif
40 continue
!
end
