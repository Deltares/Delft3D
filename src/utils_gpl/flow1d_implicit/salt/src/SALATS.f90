subroutine salats(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,&
&dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,&
&qltpar ,sltpar ,qlat   ,ntab   ,table  ,source,&
&qltgim )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SALATS (SAlt LATeral Salt)
!
! Module description: Calculate loads and outflowing lateral discharges.
!
!                     The source term is scematized partly implicit and
!                     partly explicit.
!
!                     Unconnected lateral discharge stations with out-
!                     flow are treated implicitly. This is done by the
!                     summation of all the relevant outflowing dischar-
!                     ges at every grid point (stored in array QLTGIM).
!                     For the variable QLAT,i+1/2 in the matrix coeffi-
!                     cients [ Doc: S-FO-001.5KV / Eq. 22-25 ] these
!                     sums are used.
!
!                     Inflowing discharges at lateral salt discharge
!                     stations, loads due to connected lateral discharge
!                     stations (in and outflow) and waste loads are
!                     treated explicitly. In that case the total load in
!                     every grid point will be calculated. The loads are
!                     stored in array SOURCE. This is variable Ss,i+1/2
!                     [ Doc: S-FO-001.5KV / Eq. 22-25 ].
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 csa1              P  -
! 12 csa2              P  -
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
!  9 psi               P  -
! 15 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 19 qltgim(ngrid)     O  Sum of only outgoing lateral discharges in
!                         every grid point at time t(n+1/2).
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
!  8 theta             P  -
!  6 time              I  Actual time level tn+1. in sec.
! 10 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cqlatg  Calculate Q LaTeral per Grid point
! inttab  INTerpolate in TABle
! saload  SAlt LOAD
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
! $Log: salats.pf,v $
! Revision 1.2  1995/05/30  07:06:08  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:50  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:14  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:49  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ngrid ,nqlat   ,nslat ,maxtab  ,ntabm
   real    theta ,psi
   integer ntab  (4,maxtab)
   real    x     (ngrid)  ,csa1  (ngrid)  ,csa2(ngrid),&
   &source(ngrid)  ,qltgim(ngrid)  ,&
   &qltpar(9,*)    ,sltpar(9,*)    ,qlat(*)    ,table (ntabm)
   double  precision time ,dt
!
!     Declaration of local variables
!
   integer sstat ,qstat ,itab   ,i
   real    loadst
!
!     Initialize.
!
   do 10 i= 1 , ngrid
      source(i) = 0.
      qltgim(i) = 0.
10 continue
!
!     At every grid point the sum of the outgoing lateral discharges
!     is calculated and stored in QLTGIM.
!     [ Doc. S-FO-001.5KV / Eq. 22-9 ]
!
   do 20 qstat = 1 , nqlat
      if (qlat(qstat).lt.0. .and. int(qltpar(2,qstat)).le.3) then
         call cqlatg(ngrid  ,nqlat  ,qstat  ,qlat(qstat)  ,qltpar  ,&
         &x      ,qltgim )
      endif
20 continue
!
!     Calculation of loads at lateral discharge stations with salt load.
!
   call saload(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,&
   &dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,&
   &qltpar ,sltpar ,qlat   ,ntab   ,table  ,source)
!
!     Add the dry wast loads to the already obtained loads.
!     [ Doc. S-FO-001.5KV / Eq. 22-12 ]
!
   do 30 sstat = 1 , nslat
      if (int(sltpar(2,sstat)) .eq. 0) then
!
!           Get current load from a user defined table
!
!           itab       : TABLE number load=f(t)
!           time-dt/2. : t(n+1/2)
!           loadst     : kg/m/s for diffusive load
!                        kg/s for point load
!
         itab  = int(sltpar(3,sstat))
!
         call inttab (ntab (1,itab)      ,ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time-dt/2d0        ,loadst      )
!
!           Distribution of load over grid points.
!           (Array SLTPAR has the same structure as QLTPAR, so
!           routine FLQLTG can be used).
!
         call cqlatg(ngrid  ,nslat  ,sstat  ,loadst   ,sltpar  ,&
         &x      ,source )
      endif
30 continue
!
end
