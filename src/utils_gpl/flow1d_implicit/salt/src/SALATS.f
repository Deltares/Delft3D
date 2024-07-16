      subroutine salats(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,
     &                  dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,
     &                  qltpar ,sltpar ,qlat   ,ntab   ,table  ,source,
     &                  qltgim )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SALATS (SAlt LATeral Salt)
c
c Module description: Calculate loads and outflowing lateral discharges.
c
c                     The source term is scematized partly implicit and
c                     partly explicit.
c
c                     Unconnected lateral discharge stations with out-
c                     flow are treated implicitly. This is done by the
c                     summation of all the relevant outflowing dischar-
c                     ges at every grid point (stored in array QLTGIM).
c                     For the variable QLAT,i+1/2 in the matrix coeffi-
c                     cients [ Doc: S-FO-001.5KV / Eq. 22-25 ] these
c                     sums are used.
c
c                     Inflowing discharges at lateral salt discharge
c                     stations, loads due to connected lateral discharge
c                     stations (in and outflow) and waste loads are
c                     treated explicitly. In that case the total load in
c                     every grid point will be calculated. The loads are
c                     stored in array SOURCE. This is variable Ss,i+1/2
c                     [ Doc: S-FO-001.5KV / Eq. 22-25 ].
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 csa1              P  -
c 12 csa2              P  -
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
c  9 psi               P  -
c 15 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 19 qltgim(ngrid)     O  Sum of only outgoing lateral discharges in
c                         every grid point at time t(n+1/2).
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
c  8 theta             P  -
c  6 time              I  Actual time level tn+1. in sec.
c 10 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cqlatg  Calculate Q LaTeral per Grid point
c inttab  INTerpolate in TABle
c saload  SAlt LOAD
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
c $Log: salats.pf,v $
c Revision 1.2  1995/05/30  07:06:08  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:50  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:14  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:49  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ngrid ,nqlat   ,nslat ,maxtab  ,ntabm
      real    theta ,psi
      integer ntab  (4,maxtab)
      real    x     (ngrid)  ,csa1  (ngrid)  ,csa2(ngrid),
     &        source(ngrid)  ,qltgim(ngrid)  ,
     &        qltpar(9,*)    ,sltpar(9,*)    ,qlat(*)    ,table (ntabm)
      double  precision time ,dt
c
c     Declaration of local variables
c
      integer sstat ,qstat ,itab   ,i
      real    loadst
c
c     Initialize.
c
      do 10 i= 1 , ngrid
         source(i) = 0.
         qltgim(i) = 0.
   10 continue
c
c     At every grid point the sum of the outgoing lateral discharges
c     is calculated and stored in QLTGIM.
c     [ Doc. S-FO-001.5KV / Eq. 22-9 ]
c
      do 20 qstat = 1 , nqlat
         if (qlat(qstat).lt.0. .and. int(qltpar(2,qstat)).le.3) then
            call cqlatg(ngrid  ,nqlat  ,qstat  ,qlat(qstat)  ,qltpar  ,
     &                  x      ,qltgim )
         endif
   20 continue
c
c     Calculation of loads at lateral discharge stations with salt load.
c
      call saload(ngrid  ,nqlat  ,nslat  ,maxtab ,ntabm  ,time  ,
     &            dt     ,theta  ,psi    ,x      ,csa1   ,csa2  ,
     &            qltpar ,sltpar ,qlat   ,ntab   ,table  ,source)
c
c     Add the dry wast loads to the already obtained loads.
c     [ Doc. S-FO-001.5KV / Eq. 22-12 ]
c
      do 30 sstat = 1 , nslat
         if (int(sltpar(2,sstat)) .eq. 0) then
c
c           Get current load from a user defined table
c
c           itab       : TABLE number load=f(t)
c           time-dt/2. : t(n+1/2)
c           loadst     : kg/m/s for diffusive load
c                        kg/s for point load
c
            itab  = int(sltpar(3,sstat))
c
            call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                   table(ntab(2,itab)),
     &                   table(ntab(3,itab)),
     &                   time-dt/2d0        ,loadst      )
c
c           Distribution of load over grid points.
c           (Array SLTPAR has the same structure as QLTPAR, so
c           routine FLQLTG can be used).
c
            call cqlatg(ngrid  ,nslat  ,sstat  ,loadst   ,sltpar  ,
     &                  x      ,source )
         endif
   30 continue
c
      end
