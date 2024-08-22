      subroutine cqlatg(ngrid  ,nqlat  ,istat  ,qlati  ,qltpar ,x     ,
     &                  qlatgr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             CQLATG (Calculate Q LaTeral per Grid point)
c
c Module description: Calculate the lateral discharges in grid points
c                     due to the lateral discharge of one discharge
c                     station.
c
c                     The discharge at the station can be defined over a
c                     trajectory (in one or more cells) (m2/sec) or at a
c                     grid cell or grid point (m3/sec). Several stations
c                     can contribute to the lateral discharge of one
c                     grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 istat             I  Lateral discharge station i.
c  1 ngrid             I  Number of grid points in network.
c  2 nqlat             I  Number of lateral discharge stations.
c  7 qlatgr(ngrid)     IO (i) = Actual lateral discharge in grid point
c                         i+1/2.
c  4 qlati             I  Lateral discharge in station i.
c  5 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
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
c  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: cqlatg.pf,v $
c Revision 1.5  1997/02/17  10:21:53  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.4  1997/01/20  13:13:40  kuipe_j
c Avoid devide by zero
c
c Revision 1.3  1996/12/02  10:04:56  kuipe_j
c avoid negative pointers
c
c Revision 1.2  1995/05/30  07:02:18  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:22  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:57  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ngrid ,nqlat ,istat
      real    qlati 
      real    qltpar(9,nqlat) ,x(ngrid) ,qlatgr(ngrid)
c
c     Declaration of local variables
c
      integer iopt  ,igr   ,i1   ,i2
      real    lb    ,le    
c
c     Include sobek constant file
c
      include '../include/sobcon.i'
c
c     Lateral discharges stations are completely defined in array
c     QLTPAR.
c
      iopt = int(qltpar (4,istat))
c
      if (iopt .eq. cpd1gp) then
c
c        Point discharge Q exactly in grid point.
c
c        [ Doc: S-FO-001.5KV / Eq. 6-4a/4b ]
c
c        Distribution over igr-1/2 and igr+1/2
c        Notice that lateral discharge qlatgr(i) is defined
c        in grid point i+1/2.
c
         igr           = int(qltpar(5,istat))
c
c        Grid cel igr-1/2
c
         qlatgr(igr-1) = qlatgr(igr-1) + 0.5 * qlati
c
c        Grid cel igr+1/2
c
         qlatgr(igr  ) = qlatgr(igr  ) + 0.5 * qlati 

      else if (iopt .eq. cpd1gc) then
c
c        Point discharge Q in one grid cell [igr,igr+1] .
c        [ Doc: S-FO-001.5KV / Eq. 6-4c ]
c
c        Assignment of q to grid cel igr+1/2
c
         igr         = int(qltpar(5,istat))
         qlatgr(igr) = qlatgr(igr) + qlati 

      else if (iopt .eq. ctd1gc) then
c
c        Lateral discharge over trajectory
c        within one grid cell.
c        [ Doc: S-FO-001.5KV / Eq. 6-6 ]
c
c        igr = grid cell
c        lb  = begin coordinate for cell/trajectory
c        le  = end coordinate for cell/trajectory
c
         igr = int(qltpar(5,istat))
         lb  = qltpar(7,istat)
         le  = qltpar(8,istat)

         qlatgr(igr) = qlatgr(igr) + (le - lb) * qlati
c
      else if (iopt .eq. ctdmgc) then
c
c        Lateral discharge over trajectory
c        over more than one grid cell.
c        [ Doc: S-FO-001.5KV / Eq. 6-5 ]
c
c        i1 = grid point at begin trajectory
c        i2 = grid point at end   trajectory
c        lb = begin coordinate for cell/trajectory
c        le = end coordinate for cell/trajectory
c
         i1 = int(qltpar(5,istat))
         i2 = int(qltpar(6,istat))
         lb = qltpar(7,istat)
         le = qltpar(8,istat)
c
c        Distribution over grid cells i1 until i2
c
         qlatgr(i1)  = qlatgr(i1) + (x(i1+1) - lb) * qlati

         qlatgr(i2)  = qlatgr(i2) + (le - x(i2)) * qlati

         do 20 igr = i1+1, i2-1
            qlatgr(igr) = qlatgr(igr) + (x(igr+1)-x(igr)) * qlati
   20    continue

      endif
c
      end
