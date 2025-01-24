subroutine cqlatg(ngrid  ,nqlat  ,istat  ,qlati  ,qltpar ,x     ,&
&qlatgr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             CQLATG (Calculate Q LaTeral per Grid point)
!
! Module description: Calculate the lateral discharges in grid points
!                     due to the lateral discharge of one discharge
!                     station.
!
!                     The discharge at the station can be defined over a
!                     trajectory (in one or more cells) (m2/sec) or at a
!                     grid cell or grid point (m3/sec). Several stations
!                     can contribute to the lateral discharge of one
!                     grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 istat             I  Lateral discharge station i.
!  1 ngrid             I  Number of grid points in network.
!  2 nqlat             I  Number of lateral discharge stations.
!  7 qlatgr(ngrid)     IO (i) = Actual lateral discharge in grid point
!                         i+1/2.
!  4 qlati             I  Lateral discharge in station i.
!  5 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
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
!  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: cqlatg.pf,v $
! Revision 1.5  1997/02/17  10:21:53  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.4  1997/01/20  13:13:40  kuipe_j
! Avoid devide by zero
!
! Revision 1.3  1996/12/02  10:04:56  kuipe_j
! avoid negative pointers
!
! Revision 1.2  1995/05/30  07:02:18  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:22  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:57  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:58  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ngrid ,nqlat ,istat
   real    qlati
   real    qltpar(9,nqlat) ,x(ngrid) ,qlatgr(ngrid)
!
!     Declaration of local variables
!
   integer iopt  ,igr   ,i1   ,i2
   real    lb    ,le
!
!     Include sobek constant file
!
   include '..\include\sobcon.i'
!
!     Lateral discharges stations are completely defined in array
!     QLTPAR.
!
   iopt = int(qltpar (4,istat))
!
   if (iopt .eq. cpd1gp) then
!
!        Point discharge Q exactly in grid point.
!
!        [ Doc: S-FO-001.5KV / Eq. 6-4a/4b ]
!
!        Distribution over igr-1/2 and igr+1/2
!        Notice that lateral discharge qlatgr(i) is defined
!        in grid point i+1/2.
!
      igr           = int(qltpar(5,istat))
!
!        Grid cel igr-1/2
!
      qlatgr(igr-1) = qlatgr(igr-1) + 0.5 * qlati
!
!        Grid cel igr+1/2
!
      qlatgr(igr  ) = qlatgr(igr  ) + 0.5 * qlati

   else if (iopt .eq. cpd1gc) then
!
!        Point discharge Q in one grid cell [igr,igr+1] .
!        [ Doc: S-FO-001.5KV / Eq. 6-4c ]
!
!        Assignment of q to grid cel igr+1/2
!
      igr         = int(qltpar(5,istat))
      qlatgr(igr) = qlatgr(igr) + qlati

   else if (iopt .eq. ctd1gc) then
!
!        Lateral discharge over trajectory
!        within one grid cell.
!        [ Doc: S-FO-001.5KV / Eq. 6-6 ]
!
!        igr = grid cell
!        lb  = begin coordinate for cell/trajectory
!        le  = end coordinate for cell/trajectory
!
      igr = int(qltpar(5,istat))
      lb  = qltpar(7,istat)
      le  = qltpar(8,istat)

      qlatgr(igr) = qlatgr(igr) + (le - lb) * qlati
!
   else if (iopt .eq. ctdmgc) then
!
!        Lateral discharge over trajectory
!        over more than one grid cell.
!        [ Doc: S-FO-001.5KV / Eq. 6-5 ]
!
!        i1 = grid point at begin trajectory
!        i2 = grid point at end   trajectory
!        lb = begin coordinate for cell/trajectory
!        le = end coordinate for cell/trajectory
!
      i1 = int(qltpar(5,istat))
      i2 = int(qltpar(6,istat))
      lb = qltpar(7,istat)
      le = qltpar(8,istat)
!
!        Distribution over grid cells i1 until i2
!
      qlatgr(i1)  = qlatgr(i1) + (x(i1+1) - lb) * qlati

      qlatgr(i2)  = qlatgr(i2) + (le - x(i2)) * qlati

      do 20 igr = i1+1, i2-1
         qlatgr(igr) = qlatgr(igr) + (x(igr+1)-x(igr)) * qlati
20    continue

   endif
!
end
