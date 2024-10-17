subroutine chlata(ngrid  ,nqlat ,nbran, qltpar ,grid  ,branch)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             CHLATA (CHeck LATeral Administration)
!
! Module description: Check the administration of lateral  stations.
!                     The following adaptations will be made:
!                     1) A reference outside a branch will be prevented.
!                     2) A point discharge at a node will be changed in
!                        a point discharge at the adjacent cell.
!                     3) A point discharge IN a structure will NOT be
!                        stread over 2 adjacent cells but imposed IN
!                        the structure cell.
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
!  1 ngrid             I  Number of grid points in network.
!  2 nqlat             I  Number of lateral discharge stations.
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
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: chlata.pf,v $
! Revision 1.3  1999/03/15  15:51:12  kuipe_j
! tabs removed
!
! Revision 1.2  1997/02/19  11:55:48  kuipe_j
! *** empty log message ***
!
! Revision 1.1  1997/02/19  11:50:21  kuipe_j
! Q Lateral im m3/s now
!
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ngrid ,nqlat   ,nbran
   integer grid  (ngrid)  ,branch(4,nbran)
   real    qltpar(9,nqlat)
!
!     Declaration of local variables
!
   integer istat  ,igr   ,igr1  ,igr2,  ibr
!
!     Include sobek constant file
!
   include '..\include\sobcon.i'
!
   do 10  istat = 1, nqlat
      if (int(qltpar(4,istat)) .eq. cpd1gp) then
         igr = int(qltpar(5,istat))
         ibr = int(qltpar(1,istat))
         if (igr.eq.branch(3,ibr)) then
            qltpar(4,istat) = cpd1gc
         else if (igr.eq.branch(4,ibr)) then
            qltpar(4,istat) = cpd1gc
            qltpar(5,istat) = igr-1
         endif
         if (grid(igr).eq.cstrcl) then
            qltpar(4,istat) = cpd1gc
         endif
      else if (int(qltpar(4,istat)) .eq. ctdmgc) then
         igr1 = int(qltpar(5,istat))
         igr2 = int(qltpar(6,istat))
         ibr  = int(qltpar(1,istat))
         if (igr1.eq.branch(3,ibr)-1) then
            qltpar(5,istat) = igr1+1
         else if (igr2.eq.branch(4,ibr)) then
            qltpar(6,istat) = igr2-1
         endif
      endif
10 continue

end
