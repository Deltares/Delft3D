      subroutine chlata(ngrid  ,nqlat ,nbran, qltpar ,grid  ,branch)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             CHLATA (CHeck LATeral Administration)
c
c Module description: Check the administration of lateral  stations.
c                     The following adaptations will be made:
c                     1) A reference outside a branch will be prevented.
c                     2) A point discharge at a node will be changed in
c                        a point discharge at the adjacent cell.
c                     3) A point discharge IN a structure will NOT be 
c                        stread over 2 adjacent cells but imposed IN 
c                        the structure cell.
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
c  1 ngrid             I  Number of grid points in network.
c  2 nqlat             I  Number of lateral discharge stations.
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
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: chlata.pf,v $
c Revision 1.3  1999/03/15  15:51:12  kuipe_j
c tabs removed
c
c Revision 1.2  1997/02/19  11:55:48  kuipe_j
c *** empty log message ***
c
c Revision 1.1  1997/02/19  11:50:21  kuipe_j
c Q Lateral im m3/s now
c
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ngrid ,nqlat   ,nbran
      integer grid  (ngrid)  ,branch(4,nbran)
      real    qltpar(9,nqlat)
c
c     Declaration of local variables
c
      integer istat  ,igr   ,igr1  ,igr2,  ibr
c
c     Include sobek constant file
c
      include '../include/sobcon.i'
c
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
