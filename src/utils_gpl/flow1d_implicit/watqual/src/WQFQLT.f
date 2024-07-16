      subroutine wqfqlt ( ngrid  ,x      ,qlat   ,qltpar ,
     +                    igr    ,factor ,qex    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFQLT (Water Quality Flow Q LaTeral)
c
c Module description: This routine calculates the exchange flow from a
c                     lateral discharge station to a segment.
c
c                     In the water flow module lateral discharges are
c                     transformed to point discharges on grid points
c                     i+1/2. All points get a different negative segment
c                     number. So for one lateral discharge station which
c                     is defined for a trajectory more than one negative
c                     segment can be defined. This routine calculates
c                     the point load in a particular grid point i+1/2.
c
c                     After calculating the lateral discharge the di-
c                     scharge is multiplied with a factor because one
c                     lateral discharge can take place in one or two
c                     segments.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 factor            I  Factor to divide qlat over segment.
c  5 igr               I  Gridpoint number.
c  1 ngrid             I  Number of grid points in network.
c  7 qex               IO Calculated exchange flow.
c  3 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c  4 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
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
c  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
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
c $Log: wqfqlt.pf,v $
c Revision 1.4  1999/03/15  15:53:59  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:56:37  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:08:31  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:53  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:39  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer igr,
     +        ngrid

      real    factor,
     +        qex,
     +        qlat,
     +        qltpar (9),
     +        x(ngrid)
c
c     Variables
c
      integer  iopt, i1, i2
      real     lb, le
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Calculate divided discharge on gridpoint igr
c
      iopt = int(qltpar(4))
c
      if (iopt .eq. cpd1gp) then
c
c        Point discharge Q exactly in grid point
c
         qex = 0.5 * qlat

      elseif (iopt .eq. cpd1gc) then
c
c        Point discharge Q in one grid cell [igr,igr+1]
c
         qex = qlat

      elseif (iopt .eq. ctd1gc) then
c
c        Lateral discharge over one trajectory within one grid-cell
c
         lb  = qltpar(7)
         le  = qltpar(8)
         qex = (le - lb) * qlat

      elseif (iopt .eq. ctdmgc) then
c
c        Lateral discharge over trajectory over more than one
c        grid cell
c
c        i1 = grid point at begin of trajectory
c        i2 = grid point at end   of trajectory
c
         i1 = int(qltpar(5))
         i2 = int(qltpar(6))
         lb =     qltpar(7)
         le =     qltpar(8)

         if     (igr .eq. i1) then
            qex = (x(i1+1) - lb) * qlat

         elseif (igr .eq. i2) then
            qex = (le - x(i2)) * qlat

         else
            qex = qlat * (x(i2) - x(i1))

         endif
      endif
c
c     Multiply Q-lat with factor
c
      qex = factor * qex

      return
      end
