subroutine wqfqlt ( ngrid  ,x      ,qlat   ,qltpar ,&
&igr    ,factor ,qex    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFQLT (Water Quality Flow Q LaTeral)
!
! Module description: This routine calculates the exchange flow from a
!                     lateral discharge station to a segment.
!
!                     In the water flow module lateral discharges are
!                     transformed to point discharges on grid points
!                     i+1/2. All points get a different negative segment
!                     number. So for one lateral discharge station which
!                     is defined for a trajectory more than one negative
!                     segment can be defined. This routine calculates
!                     the point load in a particular grid point i+1/2.
!
!                     After calculating the lateral discharge the di-
!                     scharge is multiplied with a factor because one
!                     lateral discharge can take place in one or two
!                     segments.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 factor            I  Factor to divide qlat over segment.
!  5 igr               I  Gridpoint number.
!  1 ngrid             I  Number of grid points in network.
!  7 qex               IO Calculated exchange flow.
!  3 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
!  4 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
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
!  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
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
! $Log: wqfqlt.pf,v $
! Revision 1.4  1999/03/15  15:53:59  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:56:37  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:08:31  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:53  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:39  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer igr,&
   &ngrid

   real    factor,&
   &qex,&
   &qlat,&
   &qltpar (9),&
   &x(ngrid)
!
!     Variables
!
   integer  iopt, i1, i2
   real     lb, le
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Calculate divided discharge on gridpoint igr
!
   iopt = int(qltpar(4))
!
   if (iopt .eq. cpd1gp) then
!
!        Point discharge Q exactly in grid point
!
      qex = 0.5 * qlat

   elseif (iopt .eq. cpd1gc) then
!
!        Point discharge Q in one grid cell [igr,igr+1]
!
      qex = qlat

   elseif (iopt .eq. ctd1gc) then
!
!        Lateral discharge over one trajectory within one grid-cell
!
      lb  = qltpar(7)
      le  = qltpar(8)
      qex = (le - lb) * qlat

   elseif (iopt .eq. ctdmgc) then
!
!        Lateral discharge over trajectory over more than one
!        grid cell
!
!        i1 = grid point at begin of trajectory
!        i2 = grid point at end   of trajectory
!
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
!
!     Multiply Q-lat with factor
!
   qex = factor * qex

   return
end
